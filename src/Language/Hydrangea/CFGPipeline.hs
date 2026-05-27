{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.CFGPipeline
--
-- Reusable CFG program pipelines.
--
-- This module assembles the CFG cleanup, tiling, vectorization, polyhedral,
-- and parallelization passes into a handful of program-level entry points.
-- These are library defaults: the CLI in @app/Main.hs@ can override
-- 'defaultPipelineOptions' to expose different end-user defaults.
module Language.Hydrangea.CFGPipeline
  ( -- * Pipeline configuration
    PipelineOptions(..)
  , defaultPipelineOptions
    -- * Pipelines
  , preparePolyhedralProgramWithOptions
  , optimizePipeline
  , optimizePipelineWithTiling
  , vectorizePipeline
  , vectorizePipelineWithWidth
  , vectorizePipelineWithOptions
  , parallelPipeline
  , parallelPipelineWithWidth
  , pipelineWithOptions
  , metalPipeline
  , metalPipelineWithTiling
  , fullPipeline
  ) where


import Language.Hydrangea.CFG
import Language.Hydrangea.CFGOpt
import Language.Hydrangea.Parallelize
import Language.Hydrangea.Polyhedral
import Language.Hydrangea.Tile
import Language.Hydrangea.Vectorize

data PipelineOptions = PipelineOptions
  { poEnableTiling :: Bool
  , poEnablePolyhedral :: Bool
  , poEnableExplicitVectorization :: Bool
  , poEnableParallelization :: Bool
  , poVectorWidth :: Int
  }

-- | Library-level default pipeline settings.
--
-- These defaults favor the C backend's optimization path. The CLI applies its
-- own flag-derived defaults on top when compiling user programs.
defaultPipelineOptions :: PipelineOptions
defaultPipelineOptions = PipelineOptions
  { poEnableTiling = True
  , poEnablePolyhedral = False
  , poEnableExplicitVectorization = True
  , poEnableParallelization = False
  , poVectorWidth = defaultVectorWidth
  }

-- | Run the local optimization pass to fixpoint.
fixpointOpt :: [Stmt] -> [Stmt]
fixpointOpt = optimizeStmts

-- | After iterate-allocation hoisting, run only the structurally safe cleanup
-- passes that expose invariant case splits without touching the ping-pong
-- swap sequence.
postHoistIterateFixpoint :: [Stmt] -> [Stmt]
postHoistIterateFixpoint = go 20
  where
    go :: Int -> [Stmt] -> [Stmt]
    go 0 stmts = stmts
    go n stmts =
      let stmts' = unswitchLoopInvariantIf stmts
      in if stmts' == stmts then stmts else go (n - 1) stmts'

-- | Normalize iterate bodies before downstream scheduling/vectorization.
-- Hoisting the ping-pong allocs can expose additional invariant shape-case
-- branching, so run the scalar cleanup pass again afterwards.
normalizeIterateBodies :: [Stmt] -> [Stmt]
normalizeIterateBodies = postHoistIterateFixpoint . hoistIterateAllocs . fixpointOpt

mapProcBodies :: ([Stmt] -> [Stmt]) -> Program -> Program
mapProcBodies rewriteBody (Program procs) =
  Program [proc { procBody = rewriteBody (procBody proc) } | proc <- procs]

programHasConditionalIterates :: Program -> Bool
programHasConditionalIterates (Program procs) = any (stmtsHaveConditionalIterate . procBody) procs
  where
    stmtsHaveConditionalIterate :: [Stmt] -> Bool
    stmtsHaveConditionalIterate = any stmtHasConditionalIterate

    stmtHasConditionalIterate :: Stmt -> Bool
    stmtHasConditionalIterate stmt = case stmt of
      SLoop _ body ->
        stmtsHaveConditionalIterate body
      SIf _ thn els ->
        branchHasIterate thn
          || branchHasIterate els
          || stmtsHaveConditionalIterate thn
          || stmtsHaveConditionalIterate els
      _ ->
        False

    branchHasIterate :: [Stmt] -> Bool
    branchHasIterate = any containsIterate

    containsIterate :: Stmt -> Bool
    containsIterate stmt = case stmt of
      SLoop spec body ->
        lsRole spec == LoopIterate || stmtsContainIterate body
      SIf _ thn els ->
        stmtsContainIterate thn || stmtsContainIterate els
      _ ->
        False

    stmtsContainIterate :: [Stmt] -> Bool
    stmtsContainIterate = any containsIterate

cleanupProgram :: Program -> Program
cleanupProgram = mapProcBodies (hoistIterateAllocs . fixpointOpt)

serializeParallelStmts :: [Stmt] -> [Stmt]
serializeParallelStmts = concatMap go
  where
    go stmt = case stmt of
      SLoop spec body ->
        let exec' = case lsExec spec of
              Parallel {} -> Serial
              Workshare {} -> Serial
              other -> other
        in [SLoop spec { lsExec = exec' } (serializeParallelStmts body)]
      SParallelRegion body ->
        serializeParallelStmts body
      SIf cond thn els ->
        [SIf cond (serializeParallelStmts thn) (serializeParallelStmts els)]
      other ->
        [other]

serializeParallelProgram :: Program -> Program
serializeParallelProgram = mapProcBodies serializeParallelStmts

-- | Optimization-only pipeline.
optimizePipeline :: [Stmt] -> [Stmt]
optimizePipeline = optimizePipelineWithTiling True

-- | Optimization-only pipeline with optional tiling.
optimizePipelineWithTiling :: Bool -> [Stmt] -> [Stmt]
optimizePipelineWithTiling enableTiling stmts =
  let stmts' = optimizeStmts stmts
      stmts'' = if enableTiling then tileStmts stmts' else stmts'
  in  optimizeStmts stmts''

-- | Run the CFG cleanup steps that prepare loop nests for polyhedral
-- extraction or scheduling.
--
-- We unconditionally apply 'normalizeIterateBodies' so that
-- 'hoistIterateAllocs' always runs before polyhedral extraction. Hoisting
-- the buffer allocation out of the iterate loop is required for the
-- wavefront schedule to fire correctly: the wavefront ring-buffer logic
-- assumes the "next" allocation is live outside the SCoP.  The previous
-- conditional (only hoist when the iterate body contains an @if@) caused
-- the allocation to stay inside the SCoP, making the wavefront use an
-- undeclared variable.
preparePolyhedralProgramWithOptions :: PipelineOptions -> Program -> Program
preparePolyhedralProgramWithOptions _opts =
  mapProcBodies (normalizeIterateBodies . optimizePipelineWithTiling False)

-- | Run optimization followed by vectorization using explicit pipeline options.
vectorizePipelineWithOptions :: PipelineOptions -> Program -> Program
vectorizePipelineWithOptions opts prog =
  let prepared = preparePolyhedralProgramWithOptions opts prog
      cleanupPolyhedral scheduled
        | programHasConditionalIterates scheduled = scheduled
        | otherwise = cleanupProgram scheduled
      optimized
        | poEnablePolyhedral opts && poEnableTiling opts =
            cleanupPolyhedral (polyhedralTileProgram prepared)
        | poEnableTiling opts = cleanupProgram (polyhedralIdentityTileProgram prepared)
        | poEnablePolyhedral opts =
            cleanupPolyhedral (polyhedralProgram prepared)
        | otherwise = applyHoist prepared
      applyHoist = mapProcBodies hoistIterateAllocs
      optimizedNoParallel
        | poEnableParallelization opts = optimized
        | otherwise = serializeParallelProgram optimized
  in  vectorizeProgramWithWidthAndExplicit
        (poVectorWidth opts)
        (poEnableExplicitVectorization opts)
        optimizedNoParallel

-- | Run optimization followed by vectorization using the given SIMD lane width.
vectorizePipelineWithWidth :: Int -> Program -> Program
vectorizePipelineWithWidth w =
  vectorizePipelineWithOptions
    defaultPipelineOptions
      { poVectorWidth = w
      , poEnableTiling = True
      , poEnableExplicitVectorization = True
      }

-- | Run optimization followed by vectorization.
vectorizePipeline :: Program -> Program
vectorizePipeline = vectorizePipelineWithWidth defaultVectorWidth

-- | Run optimization followed by loop parallelization using the given SIMD lane width.
parallelPipelineWithWidth :: Int -> Program -> Program
parallelPipelineWithWidth w prog =
  parallelizeProgram (vectorizePipelineWithWidth w prog)

-- | Run optimization followed by loop parallelization.
parallelPipeline :: Program -> Program
parallelPipeline = parallelPipelineWithWidth defaultVectorWidth

-- | Run optimization, vectorization, and optional parallelization.
pipelineWithOptions :: PipelineOptions -> Program -> Program
pipelineWithOptions opts prog =
  let vecProg = vectorizePipelineWithOptions opts prog
  in if poEnableParallelization opts
       then parallelizeProgram vecProg
       else vecProg

-- | Optimize and parallelize without SIMD vectorization. Suitable for GPU
-- backends where SIMD is handled by the hardware (e.g. Metal/MSL).
metalPipeline :: Program -> Program
metalPipeline = metalPipelineWithTiling True

-- | Optimize and parallelize without SIMD vectorization with optional tiling.
metalPipelineWithTiling :: Bool -> Program -> Program
metalPipelineWithTiling enableTiling =
  parallelizeProgram . mapProcBodies (optimizePipelineWithTiling enableTiling)

-- | Run the full CFG pipeline: optimize, vectorize, parallelize, then clean up again.
fullPipeline :: Program -> Program
fullPipeline prog =
  case parallelizeProgram (vectorizePipeline prog) of
    optimized ->
      mapProcBodies fixpointOpt optimized
