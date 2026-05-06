{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.CFGPipeline
--
-- Explicit pipeline for the canonical CFG loop representation.
-- Combines optimization, tiling, vectorization, and parallelization in order.
module Language.Hydrangea.CFGPipeline
  ( -- * Pipeline configuration
    PipelineOptions(..)
  , defaultPipelineOptions
    -- * Pipelines
  , preparePolyhedralProgramWithOptions
  , optimizePipeline2
  , optimizePipelineWithTiling
  , vectorizePipeline2
  , vectorizePipelineWithWidth
  , vectorizePipelineWithOptions
  , parallelPipeline2
  , parallelPipelineWithWidth
  , pipelineWithOptions
  , metalPipeline2
  , metalPipelineWithTiling
  , fullPipeline2
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
fixpointOpt = optimizeStmts2

-- | After iterate-allocation hoisting, run only the structurally safe cleanup
-- passes that expose invariant case splits without touching the ping-pong
-- swap sequence.
postHoistIterateFixpoint :: [Stmt] -> [Stmt]
postHoistIterateFixpoint = go 20
  where
    go :: Int -> [Stmt] -> [Stmt]
    go 0 stmts = stmts
    go n stmts =
      let stmts' = unswitchLoopInvariantIf2 stmts
      in if stmts' == stmts then stmts else go (n - 1) stmts'

-- | Normalize iterate bodies before downstream scheduling/vectorization.
-- Hoisting the ping-pong allocs can expose additional invariant shape-case
-- branching, so run the scalar cleanup pass again afterwards.
normalizeIterateBodies :: [Stmt] -> [Stmt]
normalizeIterateBodies = postHoistIterateFixpoint . hoistIterateAllocs2 . fixpointOpt

stmtsNeedIterateNormalization :: [Stmt] -> Bool
stmtsNeedIterateNormalization = any stmtNeedsIterateNormalization
  where
    stmtNeedsIterateNormalization :: Stmt -> Bool
    stmtNeedsIterateNormalization stmt = case stmt of
      SLoop spec body ->
        (lsRole spec == LoopIterate && stmtsContainIf body)
          || stmtsNeedIterateNormalization body
      SIf _ thn els ->
        stmtsNeedIterateNormalization thn || stmtsNeedIterateNormalization els
      _ ->
        False

    stmtsContainIf :: [Stmt] -> Bool
    stmtsContainIf = any containsIf

    containsIf :: Stmt -> Bool
    containsIf stmt = case stmt of
      SIf _ thn els -> True || stmtsContainIf thn || stmtsContainIf els
      SLoop _ body -> stmtsContainIf body
      _ -> False

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
cleanupProgram (Program procs) =
  Program
    [ proc
        { procBody = hoistIterateAllocs2 (fixpointOpt (procBody proc))
        }
    | proc <- procs
    ]

serializeParallelStmts :: [Stmt] -> [Stmt]
serializeParallelStmts = map go
  where
    go stmt = case stmt of
      SLoop spec body ->
        let exec' = case lsExec spec of
              Parallel {} -> Serial
              other -> other
        in SLoop spec { lsExec = exec' } (serializeParallelStmts body)
      SIf cond thn els ->
        SIf cond (serializeParallelStmts thn) (serializeParallelStmts els)
      other ->
        other

serializeParallelProgram :: Program -> Program
serializeParallelProgram (Program procs) =
  Program [proc { procBody = serializeParallelStmts (procBody proc) } | proc <- procs]

-- | Optimization-only pipeline.
optimizePipeline2 :: [Stmt] -> [Stmt]
optimizePipeline2 = optimizePipelineWithTiling True

-- | Optimization-only pipeline with optional tiling.
optimizePipelineWithTiling :: Bool -> [Stmt] -> [Stmt]
optimizePipelineWithTiling enableTiling stmts =
  let stmts' = optimizeStmts2 stmts
      stmts'' = if enableTiling then tileStmts2 stmts' else stmts'
  in  optimizeStmts2 stmts''

-- | Run the CFG cleanup steps that prepare loop nests for polyhedral
-- extraction or scheduling.
preparePolyhedralProgramWithOptions :: PipelineOptions -> Program -> Program
preparePolyhedralProgramWithOptions _opts (Program procs) =
  Program
    [ proc
        { procBody =
            let body' = optimizePipelineWithTiling False (procBody proc)
            in if stmtsNeedIterateNormalization body'
                 then normalizeIterateBodies body'
                 else body'
        }
    | proc <- procs
    ]

-- | Run optimization followed by vectorization using explicit pipeline options.
vectorizePipelineWithOptions :: PipelineOptions -> Program -> Program
vectorizePipelineWithOptions opts prog =
  let prepared = preparePolyhedralProgramWithOptions opts prog
      cleanupPolyhedral scheduled
        | programHasConditionalIterates scheduled = scheduled
        | otherwise = cleanupProgram scheduled
      optimized
        | poEnablePolyhedral opts && poEnableTiling opts =
            cleanupPolyhedral (polyhedralTileProgram2 prepared)
        | poEnableTiling opts = cleanupProgram (polyhedralIdentityTileProgram2 prepared)
        | poEnablePolyhedral opts =
            cleanupPolyhedral (polyhedralProgram2 prepared)
        | otherwise = applyHoist prepared
      applyHoist (Program procs) =
        Program [proc { procBody = hoistIterateAllocs2 (procBody proc) } | proc <- procs]
      optimizedNoParallel
        | poEnableParallelization opts = optimized
        | otherwise = serializeParallelProgram optimized
  in  vectorizeProgram2WithWidthAndExplicit
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
vectorizePipeline2 :: Program -> Program
vectorizePipeline2 = vectorizePipelineWithWidth defaultVectorWidth

-- | Run optimization followed by loop parallelization using the given SIMD lane width.
parallelPipelineWithWidth :: Int -> Program -> Program
parallelPipelineWithWidth w prog =
  parallelizeProgram2 (vectorizePipelineWithWidth w prog)

-- | Run optimization followed by loop parallelization.
parallelPipeline2 :: Program -> Program
parallelPipeline2 = parallelPipelineWithWidth defaultVectorWidth

-- | Run optimization, vectorization, and optional parallelization.
pipelineWithOptions :: PipelineOptions -> Program -> Program
pipelineWithOptions opts prog =
  let vecProg = vectorizePipelineWithOptions opts prog
  in if poEnableParallelization opts
       then parallelizeProgram2 vecProg
       else vecProg

-- | Optimize and parallelize without SIMD vectorization. Suitable for GPU
-- backends where SIMD is handled by the hardware (e.g. Metal/MSL).
metalPipeline2 :: Program -> Program
metalPipeline2 = metalPipelineWithTiling True

-- | Optimize and parallelize without SIMD vectorization with optional tiling.
metalPipelineWithTiling :: Bool -> Program -> Program
metalPipelineWithTiling enableTiling (Program procs) =
  let optimized = [proc { procBody = optimizePipelineWithTiling enableTiling (procBody proc) } | proc <- procs]
  in  parallelizeProgram2 (Program optimized)

-- | Run the full CFG pipeline: optimize, vectorize, parallelize, then clean up again.
fullPipeline2 :: Program -> Program
fullPipeline2 prog =
  case parallelizeProgram2 (vectorizePipeline2 prog) of
    Program procs ->
      Program [proc { procBody = fixpointOpt (procBody proc) } | proc <- procs]
