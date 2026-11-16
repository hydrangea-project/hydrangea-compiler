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
  , optimizePipelineWithTiling
  , vectorizePipeline
  , vectorizePipelineWithWidth
  , vectorizePipelineWithOptions
  , parallelPipeline
  , parallelPipelineWithWidth
  , pipelineWithOptions
  , metalPipeline
  , metalPipelineWithTiling
  ) where

import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Map.Strict qualified as M
import Language.Hydrangea.CFG
import Language.Hydrangea.CFGCore (Atom (..), CType (..), RHS (..))
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
normalizeIterateBodies =
  postHoistIterateFixpoint . hoistIterateAllocs . scalarizeIteratePairs . fixpointOpt

-- | Apply the same statement-list rewrite to every procedure body in a program.
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
preparePolyhedralProgramWithOptions _opts (Program procs) =
  Program [proc { procBody = prepare proc } | proc <- procs]
  where
    -- Loop scratch buffers are hoisted here because it needs the procedure's
    -- array facts.
    prepare proc =
      normalizeIterateBodies
        (optimizeStmts
          (hoistLoopArrayTemps (procArrayFacts proc)
            (optimizePipelineWithTiling False (procBody proc))))

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
metalPipelineWithTiling enableTiling (Program procs) =
  let optimized =
        [ proc' { procBody = optimizePipelineWithTiling enableTiling (procBody proc') }
        | proc <- procs
        , let proc' = copyScatterTargets proc ]
  in  parallelizeProgram (Program optimized)

-- | Replace each scatter target @dst = hyd_array_own(t)@ with an explicit
-- copy of @t@.
--
-- The Metal backend manages its buffers itself and has no reference counts,
-- so a scatter there always writes into a fresh copy of its defaults.  An
-- iterate's @cur = hyd_array_own(cur)@ is left alone.  A stream snapshot of
-- @dst@ (see 'Language.Hydrangea.Lowering.privatizeScatterStreams') is taken
-- from @t@ instead: the two hold the same contents there, and the Metal host
-- harness expects the snapshot to read an array defined outside it.
copyScatterTargets :: Proc -> Proc
copyScatterTargets proc =
  proc { procBody = go (procBody proc), procTypeEnv = M.union newTypes (procTypeEnv proc) }
  where
    sources = M.fromList [(dst, t) | (dst, t) <- ownPairs (procBody proc)]
    ownPairs = concatMap $ \stmt -> case stmt of
      SAssign dst (RCall "hyd_array_own" [AVar t]) | dst /= t -> [(dst, t)]
      SLoop _ b -> ownPairs b
      SParallelRegion b -> ownPairs b
      SIf _ thn els -> ownPairs thn ++ ownPairs els
      _ -> []
    fromSource a = case a of
      AVar v | Just t <- M.lookup v sources -> AVar t
      _ -> a
    snapshotFromSource stmt = case stmt of
      SAssign v (RArrayShape a) -> SAssign v (RArrayShape (fromSource a))
      SLoop spec@LoopSpec {lsRole = LoopCopy} b ->
        SLoop spec [ case st of
                       SAssign e (RArrayLoad a k) -> SAssign e (RArrayLoad (fromSource a) k)
                       _ -> st
                   | st <- b ]
      _ -> stmt
    types = procTypeEnv proc
    elemTyOf dst = case M.lookup dst types of
      Just (CTArray et) -> et
      _ -> CTInt64
    names dst = (dst <> BS.pack "_shp", dst <> BS.pack "_n", dst <> BS.pack "_k", dst <> BS.pack "_e")
    owns = collect (procBody proc)
    collect = concatMap $ \stmt -> case stmt of
      SAssign dst (RCall "hyd_array_own" [AVar t]) | dst /= t -> [dst]
      SLoop _ b -> collect b
      SParallelRegion b -> collect b
      SIf _ thn els -> collect thn ++ collect els
      _ -> []
    newTypes = M.fromList $ concat
      [ [(shp, CTTuple), (n, CTInt64), (k, CTInt64), (e, elemTyOf dst)]
      | dst <- owns, let (shp, n, k, e) = names dst ]
    go = concatMap $ \stmt -> case stmt of
      SAssign dst (RCall "hyd_array_own" [src@(AVar t)]) | dst /= t ->
        let (shp, n, k, e) = names dst
        in [ SAssign shp (RArrayShape src)
           , SAssign dst (RArrayAlloc (AVar shp))
           , SAssign n (RShapeSize (AVar shp))
           , SLoop (LoopSpec [k] [IVar n] Serial Nothing LoopCopy [])
               [ SAssign e (RArrayLoad src (AVar k))
               , SArrayWrite (AVar dst) (AVar k) (AVar e)
               ]
           ]
      SLoop LoopSpec {lsRole = LoopCopy} _ -> [snapshotFromSource stmt]
      SLoop spec b -> [SLoop spec (go b)]
      SParallelRegion b -> [SParallelRegion (go b)]
      SIf c thn els -> [SIf c (go thn) (go els)]
      SAssign _ (RArrayShape _) -> [snapshotFromSource stmt]
      _ -> [stmt]
