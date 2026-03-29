{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.CFGPipeline
--
-- Explicit pipeline for the canonical CFG loop representation.
-- Combines optimization, tiling, vectorization, and parallelization in order.
module Language.Hydrangea.CFGPipeline
  ( -- * Pipelines
    optimizePipeline2
  , vectorizePipeline2
  , vectorizePipelineWithWidth
  , parallelPipeline2
  , parallelPipelineWithWidth
  , metalPipeline2
  , fullPipeline2
  ) where

import Language.Hydrangea.CFG
import Language.Hydrangea.CFGOpt
import Language.Hydrangea.Parallelize
import Language.Hydrangea.Tile
import Language.Hydrangea.Vectorize

-- | Run the local optimization pass to fixpoint.
fixpointOpt :: [Stmt] -> [Stmt]
fixpointOpt = optimizeStmts2

-- | Pipeline entrypoints used by the frontend and codegen. These functions
-- apply a sequence of transformations in a stable order: optimize, then
-- optionally vectorize, then optionally parallelize. The `fullPipeline2`
-- variant performs all stages and a final optimization fixpoint. The
-- functions are intentionally small wrappers so the pipeline order is easy
-- to adjust and document.

-- Exported pipelines (short docs):
-- * @optimizePipeline2@ — run CFG local optimisations followed by automatic
--   tiling for eligible map-reduction kernels.
-- * @vectorizePipeline2@ — run vectorisation after a preliminary
--   optimization pass.
-- * @parallelPipeline2@ — run vectorization and parallelization after optimization.
-- * @fullPipeline2@ — optimize, vectorize, parallelize and run a final
--   optimization fixpoint.

-- | Optimization-only pipeline.
optimizePipeline2 :: [Stmt] -> [Stmt]
optimizePipeline2 stmts =
  let stmts' = optimizeStmts2 stmts
      stmts'' = tileStmts2 stmts'
  in  optimizeStmts2 stmts''

-- | Run optimization followed by vectorization using the given SIMD lane width.
vectorizePipelineWithWidth :: Int -> Program -> Program
vectorizePipelineWithWidth w (Program procs) =
  let optimized = [proc { procBody = optimizePipeline2 (procBody proc) } | proc <- procs]
  in  vectorizeProgram2WithWidth w (Program optimized)

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

-- | Optimize and parallelize without SIMD vectorization. Suitable for GPU
-- backends where SIMD is handled by the hardware (e.g. Metal/MSL).
metalPipeline2 :: Program -> Program
metalPipeline2 (Program procs) =
  let optimized = [proc { procBody = optimizePipeline2 (procBody proc) } | proc <- procs]
  in  parallelizeProgram2 (Program optimized)

-- | Run the full CFG pipeline: optimize, vectorize, parallelize, then clean up again.
fullPipeline2 :: Program -> Program
fullPipeline2 prog =
  case parallelizeProgram2 (vectorizePipeline2 prog) of
    Program procs ->
      Program [proc { procBody = fixpointOpt (procBody proc) } | proc <- procs]
