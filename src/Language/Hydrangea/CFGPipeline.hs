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
import Language.Hydrangea.Tile
import Language.Hydrangea.Vectorize

data PipelineOptions = PipelineOptions
  { poEnableTiling :: Bool
  , poEnableExplicitVectorization :: Bool
  , poEnableParallelization :: Bool
  , poVectorWidth :: Int
  }

defaultPipelineOptions :: PipelineOptions
defaultPipelineOptions = PipelineOptions
  { poEnableTiling = True
  , poEnableExplicitVectorization = True
  , poEnableParallelization = False
  , poVectorWidth = defaultVectorWidth
  }

-- | Run the local optimization pass to fixpoint.
fixpointOpt :: [Stmt] -> [Stmt]
fixpointOpt = optimizeStmts2

-- | Optimization-only pipeline.
optimizePipeline2 :: [Stmt] -> [Stmt]
optimizePipeline2 = optimizePipelineWithTiling True

-- | Optimization-only pipeline with optional tiling.
optimizePipelineWithTiling :: Bool -> [Stmt] -> [Stmt]
optimizePipelineWithTiling enableTiling stmts =
  let stmts' = optimizeStmts2 stmts
      stmts'' = if enableTiling then tileStmts2 stmts' else stmts'
  in  optimizeStmts2 stmts''

-- | Run optimization followed by vectorization using explicit pipeline options.
vectorizePipelineWithOptions :: PipelineOptions -> Program -> Program
vectorizePipelineWithOptions opts (Program procs) =
  let optimized = [proc { procBody = optimizePipelineWithTiling (poEnableTiling opts) (procBody proc) } | proc <- procs]
  in  vectorizeProgram2WithWidthAndExplicit
        (poVectorWidth opts)
        (poEnableExplicitVectorization opts)
        (Program optimized)

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
