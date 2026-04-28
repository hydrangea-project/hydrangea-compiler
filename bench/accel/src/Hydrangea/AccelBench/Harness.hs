{-# LANGUAGE RecordWildCards #-}

module Hydrangea.AccelBench.Harness
  ( TimingOptions(..)
  , defaultTimingOptions
  , runTimingHarness
  , runTimingHarnessIO
  ) where

import Control.Exception (evaluate)
import Control.Monad (forM, replicateM_)
import Data.IORef (newIORef, atomicModifyIORef')
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)

data TimingOptions = TimingOptions
  { timingWarmupIters :: Int
  , timingMeasureIters :: Int
  }

defaultTimingOptions :: TimingOptions
defaultTimingOptions = TimingOptions
  { timingWarmupIters = 3
  , timingMeasureIters = 10
  }

runTimingHarness
  :: String
  -> TimingOptions
  -> IO input
  -> (input -> result)
  -> (result -> Double)
  -> IO ()
runTimingHarness label TimingOptions{..} load run checksum = do
  input <- load
  counter <- newIORef 0
  let nextToken = atomicModifyIORef' counter (\i -> let j = i + 1 in (j, j))
  replicateM_ timingWarmupIters $ do
    token <- nextToken
    _ <- evaluate (checksum (run (perturb token input)))
    pure ()
  samples <- forM [1 .. timingMeasureIters] $ \_ -> do
    token <- nextToken
    start <- getCurrentTime
    _ <- evaluate (checksum (run (perturb token input)))
    end <- getCurrentTime
    pure (realToFrac (diffUTCTime end start) :: Double)
  let minMs = minimum samples * 1000.0
      meanMs = (sum samples / fromIntegral (length samples)) * 1000.0
  printf "benchmark[%s]: min=%.3f ms  mean=%.3f ms\n" label minMs meanMs

runTimingHarnessIO
  :: String
  -> TimingOptions
  -> IO input
  -> (input -> IO result)
  -> (result -> Double)
  -> IO ()
runTimingHarnessIO label TimingOptions{..} load run checksum = do
  input <- load
  counter <- newIORef 0
  let nextToken = atomicModifyIORef' counter (\i -> let j = i + 1 in (j, j))
  replicateM_ timingWarmupIters $ do
    token <- nextToken
    result <- run (perturb token input)
    _ <- evaluate (checksum result)
    pure ()
  samples <- forM [1 .. timingMeasureIters] $ \_ -> do
    token <- nextToken
    start <- getCurrentTime
    result <- run (perturb token input)
    _ <- evaluate (checksum result)
    end <- getCurrentTime
    pure (realToFrac (diffUTCTime end start) :: Double)
  let minMs = minimum samples * 1000.0
      meanMs = (sum samples / fromIntegral (length samples)) * 1000.0
  printf "benchmark[%s]: min=%.3f ms  mean=%.3f ms\n" label minMs meanMs

perturb :: Int -> a -> a
{-# NOINLINE perturb #-}
perturb token x = unsafePerformIO (evaluate token >> pure x)
