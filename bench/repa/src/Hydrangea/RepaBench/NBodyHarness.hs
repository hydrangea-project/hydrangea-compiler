module Hydrangea.RepaBench.NBodyHarness
  ( runNBodyTiming
  , runNBodyImperativeTiming
  , runNBodyAccelStyleTiming
  ) where

import Hydrangea.RepaBench.Benchmarks
import Hydrangea.RepaBench.Harness

runNBodyTiming :: TimingOptions -> IO ()
runNBodyTiming opts =
  runTimingHarnessIO "main" opts loadNBodyInputs runNBodyOutputsIO nBodyChecksum

runNBodyImperativeTiming :: TimingOptions -> IO ()
runNBodyImperativeTiming opts =
  runTimingHarnessIO "main_imperative" opts loadNBodyInputs runNBodyImperativeOutputsIO nBodyChecksum

-- Accelerate-style: extend + zipWith + foldP (single O(n²) pass, best performance)
runNBodyAccelStyleTiming :: TimingOptions -> IO ()
runNBodyAccelStyleTiming opts =
  runTimingHarnessIO "main_accel_style" opts loadNBodyInputs runNBodyAccelStyleOutputsIO nBodyChecksum
