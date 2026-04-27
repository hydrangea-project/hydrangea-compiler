module Hydrangea.RepaBench.NBodyHarness
  ( runNBodyTiming
  , runNBodyImperativeTiming
  ) where

import Hydrangea.RepaBench.Benchmarks
import Hydrangea.RepaBench.Harness

runNBodyTiming :: TimingOptions -> IO ()
runNBodyTiming opts =
  runTimingHarnessIO "main" opts loadNBodyInputs runNBodyOutputsIO nBodyChecksum

runNBodyImperativeTiming :: TimingOptions -> IO ()
runNBodyImperativeTiming opts =
  runTimingHarnessIO "main_imperative" opts loadNBodyInputs runNBodyImperativeOutputsIO nBodyChecksum
