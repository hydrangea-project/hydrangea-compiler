module Main (main) where

import Hydrangea.AccelBench.Benchmarks
import Hydrangea.AccelBench.Harness

import Data.List (isPrefixOf)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--list"]          -> mapM_ putStrLn benchmarkNames
    ["all"]             -> runAllBenchmarks
    ["bench", name]     -> runBenchmarkByNameTimed defaultTimingOptions name
    ("bench" : name : rest) -> runBenchmarkByNameTimed (parseTimingOptions rest) name
    [name]              -> runBenchmarkByName name
    [] -> die "usage: accel-bench [--list|all|bench <name> [--warmup=N --iters=N]|<benchmark-name>]"
    _  -> die "usage: accel-bench [--list|all|bench <name> [--warmup=N --iters=N]|<benchmark-name>]"

parseTimingOptions :: [String] -> TimingOptions
parseTimingOptions = foldl step defaultTimingOptions
  where
    step opts arg
      | "--warmup=" `isPrefixOf` arg =
          opts { timingWarmupIters   = read (drop (length "--warmup=") arg) }
      | "--iters=" `isPrefixOf` arg =
          opts { timingMeasureIters  = read (drop (length "--iters=") arg) }
      | otherwise = opts
