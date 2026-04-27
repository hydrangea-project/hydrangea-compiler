module Main (main) where

import Hydrangea.RepaBench.Benchmarks
import Hydrangea.RepaBench.Harness
import Hydrangea.RepaBench.NBodyHarness

import Data.List (isPrefixOf)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--list"] -> mapM_ putStrLn benchmarkNames
    ["all"] -> runAllBenchmarks
    ["bench", "nbody"] -> runNBodyTiming defaultTimingOptions
    ("bench" : "nbody" : rest) -> runNBodyTiming (parseTimingOptions rest)
    ["bench", "nbody-imperative"] -> runNBodyImperativeTiming defaultTimingOptions
    ("bench" : "nbody-imperative" : rest) -> runNBodyImperativeTiming (parseTimingOptions rest)
    ["bench", name] -> runBenchmarkByNameTimed defaultTimingOptions name
    ("bench" : name : rest) -> runBenchmarkByNameTimed (parseTimingOptions rest) name
    [name] -> runBenchmarkByName name
    [] -> die "usage: repa-bench [--list|all|bench <name> [--warmup=N --iters=N]|<benchmark-name>]"
    _ -> die "usage: repa-bench [--list|all|bench <name> [--warmup=N --iters=N]|<benchmark-name>]"

parseTimingOptions :: [String] -> TimingOptions
parseTimingOptions = foldl step defaultTimingOptions
  where
    step opts arg
      | "--warmup=" `isPrefixOf` arg = opts { timingWarmupIters = read (drop (length "--warmup=") arg) }
      | "--iters=" `isPrefixOf` arg = opts { timingMeasureIters = read (drop (length "--iters=") arg) }
      | otherwise = opts
