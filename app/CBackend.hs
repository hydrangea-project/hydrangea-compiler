-- |
-- Module: CBackend
--
-- External C compilation backend used by the CLI.
--
-- The backend writes generated C to a temporary directory, selects a compiler,
-- optionally probes for OpenMP support, builds against the Hydrangea runtime,
-- and then either runs the executable or leaves the artifacts on disk.
module CBackend (compileAndRunC) where

import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode, createProcess, proc, waitForProcess, callProcess)
import System.Environment (lookupEnv)
import System.Directory (copyFile)
import System.Exit (ExitCode(..))
import Control.Monad (when)

selectCompiler :: Maybe String -> Maybe String -> String
selectCompiler mcc envCC =
  case mcc of
    Just s | not (null s) -> s
    _ ->
      case envCC of
        Just s | not (null s) -> s
        _ -> "cc"

-- | Compile generated C, optionally preserve the artifacts, and optionally run
-- the produced executable.
--
-- If @keepC@ is true, copy the generated source and executable to the current
-- directory as @hydrangea_out.c@ and @hydrangea_out@. If @compileOnly@ is
-- true, stop after compilation. If @parallel@ is true, request OpenMP support
-- and probe whether the selected compiler accepts the relevant flags.
compileAndRunC :: Maybe String -> String -> Bool -> Bool -> Bool -> IO ExitCode
compileAndRunC mcc src keepC compileOnly parallel =
  withSystemTempDirectory "hydrangea" $ \dir -> do
    let cpath = dir </> "out.c"
        exe = dir </> "hydrangea_out"
    writeFile cpath src
    -- Prefer an explicit CLI override, then the CC environment variable, then
    -- the platform default compiler.
    envCC <- lookupEnv "CC"
    let ccCmd = selectCompiler mcc envCC
        ompFlags = if parallel then ["-fopenmp"] else []
        flags = ["-O2", "-std=c99"] ++ ompFlags ++ ["-Iruntime", "-Ithird_party/simde", "-o", exe, cpath, "runtime/hyd_write_csv.c"]
    -- If OpenMP is requested, probe whether the selected compiler accepts -fopenmp
    when parallel $ do
      let probeSrc = unlines
            [ "#include <omp.h>"
            , "int main(void) {"
            , "  int x = 0;"
            , "#pragma omp parallel reduction(+:x)"
            , "  { x += 1; }"
            , "  return x;"
            , "}"
            ]
          probeFile = dir </> "probe_openmp.c"
          probeExe = dir </> "probe_openmp"
          probeFlags = ["-fopenmp", "-std=c99", "-o", probeExe, probeFile]
      writeFile probeFile probeSrc
      (pExit, _pout, perr) <- readProcessWithExitCode ccCmd probeFlags ""
      case pExit of
        ExitSuccess -> pure ()
        ExitFailure _ -> do
          putStrLn $ "Warning: selected C compiler '" ++ ccCmd ++ "' does not accept -fopenmp (or failed to link OpenMP)."
          putStrLn "If you're on macOS, consider installing and using Homebrew's gcc (e.g. gcc-15) and pass it via --cc=gcc-15 or CC=gcc-15."
          putStrLn "You can also run with --no-parallel to disable generating OpenMP flags."
          putStrLn "Compiler output:"
          putStrLn perr
    (cExit, _cout, cerr) <- readProcessWithExitCode ccCmd flags ""
    case cExit of
      ExitFailure {} -> do
        putStrLn "C compilation failed:" >> putStrLn cerr
        pure cExit
      ExitSuccess -> do
        when keepC $ do
          let destC = "hydrangea_out.c"
              destExe = "hydrangea_out"
          copyFile cpath destC
          copyFile exe destExe
          -- Ensure the copied executable remains runnable.
          callProcess "chmod" ["+x", destExe]
        if compileOnly
          then pure ExitSuccess
          else do
            (_, _, _, ph) <- createProcess (proc exe [])
            waitForProcess ph
