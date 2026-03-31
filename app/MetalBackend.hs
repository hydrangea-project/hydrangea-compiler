-- |
-- Module: MetalBackend
--
-- Metal compute toolchain driver for the Hydrangea CLI.
--
-- Takes 'MSLArtifacts' from the MSL codegen, drives the xcrun Metal
-- toolchain to produce a @.metallib@, compiles the ObjC harness with
-- clang, and optionally runs the resulting executable.
--
-- Prerequisites (macOS only):
--   * Xcode command-line tools (@xcrun@)
--   * @clang@ in PATH (ships with Xcode CLT)
module MetalBackend (compileAndRunMetal) where

import Control.Monad (when)
import Language.Hydrangea.CodegenMSL (MSLArtifacts(..))
import System.Directory (copyFile)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Temp (withSystemTempDirectory)
import System.Process
  ( callProcess
  , createProcess
  , proc
  , readProcessWithExitCode
  , waitForProcess
  )

-- | Compile and optionally run a Metal kernel + ObjC harness.
--
-- @keepMetal@: if True, copy the generated @.metal@ and harness @.m@ to the
-- working directory as @hydrangea_out.metal@ and @hydrangea_out.m@.
--
-- @compileOnly@: if True, stop after compilation.
compileAndRunMetal :: MSLArtifacts -> Bool -> Bool -> IO ExitCode
compileAndRunMetal artifacts keepMetal compileOnly =
  withSystemTempDirectory "hydrangea_metal" $ \dir -> do
    let metalFile   = dir </> "kernel.metal"
        airFile     = dir </> "kernel.air"
        metallibFile = dir </> "kernel.metallib"
        harnessFile = dir </> "harness.m"
        exeFile     = dir </> "harness_exe"

    let ompStubFile = dir </> "omp.h"

    writeFile metalFile  (mslKernelSource artifacts)
    writeFile harnessFile (mslHarnessSource artifacts)
    -- Stub out OpenMP: the Metal harness doesn't use CPU parallelism
    writeFile ompStubFile $ unlines
      [ "/* omp.h stub for Metal harness (GPU handles parallelism) */"
      , "#ifndef OMP_H_STUB"
      , "#define OMP_H_STUB"
      , "static inline int omp_get_max_threads(void) { return 1; }"
      , "static inline int omp_get_thread_num(void)  { return 0; }"
      , "static inline int omp_get_num_threads(void) { return 1; }"
      , "#endif"
      ]

    -- Copy generated sources to working directory early (before compilation)
    when keepMetal $ do
      copyFile metalFile "hydrangea_out.metal"
      copyFile harnessFile "hydrangea_out.m"
      putStrLn "Kept Metal sources: hydrangea_out.metal, hydrangea_out.m"

    -- Detect xcrun availability
    (xcrunExit, _, _) <- readProcessWithExitCode "xcrun" ["--version"] ""
    case xcrunExit of
      ExitFailure _ -> do
        hPutStrLn stderr "hydrangea: xcrun not found. Install Xcode command-line tools:"
        hPutStrLn stderr "  xcode-select --install"
        return (ExitFailure 1)
      ExitSuccess -> do
        -- Compile .metal -> .air
        (airExit, _, airErr) <- readProcessWithExitCode "xcrun"
          ["-sdk", "macosx", "metal", "-c", "-o", airFile, metalFile] ""
        case airExit of
          ExitFailure _ -> do
            putStrLn "Metal shader compilation failed:"
            putStrLn airErr
            return airExit
          ExitSuccess -> do
            -- Link .air -> .metallib
            (libExit, _, libErr) <- readProcessWithExitCode "xcrun"
              ["-sdk", "macosx", "metallib", "-o", metallibFile, airFile] ""
            case libExit of
              ExitFailure _ -> do
                putStrLn "metallib linking failed:"
                putStrLn libErr
                return libExit
              ExitSuccess -> do
                -- Compile ObjC harness
                let harnessFlags =
                      [ "-O2"
                      , "-fobjc-arc"
                      , "-framework", "Metal"
                      , "-framework", "Foundation"
                      , "-Iruntime"
                      , "-Ithird_party/simde"
                      , "-I" ++ dir   -- provides stub omp.h
                      , "-o", exeFile
                      , harnessFile
                      , "runtime/hyd_write_csv.c"
                      ]
                (harnessExit, _, harnessErr) <- readProcessWithExitCode "clang" harnessFlags ""
                case harnessExit of
                  ExitFailure _ -> do
                    putStrLn "Harness compilation failed:"
                    putStrLn harnessErr
                    return harnessExit
                  ExitSuccess -> do
                    if compileOnly
                      then return ExitSuccess
                      else do
                        (_, _, _, ph) <- createProcess (proc exeFile [metallibFile])
                        waitForProcess ph
