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
  ( createProcess
  , proc
  , readProcessWithExitCode
  , waitForProcess
  )

keepGeneratedSources :: Bool -> FilePath -> FilePath -> IO ()
keepGeneratedSources keepMetal metalFile harnessFile =
  when keepMetal $ do
    copyFile metalFile "hydrangea_out.metal"
    copyFile harnessFile "hydrangea_out.m"
    putStrLn "Kept Metal sources: hydrangea_out.metal, hydrangea_out.m"

writeOpenMPStub :: FilePath -> IO ()
writeOpenMPStub ompStubFile =
  writeFile ompStubFile $ unlines
    [ "/* omp.h stub for Metal harness (GPU handles parallelism) */"
    , "#ifndef OMP_H_STUB"
    , "#define OMP_H_STUB"
    , "static inline int omp_get_max_threads(void) { return 1; }"
    , "static inline int omp_get_thread_num(void)  { return 0; }"
    , "static inline int omp_get_num_threads(void) { return 1; }"
    , "#endif"
    ]

ensureXcrunAvailable :: IO (Maybe ExitCode)
ensureXcrunAvailable = do
  (xcrunExit, _, _) <- readProcessWithExitCode "xcrun" ["--version"] ""
  case xcrunExit of
    ExitFailure _ -> do
      hPutStrLn stderr "hydrangea: xcrun not found. Install Xcode command-line tools:"
      hPutStrLn stderr "  xcode-select --install"
      pure (Just (ExitFailure 1))
    ExitSuccess ->
      pure Nothing

runCheckedTool :: String -> [String] -> String -> IO (Either ExitCode ())
runCheckedTool cmd args failureLabel = do
  (exitCode, _, stderrText) <- readProcessWithExitCode cmd args ""
  case exitCode of
    ExitSuccess -> pure (Right ())
    ExitFailure _ -> do
      putStrLn failureLabel
      putStrLn stderrText
      pure (Left exitCode)

runOrReturn :: String -> [String] -> String -> IO ExitCode -> IO ExitCode
runOrReturn cmd args failureLabel next = do
  result <- runCheckedTool cmd args failureLabel
  case result of
    Left exitCode -> pure exitCode
    Right () -> next

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
        harnessFlags =
          [ "-O2"
          , "-fobjc-arc"
          , "-framework", "Metal"
          , "-framework", "Foundation"
          , "-Iruntime"
          , "-Ithird_party/simde"
          , "-I" ++ dir
          , "-o", exeFile
          , harnessFile
          , "runtime/hyd_write_csv.c"
          ]

    writeFile metalFile  (mslKernelSource artifacts)
    writeFile harnessFile (mslHarnessSource artifacts)
    writeOpenMPStub ompStubFile

    keepGeneratedSources keepMetal metalFile harnessFile

    mxcrunFailure <- ensureXcrunAvailable
    case mxcrunFailure of
      Just exitCode -> pure exitCode
      Nothing ->
        runOrReturn
          "xcrun"
          ["-sdk", "macosx", "metal", "-c", "-o", airFile, metalFile]
          "Metal shader compilation failed:"
          $ runOrReturn
              "xcrun"
              ["-sdk", "macosx", "metallib", "-o", metallibFile, airFile]
              "metallib linking failed:"
              $ runOrReturn
                  "clang"
                  harnessFlags
                  "Harness compilation failed:"
                  $ if compileOnly
                      then pure ExitSuccess
                      else do
                        (_, _, _, ph) <- createProcess (proc exeFile [metallibFile])
                        waitForProcess ph
