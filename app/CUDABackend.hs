-- |
-- Module: CUDABackend
--
-- CUDA compute toolchain driver for the Hydrangea CLI.
--
-- Takes 'CUDAArtifacts' from the CUDA codegen, writes the device @kernel.cu@ and
-- host @harness.cu@, and drives @nvcc@ to build (and optionally run) the result.
--
-- Prerequisites:
--   * NVIDIA CUDA toolkit (@nvcc@) in PATH
--   * An NVIDIA GPU at run time (compilation alone needs only the toolkit)
--
-- This machine has no NVIDIA toolchain, so the build step is expected to fail
-- with a clear message; @--keep-cuda@ still writes the generated sources for
-- inspection or compilation elsewhere.
module CUDABackend (compileAndRunCUDA) where

import Control.Exception (IOException, try)
import Control.Monad (when)
import Language.Hydrangea.CodegenCUDA (CUDAArtifacts (..))
import System.Directory (copyFile)
import System.Exit (ExitCode (..))
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
keepGeneratedSources keepCuda kernelFile harnessFile =
  when keepCuda $ do
    copyFile kernelFile "hydrangea_out.kernel.cu"
    copyFile harnessFile "hydrangea_out.harness.cu"
    putStrLn "Kept CUDA sources: hydrangea_out.kernel.cu, hydrangea_out.harness.cu"

-- | @nvcc --version@ probe so a missing toolkit produces a clear message rather
-- than an opaque process error.
ensureNvccAvailable :: IO (Maybe ExitCode)
ensureNvccAvailable = do
  result <- try (readProcessWithExitCode "nvcc" ["--version"] "")
  case result of
    Right (ExitSuccess, _, _) -> pure Nothing
    Right (ExitFailure _, _, _) -> notFound
    Left (_ :: IOException) -> notFound
  where
    notFound = do
      hPutStrLn stderr "hydrangea: nvcc not found. Install the NVIDIA CUDA toolkit"
      hPutStrLn stderr "  and ensure nvcc is on PATH to build the CUDA backend output."
      pure (Just (ExitFailure 1))

runOrReturn :: String -> [String] -> String -> IO ExitCode -> IO ExitCode
runOrReturn cmd args failureLabel next = do
  (exitCode, _, stderrText) <- readProcessWithExitCode cmd args ""
  case exitCode of
    ExitSuccess -> next
    ExitFailure _ -> do
      putStrLn failureLabel
      putStrLn stderrText
      pure exitCode

-- | Compile and optionally run a CUDA kernel + host harness.
--
-- @keepCuda@: if True, copy the generated @.cu@ sources to the working directory.
--
-- @compileOnly@: if True, stop after compilation.
compileAndRunCUDA :: CUDAArtifacts -> Bool -> Bool -> IO ExitCode
compileAndRunCUDA artifacts keepCuda compileOnly =
  withSystemTempDirectory "hydrangea_cuda" $ \dir -> do
    let kernelFile = dir </> "kernel.cu"
        harnessFile = dir </> "harness.cu"
        exeFile = dir </> "harness_exe"
        -- Relocatable device code so the harness TU can launch the kernel TU.
        nvccFlags =
          [ "-O2"
          , "-rdc=true"
          , "-Iruntime"
          , "-Ithird_party/simde"
          , "-o", exeFile
          , kernelFile
          , harnessFile
          , "runtime/hyd_write_csv.c"
          ]

    writeFile kernelFile (cudaKernelSource artifacts)
    writeFile harnessFile (cudaHarnessSource artifacts)

    keepGeneratedSources keepCuda kernelFile harnessFile

    mNvccFailure <- ensureNvccAvailable
    case mNvccFailure of
      Just exitCode -> pure exitCode
      Nothing ->
        runOrReturn
          "nvcc"
          nvccFlags
          "CUDA compilation failed:"
          $ if compileOnly
              then pure ExitSuccess
              else do
                (_, _, _, ph) <- createProcess (proc exeFile [])
                waitForProcess ph
