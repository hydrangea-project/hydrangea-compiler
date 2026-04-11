-- |
-- Module: Main
--
-- Command-line entry point for the Hydrangea compiler.
--
-- The executable reads declarations from a file or stdin, selects a frontend
-- mode from the CLI flags, and either prints an intermediate form, interprets
-- the program, emits C, or compiles and runs the generated C through the
-- external backend.
module Main where

import CBackend (compileAndRunC)
import MetalBackend (compileAndRunMetal)
import Control.Monad (forM_, when)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List (find, isPrefixOf)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Language.Hydrangea.CLI (allTopLevelProcsFlag, mainFlag, selectProgramDecs)
import Language.Hydrangea.CodegenC
  ( BenchmarkConfig(..)
  , CodegenArtifacts(..)
  , CodegenOptions(..)
  , codegenProgram2WithOptionsPrune
  , defaultCodegenOptions
  )
import Language.Hydrangea.CodegenMSL (MSLArtifacts(..), MSLOptions(..), defaultMSLOptions, codegenMSL)
import Language.Hydrangea.ErrorFormat (formatEvalError, formatTypeError)
import Language.Hydrangea.Frontend
  ( compileToCOptIOWithPipelineOptionsAndCodegenOptions
  , evalDecsFrontend
  , lowerToCFG2
  , lowerToCFG2OptWithTypesWithOptions
  , lowerToCFG2WithTypesWithOptions
  , optimizeCFG2WithPipelineOptions
  , optimizeMetalCFG2WithTiling
  , readDecs
  )
import Language.Hydrangea.CFGPipeline (PipelineOptions(..), defaultPipelineOptions)
import Language.Hydrangea.Infer (InferOptions(..), defaultInferOptions, runInferDecsWithOptions)
import Language.Hydrangea.Vectorize (defaultVectorWidth)
import Language.Hydrangea.Fusion (fuseDecs)
import Language.Hydrangea.Uniquify (uniquifyDecs)
import Language.Hydrangea.Pretty
import Language.Hydrangea.Prune (checkKernelFused)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitWith, ExitCode(..))
import System.FilePath (replaceExtension)
import System.IO (hPutStrLn, stderr)
import Text.PrettyPrint.HughesPJClass

flagValue :: String -> [String] -> Maybe String
flagValue prefix flags = fmap (drop (length prefix)) $ find (prefix `isPrefixOf`) flags

dieWithMessage :: String -> IO a
dieWithMessage msg = putStrLn msg >> exitFailure

usage :: String
usage = unwords
  [ "Usage: hydrangea-compiler"
  , "[--print-fused]"
  , "[--print-cfg]"
  , "[--print-cfg-raw]"
  , "[--emit-c]"
  , "[--output-c=<file>]"
  , "[--output-h=<file>]"
  , "[--export-kernel=<name>]"
  , "[" ++ allTopLevelProcsFlag ++ "]"
   , "[--no-parallel]"
   , "[--tiling]"
   , "[--explicit-vectorization]"
   , "[--no-solver-check]"
  , "[--simd-width=<2|4>]"
  , "[--kernel=<name>]"
  , "[" ++ mainFlag ++ "]"
  , "[--prune-dead-procs]"
  , "[--keep-c]"
  , "[--compile-only]"
  , "[--cc=<compiler>]"
  , "[--benchmark=<name>]"
  , "[--bench-warmup=<n>]"
  , "[--bench-iters=<n>]"
  , "[--metal]"
  , "[--metal-kernel=<name>]"
  , "[--export-metal-kernel=<name>]"
  , "[--output-metal=<file>]"
  , "[--keep-metal]"
  , "[--no-multi-kernel]"
  , "[file]"
  ]

readInput :: [FilePath] -> IO (BS.ByteString, Maybe FilePath)
readInput paths =
  case paths of
    [path] -> do
      input <- BS.readFile path
      pure (input, Just path)
    [] -> do
      input <- BS.getContents
      pure (input, Nothing)
    _ -> dieWithMessage usage

main :: IO ()
main = do
  args <- getArgs
  let (flags, paths) = span ("--" `isPrefixOf`) args
      printFused = "--print-fused" `elem` flags
      printCFG = "--print-cfg" `elem` flags
      printCFGRaw = "--print-cfg-raw" `elem` flags
      emitC = "--emit-c" `elem` flags
      interp = "--interp" `elem` flags
      keepC = "--keep-c" `elem` flags
      compileOnly = "--compile-only" `elem` flags
      parallel = not ("--no-parallel" `elem` flags)
      enableTiling = "--tiling" `elem` flags
      enableExplicitVectorization = "--explicit-vectorization" `elem` flags
      solveRefinements = not ("--no-solver-check" `elem` flags)
      pruneDead = "--prune-dead-procs" `elem` flags
      outputCFile = flagValue "--output-c=" flags
      outputHFile = flagValue "--output-h=" flags
      kernelFlag = flagValue "--kernel=" flags
      exportKernelFlag = flagValue "--export-kernel=" flags
      benchmarkFlag = flagValue "--benchmark=" flags
      benchWarmupFlag = flagValue "--bench-warmup=" flags
      benchItersFlag = flagValue "--bench-iters=" flags
      simdWidthFlag = flagValue "--simd-width=" flags
      simdWidth = case simdWidthFlag of
        Nothing  -> defaultVectorWidth
        Just "2" -> 2
        Just "4" -> 4
        Just bad -> error $ "--simd-width must be 2 or 4, got: " ++ bad
      useMetal = "--metal" `elem` flags
      metalKernelFlag = flagValue "--metal-kernel=" flags
      exportMetalKernelFlag = flagValue "--export-metal-kernel=" flags
      outputMetalFile = flagValue "--output-metal=" flags
      keepMetal = "--keep-metal" `elem` flags
      noMultiKernel = "--no-multi-kernel" `elem` flags
      inferOptions = defaultInferOptions {inferSolveRefinements = solveRefinements}
      pipelineOptions =
        defaultPipelineOptions
          { poEnableTiling = enableTiling
          , poEnableExplicitVectorization = enableExplicitVectorization
          , poEnableParallelization = parallel
          , poVectorWidth = simdWidth
          }
  (input, mpath) <- readInput paths

  let ccFlag = flagValue "--cc=" flags
      benchmarkConfig = case benchmarkFlag of
        Nothing -> Nothing
        Just name -> Just BenchmarkConfig
          { bcKernelName  = BS.pack name
          , bcWarmupIters  = maybe 3 read benchWarmupFlag
          , bcMeasureIters = maybe 10 read benchItersFlag
          }
      exportCodegenOptions =
        (case exportKernelFlag of
          Nothing -> defaultCodegenOptions { codegenBenchmark = benchmarkConfig }
          Just name ->
            defaultCodegenOptions
              { codegenEmitMain = False
              , codegenExportKernel = Just (BS.pack name)
              , codegenBenchmark = benchmarkConfig
              })
        { codegenSimdWidth = simdWidth }

  when (isJust outputHFile && not (isJust exportKernelFlag) && not (isJust exportMetalKernelFlag)) $
    dieWithMessage "--output-h requires --export-kernel or --export-metal-kernel."

  when (isJust exportKernelFlag && not (emitC || isJust outputCFile)) $
    dieWithMessage "--export-kernel currently supports C emission only; use it with --emit-c or --output-c."

  when (isJust exportMetalKernelFlag && not useMetal) $
    dieWithMessage "--export-metal-kernel requires --metal."

  when (isJust exportMetalKernelFlag && not (isJust outputCFile)) $
    dieWithMessage "--export-metal-kernel requires --output-c=<file> for the .m harness."

  when (enableExplicitVectorization && useMetal) $
    dieWithMessage "--explicit-vectorization is only supported for the C backend."

  let generateExportArtifacts decs = do
        prog <- lowerToCFG2WithTypesWithOptions inferOptions decs
        let optimized = optimizeCFG2WithPipelineOptions pipelineOptions prog
        pure (codegenProgram2WithOptionsPrune exportCodegenOptions pruneDead optimized)

      writeExportHeaderIfRequested artifacts =
        forM_ outputHFile $ \headerPath ->
          case codegenHeader artifacts of
            Nothing -> dieWithMessage "Internal error: export mode did not produce a header."
            Just headerSrc -> do
              writeFile headerPath headerSrc
              putStrLn $ "Wrote header to " ++ headerPath

  case readDecs input of
    Left perr -> dieWithMessage $ "Parse error: " ++ perr
    Right parsedDecs -> do
      let selectFlags = if useMetal then allTopLevelProcsFlag : flags else flags
      decs <- case selectProgramDecs selectFlags parsedDecs of
        Left err -> dieWithMessage err
        Right pr -> pure pr
      let reportTypeError terr = dieWithMessage (formatTypeError mpath (Just input) terr)
          runCheckedInference = do
            inferred <- runInferDecsWithOptions inferOptions decs
            case inferred of
              Left terr -> reportTypeError terr
              Right (typedDecs, warnings) -> do
                mapM_ (hPutStrLn stderr) warnings
                pure typedDecs
          ensureSelectedKernelIsFused =
            forM_ kernelFlag $ \name ->
              case checkKernelFused decs (BS.pack name) of
                Left err -> dieWithMessage err
                Right () -> pure ()
          compileSelectedC =
            let codegenOpts = defaultCodegenOptions { codegenSimdWidth = simdWidth }
            in compileToCOptIOWithPipelineOptionsAndCodegenOptions
                 inferOptions
                 pipelineOptions
                 codegenOpts
                 pruneDead
                 decs
          renderResult (v, p, mval) =
            case mval of
              Nothing -> putStrLn $ unpack v ++ " : evaluation error"
              Just val -> do
                let typeStr = render (text (unpack v) <+> prettyTy (Just p))
                    valStr = render (text " = " <+> pPrint val)
                putStrLn (typeStr ++ valStr)
      when printFused $ do
        _ <- runCheckedInference
        let fused = fuseDecs (uniquifyDecs decs)
        mapM_ (putStrLn . render . pPrint) fused
      when printCFG $ do
        _ <- runCheckedInference
        prog <- lowerToCFG2OptWithTypesWithOptions inferOptions decs
        print prog
      when printCFGRaw $ do
        _ <- runCheckedInference
        let prog = lowerToCFG2 decs
        print prog
      when emitC $ do
        _ <- runCheckedInference
        ensureSelectedKernelIsFused
        case exportKernelFlag of
          Just _ -> do
            artifactsRes <- generateExportArtifacts decs
            case artifactsRes of
              Left err -> dieWithMessage err
              Right artifacts -> do
                putStrLn (codegenSource artifacts)
                writeExportHeaderIfRequested artifacts
          Nothing -> case benchmarkConfig of
            Just _ -> do
              artifactsRes <- generateExportArtifacts decs
              case artifactsRes of
                Left err -> dieWithMessage err
                Right artifacts -> putStrLn (codegenSource artifacts)
            Nothing -> do
              csrc <- compileSelectedC
              putStrLn csrc
      forM_ outputCFile $ \outPath -> when (not (isJust exportMetalKernelFlag)) $ do
        _ <- runCheckedInference
        ensureSelectedKernelIsFused
        case exportKernelFlag of
          Just _ -> do
            artifactsRes <- generateExportArtifacts decs
            case artifactsRes of
              Left err -> dieWithMessage err
              Right artifacts -> do
                writeFile outPath (codegenSource artifacts)
                putStrLn $ "Wrote C to " ++ outPath
                writeExportHeaderIfRequested artifacts
          Nothing -> case benchmarkConfig of
            Just _ -> do
              artifactsRes <- generateExportArtifacts decs
              case artifactsRes of
                Left err -> dieWithMessage err
                Right artifacts -> do
                  writeFile outPath (codegenSource artifacts)
                  putStrLn $ "Wrote C to " ++ outPath
            Nothing -> do
              csrc <- compileSelectedC
              writeFile outPath csrc
              putStrLn $ "Wrote C to " ++ outPath
      let metalExportMode = isJust exportMetalKernelFlag
          defaultMode =
            not printFused
              && not printCFG
              && not printCFGRaw
              && not emitC
              && (not (isJust outputCFile) || metalExportMode)
              && (not (isJust outputHFile) || metalExportMode)
      when defaultMode $ do
        if interp
          then do
            typedDecs <- runCheckedInference
            evaled <- evalDecsFrontend decs
            case evaled of
              Left err -> dieWithMessage (formatEvalError mpath (Just input) err)
              Right env -> do
                let results = map (\(v, ty) -> (v, ty, Map.lookup v env)) typedDecs
                    hasError = any (\(_, _, mval) -> mval == Nothing) results
                mapM_ renderResult results
                when hasError exitFailure
          else if useMetal then do
            _ <- runCheckedInference
            prog <- lowerToCFG2WithTypesWithOptions inferOptions decs
            let optimized = optimizeMetalCFG2WithTiling enableTiling prog
                metalOpts = defaultMSLOptions
                  { mslKernelToEmit = fmap BS.pack metalKernelFlag
                  , mslMultiKernel = not noMultiKernel
                  , mslExportKernel = fmap BS.pack exportMetalKernelFlag }
            case codegenMSL metalOpts optimized of
              Left err -> dieWithMessage ("Metal codegen error: " ++ err)
              Right metalArtifacts -> case exportMetalKernelFlag of
                Just _ -> do
                  -- Export mode: write .metal, .m, .h files
                  let metalPath = case outputMetalFile of
                        Just p  -> p
                        Nothing -> case outputCFile of
                          Just p  -> replaceExtension p ".metal"
                          Nothing -> "hydrangea_out.metal"
                      harnessPath = case outputCFile of
                        Just p  -> p
                        Nothing -> "hydrangea_out.m"
                      headerPath = case outputHFile of
                        Just p  -> p
                        Nothing -> replaceExtension harnessPath ".h"
                  writeFile metalPath (mslKernelSource metalArtifacts)
                  putStrLn $ "Wrote Metal kernel to " ++ metalPath
                  writeFile harnessPath (mslHarnessSource metalArtifacts)
                  putStrLn $ "Wrote Metal harness to " ++ harnessPath
                  case mslHeaderSource metalArtifacts of
                    Just h -> do
                      writeFile headerPath h
                      putStrLn $ "Wrote header to " ++ headerPath
                    Nothing -> pure ()
                Nothing -> do
                  ec <- compileAndRunMetal metalArtifacts keepMetal compileOnly
                  case ec of
                    ExitSuccess -> pure ()
                    ExitFailure _ -> exitWith ec
          else do
            _ <- runCheckedInference
            ensureSelectedKernelIsFused
            csrc <- case benchmarkConfig of
              Nothing -> compileSelectedC
              Just _ -> do
                artifactsRes <- generateExportArtifacts decs
                case artifactsRes of
                  Left err -> dieWithMessage err
                  Right artifacts -> pure (codegenSource artifacts)
            ec <- compileAndRunC ccFlag csrc keepC compileOnly parallel
            case ec of
              ExitSuccess -> pure ()
              ExitFailure _ -> exitWith ec
