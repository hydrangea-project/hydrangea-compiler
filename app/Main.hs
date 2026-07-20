-- |
-- Module: Main
--
-- Command-line entry point for the Hydrangea compiler.
--
-- The executable reads declarations from a file or stdin, selects a frontend
-- mode from the CLI flags, and then either prints an intermediate form,
-- interprets the program, emits C or Metal source, or invokes the matching
-- backend to compile and run generated artifacts.
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
  , codegenProgramWithOptionsPrune
  , defaultCodegenOptions
  )
import Language.Hydrangea.CodegenMSL (MSLArtifacts(..), MSLOptions(..), defaultMSLOptions, codegenMSL)
import Language.Hydrangea.ErrorFormat (formatEvalError, formatTypeError)
import Language.Hydrangea.Frontend
  ( compileToCOptIOWithAllOptions
  , evalDecsFrontend
  , FrontendOptions(..)
  , defaultFrontendOptions
  , inferAndLowerToCFGWithFrontendOptions
  , lowerToCFG
  , lowerToCFGOptWithTypesWithOptions
  , lowerToCFGWithTypesWithOptions
  , optimizeCFGWithPipelineOptions
  , optimizeMetalCFGWithTiling
  , readDecs
  )
import Language.Hydrangea.CFGPipeline
  ( PipelineOptions(..)
  , defaultPipelineOptions
  , preparePolyhedralProgramWithOptions
  )
import Language.Hydrangea.Infer (InferOptions(..), defaultInferOptions, runInferDecsWithOptions)
import Language.Hydrangea.Polyhedral (collectProgramScopDiagnostics)
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
import Text.Read (readMaybe)

flagValue :: String -> [String] -> Maybe String
flagValue prefix flags = fmap (drop (length prefix)) $ find (prefix `isPrefixOf`) flags

dieWithMessage :: String -> IO a
dieWithMessage msg = putStrLn msg >> exitFailure

resolveOrDie :: Either String a -> IO a
resolveOrDie = either dieWithMessage pure

parseSimdWidth :: Maybe String -> Either String Int
parseSimdWidth Nothing = Right defaultVectorWidth
parseSimdWidth (Just "2") = Right 2
parseSimdWidth (Just "4") = Right 4
parseSimdWidth (Just bad) =
  Left ("--simd-width must be 2 or 4, got: " ++ bad)

parseIntFlag :: String -> Int -> Maybe String -> Either String Int
parseIntFlag _ defaultValue Nothing = Right defaultValue
parseIntFlag flagName _ (Just raw) =
  case readMaybe raw of
    Just value -> Right value
    Nothing -> Left (flagName ++ " must be an integer, got: " ++ raw)

benchmarkConfigFromFlags
  :: Maybe String -> Maybe String -> Maybe String -> Either String (Maybe BenchmarkConfig)
benchmarkConfigFromFlags Nothing _ _ = Right Nothing
benchmarkConfigFromFlags (Just name) benchWarmupFlag benchItersFlag = do
  warmupIters <- parseIntFlag "--bench-warmup" 3 benchWarmupFlag
  measureIters <- parseIntFlag "--bench-iters" 10 benchItersFlag
  pure $
    Just BenchmarkConfig
      { bcKernelName = BS.pack name
      , bcWarmupIters = warmupIters
      , bcMeasureIters = measureIters
      }

usage :: String
usage = unwords
  [ "Usage: hydrangea-compiler"
  , "[--print-fused]"
  , "[--print-cfg]"
  , "[--print-cfg-raw]"
  , "[--print-polyhedral-scops]"
  , "[--emit-c]"
  , "[--output-c=<file>]"
  , "[--output-h=<file>]"
  , "[--export-kernel=<name>]"
  , "[" ++ allTopLevelProcsFlag ++ "]"
   , "[--no-parallel]"
   , "[--no-fusion]"
   , "[--tiling]"
   , "[--polyhedral]"
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
      printPolyhedralScops = "--print-polyhedral-scops" `elem` flags
      emitC = "--emit-c" `elem` flags
      interp = "--interp" `elem` flags
      keepC = "--keep-c" `elem` flags
      compileOnly = "--compile-only" `elem` flags
      parallel = not ("--no-parallel" `elem` flags)
      noFusion = "--no-fusion" `elem` flags
      enablePolyhedral = "--polyhedral" `elem` flags
      enableTiling = "--tiling" `elem` flags || enablePolyhedral
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
      useMetal = "--metal" `elem` flags
      metalKernelFlag = flagValue "--metal-kernel=" flags
      exportMetalKernelFlag = flagValue "--export-metal-kernel=" flags
      outputMetalFile = flagValue "--output-metal=" flags
      keepMetal = "--keep-metal" `elem` flags
      noMultiKernel = "--no-multi-kernel" `elem` flags
  simdWidth <- resolveOrDie (parseSimdWidth simdWidthFlag)
  benchmarkConfig <- resolveOrDie (benchmarkConfigFromFlags benchmarkFlag benchWarmupFlag benchItersFlag)
  let inferOptions = defaultInferOptions {inferSolveRefinements = solveRefinements}
      pipelineOptions =
        defaultPipelineOptions
          { poEnableTiling = enableTiling
          , poEnablePolyhedral = enablePolyhedral
          , poEnableExplicitVectorization = enableExplicitVectorization
          , poEnableParallelization = parallel
          , poVectorWidth = simdWidth
          }
      frontendOptions = defaultFrontendOptions { frontendSkipFusion = noFusion }
  (input, mpath) <- readInput paths

  let ccFlag = flagValue "--cc=" flags
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
      writeGeneratedC outPath csrc = do
        writeFile outPath csrc
        putStrLn $ "Wrote C to " ++ outPath

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
        prog <- inferAndLowerToCFGWithFrontendOptions frontendOptions inferOptions decs
        let optimized = optimizeCFGWithPipelineOptions pipelineOptions prog
        pure (codegenProgramWithOptionsPrune exportCodegenOptions pruneDead optimized)
      compileSelectedC decs =
        let codegenOpts = defaultCodegenOptions { codegenSimdWidth = simdWidth }
        in compileToCOptIOWithAllOptions
             frontendOptions
             inferOptions
             pipelineOptions
             codegenOpts
             pruneDead
             decs
      generatedArtifactsOrDie decs =
        generateExportArtifacts decs >>= resolveOrDie
      emitGeneratedC decs =
        case exportKernelFlag of
          Just _ -> do
            artifacts <- generatedArtifactsOrDie decs
            putStrLn (codegenSource artifacts)
            writeExportHeaderIfRequested artifacts
          Nothing ->
            case benchmarkConfig of
              Just _ -> do
                artifacts <- generatedArtifactsOrDie decs
                putStrLn (codegenSource artifacts)
              Nothing -> do
                csrc <- compileSelectedC decs
                putStrLn csrc
      writeGeneratedCOutput decs outPath =
        case exportKernelFlag of
          Just _ -> do
            artifacts <- generatedArtifactsOrDie decs
            writeGeneratedC outPath (codegenSource artifacts)
            writeExportHeaderIfRequested artifacts
          Nothing ->
            case benchmarkConfig of
              Just _ -> do
                artifacts <- generatedArtifactsOrDie decs
                writeGeneratedC outPath (codegenSource artifacts)
              Nothing -> do
                csrc <- compileSelectedC decs
                writeGeneratedC outPath csrc

      writeExportHeaderIfRequested artifacts =
        forM_ outputHFile $ \headerPath ->
          case codegenHeader artifacts of
            Nothing -> dieWithMessage "Internal error: export mode did not produce a header."
            Just headerSrc -> do
              writeFile headerPath headerSrc
              putStrLn $ "Wrote header to " ++ headerPath

      writeMetalExportArtifacts metalArtifacts = do
        let metalPath = case outputMetalFile of
              Just p -> p
              Nothing ->
                case outputCFile of
                  Just p -> replaceExtension p ".metal"
                  Nothing -> "hydrangea_out.metal"
            harnessPath = case outputCFile of
              Just p -> p
              Nothing -> "hydrangea_out.m"
            headerPath = case outputHFile of
              Just p -> p
              Nothing -> replaceExtension harnessPath ".h"
        writeFile metalPath (mslKernelSource metalArtifacts)
        putStrLn $ "Wrote Metal kernel to " ++ metalPath
        writeFile harnessPath (mslHarnessSource metalArtifacts)
        putStrLn $ "Wrote Metal harness to " ++ harnessPath
        case mslHeaderSource metalArtifacts of
          Just headerSrc -> do
            writeFile headerPath headerSrc
            putStrLn $ "Wrote header to " ++ headerPath
          Nothing -> pure ()

  case readDecs input of
    Left perr -> dieWithMessage $ "Parse error: " ++ perr
    Right parsedDecs -> do
      -- Metal run mode prunes to `main` like the other backends, so the harness
      -- has a single, well-defined result to compute and print. Export mode and
      -- explicit --metal-kernel selection name their own kernel, so they keep all
      -- top-level procs. (Pass --all-top-level-procs explicitly for debug.)
      let keepAllForMetal = isJust exportMetalKernelFlag || isJust metalKernelFlag
          selectFlags
            | useMetal && keepAllForMetal = allTopLevelProcsFlag : flags
            | otherwise = flags
      decs <- case selectProgramDecs selectFlags parsedDecs of
        Left err
          | useMetal && not keepAllForMetal ->
              dieWithMessage $ unlines
                [ "Metal backend: " ++ err
                , "The Metal backend runs the `main` entry point. Either:"
                , "  * name your final top-level binding `main`, or"
                , "  * use --export-metal-kernel=<name> to export a specific GPU kernel, or"
                , "  * pass --all-top-level-procs to print every binding (debug)."
                ]
          | otherwise -> dieWithMessage err
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
          renderResult (v, p, mval) =
            case mval of
              Nothing -> putStrLn $ unpack v ++ " : evaluation error"
              Just val -> do
                let typeStr = render (text (unpack v) <+> prettyTy (Just p))
                    valStr = render (text " =" <+> pPrint val)
                putStrLn (typeStr ++ valStr)
      when printFused $ do
        _ <- runCheckedInference
        let fused = fuseDecs (uniquifyDecs decs)
        mapM_ (putStrLn . render . pPrint) fused
      when printCFG $ do
        _ <- runCheckedInference
        prog <- lowerToCFGOptWithTypesWithOptions inferOptions decs
        print prog
      when printCFGRaw $ do
        _ <- runCheckedInference
        let prog = lowerToCFG decs
        print prog
      when printPolyhedralScops $ do
        _ <- runCheckedInference
        prog <- lowerToCFGWithTypesWithOptions inferOptions decs
        let prepared = preparePolyhedralProgramWithOptions pipelineOptions prog
            diagnostics = collectProgramScopDiagnostics prepared
        if null diagnostics
          then putStrLn "No polyhedral SCoPs found."
          else mapM_ print diagnostics
      when emitC $ do
        _ <- runCheckedInference
        ensureSelectedKernelIsFused
        emitGeneratedC decs
      forM_ outputCFile $ \outPath -> when (not (isJust exportMetalKernelFlag)) $ do
        _ <- runCheckedInference
        ensureSelectedKernelIsFused
        writeGeneratedCOutput decs outPath
      let metalExportMode = isJust exportMetalKernelFlag
          defaultMode =
            not printFused
              && not printCFG
              && not printCFGRaw
              && not printPolyhedralScops
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
            prog <- lowerToCFGWithTypesWithOptions inferOptions decs
            let optimized = optimizeMetalCFGWithTiling enableTiling prog
                metalOpts = defaultMSLOptions
                  { mslKernelToEmit = fmap BS.pack metalKernelFlag
                  , mslMultiKernel = not noMultiKernel
                  , mslExportKernel = fmap BS.pack exportMetalKernelFlag }
            case codegenMSL metalOpts optimized of
              Left err -> dieWithMessage ("Metal codegen error: " ++ err)
              Right metalArtifacts -> do
                mapM_ (\w -> hPutStrLn stderr ("warning: " ++ w)) (mslWarnings metalArtifacts)
                case exportMetalKernelFlag of
                  Just _ ->
                    writeMetalExportArtifacts metalArtifacts
                  Nothing -> do
                    ec <- compileAndRunMetal metalArtifacts keepMetal compileOnly
                    case ec of
                      ExitSuccess -> pure ()
                      ExitFailure _ -> exitWith ec
          else do
            _ <- runCheckedInference
            ensureSelectedKernelIsFused
            csrc <- case benchmarkConfig of
              Nothing -> compileSelectedC decs
              Just _ -> do
                artifacts <- generatedArtifactsOrDie decs
                pure (codegenSource artifacts)
            ec <- compileAndRunC ccFlag csrc keepC compileOnly parallel
            case ec of
              ExitSuccess -> pure ()
              ExitFailure _ -> exitWith ec
