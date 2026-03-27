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
import Control.Monad (forM_, when)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List (find, isPrefixOf)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Language.Hydrangea.CLI (allTopLevelProcsFlag, mainFlag, selectProgramDecs)
import Language.Hydrangea.CodegenC
  ( CodegenArtifacts(..)
  , CodegenOptions(..)
  , codegenProgram2WithOptionsPrune
  , defaultCodegenOptions
  )
import Language.Hydrangea.ErrorFormat (formatEvalError, formatTypeError)
import Language.Hydrangea.Frontend
  ( compileToCOptIOWithOptions
  , compileToCOptParallelIOWithOptions
  , evalDecsFrontend
  , lowerToCFG2
  , lowerToCFG2OptWithTypesWithOptions
  , lowerToCFG2WithTypesWithOptions
  , optimizeCFG2
  , optimizeParallelCFG2
  , readDecs
  )
import Language.Hydrangea.Infer (InferOptions(..), defaultInferOptions, runInferDecsWithOptions)
import Language.Hydrangea.Fusion (fuseDecs)
import Language.Hydrangea.Uniquify (uniquifyDecs)
import Language.Hydrangea.Pretty
import Language.Hydrangea.Prune (checkKernelFused)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitWith, ExitCode(..))
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
  , "[--no-solver-check]"
  , "[--kernel=<name>]"
  , "[" ++ mainFlag ++ "]"
  , "[--prune-dead-procs]"
  , "[--keep-c]"
  , "[--compile-only]"
  , "[--cc=<compiler>]"
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
      solveRefinements = not ("--no-solver-check" `elem` flags)
      pruneDead = "--prune-dead-procs" `elem` flags
      outputCFile = flagValue "--output-c=" flags
      outputHFile = flagValue "--output-h=" flags
      kernelFlag = flagValue "--kernel=" flags
      exportKernelFlag = flagValue "--export-kernel=" flags
      inferOptions = defaultInferOptions {inferSolveRefinements = solveRefinements}
  (input, mpath) <- readInput paths

  let ccFlag = flagValue "--cc=" flags
      exportCodegenOptions =
        case exportKernelFlag of
          Nothing -> defaultCodegenOptions
          Just name ->
            defaultCodegenOptions
              { codegenEmitMain = False
              , codegenExportKernel = Just (BS.pack name)
              }

  when (isJust outputHFile && not (isJust exportKernelFlag)) $
    dieWithMessage "--output-h requires --export-kernel."

  when (isJust exportKernelFlag && not (emitC || isJust outputCFile)) $
    dieWithMessage "--export-kernel currently supports C emission only; use it with --emit-c or --output-c."

  let generateExportArtifacts decs = do
        prog <- lowerToCFG2WithTypesWithOptions inferOptions decs
        let optimized = if parallel then optimizeParallelCFG2 prog else optimizeCFG2 prog
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
      decs <- case selectProgramDecs flags parsedDecs of
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
            if parallel
              then compileToCOptParallelIOWithOptions inferOptions pruneDead decs
              else compileToCOptIOWithOptions inferOptions pruneDead decs
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
          Nothing -> do
            csrc <- compileSelectedC
            putStrLn csrc
      forM_ outputCFile $ \outPath -> do
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
          Nothing -> do
            csrc <- compileSelectedC
            writeFile outPath csrc
            putStrLn $ "Wrote C to " ++ outPath
      let defaultMode =
            not printFused
              && not printCFG
              && not printCFGRaw
              && not emitC
              && not (isJust outputCFile)
              && not (isJust outputHFile)
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
          else do
            _ <- runCheckedInference
            ensureSelectedKernelIsFused
            csrc <- compileSelectedC
            ec <- compileAndRunC ccFlag csrc keepC compileOnly parallel
            case ec of
              ExitSuccess -> pure ()
              ExitFailure _ -> exitWith ec
