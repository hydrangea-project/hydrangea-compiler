{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.EndToEndSpec (spec) where

import Test.Hspec
import System.Directory (copyFile, createDirectoryIfMissing, findExecutable)
import System.Environment (getEnvironment)
import System.Process
  ( CreateProcess(cwd, env, std_err, std_out)
  , StdStream(NoStream)
  , readProcessWithExitCode
  , createProcess
  , proc
  , waitForProcess
  , callProcess
  )
import System.Exit (ExitCode(..))
import qualified Data.ByteString.Lazy.Char8 as BS
import Text.Printf (printf)

import Language.Hydrangea.Frontend
  ( readDecs
  , compileToCOptIO
  , compileToCOptIOWithAllOptions
  , compileToCOptIOWithPipelineOptionsAndCodegenOptions
  , compileToCOptParallelIO
  , evalDecsFrontend
  , FrontendOptions(..)
  , defaultFrontendOptions
  )
import Language.Hydrangea.Interpreter (Value(..))
import qualified Data.Map as Map
import Data.Char (isAlphaNum, isDigit)
import Data.List (intercalate, dropWhileEnd, isInfixOf, isPrefixOf, stripPrefix)
import Text.Read (readMaybe)
import Language.Hydrangea.Syntax (Dec(..), Exp(..), ScatterPhase(..))
import Language.Hydrangea.Lexer (Range)
import Language.Hydrangea.Infer (InferOptions(..), defaultInferOptions)
import Language.Hydrangea.CFGPipeline (PipelineOptions(..), defaultPipelineOptions)
import Language.Hydrangea.CodegenC (defaultCodegenOptions)
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import Control.Monad (forM_, when)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Compile and run a C source string; optionally keep artifacts.
compileAndRunC :: String -> Bool -> Bool -> Bool -> IO ExitCode
compileAndRunC = compileAndRunCWithFlags []

-- | 'compileAndRunC' with extra C compiler flags.
compileAndRunCWithFlags :: [String] -> String -> Bool -> Bool -> Bool -> IO ExitCode
compileAndRunCWithFlags extraFlags src keepC compileOnly isParallel =
  withSystemTempDirectory "hydrangea-test" $ \dir -> do
    let cpath = dir </> "out.c"
        exe = dir </> "hydrangea_out"
    writeFile cpath src
    let cc = "cc"
        ompFlags = if isParallel then ["-fopenmp"] else []
        flags = ["-O2", "-std=c99"] ++ ompFlags ++ extraFlags ++ ["-Iruntime", "-Ithird_party/simde", "-o", exe, cpath, "runtime/hyd_write_csv.c", "-lm"]
    (cExit, _cout, cerr) <- readProcessWithExitCode cc flags ""
    case cExit of
      ExitFailure {} -> do
        putStrLn "C compilation failed:" >> putStrLn cerr
        return cExit
      ExitSuccess -> do
        when keepC $ do
          copyFile cpath "hydrangea_out.c"
          copyFile exe "hydrangea_out"
          callProcess "chmod" ["+x", "hydrangea_out"]
        if compileOnly
          then return ExitSuccess
          else do
            (_, _, _, ph) <- createProcess (proc exe [])
            waitForProcess ph

compileCToExecutable :: FilePath -> String -> Bool -> IO FilePath
compileCToExecutable dir src isParallel = do
  let cpath = dir </> "out.c"
      exe = dir </> "hydrangea_out"
      cc = "cc"
      ompFlags = if isParallel then ["-fopenmp"] else []
      flags = ["-O2", "-std=c99"] ++ ompFlags ++ ["-Iruntime", "-Ithird_party/simde", "-o", exe, cpath, "runtime/hyd_write_csv.c", "-lm"]
  writeFile cpath src
  (cExit, _cout, cerr) <- readProcessWithExitCode cc flags ""
  case cExit of
    ExitFailure {} -> do
      putStrLn "C compilation failed:"
      putStrLn cerr
      expectationFailure "C compilation failed"
      pure exe
    ExitSuccess ->
      pure exe

runExecutableInDir :: FilePath -> FilePath -> [(String, String)] -> IO ExitCode
runExecutableInDir dir exe extraEnv = do
  baseEnv <- getEnvironment
  let overrideKeys = map fst extraEnv
      mergedEnv = extraEnv ++ filter (\(k, _) -> k `notElem` overrideKeys) baseEnv
  (_, _, _, ph) <-
    createProcess
      ( (proc exe [])
          { cwd = Just dir
          , env = Just mergedEnv
          , std_out = NoStream
          , std_err = NoStream
          }
      )
  waitForProcess ph

-- | Skip test if 'cc' is not available.
withCC :: IO () -> IO ()
withCC action = do
  mcc <- findExecutable "cc"
  case mcc of
    Nothing -> pendingWith "cc (C compiler) not found on PATH"
    Just _  -> action

-- | Parse, interpret and C-compile a source program; assert the two outputs agree.
checkInlineSrc :: BS.ByteString -> IO ()
checkInlineSrc =
  checkInlineSrcWithOptions
    defaultInferOptions
    defaultPipelineOptions

-- | Parse, interpret and C-compile a source program with explicit pipeline
-- options; assert the interpreter and compiled outputs agree.
checkInlineSrcWithOptions :: InferOptions -> PipelineOptions -> BS.ByteString -> IO ()
checkInlineSrcWithOptions inferOpts pipelineOpts src =
  case readDecs src of
    Left perr -> expectationFailure $ "Parse error: " ++ perr
    Right decs -> checkDecsWithOptions defaultFrontendOptions inferOpts pipelineOpts decs

-- | Interpret and C-compile already-parsed declarations; assert the two outputs
-- agree.  Lets a test hand the backend a term the surface syntax cannot write.
checkDecsWithOptions
  :: FrontendOptions -> InferOptions -> PipelineOptions -> [Dec Range] -> IO ()
checkDecsWithOptions = checkDecsWithCFlags []

-- | 'checkDecsWithOptions' with extra C compiler flags.  With
-- @-DHYD_RC_CHECK@ the program must also leak no array.
checkDecsWithCFlags
  :: [String] -> FrontendOptions -> InferOptions -> PipelineOptions -> [Dec Range] -> IO ()
checkDecsWithCFlags cflags frontOpts inferOpts pipelineOpts decs = do
  eres <- evalDecsFrontend decs
  case eres of
    Left err -> expectationFailure $ "Interpreter error: " ++ show err
    Right env -> do
      let zeroVars = [v | Dec _ v pats _ _ _ <- decs, null pats]
          lookupVal v = Map.lookup v env
          formatElem (VInt n)    = show n
          formatElem (VFloat f)  = stripTrailingZerosG (printf "%.17g" f)
          formatElem (VString s) = BS.unpack s
          formatElem (VPair a b) = "(" ++ formatElem a ++ ", " ++ formatElem b ++ ")"
          formatElem (VBool b)   = if b then "1" else "0"
          formatElem (VArray shape vals) =
            case traverse formatArrayElem vals of
              Just elems ->
                "[" ++ intercalate ", " elems
                  ++ "] (shape: [" ++ intercalate ", " (map show shape) ++ "])"
              Nothing -> "<unprintable>"
          formatElem _           = "<unprintable>"
          -- Strip trailing zeros from %g output to match C printf("%.17g")
          -- which never emits trailing zeros or a trailing decimal point.
          stripTrailingZerosG s
            | '.' `elem` s && 'e' `notElem` s && 'E' `notElem` s =
                let s' = dropWhileEnd (== '0') s
                in if last s' == '.' then init s' else s'
            | otherwise = s
          formatArrayElem (VInt n)    = Just (show n)
          formatArrayElem (VFloat f)  = Just (stripTrailingZerosG (printf "%.17g" f))
          formatArrayElem (VString s) = Just (BS.unpack s)
          formatArrayElem (VPair a b) = do
            a' <- formatArrayElem a
            b' <- formatArrayElem b
            pure $ "(" ++ a' ++ ", " ++ b' ++ ")"
          formatArrayElem (VRecord fields) = do
            fields' <- traverse (\(field, val) -> do
              val' <- formatArrayElem val
              pure $ BS.unpack field ++ " = " ++ val') fields
            pure $ "{" ++ intercalate ", " fields' ++ "}"
          formatArrayElem _           = Nothing
          formatVal (VInt n)    = show n ++ "\n"
          formatVal (VFloat f)  = stripTrailingZerosG (printf "%.17g" f) ++ "\n"
          formatVal (VString s) = BS.unpack s ++ "\n"
          formatVal (VPair a b) = "(" ++ formatElem a ++ ", " ++ formatElem b ++ ")\n"
          formatVal (VTuple vs) = "(" ++ intercalate ", " (map formatElem vs) ++ ")\n"
          formatVal (VArray shape vals) =
            case traverse formatArrayElem vals of
              Just elems ->
                let shp = intercalate ", " (map show shape)
                in "[" ++ intercalate ", " elems ++ "] (shape: [" ++ shp ++ "])\n"
              Nothing -> "<unprintable>\n"
          formatVal _ = "<unprintable>\n"
          -- Filter out declarations whose interpreter value cannot be printed
          -- by the C runtime (e.g. closures). The C codegen either inlines such
          -- declarations or omits them from the main, so they produce no output.
          isPrintable v = case lookupVal v of
            Just val -> formatVal val /= "<unprintable>\n"
            Nothing  -> True   -- keep "<eval error>" entries to surface bugs
          expected = concat [ maybe "<eval error>\n" formatVal (lookupVal v)
                            | v <- zeroVars, isPrintable v ]
      csrc <-
        compileToCOptIOWithAllOptions
          frontOpts
          inferOpts
          pipelineOpts
          defaultCodegenOptions
          False
          decs
      ec <- compileAndRunCWithFlags cflags csrc True False False
      case ec of
        ExitFailure _ -> expectationFailure "C compilation or execution failed"
        ExitSuccess -> do
          (exit, out, err) <- readProcessWithExitCode "./hydrangea_out" [] ""
          case exit of
            ExitFailure _ -> expectationFailure "Running generated executable failed"
            ExitSuccess   -> do
              normalizeFloatTokens out `shouldBe` normalizeFloatTokens expected
              when ("-DHYD_RC_CHECK" `elem` cflags) $
                filter ("hyd-rc-check" `isInfixOf`) (lines err)
                  `shouldSatisfy` all (\l -> "leaked=0;" `isInfixOf` l && "leaked=0 (" `isInfixOf` l)

-- | Does the C source contain a tile variable derived from loop iterator
-- @base@: an identifier @base@, then any fresh-name digits, then @_tile_@?
-- Matches @i59_tile_start@ or @i_tile_len@ for @base = "i"@, but not
-- @xi59_tile_start@.
hasTiledIter :: String -> String -> Bool
hasTiledIter base = go ' '
  where
    go _ [] = False
    go prev s@(c : rest)
      | not (isIdentChar prev)
      , Just afterBase <- stripPrefix base s
      , "_tile_" `isPrefixOf` dropWhile isDigit afterBase = True
      | otherwise = go c rest
    isIdentChar ch = isAlphaNum ch || ch == '_'

-- | C's printf("%.17g") and Haskell's printf "%.17g" can render the same
-- double as different digit strings (C converts the exact binary value;
-- Haskell pads the shortest round-trip digits). Both round-trip, so rewrite
-- every numeric token to its parsed-Double 'show' form before comparing.
normalizeFloatTokens :: String -> String
normalizeFloatTokens [] = []
normalizeFloatTokens s@(c : rest)
  | numStart s =
      let (tok, rest') = span numChar s
      in case readMaybe tok :: Maybe Double of
           Just d  -> show d ++ normalizeFloatTokens rest'
           Nothing -> tok ++ normalizeFloatTokens rest'
  | otherwise = c : normalizeFloatTokens rest
  where
    numStart (x : _) | isDigit x = True
    numStart ('-' : x : _) = isDigit x
    numStart _ = False
    numChar x = isDigit x || x `elem` (".eE+-" :: String)

-- ---------------------------------------------------------------------------
-- Test suite
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do

  -- ---- Original tests (kept as-is) ----------------------------------------

  describe "End-to-end C compile+run" $ do

    it "compiles and runs simple.hyd and matches the interpreter" $ withCC $ do
      src <- BS.readFile "examples/simple.hyd"
      checkInlineSrc src

    it "compiles and runs a tiny inline program and matches interpreter" $ withCC $
      checkInlineSrc "let a = 1\nlet b = 2\nlet c = a + b\n"

    it "compiles and runs dot_csv.hyd reading CSV and matches the interpreter" $ withCC $ do
      src <- BS.readFile "examples/dot_csv.hyd"
      checkInlineSrc src

    it "writes 2D float arrays to CSV with row structure preserved" $ withCC $
      withSystemTempDirectory "hydrangea-write-csv" $ \tmp -> do
        let outPath = tmp </> "out.csv"
            src = BS.pack $ unlines
              [ "let arr = generate [2, 3] (let f [i, j] = i * 3 + j in f)"
              , "let arrf = map (let g x = x +. 0.5 in g) arr"
              , "let _ = write_array_float arrf \"" ++ outPath ++ "\""
              ]
            expected = "0.5,1.5,2.5\n3.5,4.5,5.5\n"
        case readDecs src of
          Left perr -> expectationFailure $ "Parse error: " ++ perr
          Right decs -> do
            csrc <- compileToCOptIO False decs
            ec <- compileAndRunC csrc False False False
            case ec of
              ExitFailure _ -> expectationFailure "C compilation or execution failed"
              ExitSuccess -> do
                actual <- readFile outPath
                actual `shouldBe` expected

    it "compiles and runs tiled reduce_generate matmul and matches the interpreter" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let M = 6"
        , "let K = 9"
        , "let N = 6"
        , "let A = generate [M, K] (let fa [i, j] = i * 10 + j + 1 in fa)"
        , "let B = generate [K, N] (let fb [i, j] = i * 7 + j + 1 in fb)"
        , "let add x y = x + y"
        , "let mul x y = x * y"
        , "let innerGen i j [idx] = mul (index [i, idx] A) (index [idx, j] B)"
        , "let matElem [i, j] = index () (reduce_generate add 0 [K] (innerGen i j))"
        , "let result = generate [M, N] matElem"
        ]

    it "compiles and runs polyhedral tiled reduce_generate matmul and matches the interpreter" $ withCC $
      checkInlineSrcWithOptions
        defaultInferOptions
        defaultPipelineOptions
          { poEnableTiling = True
          , poEnablePolyhedral = True
          , poEnableExplicitVectorization = True
          , poEnableParallelization = False
          }
        (BS.pack $ unlines
          [ "let M = 6"
          , "let K = 9"
          , "let N = 6"
          , "let A = generate [M, K] (let fa [i, j] = i * 10 + j + 1 in fa)"
          , "let B = generate [K, N] (let fb [i, j] = i * 7 + j + 1 in fb)"
          , "let add x y = x + y"
          , "let mul x y = x * y"
          , "let innerGen i j [idx] = mul (index [i, idx] A) (index [idx, j] B)"
          , "let matElem [i, j] = index () (reduce_generate add 0 [K] (innerGen i j))"
          , "let result = generate [M, N] matElem"
          ])

    it "compiles and runs polyhedral tiled iterate stencil and matches the interpreter" $ withCC $
      checkInlineSrcWithOptions
        defaultInferOptions
        defaultPipelineOptions
          { poEnableTiling = True
          , poEnablePolyhedral = True
          , poEnableExplicitVectorization = False
          , poEnableParallelization = False
          }
        (BS.pack $ unlines
          [ "let step arr = stencil clamp"
          , "  (let acc_fn acc = (acc (-1) 0 +. acc 1 0 +. acc 0 (-1) +. acc 0 1) /. 4.0 in acc_fn)"
          , "  arr"
          , "let result = iterate 2 (generate [4, 4] (let f [i, j] = 1.0 in f)) step"
          ])

    it "compiles and runs polyhedral 1D iterate stencil clamp and matches the interpreter" $ withCC $
      checkInlineSrcWithOptions
        defaultInferOptions
        defaultPipelineOptions
          { poEnableTiling = True
          , poEnablePolyhedral = True
          , poEnableExplicitVectorization = False
          , poEnableParallelization = False
          }
        (BS.pack $ unlines
          [ "let init = generate [5] (let f [i] = 1.0 in f)"
          , "let step arr = stencil clamp"
          , "  (fn acc => (acc (-1) +. acc 0 +. acc 0 +. acc 1) /. 4.0)"
          , "  arr"
          , "let result = iterate 3 init step"
          ])

    -- Regression: with more time steps than the wavefront stage width (4),
    -- the serial skewed fallback runs multiple time tiles; the skew guard
    -- used absolute time against a tile-relative frontier range, losing all
    -- interior writes after the first tile.
    it "compiles and runs polyhedral 1D iterate stencil across multiple time tiles and matches the interpreter" $ withCC $
      checkInlineSrcWithOptions
        defaultInferOptions
        defaultPipelineOptions
          { poEnableTiling = True
          , poEnablePolyhedral = True
          , poEnableExplicitVectorization = False
          , poEnableParallelization = False
          }
        (BS.pack $ unlines
          [ "let init = generate [5] (let f [i] = float_of i in f)"
          , "let step arr = stencil clamp"
          , "  (fn acc => (acc (-1) +. acc 0 +. acc 1) /. 3.0)"
          , "  arr"
          , "let result = iterate 5 init step"
          ])

    -- Multi-array (FDTD-style) iterate state: a pair of arrays where the
    -- second field's update reads the first field's fresh value. Exercises
    -- pair-state scalarization, per-component temporal aliases, and the
    -- pair re-assignment codegen.
    it "compiles and runs a polyhedral pair-state coupled-field iterate and matches the interpreter" $ withCC $
      checkInlineSrcWithOptions
        defaultInferOptions
        defaultPipelineOptions
          { poEnableTiling = True
          , poEnablePolyhedral = True
          , poEnableExplicitVectorization = False
          , poEnableParallelization = False
          }
        (BS.pack $ unlines
          [ "let e0 = generate [6] (let f [i] = float_of i *. 0.1 in f)"
          , "let h0 = generate [6] (let f [i] = 0.0 in f)"
          , "let step s ="
          , "  let e = fst s in"
          , "  let h = snd s in"
          , "  let e2 = zipwith (fn a b => a -. b) e"
          , "    (stencil clamp (fn acc => (acc 0 -. acc (-1)) *. 0.5) h) in"
          , "  let h2 = zipwith (fn a b => a -. b) h"
          , "    (stencil clamp (fn acc => (acc 1 -. acc 0) *. 0.7) e2) in"
          , "  (e2, h2)"
          , "let result = iterate 4 (e0, h0) step"
          ])

    it "compiles and runs polyhedral constant-boundary stencil kernels and matches the interpreter" $ withCC $
      checkInlineSrcWithOptions
        defaultInferOptions
        defaultPipelineOptions
          { poEnableTiling = True
          , poEnablePolyhedral = True
          , poEnableExplicitVectorization = False
          , poEnableParallelization = False
          }
        (BS.pack $ unlines
          [ "let input = generate [8] (let f [i] = i + 1 in f)"
          , "let result = stencil (constant 0)"
          , "  (fn acc => acc (-1) + acc 0 + acc 1)"
          , "  input"
          ])

    it "compiles and runs polyhedral 3D iterate stencil clamp and matches the interpreter" $ withCC $
      checkInlineSrcWithOptions
        defaultInferOptions
        defaultPipelineOptions
          { poEnableTiling = True
          , poEnablePolyhedral = True
          , poEnableExplicitVectorization = False
          , poEnableParallelization = False
          }
        (BS.pack $ unlines
          [ "let init = generate [3, 3, 3] (let f [i, j, k] = float_of ((i * 9) + (j * 3) + k) in f)"
          , "let step arr = stencil clamp"
          , "  (fn acc => (acc (-1) 0 0 +. acc 1 0 0 +. acc 0 0 0) /. 3.0)"
          , "  arr"
          , "let result = iterate 2 init step"
          ])

    it "compiles and runs 3D constant-boundary stencil kernels and matches the interpreter" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let input = generate [2, 2, 2] (let f [i, j, k] = (i * 4) + (j * 2) + k + 1 in f)"
        , "let result = stencil (constant 0)"
        , "  (fn acc => acc 0 0 (-1) + acc 0 0 0)"
        , "  input"
        ]

    it "compiles and runs 3D iterate stencil kernels and matches the interpreter" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let init = generate [3, 3, 3] (let f [i, j, k] = float_of ((i * 9) + (j * 3) + k) in f)"
        , "let step arr = stencil clamp"
        , "  (fn acc => (acc 0 0 (-1) +. acc 0 0 0 +. acc 0 0 1) /. 3.0)"
        , "  arr"
        , "let result = iterate 2 init step"
        ]

    it "emits wavefront-tiled jacobi interior C and matches the untiled polyhedral path" $ withCC $
      withSystemTempDirectory "hydrangea-jacobi" $ \tmp -> do
        src <- BS.readFile "bench/stencil/jacobi_2d_interior.hyd"
        case readDecs src of
          Left perr -> expectationFailure $ "Parse error: " ++ perr
          Right decs -> do
            let polyOpts =
                  defaultPipelineOptions
                    { poEnableTiling = True
                    , poEnablePolyhedral = True
                    , poEnableExplicitVectorization = False
                    , poEnableParallelization = False
                    }
                untiledOpts = polyOpts { poEnableTiling = False }
                jacobiEnv =
                  [ ("JACOBI_H", "10")
                  , ("JACOBI_W", "10")
                  , ("JACOBI_ITERS", "4")
                  ]
                tiledDir = tmp </> "tiled"
                untiledDir = tmp </> "untiled"
            tiledC <-
              compileToCOptIOWithPipelineOptionsAndCodegenOptions
                defaultInferOptions
                polyOpts
                defaultCodegenOptions
                False
                decs
            untiledC <-
              compileToCOptIOWithPipelineOptionsAndCodegenOptions
                defaultInferOptions
                untiledOpts
                defaultCodegenOptions
                False
                decs
            -- The polyhedral tiling pass now uses a wavefront schedule with ring
            -- buffers; check that the wavefront profitability guard and ring-buffer
            -- allocations are present in the tiled C output.
            tiledC `shouldSatisfy` isInfixOf "__wf_frontier_profitable"
            tiledC `shouldSatisfy` isInfixOf "__wf_ring"
            createDirectoryIfMissing True tiledDir
            createDirectoryIfMissing True untiledDir
            tiledExe <- compileCToExecutable tiledDir tiledC False
            untiledExe <- compileCToExecutable untiledDir untiledC False
            tiledExit <- runExecutableInDir tiledDir tiledExe jacobiEnv
            untiledExit <- runExecutableInDir untiledDir untiledExe jacobiEnv
            tiledExit `shouldBe` ExitSuccess
            untiledExit `shouldBe` ExitSuccess
            tiledOut <- readFile (tiledDir </> "jacobi_out.csv")
            untiledOut <- readFile (untiledDir </> "jacobi_out.csv")
            tiledOut `shouldBe` untiledOut

    it "wavefront-tiled jacobi is correct for large arrays (wavefront actually fires)" $ withCC $
      withSystemTempDirectory "hydrangea-jacobi-large" $ \tmp -> do
        src <- BS.readFile "bench/stencil/jacobi_2d_interior.hyd"
        case readDecs src of
          Left perr -> expectationFailure $ "Parse error: " ++ perr
          Right decs -> do
            let polyOpts =
                  defaultPipelineOptions
                    { poEnableTiling = True
                    , poEnablePolyhedral = True
                    , poEnableExplicitVectorization = False
                    , poEnableParallelization = False
                    }
                untiledOpts = polyOpts { poEnableTiling = False }
                -- 516x516 → interior width t25 = 514 ≥ 512, so the wavefront
                -- profitability guard fires and the wavefront body actually runs.
                -- 6 iterations > stage width 4 so the run spans two time tiles.
                jacobiEnv =
                  [ ("JACOBI_H", "516")
                  , ("JACOBI_W", "516")
                  , ("JACOBI_ITERS", "6")
                  ]
                tiledDir   = tmp </> "tiled"
                untiledDir = tmp </> "untiled"
            tiledC <-
              compileToCOptIOWithPipelineOptionsAndCodegenOptions
                defaultInferOptions
                polyOpts
                defaultCodegenOptions
                False
                decs
            untiledC <-
              compileToCOptIOWithPipelineOptionsAndCodegenOptions
                defaultInferOptions
                untiledOpts
                defaultCodegenOptions
                False
                decs
            createDirectoryIfMissing True tiledDir
            createDirectoryIfMissing True untiledDir
            tiledExe   <- compileCToExecutable tiledDir   tiledC   False
            untiledExe <- compileCToExecutable untiledDir untiledC False
            tiledExit   <- runExecutableInDir tiledDir   tiledExe   jacobiEnv
            untiledExit <- runExecutableInDir untiledDir untiledExe jacobiEnv
            tiledExit   `shouldBe` ExitSuccess
            untiledExit `shouldBe` ExitSuccess
            tiledOut   <- readFile (tiledDir   </> "jacobi_out.csv")
            untiledOut <- readFile (untiledDir </> "jacobi_out.csv")
            tiledOut `shouldBe` untiledOut

    it "compiles a 1D index through a function parameter (Issue 3 fix)" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let f arr = index [0] arr + index [1] arr"
        , "let main = 0"
        ]

    it "compiles a function calling map with an array parameter (Issue 4 fix)" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let inc x = x + 1"
        , "let step arr = map inc arr"
        , "let arr = fill [4] 1"
        , "let a = step arr"
        , "let main = a"
        ]

    -- The stencil sums (no division) so interpreter and compiled float
    -- results are bit-identical and the printed outputs compare exactly.
    it "compiles an iterate in expression position (Issue 5 fix)" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let result = index [2] (iterate 3 (generate [5] (fn [i] => float_of i)) (fn arr => stencil clamp (fn acc => acc (-1) +. acc 0 +. acc 1) arr))"
        , "let main = 0"
        ]

    it "compiles a multi-use fn-valued step dec (Issue 5 fix)" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let step = fn arr => stencil clamp (fn acc => acc (-1) +. acc 0 +. acc 1) arr"
        , "let a = iterate 2 (generate [5] (fn [i] => float_of i)) step"
        , "let b = iterate 3 a step"
        , "let c = index [0] a"
        , "let main = 0"
        ]

    it "merges nested inline iterates and matches the interpreter (Issue 5 fix)" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let step = fn arr => stencil clamp (fn acc => acc (-1) +. acc 0 +. acc 1) arr"
        , "let result = iterate 3 (iterate 2 (generate [5] (fn [i] => float_of i)) step) step"
        , "let main = 0"
        ]

    it "matmul benchmark uses blocked generic tiling and explicit-vector polyhedral codegen" $ do
      src <- BS.readFile "bench/matmul/mat_mul_bench.hyd"
      case readDecs src of
        Left perr -> expectationFailure $ "Parse error: " ++ perr
        Right decs -> do
          let legacyOpts =
                defaultPipelineOptions
                  { poEnableTiling = True
                  , poEnablePolyhedral = False
                  , poEnableExplicitVectorization = False
                  , poEnableParallelization = False
                  }
              polyOpts =
                legacyOpts
                  { poEnablePolyhedral = True
                  , poEnableExplicitVectorization = True
                  }
          legacyC <-
            compileToCOptIOWithPipelineOptionsAndCodegenOptions
              defaultInferOptions
              legacyOpts
              defaultCodegenOptions
              False
              decs
          polyC <-
            compileToCOptIOWithPipelineOptionsAndCodegenOptions
              defaultInferOptions
              polyOpts
              defaultCodegenOptions
              False
              decs
          -- Each of the i, j and k loops is tiled, whatever fresh suffix
          -- the pipeline happened to give its iterator.
          forM_ ["i", "j", "k"] $ \iter -> do
            legacyC `shouldSatisfy` hasTiledIter iter
            polyC `shouldSatisfy` hasTiledIter iter
          legacyC `shouldSatisfy` not . isInfixOf "hyd_vec_loadu_f64x4"
          legacyC `shouldSatisfy` not . isInfixOf "hyd_vec_storeu_f64x4"
          polyC `shouldSatisfy` isInfixOf "hyd_vec_loadu_f64x4"
          polyC `shouldSatisfy` isInfixOf "hyd_vec_storeu_f64x4"

  -- ---- Scatter / ScatterGenerate -------------------------------------------

  describe "Scatter / ScatterGenerate" $ do

    it "scatter with non-overlapping writes matches interpreter" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let idx = generate [3] (let f [i] = i in f)"
        , "let vals = generate [3] (let g [i] = i + 10 in g)"
        , "let r = scatter (+) (fill [3] 0) idx vals"
        ]

    it "scatter with overlapping writes (combine) matches interpreter" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let idx = generate [4] (let f [i] = i / 2 in f)"
        , "let vals = fill [4] 1"
        , "let r = scatter (+) (fill [2] 0) idx vals"
        ]

    it "scatter histogram (all-to-same-bin) matches interpreter" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let idx = fill [5] 0"
        , "let vals = fill [5] 1"
        , "let r = scatter (+) (fill [1] 0) idx vals"
        ]

    it "scatter fused (values-side map) matches interpreter" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let src = generate [4] (let f [i] = i in f)"
        , "let idx = map (let r x = x / 2 in r) src"
        , "let vals = map (let g x = x * 3 in g) src"
        , "let r = scatter (+) (fill [2] 0) idx vals"
        ]

    it "weighted histogram with scalar routes matches interpreter" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let src = generate [8] (let f [i] = i in f)"
        , "let idx = map (let r x = x / 2 in r) src"
        , "let vals = map (let g x = (x * 3) + 1 in g) src"
        , "let r = scatter (+) (fill [4] 0) idx vals"
        ]

    it "scatter fused from zipwith-derived routes and values matches interpreter" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let xs = generate [8] (let fx [i] = i in fx)"
        , "let ys = generate [8] (let fy [i] = 100 + i in fy)"
        , "let idx = zipwith (let bin x y = x / 2 in bin) xs ys"
        , "let vals = zipwith (let weight x y = x + y in weight) xs ys"
        , "let r = scatter (+) (fill [4] 0) idx vals"
        ]

    it "scatter fused from map/zipwith mixed producers matches interpreter" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let xs = generate [8] (let fx [i] = i in fx)"
        , "let ys = generate [8] (let fy [i] = 100 + i in fy)"
        , "let idx = map (let bin x = x / 2 in bin) xs"
        , "let vals = zipwith (let weight x y = x + y in weight) xs ys"
        , "let r = scatter (+) (fill [4] 0) idx vals"
        ]

    it "scatter_guarded fused from same-source route, values, and guard matches interpreter" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let src = generate [8] (let f [i] = i in f)"
        , "let idx = map (let bin x = x / 2 in bin) src"
        , "let vals = map (let weight x = (x * 3) + 1 in weight) src"
        , "let r = let guard = map (let keep x = (x / 2) = ((x + 1) / 2) in keep) src in scatter_guarded (+) (fill [4] 0) idx vals guard"
        ]

    it "pair arrays compile and load correctly" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let arr = generate [3] (let f [i] = (i, i + 1) in f)"
        , "let r = snd (index [1] arr)"
        ]

    it "record arrays compile and load correctly" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let arr = generate [3] (let f [i] = {x = i, y = i + 1} in f)"
        , "let r = let rec = index [1] arr in rec.y"
        ]

    it "pair arrays print correctly when returned from main" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let main = generate [3] (let f [i] = (i, i + 1) in f)"
        ]

    it "record arrays print correctly when returned from main" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let main = generate [3] (let f [i] = {x = i, y = i + 1} in f)"
        ]

  describe "Scatter targets and loop state" $ do

    it "scatter into a top-level array that is read again afterwards" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let idx = generate [8] (let f [i] = (i * 3) % 8 in f)"
        , "let p0 = generate [8] (let f [i] = i in f)"
        , "let p2 = scatter (+) p0 idx (fill [8] 1)"
        , "let r = zipwith (let g x y = if x < y then 1 else 0 in g) p0 p2"
        ]

    it "iterate step that reads its state after scattering into it" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let idx = generate [8] (let f [i] = (i * 3) % 8 in f)"
        , "let r = iterate 2 (generate [8] (let f [i] = i in f))"
        , "  (fn p => zipwith (let g x y = x + y in g) p (scatter (+) p idx (fill [8] 1)))"
        ]

    -- EScatterChain is not surface syntax, so build one from a nested scatter.
    -- The second phase's values are the chain's own target; the chain must
    -- write into a copy so that phase still reads the pre-chain array.  Fusion
    -- is off so the lowering sees the chain as built.
    it "scatter chain whose phase reads the chain's target" $ withCC $
      case readDecs $ BS.pack $ unlines
             [ "let main ="
             , "  let p = reify (generate [8] (fn [i] => i)) in"
             , "  let i1 = reify (generate [8] (fn [i] => (i + 1) % 8)) in"
             , "  let w1 = reify (generate [8] (fn [i] => 100)) in"
             , "  let i2 = reify (generate [8] (fn [i] => (i + 3) % 8)) in"
             , "  scatter (+) (scatter (+) p i1 w1) i2 p" ] of
        Left perr -> expectationFailure ("Parse error: " ++ perr)
        Right decs ->
          checkDecsWithOptions defaultFrontendOptions { frontendSkipFusion = True }
            defaultInferOptions defaultPipelineOptions (map chainDec decs)

    it "iterate over a pair of int and bool arrays, one passed through" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let a0 = generate [8] (let f [i] = i in f)"
        , "let r = fst (iterate 3 (a0, generate [8] (let f [i] = i < 3 in f))"
        , "  (fn st => (map (let f x = x + 1 in f) (fst st), snd st)))"
        ]

    it "iterate over a pair whose step compares old and new state" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let idx = generate [8] (let f [i] = (i * 3) % 8 in f)"
        , "let r = map (let f b = if b then 1 else 0 in f)"
        , "  (snd (iterate 3 (generate [8] (let f [i] = i in f), fill [8] false)"
        , "    (fn st => let p = fst st in let p2 = scatter (+) p idx (fill [8] 1) in"
        , "              (p2, zipwith (let g x y = x < y in g) p p2))))"
        ]

    it "guard reading one array at two gathered indices (BFS on a tree)" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let n = 31"
        , "let srcs = generate [n - 1] (let f [e] = e / 2 in f)"
        , "let dsts = generate [n - 1] (let f [e] = e + 1 in f)"
        , "let parent0 = generate [n] (let f [i] = if i = 0 then 0 else (-1) in f)"
        , "let step p ="
        , "  let ready a b = (a <> (-1)) & (b = (-1)) in"
        , "  let overwrite new_ old_ = new_ in"
        , "  scatter_guarded overwrite p dsts srcs (zipwith ready (gather srcs p) (gather dsts p))"
        , "let r = iterate 5 parent0 step"
        ]

  describe "Guard-derived loop bounds" $ do

    it "ragged rows (k < len[j]) with a sum scatter" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let lens = generate [300] (let f [j] = ((j * 37) % 81) - 8 in f)"
        , "let r = scatter_guarded (+) (fill [300] 0) (generate [300, 64] (let f [j, k] = (j + k) % 300 in f)) (generate [300, 64] (let f [j, k] = j * k in f)) (generate [300, 64] (let f [j, k] = k < index [j] lens in f))"
        ]

    it "ragged rows under a row bound, with a min scatter and <=" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let lens = generate [300] (let f [j] = ((j * 37) % 81) - 8 in f)"
        , "let mn a b = if a < b then a else b"
        , "let r = scatter_guarded mn (fill [300] 999999) (generate [300, 64] (let f [j, k] = (j * 7 + k) % 300 in f)) (generate [300, 64] (let f [j, k] = j + 3 * k in f)) (generate [300, 64] (let f [j, k] = (j < 211) & ((index [j] lens) >= k) in f))"
        ]

    it "triangular nest (k < j)" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let r = scatter_guarded (+) (fill [64] 0) (generate [64, 64] (let f [j, k] = k in f)) (generate [64, 64] (let f [j, k] = j in f)) (generate [64, 64] (let f [j, k] = k < j in f))"
        ]

    it "1-D loop clamped by an invariant bound" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let r = scatter_guarded (+) (fill [500] 0) (generate [500] (let f [j] = (j * 13) % 500 in f)) (generate [500] (let f [j] = j in f)) (generate [500] (let f [j] = j < 377 in f))"
        ]
  -- ---- Example programs (file-based) ---------------------------------------

  describe "Example programs" $ do

    let exampleFiles =
          [ ("arith.hyd",                      "arithmetic expressions")
          , ("arrays.hyd",                     "fill + map")
          , ("complete.hyd",                   "closures, partial application, map")
          , ("dot.hyd",                        "generate + zipwith + reduce (no CSV)")
          , ("float_test.hyd",                 "float arithmetic")
          , ("reduce_reg.hyd",                 "basic reduce")
          , ("pair_test.hyd",                  "pair construction and projection")
          , ("pair_fold_simple.hyd",           "foldl with pair accumulator")
          , ("scatter_parallel_direct.hyd",    "direct parallel scatter example")
          , ("scatter_parallel_atomic.hyd",    "atomic scatter-add example")
          , ("scatter_parallel_privatized.hyd","privatized scatter-reduce example")
          , ("scatter_guarded_fused.hyd",      "guarded scatter fusion example")
          , ("stencil_1d.hyd",                 "1D stencil with clamp boundary")
          , ("stencil_2d.hyd",                 "2D Laplacian stencil")
          , ("heat_3d.hyd",                    "3D iterate + stencil smoothing example")
          , ("scan_prefix_sum.hyd",            "exclusive scan / offsets example")
          , ("sort_indices_gather.hyd",        "permutation sort plus gather example")
          , ("coo_sort_row_major.hyd",         "record-based COO canonical sort example")
          , ("coo_to_csr_pipeline.hyd",        "COO canonicalization plus CSR conversion")
          , ("segmented_group_sum.hyd",        "grouped aggregation over explicit offsets")
          , ("graph_messages_csr.hyd",         "CSR-backed flat messages plus segmented reduction")
          ]
    forM_ exampleFiles $ \(filename, description) ->
      it (filename ++ " - " ++ description) $ withCC $ do
        src <- BS.readFile ("examples/" ++ filename)
        checkInlineSrc src

  -- ---- Iterate temporal laws (proposal-temporal-tiling.md) -----------------

  describe "iterate temporal laws" $ do

    it "compiler-generated halo decomposition for constant-boundary iterate compiles and matches" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let init = generate [16] (let f [i] = float_of ((i * i) + 8) *. 0.25 in f)"
        , "let step arr = stencil (constant 0.0) (let s acc = (acc (-1) +. acc 0 +. acc 1) /. 3.0 in s) arr"
        , "let result = iterate 2 init step"
        ]

    it "compiler-generated halo decomposition preserves shared init consumers" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let init = generate [16] (let f [i] = float_of ((i * i) + 8) *. 0.25 in f)"
        , "let step arr = stencil (constant 0.0) (let s acc = (acc (-1) +. acc 0 +. acc 1) /. 3.0 in s) arr"
        , "let iter = iterate 2 init step"
        , "let result = zipwith (let sub x y = x -. y in sub) iter init"
        ]

    -- Shares one init array between an iterate and two halo tiles; guards
    -- against the iterate ping-pong clobbering/freeing shared init buffers
    -- (buglog Issue 7) and exercises materialized slice lowering (Issue 6).
    it "halo decomposition: split + halo + trim + append equals whole-array iterate" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let init = generate [16] (let f [i] = float_of ((i * i) + 8) *. 0.25 in f)"
        , "let step arr = stencil (constant 0.0) (let s acc = (acc (-1) +. acc 0 +. acc 0 +. acc 1) /. 4.0 in s) arr"
        , "let lhs = iterate 2 init step"
        , "let left = slice [[0, 10]] init"
        , "let right = slice [[6, 10]] init"
        , "let leftIter = iterate 2 left step"
        , "let rightIter = iterate 2 right step"
        , "let leftRes = slice [[0, 8]] leftIter"
        , "let rightRes = slice [[2, 8]] rightIter"
        , "let rhs = append leftRes rightRes"
        , "let diff = zipwith (let sub x y = x -. y in sub) lhs rhs"
        ]

    it "staged iterate chain fuses via the temporal split law and matches" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let init = generate [16] (let f [i] = float_of i *. 0.5 in f)"
        , "let step arr = stencil clamp (let s acc = (acc (-1) +. acc 0 +. acc 0 +. acc 1) /. 4.0 in s) arr"
        , "let result = let half = iterate 2 init step in iterate 3 half step"
        ]

  -- ---- Inline micro-tests: one kernel type per describe --------------------

  describe "generate" $ do

    it "1D generate [n] (\\[i] -> f i)" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let arr = generate [5] (let f [i] = i * 3 in f)" ]

    it "2D generate [m,n] (\\[i,j] -> f i j)" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let arr = generate [2, 3] (let f [i, j] = i * 3 + j in f)" ]

  describe "map" $ do

    it "map (+1) over fill" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let arr = map (let f x = x + 1 in f) (fill [4] 7)" ]

    it "map composition (double-map)" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let arr = fill [3] 2"
        , "let sq  = map (let f x = x * x in f) arr"
        , "let qd  = map (let g x = x * x in g) sq"
        ]

  describe "zipwith" $ do

    it "zipwith (+) over two generated arrays" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let a = generate [4] (let f [i] = i + 1 in f)"
        , "let b = generate [4] (let g [i] = i * 2 in g)"
        , "let r = zipwith (let add x y = x + y in add) a b"
        ]

    it "zipwith (*) then map" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let a = generate [3] (let f [i] = i + 2 in f)"
        , "let b = fill [3] 10"
        , "let r = map (let neg x = 0 - x in neg)"
        , "            (zipwith (let mul x y = x * y in mul) a b)"
        ]

  describe "reduce" $ do

    it "reduce sum over fill [n] 1 = n" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let r = reduce (let add x y = x + y in add) 0 (fill [6] 1)" ]

    it "reduce sum over 1..5 = 15" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let arr = generate [5] (let f [i] = i + 1 in f)"
        , "let r   = reduce (let add x y = x + y in add) 0 arr"
        ]

    it "reduce along trailing axis of 2D array" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let mat = generate [2, 3] (let f [i, j] = i * 3 + j + 1 in f)"
        , "let r   = reduce (let add x y = x + y in add) 0 mat"
        ]

  describe "foldl" $ do

    it "foldl sum over array of ones" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let r = foldl (let f acc x = acc + x in f) 0 (fill [5] 1)" ]

    it "foldl left-associative subtraction" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let arr = generate [4] (let f [i] = i + 1 in f)"
        , "let r   = foldl (let f acc x = acc - x in f) 20 arr"
        ]

    it "foldl max over array" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let arr = generate [5] (let f [i] = i * 3 in f)"
        , "let r   = foldl (let mx a b = if a > b then a else b in mx) 0 arr"
        ]

  describe "scan and sort_indices" $ do

    it "scan compiles and matches the interpreter" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let arr = generate [4] (let f [i] = i + 1 in f)"
        , "let r = scan (let add acc x = acc + x in add) 0 arr"
        ]

    it "scan_inclusive compiles and matches the interpreter" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let arr = generate [4] (let f [i] = i + 1 in f)"
        , "let r = scan_inclusive (let add acc x = acc + x in add) 0 arr"
        ]

    it "scanr compiles and matches the interpreter" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let arr = generate [4] (let f [i] = i + 1 in f)"
        , "let r = scanr (let add acc x = acc + x in add) 0 arr"
        ]

    it "scanr_inclusive compiles and matches the interpreter" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let arr = generate [4] (let f [i] = i + 1 in f)"
        , "let r = scanr_inclusive (let add acc x = acc + x in add) 0 arr"
        ]

    it "segmented_reduce compiles and matches the interpreter" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let offsets = generate [4] (let f [i] ="
        , "  if i = 0 then 0 else"
        , "  if i = 1 then 2 else"
        , "  if i = 2 then 5 else 5 in f)"
        , "let vals = generate [5] (let f [i] ="
        , "  if i = 0 then 1 else"
        , "  if i = 1 then 2 else"
        , "  if i = 2 then 10 else"
        , "  if i = 3 then 20 else 30 in f)"
        , "let r = segmented_reduce (let add acc x = acc + x in add) 0 offsets vals"
        ]

    it "sort_indices plus gather compiles and matches the interpreter" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let keys = generate [5] (let f [i] ="
        , "  if i = 0 then 3 else"
        , "  if i = 1 then 1 else"
        , "  if i = 2 then 3 else"
        , "  if i = 3 then 2 else 1 in f)"
        , "let vals = generate [5] (let g [i] = 10 + i in g)"
        , "let perm = sort_indices keys"
        , "let sorted_keys = gather perm keys"
        , "let sorted_vals = gather perm vals"
        , "let r = sorted_vals"
        ]

    it "coo_sum_duplicates and csr_from_sorted_coo compile and match the interpreter" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let rows = generate [6] (let f [i] ="
        , "  if i = 0 then 0 else"
        , "  if i = 1 then 0 else"
        , "  if i = 2 then 1 else"
        , "  if i = 3 then 1 else"
        , "  if i = 4 then 2 else 2 in f)"
        , "let cols = generate [6] (let f [i] ="
        , "  if i = 0 then 1 else"
        , "  if i = 1 then 1 else"
        , "  if i = 2 then 0 else"
        , "  if i = 3 then 2 else"
        , "  if i = 4 then 0 else 0 in f)"
        , "let vals = generate [6] (let f [i] ="
        , "  if i = 0 then 1 else"
        , "  if i = 1 then 2 else"
        , "  if i = 2 then 10 else"
        , "  if i = 3 then 12 else"
        , "  if i = 4 then 20 else 21 in f)"
        , "let coo = coo_sum_duplicates 3 4 6 rows cols vals"
        , "let csr = csr_from_sorted_coo coo.nrows coo.ncols coo.nnz coo.rows coo.cols coo.vals"
        , "let r = csr.row_ptr"
        ]

    it "record-based COO fields can be canonically sorted with packed keys" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let coo ="
        , "  { nrows = 3"
        , "  , ncols = 4"
        , "  , nnz = 6"
        , "  , rows = generate [6] (let f [i] ="
        , "      if i = 0 then 2 else"
        , "      if i = 1 then 0 else"
        , "      if i = 2 then 1 else"
        , "      if i = 3 then 0 else"
        , "      if i = 4 then 2 else 1 in f)"
        , "  , cols = generate [6] (let f [i] ="
        , "      if i = 0 then 1 else"
        , "      if i = 1 then 3 else"
        , "      if i = 2 then 0 else"
        , "      if i = 3 then 1 else"
        , "      if i = 4 then 0 else 2 in f)"
        , "  , vals = generate [6] (let f [i] ="
        , "      if i = 0 then 21 else"
        , "      if i = 1 then 3 else"
        , "      if i = 2 then 10 else"
        , "      if i = 3 then 1 else"
        , "      if i = 4 then 20 else 12 in f)"
        , "  }"
        , "let packed_keys ="
        , "  generate [coo.nnz]"
        , "    (let f [i] = index [i] coo.rows * coo.ncols + index [i] coo.cols in f)"
        , "let perm = sort_indices packed_keys"
        , "let sorted_rows = gather perm coo.rows"
        , "let sorted_cols = gather perm coo.cols"
        , "let r = gather perm coo.vals"
        ]

  describe "gather" $ do

    it "gather reverses array via index array [3,2,1,0]" $ withCC $
      -- Gather requires fusion: the index array must be inline so it's never
      -- materialised as a tuple array (C arrays only hold int64_t).
      checkInlineSrc $ BS.pack $ unlines
        [ "let r = let src = generate [4] (let f [i] = i * 10 in f) in"
        , "        gather (generate [4] (let g [i] = [3 - i] in g)) src"
        ]

    it "gather with repeated indices duplicates elements" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let r = let src = generate [3] (let f [i] = i + 100 in f) in"
        , "        gather (generate [5] (let g [i] = [i / 2] in g)) src"
        ]

  describe "index" $ do

    it "index [k] extracts scalar from 1D array" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let arr = generate [6] (let f [i] = i * 7 in f)"
        , "let r0  = index [0] arr"
        , "let r3  = index [3] arr"
        , "let r5  = index [5] arr"
        ]

    it "index [i,j] extracts scalar from 2D array" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let mat = generate [3, 3] (let f [i, j] = i * 10 + j in f)"
        , "let r   = index [1, 2] mat"
        ]

  describe "reshape" $ do

    it "reshape 1D→2D and back preserves elements" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let flat = generate [6] (let f [i] = i + 1 in f)"
        , "let mat  = reshape [2, 3] flat"
        , "let back = reshape [6] mat"
        ]

    it "reshape keeps the source shape observable when both aliases are used" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let src = generate [6] (let f [i] = i + 1 in f)"
        , "let mat = reshape [2, 3] src"
        , "let src_shape = shape_of src"
        , "let mat_shape = shape_of mat"
        ]

  describe "if-then-else" $ do

    it "ReLU via if-then-else inside map" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let relu x = if x > 0 then x else 0"
        , "let arr = generate [6] (let f [i] = i - 3 in f)"
        , "let r   = map relu arr"
        ]

    it "scalar branch selection" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let a = 10"
        , "let b = 20"
        , "let r = if a < b then a * 2 else b + 1"
        ]

  describe "float operations" $ do

    it "float arithmetic (pi * r^2)" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let pi   : Float = 3.14159265358979"
        , "let r    : Float = 2.0"
        , "let area : Float = pi *. (r *. r)"
        ]

    it "float dot product via zipwith + reduce" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let a = generate [4] (let f [i] = float_of (i + 1) in f)"
        , "let b = generate [4] (let g [i] = float_of (i + 1) in g)"
        , "let r = reduce (let add x y = x +. y in add) 0.0"
        , "         (zipwith (let mul x y = x *. y in mul) a b)"
        ]

    it "math function sqrt in map" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let arr = generate [4] (let f [i] = float_of (i + 1) in f)"
        , "let r   = map (fn x => sqrt x) arr"
        ]

    it "scalar sqrt values" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let x : Float = sqrt 4.0"
        , "let y : Float = sqrt 9.0"
        ]

  describe "integration" $ do

    it "map then reduce (sum of squares)" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let arr = generate [5] (let f [i] = i + 1 in f)"
        , "let sq  = map (let f x = x * x in f) arr"
        , "let r   = reduce (let add x y = x + y in add) 0 sq"
        ]

    it "generate-zipwith-reduce fused dot product" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let a = generate [4] (let f [i] = i + 1 in f)"
        , "let b = generate [4] (let g [i] = 4 - i in g)"
        , "let r = reduce (let add x y = x + y in add) 0"
        , "         (zipwith (let mul x y = x * y in mul) a b)"
        ]

    it "scatter result piped into reduce" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let src   = fill [6] 1"
        , "let idx   = generate [6] (let g [i] = i / 2 in g)"
        , "let hist  = scatter (+) (fill [3] 0) idx src"
        , "let total = reduce (let add x y = x + y in add) 0 hist"
        ]

  -- Monomorphization / specialization of polymorphic array functions.
  -- These exercise the whole-program specialization pass: a top-level helper
  -- whose element type is left polymorphic by inference is specialized to the
  -- concrete (ground) element type of the array it is actually applied to, so
  -- code generation never sees an unknown element type.  Each program's C
  -- output is checked against the interpreter.
  describe "polymorphic array specialization" $ do

    it "specializes a polymorphic identity over a float array" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let id x = x"
        , "let main = foldl (fn acc x => acc +. x) 0.0"
        , "             (id (generate [4] (fn [i] => float_of i)))"
        ]

    it "specializes a polymorphic copy with a polymorphic combine over a float array" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let dup a = zipwith (fn x y => x) a a"
        , "let main = foldl (fn acc x => acc +. x) 0.0"
        , "             (dup (generate [4] (fn [i] => float_of i)))"
        ]

    it "specializes a polymorphic map-identity over a float array" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let myid a = map (fn x => x) a"
        , "let main = foldl (fn acc x => acc +. x) 0.0"
        , "             (myid (generate [4] (fn [i] => float_of i)))"
        ]

    it "specializes a polymorphic indexing helper over a float array" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let firstOf a = index [0] a"
        , "let main = firstOf (generate [4] (fn [i] => float_of i +. 5.0))"
        ]

    it "specializes one polymorphic function at both int and float instantiations" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let dup a = zipwith (fn x y => x) a a"
        , "let main ="
        , "  let fs = foldl (fn acc x => acc +. x) 0.0"
        , "             (dup (generate [4] (fn [i] => float_of i))) in"
        , "  let is = foldl (fn acc x => acc + x) 0"
        , "             (dup (generate [4] (fn [i] => i))) in"
        , "  fs +. (float_of is)"
        ]

  -- Higher-order user functions: a function passed as an ordinary parameter and
  -- used as a combine/transform inside the callee.  Lowering aliases the
  -- parameter to the supplied function so it can be inlined at its use site.
  describe "higher-order function parameters" $ do

    it "passes a named function to a map-applying helper" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let inc x = x + 1"
        , "let apply f a = map f a"
        , "let main = foldl (fn acc x => acc + x) 0"
        , "             (apply inc (generate [4] (fn [i] => i)))"
        ]

    it "passes a lambda to a map-applying helper over a float array" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let apply f a = map f a"
        , "let main = foldl (fn acc x => acc +. x) 0.0"
        , "             (apply (fn x => x +. 1.0) (generate [4] (fn [i] => float_of i)))"
        ]

    it "applies a function parameter directly (twice)" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let twice f x = f (f x)"
        , "let inc x = x + 1"
        , "let main = twice inc 10"
        ]

    it "passes a function parameter through to zipwith" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let zw f a b = zipwith f a b"
        , "let main = foldl (fn acc x => acc + x) 0"
        , "             (zw (fn x y => x + y)"
        , "                 (generate [4] (fn [i] => i))"
        , "                 (generate [4] (fn [i] => i)))"
        ]

    it "threads a function value through a let binding" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let inc x = x + 1"
        , "let main = let g = inc in"
        , "  foldl (fn acc x => acc + x) 0 (map g (generate [4] (fn [i] => i)))"
        ]

    it "specializes a higher-order helper at both int and float instantiations" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let apply f a = map f a"
        , "let main ="
        , "  let fs = foldl (fn acc x => acc +. x) 0.0"
        , "             (apply (fn x => x +. 1.0) (generate [4] (fn [i] => float_of i))) in"
        , "  let is = foldl (fn acc x => acc + x) 0"
        , "             (apply (fn x => x + 1) (generate [4] (fn [i] => i))) in"
        , "  fs +. (float_of is)"
        ]

  -- Printing compound results whose components are arrays.  Previously the
  -- generated 'hyd_print_pair' _Generic macro could not print a pair whose
  -- fields are 'hyd_array_t*' (e.g. a pair of reduce results, each a 0-D array),
  -- causing a C compile error.  Pairs are now printed recursively per component.
  describe "printing pairs/tuples containing arrays" $ do

    it "prints a pair of 0-D arrays (reduce results)" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let main ="
        , "  let a = reduce (fn acc x => acc + x) 0 (generate [4] (fn [i] => i)) in"
        , "  let b = reduce (fn acc x => acc + x) 0 (generate [3] (fn [i] => i + 10)) in"
        , "  (a, b)"
        ]

    it "prints a pair of 1-D arrays" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let main ="
        , "  let a = generate [3] (fn [i] => i + 1) in"
        , "  let b = generate [3] (fn [i] => float_of i) in"
        , "  (a, b)"
        ]

    it "prints a nested pair mixing a 0-D array and a scalar" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let main ="
        , "  let s = reduce (fn acc x => acc + x) 0 (generate [4] (fn [i] => i)) in"
        , "  ((s, 7), s)"
        ]

  -- Vectorization correctness: a float reduction whose element is computed from
  -- the loop index (a fused 'generate') must not splat the per-lane value.
  -- Regression test for the explicit width-4 vector path, which previously
  -- broadcast a single lane's value across all lanes (e.g. summing 0..7 to 16
  -- instead of 28).
  describe "vectorized float reductions over fused generators" $ do

    it "reduces a fused float generator (sum 0..7 = 28)" $ withCC $
      checkInlineSrc $ BS.pack
        "let main = reduce (fn acc x => acc +. x) 0.0 (generate [8] (fn [i] => float_of i))"

    it "reduces a fused float generator with arithmetic on the index" $ withCC $
      checkInlineSrc $ BS.pack
        "let main = reduce (fn acc x => acc +. x) 0.0 (generate [8] (fn [i] => float_of i *. 2.0))"

    it "sums squares of a float array (map then reduce)" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let a = generate [8] (fn [i] => float_of i)"
        , "let main = reduce (fn acc x => acc +. x) 0.0 (map (fn x => x *. x) a)"
        ]

  describe "Compaction" $ do

    it "compact_stable over several blocks keeps source order" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let xs = generate [20011] (let f [i] = float_of i *. 0.5 in f)"
        , "let r = compact_stable (0.0 -. 1.0) (generate [20011] (let f [i] = ((i * 7919) % 101) < 37 in f)) xs"
        ]

    it "compact_unordered with keepers in different blocks" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let xs = generate [9000] (let f [i] = i * 3 in f)"
        , "let r = compact_unordered 0 (generate [9000] (let f [i] = (i % 4097) = 4096 in f)) xs"
        ]

    it "compaction that keeps nothing is all filler" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let xs = generate [5000] (let f [i] = i in f)"
        , "let r = compact_stable 7 (generate [5000] (let f [i] = false in f)) xs"
        ]

    it "compaction feeding a reduction" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let xs = generate [9000] (let f [i] = i * 3 in f)"
        , "let r = reduce (+) 0 (compact_unordered 0 (map (let f x = x % 2 = 0 in f) xs) xs)"
        ]

    it "compact_stable of an inline generate" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let r = compact_stable 0 (generate [10007] (let f [i] = ((i * 31) % 17) < 5 in f))"
        , "          (generate [10007] (fn [v] => v))"
        ]

    it "compact_unordered of an inline generate that reads another array" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let xs = generate [9001] (let f [i] = float_of i *. 0.25 in f)"
        , "let r = compact_unordered 0.0 (generate [9001] (let f [i] = (i % 3) = 1 in f))"
        , "          (generate [9001] (let h [i] = index [i] xs +. 1.0 in h))"
        ]

    it "does not materialize a generate source" $ do
      decs <- either (fail . ("parse: " ++)) pure $ readDecs $ BS.pack $ unlines
        [ "let main ="
        , "  let g = reify (generate [50000] (fn [i] => (i % 7) = 0)) in"
        , "  compact_stable 0 g (generate [50000] (fn [v] => v * 2))" ]
      csrc <- compileToCOptParallelIO True decs
      -- guard, output, block counts, block offsets; no array for the source
      length (filter ("hyd_array_alloc" `isInfixOf`) (lines csrc)) `shouldBe` 4

  describe "iterate_until" $ do

    it "stops as soon as the predicate holds, or after n steps" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let a = iterate_until 100 0 (fn x => x + 1) (fn x => x >= 7)"
        , "let b = iterate_until 5 0 (fn x => x + 1) (fn x => x >= 7)"
        , "let c = iterate_until 5 9 (fn x => x + 1) (fn x => x >= 7)"
        , "let d = iterate_until 0 3 (fn x => x + 1) (fn x => false)"
        ]

    it "carries array and nested-pair state" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let a = iterate_until 1000 (generate [4] (fn [i] => i))"
        , "  (fn xs => map (fn x => x * 2) xs) (fn xs => index [3] xs > 100)"
        , "let b = fst (fst (iterate_until 50 ((generate [3] (fn [i] => i), fill [3] 1), 0)"
        , "  (fn st => ((map (fn x => x + 1) (fst (fst st)), snd (fst st)), snd st + 1))"
        , "  (fn st => snd st = 4)))"
        ]

    it "leaves the loop early instead of running out the count" $ do
      decs <- either (fail . ("parse: " ++)) pure $ readDecs $ BS.pack $ unlines
        [ "let main = iterate_until 1000000 0 (fn x => x + 1) (fn x => x >= 7)" ]
      csrc <- compileToCOptIO False decs
      csrc `shouldSatisfy` isInfixOf "break;"

    it "frees per-step temporaries allocated after the stop test" $ do
      decs <- either (fail . ("parse: " ++)) pure $ readDecs $ BS.pack $ unlines
        [ "let step st ="
        , "  let xs = fst st in"
        , "  let ks = compact_stable 0 (generate [300] (fn [i] => index [i] xs % 3 = 0)) xs in"
        , "  let k = index [0] ks + 1 in"
        , "  (map (fn x => x + k) xs, snd st + 1)"
        , "let main = fst (iterate_until 100 (generate [300] (fn [i] => i), 0) step"
        , "  (fn st => snd st = 6))" ]
      csrc <- compileToCOptIO False decs
      csrc `shouldSatisfy` isInfixOf "hyd_array_free(cmp_out"

    it "an early-stopping loop with per-step temporaries computes the same result" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let step st ="
        , "  let xs = fst st in"
        , "  let ks = compact_stable 0 (generate [300] (fn [i] => index [i] xs % 3 = 0)) xs in"
        , "  let k = index [0] ks + 1 in"
        , "  (map (fn x => x + k) xs, snd st + 1)"
        , "let main = fst (iterate_until 100 (generate [300] (fn [i] => i), 0) step"
        , "  (fn st => snd st = 6))" ]

  describe "iterate buffer ownership" $ do

    -- The double-buffered iterate writes into the buffer its initial state
    -- came in from the second trip on, so a shared initial state is copied
    -- first.
    it "leaves a shared initial state intact" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let a0 = generate [8] (fn [i] => i)"
        , "let step a = generate [8] (fn [i] => index [i] a + 1)"
        , "let r = zipwith (fn x y => x * 100 + y) (iterate 3 a0 step) a0"
        , "let again = reduce (fn x y => x + y) 0 a0"
        ]

    it "returns the initial state from an iterate of zero steps" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let a0 = generate [8] (fn [i] => i * 2)"
        , "let step a = generate [8] (fn [i] => index [i] a + 1)"
        , "let r = iterate 0 a0 step"
        , "let s = reduce (fn x y => x + y) 0 a0"
        ]

    it "carries a pair state whose step passes one component through" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let a0 = generate [8] (fn [i] => i)"
        , "let step st ="
        , "  let x = fst st in"
        , "  let y = snd st in"
        , "  (generate [8] (fn [i] => index [i] x + index [i] y), x)"
        , "let r = fst (iterate 5 (a0, a0) step)"
        , "let s = reduce (fn x y => x + y) 0 a0"
        ]

  -- A scatter writes into hyd_array_own(target): the target itself when
  -- nothing else references its storage, a copy otherwise.  Each program
  -- below gives the target a second reference some other way.
  describe "in-place scatter" $ do

    it "copies a target shared with the other half of an iterate pair state" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let p0 = generate [8] (fn [i] => i)"
        , "let step st ="
        , "  let a = fst st in"
        , "  let b = snd st in"
        , "  (scatter (fn x y => x + y) a (generate [4] (fn [i] => i * 2))"
        , "     (generate [4] (fn [i] => index [i] b + 100)), b)"
        , "let r = fst (iterate 3 (p0, p0) step)"
        , "let r2 = snd (iterate 3 (p0, p0) step)"
        ]

    it "leaves a top-level target intact for a second scatter" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let h0 = generate [4] (fn [i] => i)"
        , "let a = scatter (fn x y => x + y) h0 (generate [8] (fn [i] => i / 2)) (generate [8] (fn [i] => 1))"
        , "let b = scatter (fn x y => x + y) h0 (generate [8] (fn [i] => i / 2)) (generate [8] (fn [i] => 10))"
        , "let c = h0"
        ]

    it "copies a target read after the scatter" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let r ="
        , "  let t = map (fn x => x + 1) (generate [4] (fn [i] => i * 3)) in"
        , "  let s = scatter (fn x y => x + y) t (generate [4] (fn [i] => 3 - i)) (generate [4] (fn [i] => 1)) in"
        , "  zipwith (fn x y => x * 100 + y) s t"
        ]

    it "keeps the streams reading the old target when writing in place" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let r ="
        , "  let t = map (fn x => x + 1) (generate [6] (fn [i] => i)) in"
        , "  scatter (fn x y => x + y) t (generate [6] (fn [i] => 5 - i)) (generate [6] (fn [i] => index [i] t))"
        ]

    it "copies a target that is a view of a live array" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let r ="
        , "  let a = generate [2, 3] (fn [i, j] => i * 3 + j) in"
        , "  let v = reshape [6] a in"
        , "  let s = scatter (fn x y => x + y) v (generate [6] (fn [i] => i)) (generate [6] (fn [i] => 10)) in"
        , "  zipwith (fn x y => x * 100 + y) s (reshape [6] a)"
        ]

    it "copies a target passed into a function whose caller keeps it" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let bump xs = scatter (fn x y => x + y) xs (generate [2] (fn [i] => i)) (generate [2] (fn [i] => 100))"
        , "let r ="
        , "  let a = generate [4] (fn [i] => i) in"
        , "  zipwith (fn x y => x * 1000 + y) (bump a) a"
        ]

    it "writes an iterate state that dies at the scatter in place" $ withCC $ do
      let src = BS.pack $ unlines
            [ "let r ="
            , "  let step p = scatter (fn x y => x + y) p (generate [3] (fn [i] => i * 2)) (generate [3] (fn [i] => 1)) in"
            , "  iterate 4 (generate [8] (fn [i] => 0)) step"
            ]
      checkInlineSrc src
      csrc <- emitC src
      csrc `shouldSatisfy` isInfixOf "hyd_array_own("
      csrc `shouldSatisfy` not . isInfixOf "hyd_array_own(__rc"

    it "copies, rather than moves, a target read after the scatter" $ withCC $ do
      csrc <- emitC $ BS.pack $ unlines
        [ "let r ="
        , "  let t = map (fn x => x + 1) (generate [4] (fn [i] => i * 3)) in"
        , "  let s = scatter (fn x y => x + y) t (generate [4] (fn [i] => 3 - i)) (generate [4] (fn [i] => 1)) in"
        , "  zipwith (fn x y => x * 100 + y) s t"
        ]
      csrc `shouldSatisfy` isInfixOf "hyd_array_own(__rc"

  -- An element of an array of arrays holds a reference of its own, released
  -- with the outer array.  Under the reference-count checker a freed array's
  -- data pointer is NULL, so a read through an element whose array was
  -- released crashes, and the checker reports any array never released.
  describe "arrays of arrays" $ do

    it "keeps the arrays stored in elements alive" $ withCC $
      checkWithRcCheck $ BS.pack $ unlines
        [ "let xs = generate [3] (fn [i] => generate [2] (fn [j] => i + j))"
        , "let r = map (fn q => index [1] q) xs"
        ]

    it "retains an array stored in every element" $ withCC $
      checkWithRcCheck $ BS.pack $ unlines
        [ "let a = generate [2] (fn [j] => j * 7)"
        , "let xs = generate [3] (fn [i] => a)"
        , "let r = map (fn q => index [1] q) xs"
        , "let b = a"
        ]

    it "releases the arrays in an array of arrays that dies" $ withCC $
      checkWithRcCheck $ BS.pack $ unlines
        [ "let mk n = generate [n] (fn [i] => generate [2] (fn [j] => i + j))"
        , "let step xs = map (fn a => map (fn v => v + 1) a) xs"
        , "let r = map (fn a => index [1] a) (iterate 3 (mk 3) step)"
        ]

    it "releases the arrays a scatter overwrites" $ withCC $
      checkWithRcCheck $ BS.pack $ unlines
        [ "let mk n = generate [n] (fn [i] => generate [2] (fn [j] => i + j))"
        , "let r ="
        , "  let s = scatter (fn x y => map (fn v => v * 2) y) (mk 3) (generate [4] (fn [k] => k / 2))"
        , "            (generate [4] (fn [k] => generate [2] (fn [j] => k))) in"
        , "  map (fn a => index [1] a) s"
        ]

    it "retains the arrays in a copied scatter target" $ withCC $ do
      let src = BS.pack $ unlines
            [ "let mk n = generate [n] (fn [i] => generate [2] (fn [j] => i + j))"
            , "let r ="
            , "  let t = mk 3 in"
            , "  let s = scatter (fn x y => x) t (generate [1] (fn [k] => 1))"
            , "            (generate [1] (fn [k] => generate [2] (fn [j] => 100 + j))) in"
            , "  zipwith (fn a b => index [1] a * 1000 + index [1] b) s t"
            ]
      checkWithRcCheck src
      csrc <- emitC src
      csrc `shouldSatisfy` isInfixOf "hyd_array_own(__rc"

  describe "printing pairs" $ do

    it "prints arrays, floats and nested pairs inside a pair" $ withCC $
      checkWithRcCheck $ BS.pack $ unlines
        [ "let two = let a = generate [2] (fn [j] => j * 7) in (generate [3] (fn [i] => 7), a)"
        , "let mixed = (generate [2] (fn [i] => 0.5 *. 3.0), 4)"
        , "let nested = (1, (2.5, generate [2, 2] (fn [i, j] => i + j)))"
        , "let scalars = (1.5, 2)"
        ]

  describe "Parallel reductions" $ do

    -- A max has no OpenMP reduction operator: run in parallel, its
    -- accumulator would be a plain data race.
    it "keeps a reduction without an OpenMP operator serial" $ do
      decs <- either (fail . ("parse: " ++)) pure $ readDecs $ BS.pack $ unlines
        [ "let main = reduce_generate (let mx a b = if a < b then b else a in mx) 0 [4000000]"
        , "  (fn [i] => (i * 7919) % 1000003)" ]
      csrc <- compileToCOptParallelIO False decs
      csrc `shouldNotSatisfy` isInfixOf "omp parallel for"

    it "parallelizes a scalar 1-D sum under index ()" $ do
      decs <- either (fail . ("parse: " ++)) pure $ readDecs $ BS.pack $ unlines
        [ "let xs = generate [4000000] (fn [i] => i % 3)"
        , "let main = index () (reduce_generate (+) 0 [4000000] (fn [i] => index [i] xs))" ]
      csrc <- compileToCOptParallelIO False decs
      lines csrc `shouldSatisfy`
        any (\l -> "omp parallel for" `isInfixOf` l && "reduction(+:" `isInfixOf` l)

    -- The scatter writes its state in place only when nothing read later can
    -- reach that buffer, and into a copy otherwise, so its inputs that come
    -- from the carried state cannot be the buffer it writes.
    it "runs a scatter inside iterate in parallel when its inputs come from the carried state" $ do
      decs <- either (fail . ("parse: " ++)) pure $ readDecs $ BS.pack $ unlines
        [ "let n = 1000"
        , "let idx = generate [n] (fn [i] => (i * 7) % n)"
        , "let step st ="
        , "  let k = snd st in"
        , "  let mn a b = if a < b then a else b in"
        , "  let p2 = scatter_guarded mn (fst st) idx (fill [n] k) (generate [n] (fn [i] => i < k)) in"
        , "  (p2, index () (reduce_generate (+) 0 [n] (fn [i] => index [i] p2)) % 97)"
        , "let main = fst (iterate 3 (fill [n] 5000, 50) step)" ]
      csrc <- compileToCOptParallelIO True decs
      csrc `shouldSatisfy` isInfixOf "scatter-rmw-cas-int"

    it "parallelizes a scalar sum over a local 1-D array" $ do
      decs <- either (fail . ("parse: " ++)) pure $ readDecs $ BS.pack $ unlines
        [ "let main ="
        , "  let xs = reify (generate [4000000] (fn [i] => i % 3)) in"
        , "  reduce (+) 0 xs" ]
      csrc <- compileToCOptParallelIO False decs
      lines csrc `shouldSatisfy`
        any (\l -> "omp parallel for" `isInfixOf` l && "reduction(+:" `isInfixOf` l)

  -- A combine declared with @let regroup@ may group its contributions in any
  -- way, so a reduction with it runs over blocks in parallel even without an
  -- OpenMP operator.
  describe "regroup reductions" $ do

    it "runs a reduction with a regroup-declared combine in parallel" $ do
      decs <- either (fail . ("parse: " ++)) pure $ readDecs $ BS.pack $ unlines
        [ "let regroup mx a b = if a < b then b else a"
        , "let main = reduce_generate mx 0 [4000000] (fn [i] => (i * 7919) % 1000003)" ]
      csrc <- compileToCOptParallelIO False decs
      csrc `shouldSatisfy` isInfixOf "omp parallel for"

    it "keeps a reduction with an undeclared combine serial" $ do
      decs <- either (fail . ("parse: " ++)) pure $ readDecs $ BS.pack $ unlines
        [ "let mx a b = if a < b then b else a"
        , "let main = reduce_generate mx 0 [4000000] (fn [i] => (i * 7919) % 1000003)" ]
      csrc <- compileToCOptParallelIO False decs
      csrc `shouldNotSatisfy` isInfixOf "omp parallel for"

    it "computes regroup reductions over empty, short and multi-block inputs" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let regroup mx a b = if a < b then b else a"
        , "let regroup mnf a b = if a <. b then a else b"
        , "let e = reduce_generate mx 7 [0] (fn [i] => i)"
        , "let one = reduce_generate mx 0 [1] (fn [i] => i + 5)"
        , "let xs = generate [4097] (fn [i] => (i * 37) % 4099)"
        , "let big = reduce mx 0 xs"
        , "let f = reduce_generate mnf 1000.0 [10000] (fn [i] => float_of ((i * 31) % 997) *. 0.5)"
        ]

  -- A scatter's index stream may hold index tuples, which the target is
  -- indexed by after flattening against its shape.
  describe "scatter with tuple indices" $ do

    it "scatters into a 1-D target" $ withCC $
      checkWithRcCheck $ BS.pack $ unlines
        [ "let r = scatter (fn x y => x) (generate [3] (fn [i] => i * 10)) (generate [2] (fn [k] => [k + 1])) (generate [2] (fn [k] => 100 + k))"
        ]

    it "scatters into a 2-D target" $ withCC $
      checkWithRcCheck $ BS.pack $ unlines
        [ "let r = scatter (+) (generate [3, 4] (fn [i, j] => 0)) (generate [5] (fn [k] => [k / 2, k % 4])) (generate [5] (fn [k] => k + 1))"
        , "let g = scatter_guarded (+) (generate [3, 4] (fn [i, j] => 0)) (generate [5] (fn [k] => [k / 2, k % 4]))"
        , "          (generate [5] (fn [k] => k + 1)) (generate [5] (fn [k] => k > 1))"
        ]

    it "scatters from a 2-D index stream" $ withCC $
      checkWithRcCheck $ BS.pack $ unlines
        [ "let r = scatter (+) (generate [3, 4] (fn [i, j] => 0)) (generate [2, 2] (fn [a, b] => [a + 1, b * 3])) (generate [2, 2] (fn [a, b] => a * 10 + b))"
        ]

    it "scatters from a materialised index array" $ withCC $
      checkWithRcCheck $ BS.pack $ unlines
        [ "let idx = generate [4] (fn [k] => [k % 3, 3 - k])"
        , "let r = scatter (+) (generate [3, 4] (fn [i, j] => 0)) idx (generate [4] (fn [k] => k + 1))"
        ]

  describe "slice" $ do

    it "slices along each dimension" $ withCC $
      checkWithRcCheck $ BS.pack $ unlines
        [ "let a = generate [4, 3] (fn [i, j] => i * 10 + j)"
        , "let rows = slice [[1, 2], All] a"
        , "let cols = slice [All, [1, 2]] a"
        , "let v = slice [[2, 3]] (generate [6] (fn [i] => i * i))"
        , "let c = slice [[1, 1], All, [0, 2]] (generate [2, 3, 4] (fn [i, j, k] => i * 100 + j * 10 + k))"
        ]

    it "slices a view and feeds a scatter" $ withCC $
      checkWithRcCheck $ BS.pack $ unlines
        [ "let v = slice [[1, 2]] (reshape [6] (generate [2, 3] (fn [i, j] => i * 3 + j)))"
        , "let s ="
        , "  let t = slice [[1, 2]] (generate [5] (fn [i] => i)) in"
        , "  scatter (+) t (generate [2] (fn [k] => k)) (generate [2] (fn [k] => 100))"
        , "let m ="
        , "  let a = generate [4, 3] (fn [i, j] => i * 10 + j) in"
        , "  map (fn x => x + 1) (slice [[1, 2], All] a)"
        ]

  describe "replicate" $ do

    it "adds dimensions around the source's" $ withCC $
      checkWithRcCheck $ BS.pack $ unlines
        [ "let outer = replicate [3, All] (generate [2] (fn [i] => i + 1))"
        , "let inner = replicate [All, 2] (generate [3] (fn [i] => i * 10))"
        , "let mixed = replicate [2, All, 3, All] (generate [2, 2] (fn [i, j] => i * 10 + j))"
        ]

    it "feeds a reduce and a map" $ withCC $
      checkWithRcCheck $ BS.pack $ unlines
        [ "let r = let a = generate [3] (fn [i] => i + 1) in reduce (+) 0 (replicate [All, 4] a)"
        , "let m = let a = generate [3] (fn [i] => i + 1) in map (fn x => x * 2) (replicate [2, All] a)"
        ]

    it "takes computed sizes" $ withCC $
      checkWithRcCheck $ BS.pack $ unlines
        [ "let a = let n = 1 + 2 in replicate [n, All] (generate [2] (fn [i] => i))"
        , "let b = let v = generate [4] (fn [i] => i) in replicate [index [3] v, All] (generate [2] (fn [i] => i * 5))"
        , "let c = replicate [Any 2, All] (generate [3] (fn [i] => i))"
        ]

  describe "function values" $ do

    it "lowers top-level lambda values and partial applications" $ withCC $
      checkInlineSrc $ BS.pack $ unlines
        [ "let add = fn a => fn b => a + b"
        , "let scale k x = x *. k"
        , "let ys = map (scale 2.5) (generate [4] (fn [i] => float_of i))"
        , "let zs = zipwith add (generate [3] (fn [i] => i)) (generate [3] (fn [i] => i * 2))"
        , "let main = add 3 4"
        ]

    it "truncates integer division toward zero" $ withCC $
      checkInlineSrc "let a = (0 - 7) / 2\nlet b = (0 - 7) % 2\n"

  describe "scatter nests" $ do

    -- The outer scatter's guard reads p, which the inner scatter writes.  The
    -- guard must see p as it was before either scatter: [0, 0, -1], not
    -- [0, 0, 1].
    let nestedScatterReadingP = BS.pack $ unlines
          [ "let srcs = generate [2] (let f [e] = 2 * e in f)"
          , "let dsts = generate [2] (let f [e] = 1 in f)"
          , "let step p ="
          , "  let ready a b = (a <> (-1)) & (b = (-1)) in"
          , "  let overwrite new_ old_ = new_ in"
          , "  let fwd = zipwith ready (gather srcs p) (gather dsts p) in"
          , "  let bwd = zipwith ready (gather dsts p) (gather srcs p) in"
          , "  scatter_guarded overwrite (scatter_guarded overwrite p dsts srcs fwd) srcs dsts bwd"
          , "let main = step (generate [3] (let f [i] = if i = 0 then 0 else (-1) in f))" ]
        localLetNest = BS.pack $ unlines
          [ "let main ="
          , "  let srcs = generate [2] (let f [e] = 2 * e in f) in"
          , "  let dsts = generate [2] (let f [e] = 1 in f) in"
          , "  let p = generate [3] (let f [i] = if i = 0 then 0 else (-1) in f) in"
          , "  let ready a b = (a <> (-1)) & (b = (-1)) in"
          , "  let overwrite new_ old_ = new_ in"
          , "  let fwd = zipwith ready (gather srcs p) (gather dsts p) in"
          , "  let bwd = zipwith ready (gather dsts p) (gather srcs p) in"
          , "  scatter_guarded overwrite (scatter_guarded overwrite p dsts srcs fwd) srcs dsts bwd" ]
        noFusion = defaultFrontendOptions { frontendSkipFusion = True }
        check opts src = case readDecs src of
          Left perr -> expectationFailure ("Parse error: " ++ perr)
          Right decs ->
            checkDecsWithCFlags ["-DHYD_RC_CHECK"] opts defaultInferOptions defaultPipelineOptions decs

    it "keeps the outer streams reading the original target" $ withCC $
      check defaultFrontendOptions nestedScatterReadingP

    it "keeps the outer streams reading the original target (no fusion)" $ withCC $
      check noFusion nestedScatterReadingP

    it "handles the nest over local-let arrays" $ withCC $
      check defaultFrontendOptions localLetNest

    it "handles the nest over local-let arrays (no fusion)" $ withCC $
      check noFusion localLetNest

  describe "materialized baseline" $ do

    it "computes the same results with every intermediate array written out" $ withCC $
      case readDecs $ BS.pack $ unlines
             [ "let xs = generate [64] (fn [i] => i % 7)"
             , "let h = scatter (+) (fill [7] 0) xs (map (fn x => x * 2) xs)"
             , "let s = reduce (+) 0 (zipwith (fn a b => a * b) xs (gather (generate [64] (fn [i] => [63 - i])) xs))" ] of
        Left perr -> expectationFailure ("Parse error: " ++ perr)
        Right decs ->
          checkDecsWithOptions defaultFrontendOptions { frontendMaterialize = True }
            defaultInferOptions defaultPipelineOptions decs

-- | 'checkInlineSrc' with the runtime's reference-count checker enabled.
checkWithRcCheck :: BS.ByteString -> IO ()
checkWithRcCheck src =
  case readDecs src of
    Left perr -> expectationFailure $ "Parse error: " ++ perr
    Right decs ->
      checkDecsWithCFlags ["-DHYD_RC_CHECK"]
        defaultFrontendOptions defaultInferOptions defaultPipelineOptions decs


-- | The C a source program compiles to.
emitC :: BS.ByteString -> IO String
emitC src =
  case readDecs src of
    Left perr -> fail ("Parse error: " ++ perr)
    Right decs ->
      compileToCOptIOWithAllOptions defaultFrontendOptions defaultInferOptions defaultPipelineOptions defaultCodegenOptions False decs

-- | Turn the nested scatter at the end of a declaration's let spine into an
-- 'EScatterChain' with the same combine.
chainDec :: Dec Range -> Dec Range
chainDec (Dec a v pats ty sch body) = Dec a v pats ty sch (go body)
  where
    go e = case e of
      ELetIn b d rest -> ELetIn b d (go rest)
      EBoundLetIn b x t rhs rest -> EBoundLetIn b x t rhs (go rest)
      EScatter b c (EScatter _ _ d i1 v1) i2 v2 ->
        EScatterChain b c d [ScatterPhase i1 v1 Nothing, ScatterPhase i2 v2 Nothing]
      _ -> e

