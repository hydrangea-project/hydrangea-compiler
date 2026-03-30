{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.EndToEndSpec (spec) where

import Test.Hspec
import System.Directory (findExecutable)
import System.Process (readProcessWithExitCode, createProcess, proc, waitForProcess, callProcess)
import System.Exit (ExitCode(..))
import qualified Data.ByteString.Lazy.Char8 as BS
import Text.Printf (printf)

import Language.Hydrangea.Frontend (readDecs, compileToCOptIO, evalDecsFrontend)
import Language.Hydrangea.Interpreter (Value(..))
import qualified Data.Map as Map
import Data.List (intercalate, dropWhileEnd)
import Language.Hydrangea.Syntax (Dec(..))
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import System.Directory (copyFile)
import Control.Monad (forM_, when)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Compile and run a C source string; optionally keep artifacts.
compileAndRunC :: String -> Bool -> Bool -> Bool -> IO ExitCode
compileAndRunC src keepC compileOnly isParallel =
  withSystemTempDirectory "hydrangea-test" $ \dir -> do
    let cpath = dir </> "out.c"
        exe = dir </> "hydrangea_out"
    writeFile cpath src
    let cc = "cc"
        ompFlags = if isParallel then ["-fopenmp"] else []
        flags = ["-O2", "-std=c99"] ++ ompFlags ++ ["-Iruntime", "-Ithird_party/simde", "-o", exe, cpath, "runtime/hyd_write_csv.c"]
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

-- | Skip test if 'cc' is not available.
withCC :: IO () -> IO ()
withCC action = do
  mcc <- findExecutable "cc"
  case mcc of
    Nothing -> pendingWith "cc (C compiler) not found on PATH"
    Just _  -> action

-- | Parse, interpret and C-compile a source program; assert the two outputs agree.
checkInlineSrc :: BS.ByteString -> IO ()
checkInlineSrc src = do
  case readDecs src of
    Left perr -> expectationFailure $ "Parse error: " ++ perr
    Right decs -> do
      eres <- evalDecsFrontend decs
      case eres of
        Left err -> expectationFailure $ "Interpreter error: " ++ show err
        Right env -> do
          let zeroVars = [v | Dec _ v pats _ _ <- decs, null pats]
              lookupVal v = Map.lookup v env
              formatElem (VInt n)    = show n
              formatElem (VFloat f)  = stripTrailingZerosG (printf "%.17g" f)
              formatElem (VString s) = BS.unpack s
              formatElem (VPair a b) = "(" ++ formatElem a ++ ", " ++ formatElem b ++ ")"
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
          csrc <- compileToCOptIO False decs
          ec <- compileAndRunC csrc True False False
          case ec of
            ExitFailure _ -> expectationFailure "C compilation or execution failed"
            ExitSuccess -> do
              (exit, out, _) <- readProcessWithExitCode "./hydrangea_out" [] ""
              case exit of
                ExitFailure _ -> expectationFailure "Running generated executable failed"
                ExitSuccess   -> out `shouldBe` expected

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
