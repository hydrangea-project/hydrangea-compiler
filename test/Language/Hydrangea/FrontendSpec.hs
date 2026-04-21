{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Frontend-focused tests covering parsing, typing, lowering, and code
-- generation helpers used throughout the compiler pipeline.
module Language.Hydrangea.FrontendSpec (spec) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, tails)
import Language.Hydrangea.CFG (Program(..), Proc(..), Stmt(..))
import Language.Hydrangea.CFGCore (RHS(..))
import Language.Hydrangea.CodegenC
  ( CodegenArtifacts(..)
  , CodegenOptions(..)
  , codegenProgram2WithOptionsPrune
  , defaultCodegenOptions
  )
import Language.Hydrangea.Frontend
import Language.Hydrangea.Infer (defaultInferOptions, InferOptions(..))
import Language.Hydrangea.Lexer (Range)
import Language.Hydrangea.Syntax
import Test.Hspec

parseExpOrFail :: ByteString -> IO (Exp Range)
parseExpOrFail src =
  case readExp src of
    Left err -> fail err
    Right expr -> pure expr

parseDecsOrFail :: ByteString -> IO [Dec Range]
parseDecsOrFail src =
  case readDecs src of
    Left err -> fail $ "Parse error: " ++ err
    Right decs -> pure decs

expectType :: ByteString -> Polytype -> Expectation
expectType src expected = do
  expr <- parseExpOrFail src
  res <- typeCheckExp expr
  case res of
    Left msg -> expectationFailure msg
    Right actual -> stripPreds actual `shouldBe` stripPreds expected

stripPreds :: Polytype -> Polytype
stripPreds (Forall vars _ ty) = Forall vars [] (stripRefines ty)

stripRefines :: Type -> Type
stripRefines ty =
  case ty of
    TyRefine _ inner -> stripRefines inner
    TyCons t1 t2 -> TyCons (stripRefines t1) (stripRefines t2)
    TyFun t1 t2 -> TyFun (stripRefines t1) (stripRefines t2)
    TyArray s e -> TyArray (stripRefines s) (stripRefines e)
    TyVar v -> TyVar v
    TyInt -> TyInt
    TyBool -> TyBool
    TyUnit -> TyUnit
    TyString -> TyString
    _ -> ty

expectTypeError :: ByteString -> (String -> Bool) -> Expectation
expectTypeError src predicate = do
  expr <- parseExpOrFail src
  res <- typeCheckExp expr
  case res of
    Left msg -> msg `shouldSatisfy` predicate
    Right _ -> expectationFailure "Expected type error, but got success"

expectDecsNoWarnings :: ByteString -> Expectation
expectDecsNoWarnings src = do
  decs <- parseDecsOrFail src
  res <- inferDecsTopWithWarnings decs
  case res of
    Left msg -> expectationFailure ("Expected success but got error: " ++ msg)
    Right (_, warnings) -> warnings `shouldBe` []

emitCFromSource :: ByteString -> IO String
emitCFromSource src = parseDecsOrFail src >>= compileToCOptIO False

emitParallelCFromSource :: ByteString -> IO String
emitParallelCFromSource src = parseDecsOrFail src >>= compileToCOptParallelIO False

emitExportArtifactsFromSource :: ByteString -> ByteString -> IO CodegenArtifacts
emitExportArtifactsFromSource kernel src = do
  decs <- parseDecsOrFail src
  prog <- lowerToCFG2WithTypes decs
  case codegenProgram2WithOptionsPrune
         defaultCodegenOptions
           { codegenEmitMain = False
           , codegenExportKernel = Just kernel
           }
         False
         (optimizeParallelCFG2 prog) of
    Left err -> fail err
    Right artifacts -> pure artifacts

lowerOptFromSource :: ByteString -> IO Program
lowerOptFromSource src = parseDecsOrFail src >>= lowerToCFG2OptWithTypes

lookupProc :: ByteString -> Program -> Proc
lookupProc name (Program procs) =
  case filter (\proc -> procName proc == name) procs of
    proc:_ -> proc
    [] -> error $ "Missing proc: " ++ BS.unpack name

countArrayAllocs :: Proc -> Int
countArrayAllocs Proc { procBody = stmts } =
  length [() | SAssign _ (RArrayAlloc _) <- stmts]

countZeroArgCallsInLoops :: ByteString -> Proc -> Int
countZeroArgCallsInLoops name Proc { procBody = stmts } = sum (map go stmts)
  where
    go stmt =
      case stmt of
        SLoop _ body -> countInBody body + sum (map go body)
        SIf _ tBranch eBranch -> sum (map go tBranch) + sum (map go eBranch)
        _ -> 0
    countInBody body =
      length [() | SAssign _ (RCall f []) <- body, f == name]

countLoopRhsMatches :: (RHS -> Bool) -> Proc -> Int
countLoopRhsMatches predicate Proc { procBody = stmts } = sum (map go stmts)
  where
    go stmt =
      case stmt of
        SLoop _ body -> countInBody body + sum (map go body)
        SIf _ tBranch eBranch -> sum (map go tBranch) + sum (map go eBranch)
        _ -> 0
    countInBody body =
      length [() | SAssign _ rhs <- body, predicate rhs]

countOccurrences :: String -> String -> Int
countOccurrences needle haystack
  | null needle = 0
  | otherwise = length [() | suffix <- tails haystack, needle `isPrefixOf` suffix]

countStandaloneInt64Decls :: String -> Int
countStandaloneInt64Decls src =
  length
    [ ()
    | rawLine <- lines src
    , let line = dropWhile (== ' ') rawLine
    , "int64_t " `isPrefixOf` line
    , ";" `isSuffixOf` line
    , not ("=" `isInfixOf` line)
    , not ("(" `isInfixOf` line)
    ]

spec :: Spec
spec = do
  describe "typechecking" $ do
    it "typechecks simple arithmetic expressions" $ do
      expectType "40 + 2" (Forall [] [] TyInt)
      expectType "1 + 2 * 3" (Forall [] [] TyInt)
    it "typechecks let bindings" $ do
      expectType "let x = 10 in x + 32" (Forall [] [] TyInt)
      expectType "let f x y = x + y in f" (Forall [] [] (TyFun TyInt (TyFun TyInt TyInt)))
      expectType "let f x y = x + y in f 1" (Forall [] [] (TyFun TyInt TyInt))
      expectType "let f x y = x + y in f 1 2" (Forall [] [] TyInt)
    it "typechecks let bindings with annotations" $ do
      expectType "let x : int = 10 in x + 32" (Forall [] [] TyInt)
      expectType "let f x y : int -> int -> int = x + y in f" (Forall [] [] (TyFun TyInt (TyFun TyInt TyInt)))
      expectType "let f x y : int -> int -> int = x + y in f 1" (Forall [] [] (TyFun TyInt TyInt))
      expectType "let f x y : int -> int -> int = x + y in f 1 2" (Forall [] [] TyInt)
    it "typechecks simple polymorphic functions" $ do
      expectType "let f x = x in f 10" (Forall [] [] TyInt)
      expectType "let f x = x in f true" (Forall [] [] TyBool)
      expectType "let f x : forall a . a -> a = x in f 1" (Forall [] [] TyInt)
      expectType "let f z : a * b -> a = proj 0 z in f [1,true]" (Forall [] [] TyInt)
    it "typechecks operator-as-values" $ do
      expectType "(+)" (Forall [] [] (TyFun TyInt (TyFun TyInt TyInt)))
      expectType "(-)" (Forall [] [] (TyFun TyInt (TyFun TyInt TyInt)))
      expectType "(*)" (Forall [] [] (TyFun TyInt (TyFun TyInt TyInt)))
      expectType "( * )" (Forall [] [] (TyFun TyInt (TyFun TyInt TyInt)))
      expectType "(/)" (Forall [] [] (TyFun TyInt (TyFun TyInt TyInt)))
      expectType "(=)" (Forall [] [] (TyFun TyInt (TyFun TyInt TyBool)))
      expectType "(&)" (Forall [] [] (TyFun TyBool (TyFun TyBool TyBool)))
      expectType "(*.)" (Forall [] [] (TyFun TyFloat (TyFun TyFloat TyFloat)))
    it "typechecks immutable arrays generated from a function" $ do
      expectType "let f [i] = i + 1 in generate [3] f" (Forall [] [] (TyArray (TyCons TyInt TyUnit) TyInt))
      expectType "let f [i] = i + 1 in index [1] (generate [3] f)" (Forall [] [] TyInt)
    it "typechecks fill arrays" $ do
      expectType "fill [3] 1" (Forall [] [] (TyArray (TyCons TyInt TyUnit) TyInt))
    it "typechecks immutable arrays with non-Int elements (bool)" $ do
      expectType "let f _ = true in generate [3] f" (Forall [] [] (TyArray (TyCons TyInt TyUnit) TyBool))
    it "typechecks map over arrays" $ do
      expectType "let g [i] = i + 1 in let f x = x + 1 in map f (generate [3] g)" (Forall [] [] (TyArray (TyCons TyInt TyUnit) TyInt))
      expectType "let g [i] = true in let f x = x in map f (generate [3] g)" (Forall [] [] (TyArray (TyCons TyInt TyUnit) TyBool))
    it "typechecks zipwith over arrays" $ do
      expectType
        "let g [i] = i + 1 in let h [i] = i + 2 in let f x y = x + y in zipwith f (generate [3] g) (generate [3] h)"
        (Forall [] [] (TyArray (TyCons TyInt TyUnit) TyInt))
    it "typechecks reduce over arrays" $ do
      expectType "let g [i] = i + 1 in let f x y = x + y in reduce f 0 (generate [3] g)" (Forall [] [] (TyArray TyUnit TyInt))
      expectType "let g _ = 1 in let f x y = x + y in reduce f 0 (generate [3,4] g)" (Forall [] [] (TyArray (TyCons TyInt TyUnit) TyInt))
      expectType "let g [i] = i + 1 in let f x y = x + y in index () (reduce f 0 (generate [3] g))" (Forall [] [] TyInt)
    it "typechecks scan over 1D arrays" $ do
      expectType
        "let g [i] = i + 1 in let f acc x = acc + x in scan f 0 (generate [4] g)"
        (Forall [] [] (TyArray (TyCons TyInt TyUnit) TyInt))
      expectType
        "let g [i] = i + 1 in let f acc x = acc + x in scan_inclusive f 0 (generate [4] g)"
        (Forall [] [] (TyArray (TyCons TyInt TyUnit) TyInt))
      expectType
        "let g [i] = i + 1 in let f acc x = acc + x in scanr f 0 (generate [4] g)"
        (Forall [] [] (TyArray (TyCons TyInt TyUnit) TyInt))
      expectType
        "let g [i] = i + 1 in let f acc x = acc + x in scanr_inclusive f 0 (generate [4] g)"
        (Forall [] [] (TyArray (TyCons TyInt TyUnit) TyInt))
    it "typechecks segmented_reduce over 1D offsets and values" $ do
      expectType
        "let offsets = generate [4] (let f [i] = if i = 0 then 0 else if i = 1 then 2 else if i = 2 then 5 else 5 in f) in let vals = generate [5] (let f [i] = i + 1 in f) in segmented_reduce (let add acc x = acc + x in add) 0 offsets vals"
        (Forall [] [] (TyArray (TyCons TyInt TyUnit) TyInt))
    it "can skip solver checks when requested" $ do
      case readExp "let f [i] = i + 1 in index [5] (generate [3] f)" of
        Left err -> expectationFailure err
        Right exp' -> do
          let opts = defaultInferOptions {inferSolveRefinements = False}
          res <- typeCheckExpWithOptions opts exp'
          case res of
            Left msg -> expectationFailure msg
            Right actual -> stripPreds actual `shouldBe` Forall [] [] TyInt
    it "can emit an exported header and wrapper for a zero-argument kernel" $ do
      CodegenArtifacts { codegenSource = csrc, codegenHeader = mHeader } <-
        emitExportArtifactsFromSource
          "main"
          "let grid = fill [4] 7\nlet main = grid"
      csrc `shouldSatisfy` isInfixOf "hyd_export_main"
      csrc `shouldNotSatisfy` isInfixOf "int main(void)"
      mHeader `shouldSatisfy` maybe False (isInfixOf "hyd_array_t* hyd_export_main(void);")
    it "reuses repeated scalar divisions in exported generated C" $ do
      CodegenArtifacts { codegenSource = csrc } <-
        emitExportArtifactsFromSource
          "main"
          (BS.pack $ unlines
            [ "let main ="
            , "  generate [16]"
            , "    (let g [i] ="
            , "       let q = i / 8 in"
            , "       let r = i / 8 in"
            , "       q + r"
            , "     in g)"
            ])
      countOccurrences "/ 8LL" csrc `shouldBe` 1
      csrc `shouldSatisfy` isInfixOf "hyd_export_main"
    it "avoids 1D tuple index round-trips in generated map kernels" $ do
      let src =
            BS.pack $ unlines
              [ "let src = generate [16] (let f [i] = i in f)"
              , "let main ="
              , "  generate [16]"
              , "    (let g [i] = index [i] src in g)"
              ]
      prog <- lowerOptFromSource src
      let mainProc = lookupProc "main" prog
      countLoopRhsMatches (\rhs -> case rhs of
        RFlatToNd {} -> True
        RNdToFlat {} -> True
        _ -> False) mainProc `shouldBe` 0
    it "hoists memoized zero-arg array calls out of generated loops" $ do
      CodegenArtifacts { codegenSource = csrc } <-
        emitExportArtifactsFromSource
          "main"
          (BS.pack $ unlines
            [ "let src = generate [16] (let f [i] = i in f)"
            , "let main ="
            , "  generate [16]"
            , "    (let g [i] = index [i] src in g)"
            ])
      countOccurrences "src();" csrc `shouldBe` 1
      csrc `shouldNotSatisfy` isInfixOf "hyd_flat_to_nd("
      csrc `shouldNotSatisfy` isInfixOf "hyd_nd_to_flat("
    it "only hoists live-out int temporaries before nested if blocks" $ do
      CodegenArtifacts { codegenSource = csrc } <-
        emitExportArtifactsFromSource
          "main"
          (BS.pack $ unlines
            [ "let main ="
            , "  if 0 = 0 then"
            , "    let a = 0 + 1 in"
            , "    let b = a + 2 in"
            , "    b"
            , "  else"
            , "    let c = 0 + 3 in"
            , "    let d = c + 4 in"
            , "    d"
            ])
      countStandaloneInt64Decls csrc `shouldBe` 1
      csrc `shouldNotSatisfy` isInfixOf "int64_t t1;"
    it "typechecks sort_indices over integer arrays" $ do
      expectType
        "sort_indices (generate [4] (let f [i] = i in f))"
        (Forall [] [] (TyArray (TyCons TyInt TyUnit) TyInt))
    it "typechecks coo_sum_duplicates over sorted integer COO arrays" $ do
      expectType
        "let rows = generate [6] (let f [i] = i / 2 in f) in let cols = generate [6] (let f [i] = i / 2 in f) in let vals = generate [6] (let f [i] = i + 1 in f) in coo_sum_duplicates 3 4 6 rows cols vals"
        (Forall [] [] (TyRecord [("cols", TyArray (TyCons TyInt TyUnit) TyInt), ("ncols", TyInt), ("nnz", TyInt), ("nrows", TyInt), ("rows", TyArray (TyCons TyInt TyUnit) TyInt), ("vals", TyArray (TyCons TyInt TyUnit) TyInt)]))
    it "typechecks csr_from_sorted_coo over canonical integer COO arrays" $ do
      expectType
        "let rows = generate [4] (let f [i] = i / 2 in f) in let cols = generate [4] (let f [i] = i in f) in let vals = generate [4] (let f [i] = i + 1 in f) in csr_from_sorted_coo 3 4 4 rows cols vals"
        (Forall [] [] (TyRecord [("col_idx", TyArray (TyCons TyInt TyUnit) TyInt), ("ncols", TyInt), ("nnz", TyInt), ("nrows", TyInt), ("row_ptr", TyArray (TyCons TyInt TyUnit) TyInt), ("vals", TyArray (TyCons TyInt TyUnit) TyInt)]))
    it "typechecks shape_of" $ do
      expectType "let f [i] = i + 1 in shape_of (generate [3] f)" (Forall [] [] (TyCons TyInt TyUnit))
      expectType "let f _ = 1 in shape_of (generate [3,4] f)" (Forall [] [] (TyCons TyInt (TyCons TyInt TyUnit)))
    it "typechecks reduce_generate" $ do
      expectType "let g [i] = i + 1 in let f x y = x + y in reduce_generate f 0 [3] g" (Forall [] [] (TyArray TyUnit TyInt))
      expectType "let g _ = 1 in let f x y = x + y in reduce_generate f 0 [3,4] g" (Forall [] [] (TyArray (TyCons TyInt TyUnit) TyInt))
    it "typechecks permute over arrays" $ do
      expectType
        "let d _ = 0 in let v [i] = i in let p [i] = [i] in let c x y = x + y in permute c (generate [3] d) p (generate [3] v)"
        (Forall [] [] (TyArray (TyCons TyInt TyUnit) TyInt))
    it "typechecks scatter over arrays" $ do
      expectType
        "let d _ = 0 in let v [i] = i in let idx [i] = [i] in let c x y = x + y in scatter c (generate [3] d) (generate [3] idx) (generate [3] v)"
        (Forall [] [] (TyArray (TyCons TyInt TyUnit) TyInt))
    it "typechecks scatter over arrays with scalar 1D routes" $ do
      expectType
        "let d _ = 0 in let v [i] = i in let idx [i] = i in let c x y = x + y in scatter c (generate [3] d) (generate [3] idx) (generate [3] v)"
        (Forall [] [] (TyArray (TyCons TyInt TyUnit) TyInt))
    it "typechecks gather over arrays" $ do
      expectType
        "let idx [i] = [i] in let v [i] = i + 1 in gather (generate [3] idx) (generate [3] v)"
        (Forall [] [] (TyArray (TyCons TyInt TyUnit) TyInt))
    it "typechecks gather over arrays with scalar 1D routes" $ do
      expectType
        "let idx [i] = 2 - i in let v [i] = i + 1 in gather (generate [3] idx) (generate [3] v)"
        (Forall [] [] (TyArray (TyCons TyInt TyUnit) TyInt))
    it "typechecks composed array pipelines" $ do
      expectType
        "let g [i] = i + 1 in let h [i] = i + 2 in let add x y = x + y in let inc x = x + 1 in reduce add 0 (map inc (zipwith add (generate [3] g) (generate [3] h)))"
        (Forall [] [] (TyArray TyUnit TyInt))
      expectType
        "let idx [i] = [i] in let v [i] = i + 1 in let f x = x + 1 in index [1] (map f (gather (generate [3] idx) (generate [3] v)))"
        (Forall [] [] TyInt)
      expectType
        "let d _ = 0 in let v [i] = i in let idx [i] = [i] in let c x y = x + y in let s = scatter c (generate [3] d) (generate [3] idx) (generate [3] v) in zipwith c s (generate [3] v)"
        (Forall [] [] (TyArray (TyCons TyInt TyUnit) TyInt))
      expectType
        "let d _ = 0 in let v [i] = i in let p [i] = [i] in let c x y = x + y in reduce c 0 (permute c (generate [3] d) p (generate [3] v))"
        (Forall [] [] (TyArray TyUnit TyInt))
    it "typechecks 2D arrays generated from a function and indexing" $ do
      expectType "let f _ = 1 in generate [3,4] f" (Forall [] [] (TyArray (TyCons TyInt (TyCons TyInt TyUnit)) TyInt))
      expectType "let f _ = 1 in index [1,2] (generate [3,4] f)" (Forall [] [] TyInt)
    it "typechecks replicate arrays with All/Any" $ do
      expectType
        "let f [i] = i in replicate [4, All] (generate [3] f)"
        (Forall [] [] (TyArray (TyCons TyInt (TyCons TyInt TyUnit)) TyInt))
      expectType
        "let f [i] = i in replicate [Any 2, All, 3] (generate [3] f)"
        (Forall [] [] (TyArray (TyCons TyInt (TyCons TyInt (TyCons TyInt TyUnit))) TyInt))
    it "typechecks slice arrays" $ do
      expectType
        "let f _ = 1 in slice [[0,2], All] (generate [3,4] f)"
        (Forall [] [] (TyArray (TyCons TyInt (TyCons TyInt TyUnit)) TyInt))
    it "typechecks reshape arrays" $ do
      expectType
        "let f _ = 1 in reshape [6] (generate [2,3] f)"
        (Forall [] [] (TyArray (TyCons TyInt TyUnit) TyInt))
    it "typechecks projection-based 2D generator with annotated helper" $ do
      expectType "let f a b : int -> int -> int = a + b in let g p = f (proj 0 p) (proj 1 p) in generate [3,4] g" (Forall [] [] (TyArray (TyCons TyInt (TyCons TyInt TyUnit)) TyInt))
      expectType "let f a b : int -> int -> int = a + b in let g p = f (proj 0 p) (proj 1 p) in index [1,2] (generate [3,4] g)" (Forall [] [] TyInt)
    it "typechecks projection-based 2D generator without annotation (deferred generalize)" $ do
      expectType "let f p = proj 0 p + proj 1 p in generate [3,4] f" (Forall [] [] (TyArray (TyCons TyInt (TyCons TyInt TyUnit)) TyInt))
      expectType "let f p = proj 0 p + proj 1 p in index [1,2] (generate [3,4] f)" (Forall [] [] TyInt)
    it "typechecks tuple creation" $ do
      expectType "let x : int * int * int = [1,3,4] in proj 1 x" (Forall [] [] TyInt)
      expectType
        "let x : int * int * int = [1,3,4] in x"
        (Forall [] [] (TyCons TyInt (TyCons TyInt (TyCons TyInt TyUnit))))
      expectType
        "let x : bool * int = [true,3] in x"
        (Forall [] [] (TyCons TyBool (TyCons TyInt TyUnit)))
      expectType
        "let x : bool * int = [true,3] in x"
        (Forall [] [] (TyCons TyBool (TyCons TyInt TyUnit)))
      expectType
        "[1,2,3,4,5,true,false]"
        (Forall [] [] (TyCons TyInt (TyCons TyInt (TyCons TyInt (TyCons TyInt (TyCons TyInt (TyCons TyBool (TyCons TyBool TyUnit))))))))
    it "checks refinement bounds for array indexing" $ do
      expectType "let f [i] = i + 1 in index [0] (generate [3] f)" (Forall [] [] TyInt)
      expectType "let f [i] = i + 1 in index [2] (generate [3] f)" (Forall [] [] TyInt)
      expectTypeError
        "let f [i] = i + 1 in index [3] (generate [3] f)"
        (isInfixOf "UnsatConstraints")
      expectTypeError
        "let f _ = 1 in index [1,2] (generate [1,2] f)"
        (isInfixOf "UnsatConstraints")
      expectTypeError
        "let f [i] = i + 1 in index [-1] (generate [3] f)"
        (isInfixOf "UnsatConstraints")
      expectTypeError
        "let f _ = 1 in index [0,2] (generate [1,2] f)"
        (isInfixOf "UnsatConstraints")
    it "rejects scan on non-1D arrays" $ do
      expectTypeError
        "let f acc x = acc + x in scan f 0 (generate [2,2] (let g _ = 1 in g))"
        (const True)
      expectTypeError
        "let f acc x = acc + x in scan_inclusive f 0 (generate [2,2] (let g _ = 1 in g))"
        (const True)
      expectTypeError
        "let f acc x = acc + x in scanr f 0 (generate [2,2] (let g _ = 1 in g))"
        (const True)
      expectTypeError
        "let f acc x = acc + x in scanr_inclusive f 0 (generate [2,2] (let g _ = 1 in g))"
        (const True)
    it "rejects segmented_reduce on non-int offsets" $ do
      expectTypeError
        "let offsets = generate [3] (let f [i] = i +. 1.0 in f) in let vals = generate [4] (let f [i] = i in f) in segmented_reduce (let add acc x = acc + x in add) 0 offsets vals"
        (const True)
    it "rejects sort_indices on non-int arrays" $ do
      expectTypeError
        "sort_indices (generate [3] (let f [i] = i +. 1.0 in f))"
        (const True)
    it "rejects coo_sum_duplicates on non-int values arrays" $ do
      expectTypeError
        "let rows = generate [3] (let f [i] = i in f) in let cols = generate [3] (let f [i] = i in f) in let vals = generate [3] (let f [i] = i +. 1.0 in f) in coo_sum_duplicates 3 4 3 rows cols vals"
        (const True)

  describe "optimized lowering" $ do
    it "emits explicit scan loop structure for scan" $ do
      c <- emitCFromSource $ BS.pack $ unlines
        [ "let main ="
        , "  let arr = generate [4] (let f [i] = i + 1 in f) in"
        , "  scan (let add acc x = acc + x in add) 0 arr"
        ]
      c `shouldSatisfy` isInfixOf "scan_arr"
      c `shouldSatisfy` isInfixOf "scan_k"

    it "emits explicit scan loop structure for scan_inclusive" $ do
      c <- emitCFromSource $ BS.pack $ unlines
        [ "let main ="
        , "  let arr = generate [4] (let f [i] = i + 1 in f) in"
        , "  scan_inclusive (let add acc x = acc + x in add) 0 arr"
        ]
      c `shouldSatisfy` isInfixOf "scan_incl_arr"
      c `shouldSatisfy` isInfixOf "scan_incl_k"

    it "emits explicit scan loop structure for scanr" $ do
      c <- emitCFromSource $ BS.pack $ unlines
        [ "let main ="
        , "  let arr = generate [4] (let f [i] = i + 1 in f) in"
        , "  scanr (let add acc x = acc + x in add) 0 arr"
        ]
      c `shouldSatisfy` isInfixOf "scanr_arr"
      c `shouldSatisfy` isInfixOf "scanr_ix"

    it "emits explicit scan loop structure for scanr_inclusive" $ do
      c <- emitCFromSource $ BS.pack $ unlines
        [ "let main ="
        , "  let arr = generate [4] (let f [i] = i + 1 in f) in"
        , "  scanr_inclusive (let add acc x = acc + x in add) 0 arr"
        ]
      c `shouldSatisfy` isInfixOf "scanr_incl_arr"
      c `shouldSatisfy` isInfixOf "scanr_incl_ix"

    it "emits explicit segment-bounded loop structure for segmented_reduce" $ do
      c <- emitCFromSource $ BS.pack $ unlines
        [ "let offsets = generate [4] (let f [i] = if i = 0 then 0 else if i = 1 then 2 else if i = 2 then 5 else 5 in f)"
        , "let vals = generate [5] (let f [i] = i + 1 in f)"
        , "let main = segmented_reduce (let add acc x = acc + x in add) 0 offsets vals"
        ]
      c `shouldSatisfy` isInfixOf "segred_out"
      c `shouldSatisfy` isInfixOf "segred_start"
      c `shouldSatisfy` isInfixOf "segred_len"

    it "emits the sort helper call for sort_indices" $ do
      c <- emitCFromSource $ BS.pack $ unlines
        [ "let arr = generate [5] (let f [i] ="
        , "  if i = 0 then 3 else"
        , "  if i = 1 then 1 else"
        , "  if i = 2 then 3 else"
        , "  if i = 3 then 2 else 1 in f)"
        , "let main = sort_indices arr"
        ]
      c `shouldSatisfy` isInfixOf "hyd_sort_indices("

    it "lowers COO duplicate reduction and CSR construction to explicit arrays" $ do
      c <- emitCFromSource $ BS.pack $ unlines
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
        , "let main = csr_from_sorted_coo coo.nrows coo.ncols coo.nnz coo.rows coo.cols coo.vals"
        ]
      c `shouldSatisfy` isInfixOf "coo_rows"
      c `shouldSatisfy` isInfixOf "csr_counts"
      c `shouldSatisfy` isInfixOf "csr_row_ptr"

    it "uses fused lowering for scatter CFG inspection" $ do
      let src = BS.pack $ unlines
            [ "let src = generate [8] (let f [i] = i in f)"
            , "let bin x = [x / 2]"
            , "let weight x = (x * 3) + 1"
            , "let hist = scatter (+) (fill [4] 0) (map bin src) (map weight src)"
            , "let main = hist"
            ]
      histProc <- lookupProc "hist" <$> lowerOptFromSource src
      countArrayAllocs histProc `shouldBe` 1

    it "eliminates route and values arrays for scalar weighted histogram" $ do
      let src = BS.pack $ unlines
            [ "let src = generate [8] (let f [i] = i in f)"
            , "let bin x = x / 2"
            , "let weight x = (x * 3) + 1"
            , "let hist = scatter (+) (fill [4] 0) (map bin src) (map weight src)"
            , "let main = hist"
            ]
      histProc <- lookupProc "hist" <$> lowerOptFromSource src
      countArrayAllocs histProc `shouldBe` 1

    it "eliminates route and values arrays for zipwith-derived scatter" $ do
      let src = BS.pack $ unlines
            [ "let xs = generate [8] (let fx [i] = i in fx)"
            , "let ys = generate [8] (let fy [i] = 100 + i in fy)"
            , "let bin x y = x / 2"
            , "let weight x y = x + y"
            , "let hist = scatter (+) (fill [4] 0) (zipwith bin xs ys) (zipwith weight xs ys)"
            , "let main = hist"
            ]
      histProc <- lookupProc "hist" <$> lowerOptFromSource src
      countArrayAllocs histProc `shouldBe` 1
      countZeroArgCallsInLoops "xs" histProc `shouldBe` 0
      countZeroArgCallsInLoops "ys" histProc `shouldBe` 0

    it "eliminates route and values arrays for mixed map/zipwith scatter" $ do
      let src = BS.pack $ unlines
            [ "let xs = generate [8] (let fx [i] = i in fx)"
            , "let ys = generate [8] (let fy [i] = 100 + i in fy)"
            , "let bin x = x / 2"
            , "let weight x y = x + y"
            , "let hist = scatter (+) (fill [4] 0) (map bin xs) (zipwith weight xs ys)"
            , "let main = hist"
            ]
      histProc <- lookupProc "hist" <$> lowerOptFromSource src
      countArrayAllocs histProc `shouldBe` 1
      countZeroArgCallsInLoops "xs" histProc `shouldBe` 0
      countZeroArgCallsInLoops "ys" histProc `shouldBe` 0

    it "eliminates route, values, and guard arrays for guarded scatter" $ do
      let src = BS.pack $ unlines
            [ "let src = generate [8] (let f [i] = i in f)"
            , "let bin x = x / 2"
            , "let weight x = (x * 3) + 1"
            , "let keep x = (x / 2) = ((x + 1) / 2)"
            , "let hist = scatter_guarded (+) (fill [4] 0) (map bin src) (map weight src) (map keep src)"
            , "let main = hist"
            ]
      histProc <- lookupProc "hist" <$> lowerOptFromSource src
      countArrayAllocs histProc `shouldBe` 1
      countZeroArgCallsInLoops "src" histProc `shouldBe` 0

    it "lowers reshape as a metadata-only view" $ do
      let src = BS.pack $ unlines
            [ "let flat = generate [6] (let f [i] = i + 1 in f)"
            , "let main = reshape [2, 3] flat"
            ]
      mainProc <- lookupProc "main" <$> lowerOptFromSource src
      countArrayAllocs mainProc `shouldBe` 0

  describe "code generation" $ do
    it "emits scalar helpers for projected shape dimensions" $ do
      csrc <- emitCFromSource $ BS.pack $ unlines
        [ "let M = get_env_int \"M\""
        , "let N = get_env_int \"N\""
        , "let arr = read_array_float [M,N] \"data.csv\""
        , "let s = shape_of arr"
        , "let h = proj 0 s"
        , "let w = proj 1 s"
        , "let f [i,j] = index [i,j] arr"
        , "let main = generate [h,w] f"
        ]
      csrc `shouldSatisfy` isInfixOf "int64_t h(void)"
      csrc `shouldSatisfy` isInfixOf "int64_t w(void)"

    it "emits OpenMP parallel loops for injective scatter kernels" $ do
      csrc <- emitParallelCFromSource $ BS.pack $ unlines
        [ "let main = scatter (+) (fill [8] 0)"
        , "  (generate [8] (let f [i] = i in f))"
        , "  (generate [8] (let g [i] = i + 1 in g))"
        ]
      csrc `shouldSatisfy` isInfixOf "#pragma omp parallel for"

    it "emits the reshape view helper for size-preserving reshape" $ do
      csrc <- emitCFromSource $ BS.pack $ unlines
        [ "let flat = generate [6] (let f [i] = i + 1 in f)"
        , "let main = reshape [2, 3] flat"
        ]
      csrc `shouldSatisfy` isInfixOf "hyd_array_reshape_view("

    it "emits atomic parallel scatter code for weighted histogram" $ do
      csrc <- emitParallelCFromSource $ BS.pack $ unlines
        [ "let src = generate [8] (let f [i] = i in f)"
        , "let idx = map (let r x = x / 2 in r) src"
        , "let vals = map (let g x = (x * 3) + 1 in g) src"
        , "let main = scatter (+) (fill [4] 0) idx vals"
        ]
      csrc `shouldSatisfy` isInfixOf "#pragma omp parallel for /* scatter-atomic-add-int */"
      csrc `shouldSatisfy` isInfixOf "#pragma omp atomic update"

    it "emits privatized parallel scatter code for dense small histograms" $ do
      csrc <- emitParallelCFromSource $ BS.pack $ unlines
        [ "let main = scatter (+) (fill [8] 0)"
        , "  (generate [64] (let bucket [i] = i / 8 in bucket))"
        , "  (generate [64] (let one [i] = 1 in one))"
        ]
      csrc `shouldSatisfy` isInfixOf "#pragma omp parallel /* scatter-privatized-int-add */"
      csrc `shouldSatisfy` isInfixOf "#pragma omp for"
      csrc `shouldSatisfy` isInfixOf "#pragma omp critical"
      csrc `shouldSatisfy` isInfixOf "calloc((size_t)"

    it "emits atomic guarded parallel scatter code for masked weighted histogram" $ do
      csrc <- emitParallelCFromSource $ BS.pack $ unlines
        [ "let main ="
        , "  let src = generate [16] (let f [i] = i in f) in"
        , "  scatter_guarded (+) (fill [4] 0)"
        , "    (map (let r x = x / 4 in r) src)"
        , "    (map (let g x = (x * 3) + 1 in g) src)"
        , "    (map (let keep x = ((x / 3) * 3) = x in keep) src)"
        ]
      csrc `shouldSatisfy` isInfixOf "#pragma omp parallel for /* scatter-atomic-add-int */"
      csrc `shouldSatisfy` isInfixOf "#pragma omp atomic update"
      csrc `shouldSatisfy` isInfixOf "if ("

    it "emits atomic parallel scatter code for floating-point weighted histogram" $ do
      csrc <- emitParallelCFromSource $ BS.pack $ unlines
        [ "let src = generate [8] (let f [i] = i in f)"
        , "let idx = map (let r x = x / 2 in r) src"
        , "let vals = map (let g x = float_of ((x * 3) + 1) /. 8.0 in g) src"
        , "let main = scatter (+.) (fill [4] 0.0) idx vals"
        ]
      csrc `shouldSatisfy` isInfixOf "#pragma omp parallel for /* scatter-atomic-add-float */"
      csrc `shouldSatisfy` isInfixOf "#pragma omp atomic update"

    it "emits array-valued hyd_main with array return type" $ do
      csrc <- emitCFromSource "let main = fill [3] 1"
      csrc `shouldSatisfy` isInfixOf "hyd_array_t* hyd_main(void)"
      expectTypeError
        "let f _ = 1 in index [1,0] (generate [1,2] f)"
        (isInfixOf "UnsatConstraints")
      expectTypeError
        "let f _ = 1 in index [2,1] (generate [2,2] f)"
        (isInfixOf "UnsatConstraints")
    it "typechecks check_index with matching default value" $ do
      expectType "let f [i] = i + 1 in check_index [3] 0 (generate [3] f)" (Forall [] [] TyInt)
      expectType "let f [i] = i + 1 in check_index [0] 0 (generate [3] f)" (Forall [] [] TyInt)
      expectType "let f [i] = i + 1 in check_index [10] (-1) (generate [3] f)" (Forall [] [] TyInt)
    it "typechecks check_index with 2D arrays" $ do
      expectType "let f _ = 1 in check_index [5,5] 0 (generate [3,4] f)" (Forall [] [] TyInt)
      expectType "let f _ = true in check_index [1,2] false (generate [2,3] f)" (Forall [] [] TyBool)
    it "check_index does not need bounds checking" $ do
      expectType "let f [i] = i + 1 in check_index [3] 0 (generate [3] f)" (Forall [] [] TyInt)
      expectType "let f [i] = i + 1 in check_index [-1] 0 (generate [3] f)" (Forall [] [] TyInt)
      expectType "let f _ = 1 in check_index [100,200] 0 (generate [1,2] f)" (Forall [] [] TyInt)
    it "check_index type error when default value has wrong type" $ do
      expectTypeError
        "let f [i] = i + 1 in check_index [3] true (generate [3] f)"
        (isInfixOf "")
    it "checks symbolic bounds for unknown sizes" $ do
      expectType
        "let g n = let f [i] = i in index [0] (generate [n] f) in g"
        (Forall [] [] (TyFun TyInt TyInt))
      expectType
        "let g n = let f [i] = i in index [n - 1] (generate [n] f) in g"
        (Forall [] [] (TyFun TyInt TyInt))
      expectTypeError
        "let g n = let f [i] = i in index [n] (generate [n] f) in g"
        (isInfixOf "UnsatConstraints")
    it "checks symbolic bounds without literals" $ do
      expectType
        "let h n = let f [i] = i in index [n - n] (generate [n] f) in h"
        (Forall [] [] (TyFun TyInt TyInt))
      expectTypeError
        "let h n = let f [i] = i in index [n] (generate [n] f) in h"
        (isInfixOf "UnsatConstraints")
      expectType
        "let g n m = let f [i,j] = i + j in index [n - n, m - m] (generate [n,m] f) in g"
        (Forall [] [] (TyFun TyInt (TyFun TyInt TyInt)))
      expectTypeError
        "let g n m = let f [i,j] = i + j in index [n, m - m] (generate [n,m] f) in g"
        (isInfixOf "UnsatConstraints")
      expectType
        "let z n x = let f [i] = i in let a = generate [n] f in reduce (+) x (zipwith (+) a a) in z"
        (Forall [] [] (TyFun TyInt (TyFun TyInt (TyArray TyUnit TyInt))))
    it "checks indexing with array parameters" $ do
      expectType
        "let f a = index [0] a in let g [i] = i in f (generate [3] g)"
        (Forall [] [] TyInt)
      expectTypeError
        "let f a = index [3] a in let g [i] = i in f (generate [3] g)"
        (isInfixOf "UnsatConstraints")
      expectType
        "let f a = index [1,1] a in let g _ = 1 in f (generate [2,2] g)"
        (Forall [] [] TyInt)
      expectTypeError
        "let f a = index [2,1] a in let g _ = 1 in f (generate [2,2] g)"
        (isInfixOf "UnsatConstraints")
    it "links refinement binders across applications" $ do
      expectTypeError
        "let f a = index [3] a in let g [i] = i in let h = f in h (generate [3] g)"
        (isInfixOf "UnsatConstraints")
      expectTypeError
        "let f a b = index [2] b in let g [i] = i in f 0 (generate [2] g)"
        (isInfixOf "UnsatConstraints")
      expectType
        "let f a b = index [1] b in let g [i] = i in f 0 (generate [2] g)"
        (Forall [] [] TyInt)
    it "checks bounds for slice and reshape" $ do
      expectTypeError
        "let f _ = 1 in slice [[2,2], All] (generate [3,4] f)"
        (isInfixOf "UnsatConstraints")
      expectTypeError
        "let f _ = 1 in reshape [5] (generate [2,3] f)"
        (isInfixOf "UnsatConstraints")
    it "checks bounds for shape operations" $ do
      expectTypeError
        "index [3] (fill [3] 0)"
        (isInfixOf "UnsatConstraints")
      expectTypeError
        "let f [i] = i in index [4,0] (replicate [4, All] (generate [3] f))"
        (isInfixOf "UnsatConstraints")
      expectTypeError
        "let f _ = 1 in slice [[0,-1], All] (generate [3,4] f)"
        (isInfixOf "UnsatConstraints")
      expectTypeError
        "let f _ = 1 in reshape [-1] (generate [1] f)"
        (isInfixOf "UnsatConstraints")
    it "typechecks shape extraction with shape_of" $ do
      expectType "let f _ = 1 in shape_of (generate [3] f)" (Forall [] [] (TyCons TyInt TyUnit))
      expectType "let f _ = 1 in shape_of (generate [3,4] f)" (Forall [] [] (TyCons TyInt (TyCons TyInt TyUnit)))
      expectType "let f _ = 1 in shape_of (generate [2,3,4] f)" (Forall [] [] (TyCons TyInt (TyCons TyInt (TyCons TyInt TyUnit))))
    it "typechecks shape tuple projection" $ do
      expectType "let f _ = 1 in let s = shape_of (generate [3] f) in proj 0 s" (Forall [] [] TyInt)
      expectType "let f _ = 1 in let s = shape_of (generate [3,4] f) in proj 0 s" (Forall [] [] TyInt)
      expectType "let f _ = 1 in let s = shape_of (generate [3,4] f) in proj 1 s" (Forall [] [] TyInt)
    it "typechecks array generation using extracted shape" $ do
      expectType
        "let f _ = 1 in let arr1 = generate [3] f in let s = shape_of arr1 in let g [i] = i in generate s g"
        (Forall [] [] (TyArray (TyCons TyInt TyUnit) TyInt))
      expectType
        "let f _ = 1 in let arr = generate [2,3] f in let s = shape_of arr in let g p = proj 0 p + proj 1 p in generate s g"
        (Forall [] [] (TyArray (TyCons TyInt (TyCons TyInt TyUnit)) TyInt))
    it "typechecks shape manipulation with pattern matching" $ do
      expectType
        "let f _ = 1 in let arr = generate [3,4] f in let s = shape_of arr in let d1 = proj 0 s in fill [d1] 0"
        (Forall [] [] (TyArray (TyCons TyInt TyUnit) TyInt))
      expectType
        "let f _ = 1 in let arr = generate [3,4] f in let s = shape_of arr in let d2 = proj 1 s in fill [d2] 1"
        (Forall [] [] (TyArray (TyCons TyInt TyUnit) TyInt))
    it "typechecks replicate with shape derived from another array" $ do
      expectType
        "let f _ = 1 in let src = generate [3] f in let s = shape_of src in let g _ = 5 in replicate [Any 2, All] (generate s g)"
        (Forall [] [] (TyArray (TyCons TyInt (TyCons TyInt TyUnit)) TyInt))
    it "typechecks reshape using shape from generated array" $ do
      expectType
        "let f _ = 1 in let src = generate [6] f in let total = 2 * 3 in reshape [total] src"
        (Forall [] [] (TyArray (TyCons TyInt TyUnit) TyInt))
    it "typechecks complex shape transformation pipeline" $ do
      expectType
        "let f _ = 1 in let src = generate [2,3] f in let s = shape_of src in let r = proj 0 s in let c = proj 1 s in let new_shape = [c,r] in let g p = proj 0 p + proj 1 p in reshape [r * c] (generate new_shape g)"
        (Forall [] [] (TyArray (TyCons TyInt TyUnit) TyInt))
    it "typechecks shape-dependent array composition" $ do
      expectType
        "let f _ = 1 in let arr1 = generate [3] f in let s = shape_of arr1 in let inc x = x + 1 in let zero _ = 0 in map inc (generate s zero)"
        (Forall [] [] (TyArray (TyCons TyInt TyUnit) TyInt))
    it "typechecks indexing arrays generated from extracted shapes" $ do
      expectType
        "let f _ = 1 in let arr = generate [4,5] f in let s = shape_of arr in let g p = proj 0 p in index [0,0] (generate s g)"
        (Forall [] [] TyInt)
    it "typechecks dimension extraction and reuse in operations" $ do
      expectType
        "let f _ = 1 in let arr = generate [3,4] f in let s = shape_of arr in let h = proj 0 s in let w = proj 1 s in let g1 [i] = i in let g2 [i] = i + w in zipwith (+) (generate [h] g1) (generate [h] g2)"
        (Forall [] [] (TyArray (TyCons TyInt TyUnit) TyInt))
    it "proves projected generated dimensions are safe index bounds without warnings" $ do
      expectDecsNoWarnings $ BS.unlines
        [ "let g n m ="
        , "  let f [i,j] = i + j in"
        , "  let arr = generate [n,m] f in"
        , "  let s = shape_of arr in"
        , "  let h = proj 0 s in"
        , "  let w = proj 1 s in"
        , "  index [h - 1, w - 1] arr"
        ]
    it "accepts zero-sized generate and gather pipelines without warnings" $ do
      expectDecsNoWarnings $ BS.unlines
        [ "let idx = generate [0] (let f [i] = i in f)"
        , "let src = fill [0] 0"
        , "let main = gather idx src"
        ]
    it "typechecks the matmul benchmark with solver-backed refinement checking" $ do
      src <- BS.readFile "bench/matmul/mat_mul_bench.hyd"
      expectDecsNoWarnings src
    it "proves projected-shape helper callback chains without warnings" $ do
      expectDecsNoWarnings $ BS.unlines
        [ "let add x y = x + y"
        , "let src = generate [4, 5] (let cell [i, j] = i + j in cell)"
        , "let s = shape_of src"
        , "let h = proj 0 s"
        , "let w = proj 1 s"
        , "let rowElem row [i] = index [row, i] src"
        , "let rowSum [row] = index () (reduce_generate add 0 [w] (rowElem row))"
        , "let main = generate [h] rowSum"
        ]
    it "typechecks read_array with 1D shape" $ do
      expectType "read_array [3] \"data.csv\"" (Forall [] [] (TyArray (TyCons TyInt TyUnit) TyInt))
    it "typechecks read_array with 2D shape" $ do
      expectType "read_array [2,3] \"data.csv\"" (Forall [] [] (TyArray (TyCons TyInt (TyCons TyInt TyUnit)) TyInt))
    it "typechecks read_array used in a pipeline" $ do
      expectType "let f x = x + 1 in map f (read_array [4] \"data.csv\")" (Forall [] [] (TyArray (TyCons TyInt TyUnit) TyInt))
    it "typechecks read_array in reduce" $ do
      expectType "let add x y = x + y in reduce add 0 (read_array [5] \"data.csv\")" (Forall [] [] (TyArray TyUnit TyInt))

  describe "vectorization codegen" $ do
    it "emits explicit vector loads and stores for float LoopMap kernels" $ do
      let src = BS.pack $ unlines
            [ "let arr = fill [8] 1.0"
            , "let inc x = x +. 1.0"
            , "let main = map inc arr"
            ]
      case readDecs src of
        Left err -> expectationFailure err
        Right ds -> do
          let csrc = compileToCOpt ds
          csrc `shouldSatisfy` isInfixOf "hyd_vec_loadu_f64"
          csrc `shouldSatisfy` isInfixOf "hyd_vec_storeu_f64"

    it "emits explicit vector loads for dense generate/index float kernels" $ do
      let src = BS.pack $ unlines
            [ "let arr = fill [8] 1.0"
            , "let main = generate [8] (fn [i] => index [i] arr +. 1.0)"
            ]
      case readDecs src of
        Left err -> expectationFailure err
        Right ds -> do
          let csrc = compileToCOpt ds
          csrc `shouldSatisfy` isInfixOf "hyd_vec_loadu_f64"
          csrc `shouldSatisfy` isInfixOf "hyd_vec_storeu_f64"

    it "emits explicit vector unary math helpers for float map kernels" $ do
      let src = BS.pack $ unlines
            [ "let arr = fill [8] 2.0"
            , "let f x = sqrt x +. log x +. expf ((-1.0) *. x) +. erf x"
            , "let main = map f arr"
            ]
      case readDecs src of
        Left err -> expectationFailure err
        Right ds -> do
          let csrc = compileToCOpt ds
          csrc `shouldSatisfy` isInfixOf "hyd_vec_sqrt_f64"
          csrc `shouldSatisfy` isInfixOf "hyd_vec_log_f64"
          csrc `shouldSatisfy` isInfixOf "hyd_vec_exp_f64"
          csrc `shouldSatisfy` isInfixOf "hyd_vec_erf_f64"
          csrc `shouldSatisfy` isInfixOf "hyd_vec_storeu_f64"

    it "keeps integer LoopMap kernels on the hint-only path" $ do
      let src = BS.pack $ unlines
            [ "let arr = fill [8] 1"
            , "let inc x = x + 1"
            , "let main = map inc arr"
            ]
      case readDecs src of
        Left err -> expectationFailure err
        Right ds -> do
          let csrc = compileToCOpt ds
          csrc `shouldSatisfy` isInfixOf "#pragma omp simd"
          csrc `shouldSatisfy` (not . isInfixOf "hyd_vec_loadu_f64")
          csrc `shouldSatisfy` (not . isInfixOf "hyd_vec_storeu_f64")

  describe "parsing" $ do
    it "parses simple arithmetic expressions" $ do
      (readExp "40 + 2" >>= stripRange)
        `shouldBe` Right
          ( EBinOp
              ()
              (EInt () 40)
              (Plus ())
              (EInt () 2)
          )
      (readExp "x + y * z" >>= stripRange)
        `shouldBe` Right
          ( EBinOp
              ()
              (EVar () "x")
              (Plus ())
              ( EBinOp
                  ()
                  (EVar () "y")
                  (Times ())
                  (EVar () ("z"))
              )
          )
      (readExp "(x + y) * z" >>= stripRange)
        `shouldBe` Right
          ( EBinOp
              ()
              ( EBinOp
                  ()
                  (EVar () "x")
                  (Plus ())
                  (EVar () "y")
              )
              (Times ())
              (EVar () "z")
          )
    it "parses vector values" $ do
      (readExp "[1,2]" >>= stripRange)
        `shouldBe` Right (EVec () [EInt () 1, EInt () 2])
      (readExp "[1,2,3]" >>= stripRange)
        `shouldBe` Right (EVec () [EInt () 1, EInt () 2, EInt () 3])
    it "parses shape operations" $ do
      (readExp "fill [3] 1" >>= stripRange)
        `shouldBe` Right (EFill () (EVec () [EInt () 3]) (EInt () 1))
      (readExp "replicate [4, All] x" >>= stripRange)
        `shouldBe` Right
          (EReplicate () [ShapeDim () (EInt () 4), ShapeAll ()] (EVar () "x"))
      (readExp "replicate [Any 2, All] x" >>= stripRange)
        `shouldBe` Right
          (EReplicate () [ShapeAny () (EInt () 2), ShapeAll ()] (EVar () "x"))
      (readExp "slice [[0,2], All] x" >>= stripRange)
        `shouldBe` Right
          (ESlice () [SliceRange () (EInt () 0) (EInt () 2), SliceAll ()] (EVar () "x"))
      (readExp "reshape [3] x" >>= stripRange)
        `shouldBe` Right (EReshape () (EVec () [EInt () 3]) (EVar () "x"))
      (readExp "shape_of x" >>= stripRange)
        `shouldBe` Right (EShapeOf () (EVar () "x"))
      (readExp "reduce_generate f 0 [3] g" >>= stripRange)
        `shouldBe` Right
          ( EReduceGenerate
              ()
              (EVar () "f")
              (EInt () 0)
              (EVec () [EInt () 3])
              (EVar () "g")
          )
    it "parses let bindings" $ do
      (readExp "let x : int = 10 in 1" >>= stripRange)
        `shouldBe` Right
          ( ELetIn
              ()
              (Dec () "x" [] Nothing (Just (Forall [] [] TyInt)) (EInt () 10))
              (EInt () 1)
          )
      (readExp "let x : forall a . a = f y in x" >>= stripRange)
        `shouldBe` Right
          ( ELetIn
              ()
              ( Dec
                  ()
                  "x"
                  []
                  Nothing
                  (Just (Forall ["a"] [] (TyVar "a")))
                  (EApp () (EVar () "f") (EVar () "y"))
              )
              (EVar () ("x"))
          )
      (readExp "let f x : int -> int = x + 1 in f 10" >>= stripRange)
        `shouldBe` Right
          ( ELetIn
              ()
              ( Dec
                  ()
                  ("f")
                  [PVar () ("x")]
                  Nothing
                  (Just (Forall [] [] (TyFun TyInt TyInt)))
                  (EBinOp () (EVar () ("x")) (Plus ()) (EInt () 1))
              )
              (EApp () (EVar () ("f")) (EInt () 10))
          )
      (readExp "let f a b : int -> int -> int = a + b in f 10 11" >>= stripRange)
        `shouldBe` Right
          ( ELetIn
              ()
              ( Dec
                  ()
                  ("f")
                  [PVar () ("a"), PVar () ("b")]
                  Nothing
                  (Just (Forall [] [] (TyFun TyInt (TyFun TyInt TyInt))))
                  (EBinOp () (EVar () ("a")) (Plus ()) (EVar () ("b")))
              )
              (EApp () (EApp () (EVar () ("f")) (EInt () 10)) (EInt () 11))
          )
      (readExp "let f [x,y] : int * int -> int = x + y in f [1,2]" >>= stripRange)
        `shouldBe` Right
          ( ELetIn
              ()
              ( Dec
                  ()
                  "f"
                  [PVec () [PVar () "x", PVar () "y"]]
                  Nothing
                  ( Just
                      ( Forall
                          []
                          []
                          (TyFun (TyCons TyInt (TyCons TyInt TyUnit)) TyInt)
                      )
                  )
                  (EBinOp () (EVar () "x") (Plus ()) (EVar () "y"))
              )
              (EApp () (EVar () "f") (EVec () [EInt () 1, EInt () 2]))
          )

    it "parses types" $ do
      (readPolytype "int")
        `shouldBe` Right (Forall [] [] TyInt)
      (readPolytype "()")
        `shouldBe` Right (Forall [] [] TyUnit)
      (readPolytype "() -> int")
        `shouldBe` Right (Forall [] [] (TyFun TyUnit TyInt))
      (readPolytype "int * int")
        `shouldBe` Right (Forall [] [] (TyCons TyInt (TyCons TyInt TyUnit)))
      (readPolytype "int * int * int")
        `shouldBe` Right (Forall [] [] (TyCons TyInt (TyCons TyInt (TyCons TyInt TyUnit))))
      (readPolytype "forall a . a * int")
        `shouldBe` Right (Forall ["a"] [] (TyCons (TyVar "a") (TyCons TyInt TyUnit)))
      (readPolytype "forall a . a -> a")
        `shouldBe` Right
          (Forall ["a"] [] (TyFun (TyVar "a") (TyVar "a")))
      (readPolytype "forall a b . a * b -> b * a")
        `shouldBe` Right
          ( Forall
              ["a", "b"]
              []
              ( TyFun
                  (TyCons (TyVar "a") (TyCons (TyVar "b") TyUnit))
                  (TyCons (TyVar "b") (TyCons (TyVar "a") TyUnit))
              )
          )
