{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.ParserSpec (spec) where

import Test.Hspec
import Language.Hydrangea.Lexer (runAlex)
import Language.Hydrangea.Parser (parseMiniML, parsePolytype)
import Language.Hydrangea.Syntax

spec :: Spec
spec = describe "parser" $ do
  it "parses generate as a special form" $ do
    let res = runAlex "generate 3 f" parseMiniML
    case res of
      Left err -> expectationFailure ("lexer/parser failed: " ++ err)
      Right ast -> fmap (const ()) ast `shouldBe` EGenerate () (EInt () 3) (EVar () "f")

  it "parses index combined with generate" $ do
    let res = runAlex "index 1 (generate 3 f)" parseMiniML
    case res of
      Left err -> expectationFailure ("lexer/parser failed: " ++ err)
      Right ast -> fmap (const ()) ast `shouldBe` EIndex () (EInt () 1) (EGenerate () (EInt () 3) (EVar () "f"))

  it "parses map as a special form" $ do
    let res = runAlex "map f arr" parseMiniML
    case res of
      Left err -> expectationFailure ("lexer/parser failed: " ++ err)
      Right ast -> fmap (const ()) ast `shouldBe` EMap () (EVar () "f") (EVar () "arr")

  it "parses zipwith as a special form" $ do
    let res = runAlex "zipwith f a b" parseMiniML
    case res of
      Left err -> expectationFailure ("lexer/parser failed: " ++ err)
      Right ast -> fmap (const ()) ast `shouldBe` EZipWith () (EVar () "f") (EVar () "a") (EVar () "b")

  it "parses reduce as a special form" $ do
    let res = runAlex "reduce f 0 arr" parseMiniML
    case res of
      Left err -> expectationFailure ("lexer/parser failed: " ++ err)
      Right ast -> fmap (const ()) ast `shouldBe` EReduce () (EVar () "f") (EInt () 0) (EVar () "arr")

  it "parses scan as a special form" $ do
    let res = runAlex "scan f 0 arr" parseMiniML
    case res of
      Left err -> expectationFailure ("lexer/parser failed: " ++ err)
      Right ast -> fmap (const ()) ast `shouldBe` EScan () (EVar () "f") (EInt () 0) (EVar () "arr")

  it "parses segmented_reduce as a special form" $ do
    let res = runAlex "segmented_reduce f 0 offsets vals" parseMiniML
    case res of
      Left err -> expectationFailure ("lexer/parser failed: " ++ err)
      Right ast ->
        fmap (const ()) ast `shouldBe`
          ESegmentedReduce () (EVar () "f") (EInt () 0) (EVar () "offsets") (EVar () "vals")

  it "parses sort_indices as a special form" $ do
    let res = runAlex "sort_indices arr" parseMiniML
    case res of
      Left err -> expectationFailure ("lexer/parser failed: " ++ err)
      Right ast -> fmap (const ()) ast `shouldBe` ESortIndices () (EVar () "arr")

  it "parses coo_sum_duplicates as a special form" $ do
    let res = runAlex "coo_sum_duplicates nrows ncols nnz rows cols vals" parseMiniML
    case res of
      Left err -> expectationFailure ("lexer/parser failed: " ++ err)
      Right ast ->
        fmap (const ()) ast `shouldBe`
          ECOOSumDuplicates () (EVar () "nrows") (EVar () "ncols") (EVar () "nnz")
            (EVar () "rows") (EVar () "cols") (EVar () "vals")

  it "parses csr_from_sorted_coo as a special form" $ do
    let res = runAlex "csr_from_sorted_coo nrows ncols nnz rows cols vals" parseMiniML
    case res of
      Left err -> expectationFailure ("lexer/parser failed: " ++ err)
      Right ast ->
        fmap (const ()) ast `shouldBe`
          ECSRFromSortedCOO () (EVar () "nrows") (EVar () "ncols") (EVar () "nnz")
            (EVar () "rows") (EVar () "cols") (EVar () "vals")

  it "parses permute as a special form" $ do
    let res = runAlex "permute c defaults perm src" parseMiniML
    case res of
      Left err -> expectationFailure ("lexer/parser failed: " ++ err)
      Right ast ->
        fmap (const ()) ast `shouldBe` EPermute () (EVar () "c") (EVar () "defaults") (EVar () "perm") (EVar () "src")

  it "parses scatter as a special form" $ do
    let res = runAlex "scatter c defaults idx vals" parseMiniML
    case res of
      Left err -> expectationFailure ("lexer/parser failed: " ++ err)
      Right ast ->
        fmap (const ()) ast `shouldBe` EScatter () (EVar () "c") (EVar () "defaults") (EVar () "idx") (EVar () "vals")

  it "parses scatter_guarded as a special form" $ do
    let res = runAlex "scatter_guarded c defaults idx vals guard" parseMiniML
    case res of
      Left err -> expectationFailure ("lexer/parser failed: " ++ err)
      Right ast ->
        fmap (const ()) ast `shouldBe` EScatterGuarded () (EVar () "c") (EVar () "defaults") (EVar () "idx") (EVar () "vals") (EVar () "guard")

  it "parses gather as a special form" $ do
    let res = runAlex "gather idx arr" parseMiniML
    case res of
      Left err -> expectationFailure ("lexer/parser failed: " ++ err)
      Right ast -> fmap (const ()) ast `shouldBe` EGather () (EVar () "idx") (EVar () "arr")

  it "parses polytypes via the parser entrypoint" $ do
    let res = runAlex "forall a . a -> a" parsePolytype
    res `shouldBe` Right (Forall ["a"] [] (TyFun (TyVar "a") (TyVar "a")))

  it "fails to parse generate when args are not parenthesized" $ do
    -- according to the grammar generate requires atom arguments; `1+2`
    -- without parentheses is not an atom and should therefore be a parse error
    let res = runAlex "generate 1+2 f" parseMiniML
    res `shouldSatisfy` either (const True) (const False)

  it "parses generate when arguments are parenthesized" $ do
    let res = runAlex "generate (1 + 2) f" parseMiniML
    case res of
      Left err -> expectationFailure ("lexer/parser failed: " ++ err)
      Right ast -> fmap (const ()) ast `shouldBe` EGenerate () (EBinOp () (EInt () 1) (Plus ()) (EInt () 2)) (EVar () "f")

  it "parses read_array as a special form" $ do
    let res = runAlex "read_array [3] \"data.csv\"" parseMiniML
    case res of
      Left err -> expectationFailure ("lexer/parser failed: " ++ err)
      Right ast -> fmap (const ()) ast `shouldBe` EReadArray () (EVec () [EInt () 3]) (EString () "\"data.csv\"")

  it "parses read_array with 2D shape" $ do
    let res = runAlex "read_array [2,3] \"matrix.csv\"" parseMiniML
    case res of
      Left err -> expectationFailure ("lexer/parser failed: " ++ err)
      Right ast -> fmap (const ()) ast `shouldBe` EReadArray () (EVec () [EInt () 2, EInt () 3]) (EString () "\"matrix.csv\"")

  it "reports parse error for malformed let" $ do
    let res = runAlex "let x = in x" parseMiniML
    res `shouldSatisfy` either (const True) (const False)
