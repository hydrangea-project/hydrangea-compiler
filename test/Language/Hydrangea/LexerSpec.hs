{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.LexerSpec (spec) where

import Data.Either (isRight)
import Language.Hydrangea.Lexer
import Test.Hspec

lexAll :: Alex [RangedToken]
lexAll = do
  let go = do
        t <- alexMonadScan
        if rtToken t == EOF
          then pure [t]
          else (t :) <$> go
  go

spec :: Spec
spec = describe "lexer" $ do
  it "tokenizes identifiers, integers, and keywords" $ do
    let res = runAlex "let x = 10" lexAll
    res `shouldSatisfy` isRight
    case res of
      Left err -> expectationFailure ("lexer failed: " ++ err)
      Right toks -> map rtToken toks `shouldBe` [Let, Identifier "x", Eq, Integer 10, EOF]

  it "recognizes generate, map, zipwith, reduce, permute, scatter, gather, and index keywords" $ do
    case runAlex "generate map zipwith reduce permute scatter gather index" lexAll of
      Left err -> expectationFailure ("lexer failed: " ++ err)
      Right toks ->
        map (show . rtToken) toks
          `shouldBe` ["Generate", "Map", "ZipWith", "Reduce", "Permute", "Scatter", "Gather", "Index", "EOF"]

  it "recognizes standalone multiplication operators without entering comment mode" $ do
    case runAlex "(*) (*.)" lexAll of
      Left err -> expectationFailure ("lexer failed: " ++ err)
      Right toks ->
        map rtToken toks `shouldBe` [TimesValue, TTimesFValue, EOF]

  it "errors on unclosed comment" $ do
    runAlex "(*" lexAll `shouldSatisfy` either (const True) (const False)

  it "recognizes read_array keyword" $ do
    case runAlex "read_array [3] \"data.csv\"" lexAll of
      Left err -> expectationFailure ("lexer failed: " ++ err)
      Right toks ->
        map rtToken toks
          `shouldBe` [ReadArray, LBrack, Integer 3, RBrack, String "\"data.csv\"", EOF]

  it "errors on unclosed string" $ do
    runAlex "\"abc" lexAll `shouldSatisfy` either (const True) (const False)
