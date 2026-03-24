{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.LexerSpec (spec) where

import Test.Hspec
import Language.Hydrangea.Lexer
import Data.Either (isRight)

spec :: Spec
spec = describe "lexer" $ do
  it "tokenizes identifiers, integers, and keywords" $ do
    let lexAll = do
          let go = do
                t <- alexMonadScan
                if rtToken t == EOF
                  then pure [t]
                  else (t :) <$> go
          go
    let res = runAlex "let x = 10" lexAll
    res `shouldSatisfy` isRight
    case res of
      Left err -> expectationFailure ("lexer failed: " ++ err)
      Right toks -> map rtToken toks `shouldBe` [Let, Identifier "x", Eq, Integer 10, EOF]

  it "recognizes generate, map, zipwith, reduce, permute, scatter, gather, and index keywords" $ do
    let lexAll = do
          let go = do
                t <- alexMonadScan
                if rtToken t == EOF
                  then pure [t]
                  else (t :) <$> go
          go
    case runAlex "generate map zipwith reduce permute scatter gather index" lexAll of
      Left err -> expectationFailure ("lexer failed: " ++ err)
      Right toks ->
        map (show . rtToken) toks
          `shouldBe` ["Generate", "Map", "ZipWith", "Reduce", "Permute", "Scatter", "Gather", "Index", "EOF"]

  it "errors on unclosed comment" $ do
    let lexAll = do
          let go = do
                t <- alexMonadScan
                if rtToken t == EOF
                  then pure [t]
                  else (t :) <$> go
          go
    runAlex "(*" lexAll `shouldSatisfy` either (const True) (const False)

  it "recognizes read_array keyword" $ do
    let lexAll = do
          let go = do
                t <- alexMonadScan
                if rtToken t == EOF
                  then pure [t]
                  else (t :) <$> go
          go
    case runAlex "read_array [3] \"data.csv\"" lexAll of
      Left err -> expectationFailure ("lexer failed: " ++ err)
      Right toks ->
        map rtToken toks
          `shouldBe` [ReadArray, LBrack, Integer 3, RBrack, String "\"data.csv\"", EOF]

  it "errors on unclosed string" $ do
    let lexAll = do
          let go = do
                t <- alexMonadScan
                if rtToken t == EOF
                  then pure [t]
                  else (t :) <$> go
          go
    runAlex "\"abc" lexAll `shouldSatisfy` either (const True) (const False)
