{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.SyntaxSpec (spec) where

import Language.Hydrangea.Syntax
import Test.Hspec

spec :: Spec
spec = do
  describe "tyRecord" $ do
    it "canonicalizes record type fields by name" $ do
      tyRecord [("z", tyInt), ("a", tyBool)]
        `shouldBe` TyRecord [("a", tyBool), ("z", tyInt)]

  describe "firstParam" $ do
    it "returns the annotation for record and scatter syntax nodes" $ do
      firstParam (ERecord "record-ann" [("x", EInt "field-ann" 1)] :: Exp String)
        `shouldBe` "record-ann"
      firstParam
        (EScatterChain "scatter-ann" (EVar "comb-ann" "comb") (EVar "defaults-ann" "defaults") [] :: Exp String)
        `shouldBe` "scatter-ann"

  describe "pattern ordering" $ do
    it "ignores the bound expression inside PBound comparisons" $ do
      let pat1 :: Pat String
          pat1 = PBound "ann" "i" (EInt "lhs" 4)
          pat2 :: Pat String
          pat2 = PBound "ann" "i" (EInt "rhs" 16)
      pat1 `shouldBe` pat2
      compare pat1 pat2 `shouldBe` EQ

    it "orders pattern constructors consistently" $ do
      compare
        (PVec "ann" [] :: Pat String)
        (PPair "ann" (PVar "ann" "x") (PVar "ann" "y") :: Pat String)
        `shouldBe` LT

  describe "declaration helpers" $ do
    it "extracts the declaration name and optional polytype" $ do
      let poly = Forall ["a"] [] TyInt
          dec :: Dec String
          dec = Dec "ann" "main" [] Nothing (Just poly) (EInt "ann" 0)
      decName dec `shouldBe` "main"
      decPolyTy dec `shouldBe` Just poly
