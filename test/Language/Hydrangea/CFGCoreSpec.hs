{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.CFGCoreSpec (spec) where

import Language.Hydrangea.CFGCore
import Test.Hspec

spec :: Spec
spec = describe "CFGCore" $ do
  describe "elemTypeToCType / ctypeToElemType" $ do
    it "roundtrips pair-compatible element types" $ do
      let elemTy = CEPair CEInt (CEPair CEBool (CEArray CTDouble))
      ctypeToElemType (elemTypeToCType elemTy) `shouldBe` Just elemTy

    it "keeps the element type of an array pair element" $ do
      elemTypeToCType (CEArray CTInt64) `shouldBe` CTArray CTInt64
      ctypeToElemType (CTArray CTInt64) `shouldBe` Just (CEArray CTInt64)

    it "rejects tuple and record types as pair elements" $ do
      ctypeToElemType CTTuple `shouldBe` Nothing
      ctypeToElemType (CTRecord [("x", CTInt64)]) `shouldBe` Nothing
