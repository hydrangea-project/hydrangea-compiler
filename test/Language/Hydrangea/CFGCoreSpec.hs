{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.CFGCoreSpec (spec) where

import Language.Hydrangea.CFGCore
import Test.Hspec

spec :: Spec
spec = describe "CFGCore" $ do
  describe "elemTypeToCType / ctypeToElemType" $ do
    it "roundtrips pair-compatible element types" $ do
      let elemTy = CEPair CEInt (CEPair CEBool CEArray)
      ctypeToElemType (elemTypeToCType elemTy) `shouldBe` Just elemTy

    it "treats arrays as opaque pair elements" $ do
      elemTypeToCType CEArray `shouldBe` CTArray CTDouble
      ctypeToElemType (CTArray CTInt64) `shouldBe` Just CEArray

    it "rejects tuple and record types as pair elements" $ do
      ctypeToElemType CTTuple `shouldBe` Nothing
      ctypeToElemType (CTRecord [("x", CTInt64)]) `shouldBe` Nothing
