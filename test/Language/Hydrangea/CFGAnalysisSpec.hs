{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.CFGAnalysisSpec (spec) where

import Test.Hspec
import Data.Maybe (isJust)
import Language.Hydrangea.CFG
import Language.Hydrangea.CFGAnalysis
import Language.Hydrangea.CFGCore (Atom(..), RHS(..))

spec :: Spec
spec = describe "CFGAnalysis loop analysis" $ do
  it "extracts loop info from SLoop with constant bound" $ do
    let loop = SLoop (LoopSpec { lsIters = ["i"], lsBounds = [IConst 10], lsExec = Serial, lsRed = Nothing, lsRole = LoopPlain }) []
        result = analyzeLoop2 loop
    result `shouldSatisfy` isJust
    case result of
      Just info -> do
        li2LoopVar info `shouldBe` "i"
        li2TripCount info `shouldBe` TC2Constant 10
      Nothing -> expectationFailure "Expected loop info"

  it "extracts loop info from SLoop with variable bound" $ do
    let loop = SLoop (LoopSpec { lsIters = ["i"], lsBounds = [IVar "n"], lsExec = Serial, lsRed = Nothing, lsRole = LoopPlain }) []
        result = analyzeLoop2 loop
    result `shouldSatisfy` isJust
    case result of
      Just info -> do
        li2LoopVar info `shouldBe` "i"
        li2TripCount info `shouldBe` TC2Var "n"
      Nothing -> expectationFailure "Expected loop info"

  it "detects constant trip count >= 4 as vectorizable" $ do
    let loop = SLoop (LoopSpec { lsIters = ["i"], lsBounds = [IConst 8], lsExec = Serial, lsRed = Nothing, lsRole = LoopPlain }) []
        result = analyzeLoop2 loop
    case result of
      Just info -> isVectorizableLoop2 info `shouldBe` True
      Nothing -> expectationFailure "Expected loop info"

  it "detects small constant trip count as not vectorizable" $ do
    let loop = SLoop (LoopSpec { lsIters = ["i"], lsBounds = [IConst 2], lsExec = Serial, lsRed = Nothing, lsRole = LoopPlain }) []
        result = analyzeLoop2 loop
    case result of
      Just info -> isVectorizableLoop2 info `shouldBe` False
      Nothing -> expectationFailure "Expected loop info"

  it "extracts trip count value from constant" $ do
    tripCountValue2 (TC2Constant 10) `shouldBe` Just 10

  it "extracts trip count value from multiplication" $ do
    tripCountValue2 (TC2Mul (TC2Constant 4) (TC2Constant 5)) `shouldBe` Just 20

  it "extracts trip count value from addition" $ do
    tripCountValue2 (TC2Add (TC2Constant 3) (TC2Constant 7)) `shouldBe` Just 10

  it "returns Nothing for variable trip count" $ do
    tripCountValue2 (TC2Var "n") `shouldBe` Nothing

  it "returns Nothing for unknown trip count" $ do
    tripCountValue2 TC2Unknown `shouldBe` Nothing

  it "identifies constant trip count correctly" $ do
    isConstantTripCount2 (TC2Constant 10) `shouldBe` True
    isConstantTripCount2 (TC2Mul (TC2Constant 2) (TC2Constant 3)) `shouldBe` True
    isConstantTripCount2 (TC2Var "n") `shouldBe` False
    isConstantTripCount2 TC2Unknown `shouldBe` False

  it "computes combined loop trip count across dimensions" $ do
    let loopSpec = LoopSpec
          { lsIters = ["i", "j"]
          , lsBounds = [IConst 3, IAdd (IConst 1) (IConst 1)]
          , lsExec = Serial
          , lsRed = Nothing
          , lsRole = LoopPlain
          }
    loopTripCount2 loopSpec `shouldBe` TC2Constant 6

  it "treats large or symbolic trip counts as parallel-worthy" $ do
    isParallelTripCount2 (TC2Constant 4) `shouldBe` True
    isParallelTripCount2 (TC2Var "n") `shouldBe` True
    isParallelTripCount2 (TC2Constant 3) `shouldBe` False

  it "extracts loop info from nested SLoop" $ do
    let innerLoop = SLoop (LoopSpec { lsIters = ["j"], lsBounds = [IConst 5], lsExec = Serial, lsRed = Nothing, lsRole = LoopPlain }) []
        outerLoop = SLoop (LoopSpec { lsIters = ["i"], lsBounds = [IConst 3], lsExec = Serial, lsRed = Nothing, lsRole = LoopPlain }) [innerLoop]
        result = analyzeStmts2 [outerLoop]
    length result `shouldBe` 2

  it "extracts exec policy from loop spec" $ do
    let loop = SLoop (LoopSpec { lsIters = ["i"], lsBounds = [IConst 10], lsExec = Vector (VectorSpec 4 TailRemainder), lsRed = Nothing, lsRole = LoopPlain }) []
        result = analyzeLoop2 loop
    case result of
      Just info -> li2ExecPolicy info `shouldBe` Vector (VectorSpec 4 TailRemainder)
      Nothing -> expectationFailure "Expected loop info"

  it "returns Nothing for non-loop statements" $ do
    let stmt = SAssign "x" (RAtom (AInt 5))
    analyzeLoop2 stmt `shouldBe` Nothing
