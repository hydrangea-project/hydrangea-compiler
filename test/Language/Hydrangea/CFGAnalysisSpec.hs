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
    let loop = SLoop (LoopSpec { lsIters = ["i"], lsBounds = [IConst 10], lsExec = Serial, lsRed = Nothing, lsRole = LoopPlain, lsOrigins = [] }) []
        result = analyzeLoop loop
    result `shouldSatisfy` isJust
    case result of
      Just info -> do
        liLoopVar info `shouldBe` "i"
        liTripCount info `shouldBe` TCConstant 10
      Nothing -> expectationFailure "Expected loop info"

  it "extracts loop info from SLoop with variable bound" $ do
    let loop = SLoop (LoopSpec { lsIters = ["i"], lsBounds = [IVar "n"], lsExec = Serial, lsRed = Nothing, lsRole = LoopPlain, lsOrigins = [] }) []
        result = analyzeLoop loop
    result `shouldSatisfy` isJust
    case result of
      Just info -> do
        liLoopVar info `shouldBe` "i"
        liTripCount info `shouldBe` TCVar "n"
      Nothing -> expectationFailure "Expected loop info"

  it "detects constant trip count >= 4 as vectorizable" $ do
    let loop = SLoop (LoopSpec { lsIters = ["i"], lsBounds = [IConst 8], lsExec = Serial, lsRed = Nothing, lsRole = LoopPlain, lsOrigins = [] }) []
        result = analyzeLoop loop
    case result of
      Just info -> isVectorizableLoop info `shouldBe` True
      Nothing -> expectationFailure "Expected loop info"

  it "detects small constant trip count as not vectorizable" $ do
    let loop = SLoop (LoopSpec { lsIters = ["i"], lsBounds = [IConst 2], lsExec = Serial, lsRed = Nothing, lsRole = LoopPlain, lsOrigins = [] }) []
        result = analyzeLoop loop
    case result of
      Just info -> isVectorizableLoop info `shouldBe` False
      Nothing -> expectationFailure "Expected loop info"

  it "extracts trip count value from constant" $ do
    tripCountValue (TCConstant 10) `shouldBe` Just 10

  it "extracts trip count value from multiplication" $ do
    tripCountValue (TCMul (TCConstant 4) (TCConstant 5)) `shouldBe` Just 20

  it "extracts trip count value from addition" $ do
    tripCountValue (TCAdd (TCConstant 3) (TCConstant 7)) `shouldBe` Just 10

  it "returns Nothing for variable trip count" $ do
    tripCountValue (TCVar "n") `shouldBe` Nothing

  it "returns Nothing for unknown trip count" $ do
    tripCountValue TCUnknown `shouldBe` Nothing

  it "identifies constant trip count correctly" $ do
    isConstantTripCount (TCConstant 10) `shouldBe` True
    isConstantTripCount (TCMul (TCConstant 2) (TCConstant 3)) `shouldBe` True
    isConstantTripCount (TCVar "n") `shouldBe` False
    isConstantTripCount TCUnknown `shouldBe` False

  it "computes combined loop trip count across dimensions" $ do
    let loopSpec = LoopSpec
          { lsIters = ["i", "j"]
          , lsBounds = [IConst 3, IAdd (IConst 1) (IConst 1)]
          , lsExec = Serial
          , lsRed = Nothing
          , lsRole = LoopPlain
          , lsOrigins = []
          }
    loopTripCount loopSpec `shouldBe` TCConstant 6

  it "treats large or symbolic trip counts as parallel-worthy" $ do
    isParallelTripCount (TCConstant 4) `shouldBe` True
    isParallelTripCount (TCVar "n") `shouldBe` True
    isParallelTripCount (TCConstant 3) `shouldBe` False

  it "extracts loop info from nested SLoop" $ do
    let innerLoop = SLoop (LoopSpec { lsIters = ["j"], lsBounds = [IConst 5], lsExec = Serial, lsRed = Nothing, lsRole = LoopPlain, lsOrigins = [] }) []
        outerLoop = SLoop (LoopSpec { lsIters = ["i"], lsBounds = [IConst 3], lsExec = Serial, lsRed = Nothing, lsRole = LoopPlain, lsOrigins = [] }) [innerLoop]
        result = analyzeStmts [outerLoop]
    length result `shouldBe` 2

  it "extracts exec policy from loop spec" $ do
    let loop = SLoop (LoopSpec { lsIters = ["i"], lsBounds = [IConst 10], lsExec = Vector (VectorSpec 4 TailRemainder), lsRed = Nothing, lsRole = LoopPlain, lsOrigins = [] }) []
        result = analyzeLoop loop
    case result of
      Just info -> liExecPolicy info `shouldBe` Vector (VectorSpec 4 TailRemainder)
      Nothing -> expectationFailure "Expected loop info"

  it "returns Nothing for non-loop statements" $ do
    let stmt = SAssign "x" (RAtom (AInt 5))
    analyzeLoop stmt `shouldBe` Nothing
