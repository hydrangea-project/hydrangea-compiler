{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.DependenceSpec (spec) where

import Test.Hspec
import Data.ByteString.Lazy.Char8 qualified as BS
import Language.Hydrangea.CFG
import Language.Hydrangea.CFGCore (Atom(..), RHS(..), Redop(..))
import Language.Hydrangea.Dependence

spec :: Spec
spec = describe "Dependence" $ do
  it "detects identical index as forward dependence" $ do
    let a1 = ArrayAccess "arr" (IVar "i") Read 0
        a2 = ArrayAccess "arr" (IVar "i") Write 1
        deps = findDependences [a1,a2]
    length deps `shouldBe` 1
    head deps `shouldSatisfy` (\d -> depDirection d == DDForward)

  it "detects simple i and i+1 pattern" $ do
    let a1 = ArrayAccess "arr" (IVar "i") Write 0
        a2 = ArrayAccess "arr" (IAdd (IVar "i") (IConst 1)) Read 1
        deps = findDependences [a1,a2]
    length deps `shouldBe` 1
    head deps `shouldSatisfy` (\d -> depDirection d == DDForward && depDistance d == Just [1])

  it "handles ND indices element-wise" $ do
    let a1 = ArrayAccess "A" (ITuple [IVar "i", IVar "j"]) Write 0
        a2 = ArrayAccess "A" (ITuple [IVar "i", IAdd (IVar "j") (IConst 1)]) Read 1
        deps = findDependences [a1,a2]
    length deps `shouldBe` 1
    head deps `shouldSatisfy` (\d -> depDirection d == DDForward && depDistance d == Just [0,1])

  it "marks non-affine/opaque as unknown" $ do
    let a1 = ArrayAccess "arr" (ICall "foo" []) Write 0
        a2 = ArrayAccess "arr" (ICall "bar" []) Read 1
        deps = findDependences [a1,a2]
    length deps `shouldBe` 1
    head deps `shouldSatisfy` (\d -> depDirection d == DDUnknown)

  it "detects coefficient multiplication i*2" $ do
    let a1 = ArrayAccess "arr" (IMul (IVar "i") (IConst 2)) Write 0
        a2 = ArrayAccess "arr" (IMul (IVar "i") (IConst 2)) Read 1
        deps = findDependences [a1,a2]
    length deps `shouldBe` 1
    head deps `shouldSatisfy` (\d -> depDirection d == DDForward && depDistance d == Just [0])

  it "detects backward dependence with negative distance" $ do
    let a1 = ArrayAccess "arr" (IAdd (IVar "i") (IConst 1)) Write 0
        a2 = ArrayAccess "arr" (IVar "i") Read 1
        deps = findDependences [a1,a2]
    length deps `shouldBe` 1
    head deps `shouldSatisfy` (\d -> depDirection d == DDBackward && depDistance d == Just [-1])

  it "returns unknown when distance not divisible by coefficient" $ do
    let a1 = ArrayAccess "arr" (IVar "i") Write 0
        a2 = ArrayAccess "arr" (IAdd (IMul (IVar "i") (IConst 2)) (IConst 1)) Read 1
        deps = findDependences [a1,a2]
    length deps `shouldBe` 1
    head deps `shouldSatisfy` (\d -> depDirection d == DDUnknown)

  it "returns unknown for multi-variable affine forms" $ do
    let a1 = ArrayAccess "arr" (IAdd (IVar "i") (IVar "j")) Write 0
        a2 = ArrayAccess "arr" (IAdd (IVar "i") (IConst 1)) Read 1
        deps = findDependences [a1,a2]
    length deps `shouldBe` 1
    head deps `shouldSatisfy` (\d -> depDirection d == DDUnknown)

  it "handles commuted IMul forms" $ do
    let a1 = ArrayAccess "arr" (IMul (IConst 2) (IVar "i")) Write 0
        a2 = ArrayAccess "arr" (IMul (IConst 2) (IVar "i")) Read 1
        deps = findDependences [a1,a2]
    length deps `shouldBe` 1
    head deps `shouldSatisfy` (\d -> depDirection d == DDForward && depDistance d == Just [0])
