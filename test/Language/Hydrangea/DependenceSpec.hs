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
    let a1 = ArrayAccess2 "arr" (IVar "i") Read2 0
        a2 = ArrayAccess2 "arr" (IVar "i") Write2 1
        deps = findDependences2 [a1,a2]
    length deps `shouldBe` 1
    head deps `shouldSatisfy` (\d -> depDirection2 d == DDForward)

  it "detects simple i and i+1 pattern" $ do
    let a1 = ArrayAccess2 "arr" (IVar "i") Write2 0
        a2 = ArrayAccess2 "arr" (IAdd (IVar "i") (IConst 1)) Read2 1
        deps = findDependences2 [a1,a2]
    length deps `shouldBe` 1
    head deps `shouldSatisfy` (\d -> depDirection2 d == DDForward && depDistance2 d == Just [1])

  it "handles ND indices element-wise" $ do
    let a1 = ArrayAccess2 "A" (ITuple [IVar "i", IVar "j"]) Write2 0
        a2 = ArrayAccess2 "A" (ITuple [IVar "i", IAdd (IVar "j") (IConst 1)]) Read2 1
        deps = findDependences2 [a1,a2]
    length deps `shouldBe` 1
    head deps `shouldSatisfy` (\d -> depDirection2 d == DDForward && depDistance2 d == Just [0,1])

  it "marks non-affine/opaque as unknown" $ do
    let a1 = ArrayAccess2 "arr" (ICall "foo" []) Write2 0
        a2 = ArrayAccess2 "arr" (ICall "bar" []) Read2 1
        deps = findDependences2 [a1,a2]
    length deps `shouldBe` 1
    head deps `shouldSatisfy` (\d -> depDirection2 d == DDUnknown)

  it "detects coefficient multiplication i*2" $ do
    let a1 = ArrayAccess2 "arr" (IMul (IVar "i") (IConst 2)) Write2 0
        a2 = ArrayAccess2 "arr" (IMul (IVar "i") (IConst 2)) Read2 1
        deps = findDependences2 [a1,a2]
    length deps `shouldBe` 1
    head deps `shouldSatisfy` (\d -> depDirection2 d == DDForward && depDistance2 d == Just [0])

  it "detects backward dependence with negative distance" $ do
    let a1 = ArrayAccess2 "arr" (IAdd (IVar "i") (IConst 1)) Write2 0
        a2 = ArrayAccess2 "arr" (IVar "i") Read2 1
        deps = findDependences2 [a1,a2]
    length deps `shouldBe` 1
    head deps `shouldSatisfy` (\d -> depDirection2 d == DDBackward && depDistance2 d == Just [-1])

  it "returns unknown when distance not divisible by coefficient" $ do
    let a1 = ArrayAccess2 "arr" (IVar "i") Write2 0
        a2 = ArrayAccess2 "arr" (IAdd (IMul (IVar "i") (IConst 2)) (IConst 1)) Read2 1
        deps = findDependences2 [a1,a2]
    length deps `shouldBe` 1
    head deps `shouldSatisfy` (\d -> depDirection2 d == DDUnknown)

  it "returns unknown for multi-variable affine forms" $ do
    let a1 = ArrayAccess2 "arr" (IAdd (IVar "i") (IVar "j")) Write2 0
        a2 = ArrayAccess2 "arr" (IAdd (IVar "i") (IConst 1)) Read2 1
        deps = findDependences2 [a1,a2]
    length deps `shouldBe` 1
    head deps `shouldSatisfy` (\d -> depDirection2 d == DDUnknown)

  it "handles commuted IMul forms" $ do
    let a1 = ArrayAccess2 "arr" (IMul (IConst 2) (IVar "i")) Write2 0
        a2 = ArrayAccess2 "arr" (IMul (IConst 2) (IVar "i")) Read2 1
        deps = findDependences2 [a1,a2]
    length deps `shouldBe` 1
    head deps `shouldSatisfy` (\d -> depDirection2 d == DDForward && depDistance2 d == Just [0])
