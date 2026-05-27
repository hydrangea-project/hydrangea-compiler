{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.CFGTypingSpec (spec) where

import Data.Map.Strict qualified as Map
import Language.Hydrangea.CFG
import Language.Hydrangea.CFGCore (Atom(..), CType(..), RHS(..))
import Language.Hydrangea.CFGTyping
import Test.Hspec

spec :: Spec
spec = describe "CFGTyping" $ do
  describe "inferAtomType" $ do
    it "uses the environment for variables and known literal types for literals" $ do
      let typeEnv = Map.fromList [("arr", CTArray CTDouble)]
      inferAtomType typeEnv (AVar "arr") `shouldBe` Just (CTArray CTDouble)
      inferAtomType typeEnv (AFloat 1.0) `shouldBe` Just CTDouble
      inferAtomType typeEnv (ABool True) `shouldBe` Just CTBool

  describe "lookupArrayElemType" $ do
    it "returns the element type for typed array variables only" $ do
      let typeEnv = Map.fromList [("arr", CTArray CTDouble), ("x", CTDouble)]
      lookupArrayElemType typeEnv (AVar "arr") `shouldBe` Just CTDouble
      lookupArrayElemType typeEnv (AVar "x") `shouldBe` Nothing
      lookupArrayElemType typeEnv (AInt 0) `shouldBe` Nothing

  describe "buildCallParamTypes" $ do
    it "recovers parameter types from array loads and existing annotations" $ do
      let proc =
            (mkProc "callee" ["arr", "ix"]
              [ SAssign "x" (RArrayLoad (AVar "arr") (AVar "ix"))
              , SReturn (AVar "x")
              ])
              { procTypeEnv = Map.fromList [("x", CTDouble), ("ix", CTInt64)] }
      buildCallParamTypes Map.empty [proc]
        `shouldBe` Map.fromList [("callee", [Just (CTArray CTDouble), Just CTInt64])]

  describe "inferProgramReturnTypes" $ do
    it "collects return types for procedures with a single recoverable return" $ do
      let proc =
            (mkProc "callee" []
              [ SAssign "x" (RAtom (AFloat 3.5))
              , SReturn (AVar "x")
              ])
              { procTypeEnv = Map.fromList [("x", CTDouble)] }
      inferProgramReturnTypes (Program [proc])
        `shouldBe` Map.fromList [("callee", CTDouble)]

  describe "recoverProcTypeEnv" $ do
    it "combines direct-call information with recovered argument types" $ do
      let proc =
            mkProc "caller" ["input"]
              [ SAssign "out" (RCall "callee" [AVar "input"])
              , SReturn (AVar "out")
              ]
          recovered =
            recoverProcTypeEnv
              (Map.fromList [("callee", CTDouble)])
              (Map.fromList [("callee", [Just (CTArray CTDouble)])])
              proc
      Map.lookup "out" recovered `shouldBe` Just CTDouble
      Map.lookup "input" recovered `shouldBe` Just (CTArray CTDouble)
