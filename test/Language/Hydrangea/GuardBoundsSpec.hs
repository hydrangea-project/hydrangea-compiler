{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.GuardBoundsSpec (spec) where

import Data.Map.Strict qualified as M

import Language.Hydrangea.CFG
import Language.Hydrangea.CFGCore (Atom (..), BinOp (..), CType (..), RHS (..))
import Language.Hydrangea.GuardBounds
import Test.Hspec

-- | A flat loop over an n x m rectangle whose body computes @bound@ from the
-- row index @j@ and scatters under the guard @k < bound@.
flatLoop :: [Stmt] -> RHS -> [Stmt] -> [Stmt]
flatLoop pre boundRhs els =
  pre ++
  [ SAssign "shp" (RTuple [AVar "n", AVar "m"])
  , SAssign "sz" (RShapeSize (AVar "shp"))
  , SLoop (LoopSpec ["i"] [IVar "sz"] Serial Nothing LoopPlain [])
      [ SAssign "nd" (RFlatToNd (AVar "i") (AVar "shp"))
      , SAssign "j" (RProj 0 (AVar "nd"))
      , SAssign "k" (RProj 1 (AVar "nd"))
      , SAssign "len" boundRhs
      , SAssign "g" (RBinOp CLt (AVar "k") (AVar "len"))
      , SIf (AVar "g") [SArrayWrite (AVar "out") (AVar "j") (AVar "k")] els
      ]
  ]

tyEnv :: M.Map CVar CType
tyEnv = M.fromList
  [ ("j", CTInt64), ("k", CTInt64), ("len", CTInt64), ("g", CTBool), ("i", CTInt64)
  , ("lens", CTArray CTInt64), ("out", CTArray CTInt64), ("out2", CTArray CTInt64) ]

loopsOf :: [Stmt] -> M.Map CVar RaggedLoop
loopsOf = fst . raggedLoops tyEnv

spec :: Spec
spec = describe "GuardBounds" $ do

  it "bounds the columns of a row by a guard on the column index" $ do
    let (loops, tys) = raggedLoops tyEnv (flatLoop [] (RArrayLoad (AVar "lens") (AVar "j")) [])
    case M.lookup "i" loops of
      Just (RaggedLoop Nothing (Just nest)) -> do
        inRows nest `shouldBe` AVar "n"
        inCols nest `shouldBe` AVar "m"
        inLimit nest `shouldBe`
          Just (Limit [SAssign "len_gb_i" (RArrayLoad (AVar "lens") (AVar (inRowIter nest)))]
                      (AVar "len_gb_i") False)
      other -> expectationFailure ("unexpected: " ++ show other)
    M.lookup "len_gb_i" tys `shouldBe` Just CTInt64

  it "does not fire when the bound reads the array the loop writes" $
    loopsOf (flatLoop [] (RArrayLoad (AVar "out") (AVar "j")) []) `shouldBe` M.empty

  it "does not fire when the bound reads a copy of the array the loop writes" $
    loopsOf (flatLoop [SAssign "out2" (RAtom (AVar "out"))]
                      (RArrayLoad (AVar "out2") (AVar "j")) [])
      `shouldBe` M.empty

  it "does not fire when the bound depends on the column" $
    loopsOf (flatLoop [] (RArrayLoad (AVar "lens") (AVar "k")) []) `shouldBe` M.empty

  it "does not fire when the guard has an else branch" $
    loopsOf (flatLoop [] (RArrayLoad (AVar "lens") (AVar "j"))
                      [SArrayWrite (AVar "out") (AVar "j") (AInt 0)])
      `shouldBe` M.empty

  it "does not fire when the body allocates before the guard" $
    let body = [ SAssign "a" (RArrayAlloc (AVar "shp"))
               , SAssign "g" (RBinOp CLt (AVar "i") (AVar "cnt"))
               , SIf (AVar "g") [SArrayWrite (AVar "out") (AVar "i") (AInt 1)] [] ]
        stmts = [SLoop (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopPlain []) body]
     in loopsOf stmts `shouldBe` M.empty

  it "clamps a 1-D loop by an invariant bound on its iterator" $
    let body = [ SAssign "g" (RBinOp CGe (AVar "cnt") (AVar "i"))
               , SIf (AVar "g") [SArrayWrite (AVar "out") (AVar "i") (AInt 1)] [] ]
        stmts = [SLoop (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopPlain []) body]
     in M.lookup "i" (loopsOf stmts)
          `shouldBe` Just (RaggedLoop (Just (Limit [] (AVar "cnt") True)) Nothing)

  it "fires past a pure conditional before the guard (an index clamp)" $
    let clamp = SIf (AVar "c")
                  [SAssign "e" (RAtom (AVar "k"))]
                  [SAssign "e" (RAtom (AInt 0))]
        (pre, rest) = splitAt 3 (loopBody (flatLoop [] (RArrayLoad (AVar "lens") (AVar "j")) []))
        stmts = withBody (pre ++ [SAssign "c" (RBinOp CLt (AVar "k") (AInt 9)), clamp] ++ rest)
     in case M.lookup "i" (loopsOf stmts) of
          Just (RaggedLoop Nothing (Just nest)) -> inLimit nest `shouldSatisfy` (/= Nothing)
          other -> expectationFailure ("unexpected: " ++ show other)

  it "does not take a bound from a variable a conditional assigns" $
    let clamp = SIf (AVar "c")
                  [SAssign "len" (RArrayLoad (AVar "lens") (AVar "j"))]
                  [SAssign "len" (RAtom (AInt 0))]
        body = [ SAssign "nd" (RFlatToNd (AVar "i") (AVar "shp"))
               , SAssign "j" (RProj 0 (AVar "nd"))
               , SAssign "k" (RProj 1 (AVar "nd"))
               , SAssign "c" (RBinOp CLt (AVar "j") (AInt 9))
               , clamp
               , SAssign "g" (RBinOp CLt (AVar "k") (AVar "len"))
               , SIf (AVar "g") [SArrayWrite (AVar "out") (AVar "j") (AVar "k")] [] ]
     in loopsOf (withBody body) `shouldBe` M.empty
  where
    loopBody stmts = case last stmts of
      SLoop _ b -> b
      _ -> []
    withBody b =
      [ SAssign "shp" (RTuple [AVar "n", AVar "m"])
      , SAssign "sz" (RShapeSize (AVar "shp"))
      , SLoop (LoopSpec ["i"] [IVar "sz"] Serial Nothing LoopPlain []) b ]
