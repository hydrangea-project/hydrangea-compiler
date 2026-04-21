{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.VectorizeSpec (spec) where

import Data.Map.Strict qualified as Map
import Language.Hydrangea.CFG
import Language.Hydrangea.CFGCore qualified as C
import Language.Hydrangea.Vectorize (vectorizeProc2, vectorizeStmts2)
import Test.Hspec

hasRHS :: (C.RHS -> Bool) -> [Stmt] -> Bool
hasRHS p = any go
  where
    go stmt = case stmt of
      SAssign _ rhs -> p rhs
      SLoop _ body -> hasRHS p body
      SIf _ thn els -> hasRHS p thn || hasRHS p els
      _ -> False

hasVecIndexVar :: C.CVar -> [Stmt] -> Bool
hasVecIndexVar wanted = any go
  where
    go stmt = case stmt of
      SAssign _ rhs -> case rhs of
        C.RVecLoad _ (C.AVar v) -> v == wanted
        C.RVecStore _ (C.AVar v) _ -> v == wanted
        _ -> False
      SLoop _ body -> hasVecIndexVar wanted body
      SIf _ thn els -> hasVecIndexVar wanted thn || hasVecIndexVar wanted els
      _ -> False

hasLoop :: (LoopSpec -> Bool) -> [Stmt] -> Bool
hasLoop p = any go
  where
    go stmt = case stmt of
      SLoop loopSpec body -> p loopSpec || hasLoop p body
      SIf _ thn els -> hasLoop p thn || hasLoop p els
      _ -> False

vectorizeWithTypes :: [(C.CVar, C.CType)] -> [Stmt] -> [Stmt]
vectorizeWithTypes bindings body =
  procBody $
    vectorizeProc2 $
      (mkProc "p" [] body) {procTypeEnv = Map.fromList bindings}

vectorizeWithFacts :: [(C.CVar, C.CType)] -> [(C.CVar, VectorAccessFact)] -> [Stmt] -> [Stmt]
vectorizeWithFacts typeBindings factBindings body =
  procBody $
    vectorizeProc2 $
      (mkProc "p" [] body)
        { procTypeEnv = Map.fromList typeBindings
        , procVectorAccessFacts = Map.fromList factBindings
        }

spec :: Spec
spec = describe "Vectorize" $ do
  it "lowers simple floating reduction loops to explicit vector IR when types are available" $ do
    let loop =
          SLoop
            (LoopSpec ["j"] [IVar "n"] Serial (Just (ReductionSpec "acc" (IConst 0) C.RAdd)) LoopReduction)
            [ SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "j"))
            , SAssign "acc" (C.RBinOp C.CAddF (C.AVar "acc") (C.AVar "x"))
            ]
        lowered =
          vectorizeWithTypes
            [ ("n", C.CTInt64)
            , ("arr", C.CTArray C.CTDouble)
            , ("x", C.CTDouble)
            , ("acc", C.CTDouble)
            ]
            [loop]
    hasRHS (\rhs -> case rhs of C.RVecLoad {} -> True; _ -> False) lowered `shouldBe` True
    hasRHS (\rhs -> case rhs of C.RVecReduce {} -> True; _ -> False) lowered `shouldBe` True
    hasLoop (\ls -> lsExec ls == Serial && lsRole ls == LoopReduction) lowered `shouldBe` True

  it "keeps reduction wrapper loops scalar" $ do
    let wrapper =
          SLoop
            (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopReductionWrapper)
            [SAssign "x" (C.RAtom (C.AFloat 1.0))]
    case vectorizeStmts2 [wrapper] of
      [SLoop wrapperSpec _] -> lsExec wrapperSpec `shouldBe` Serial
      other -> expectationFailure ("unexpected wrapper lowering: " <> show other)

  it "explicitly vectorizes nested unary float loops when lowering is supported" $ do
    let innerLoop =
          SLoop
            (LoopSpec ["i"] [IConst 8] Serial Nothing LoopPlain)
            [ SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "i"))
            , SAssign "y" (C.RUnOp C.CSqrt (C.AVar "x"))
            , SArrayWrite (C.AVar "out") (C.AVar "i") (C.AVar "y")
            ]
        outerLoop =
          SLoop
            (LoopSpec ["j"] [IConst 4] Serial Nothing LoopPlain)
            [innerLoop]
        lowered =
          vectorizeWithTypes
            [ ("arr", C.CTArray C.CTDouble)
            , ("out", C.CTArray C.CTDouble)
            , ("x", C.CTDouble)
            , ("y", C.CTDouble)
            ]
            [outerLoop]
    case lowered of
      [SLoop _ loweredOuterBody] -> do
        hasLoop (\ls -> lsRole ls == LoopPlain && lsExec ls == Serial) loweredOuterBody `shouldBe` True
        hasRHS (\rhs -> case rhs of C.RVecLoad {} -> True; _ -> False) lowered `shouldBe` True
        hasRHS (\rhs -> case rhs of C.RVecUnOp C.CSqrt _ -> True; _ -> False) lowered `shouldBe` True
        hasRHS (\rhs -> case rhs of C.RVecStore {} -> True; _ -> False) lowered `shouldBe` True
      other -> expectationFailure ("unexpected nested unary lowering: " <> show other)

  it "explicitly vectorizes top-level unary float LoopMap kernels when lowering is supported" $ do
    let loop =
          SLoop
            (LoopSpec ["i"] [IConst 8] Serial Nothing LoopMap)
            [ SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "i"))
            , SAssign "y" (C.RUnOp C.CSqrt (C.AVar "x"))
            , SArrayWrite (C.AVar "out") (C.AVar "i") (C.AVar "y")
            ]
        lowered =
          vectorizeWithTypes
            [ ("arr", C.CTArray C.CTDouble)
            , ("out", C.CTArray C.CTDouble)
            , ("x", C.CTDouble)
            , ("y", C.CTDouble)
            ]
            [loop]
    case lowered of
      [SAssign _ _, SLoop loweredSpec loweredBody] -> do
        lsExec loweredSpec `shouldBe` Serial
        lsRole loweredSpec `shouldBe` LoopMap
        hasRHS (\rhs -> case rhs of C.RVecLoad {} -> True; _ -> False) loweredBody `shouldBe` True
        hasRHS (\rhs -> case rhs of C.RVecUnOp C.CSqrt _ -> True; _ -> False) loweredBody `shouldBe` True
        hasRHS (\rhs -> case rhs of C.RVecStore {} -> True; _ -> False) loweredBody `shouldBe` True
      other -> expectationFailure ("unexpected top-level unary lowering: " <> show other)

  it "explicitly vectorizes top-level LoopMap kernels when types prove float arrays" $ do
    let loop =
          SLoop
            (LoopSpec ["i"] [IConst 8] Serial Nothing LoopMap)
            [ SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "i"))
            , SAssign "y" (C.RBinOp C.CAddF (C.AVar "x") (C.AFloat 1.0))
            , SArrayWrite (C.AVar "out") (C.AVar "i") (C.AVar "y")
            ]
        lowered =
          vectorizeWithTypes
            [ ("arr", C.CTArray C.CTDouble)
            , ("out", C.CTArray C.CTDouble)
            , ("x", C.CTDouble)
            , ("y", C.CTDouble)
            ]
            [loop]
    hasRHS (\rhs -> case rhs of C.RVecLoad {} -> True; _ -> False) lowered `shouldBe` True
    hasRHS (\rhs -> case rhs of C.RVecStore {} -> True; _ -> False) lowered `shouldBe` True
    -- The loop policy stays Serial; only the RHS load/store ops become
    -- vector IR.  An explicit Vector loop policy is not emitted here.
    hasLoop (\ls -> lsExec ls == Vector (VectorSpec 2 TailNone)) lowered `shouldBe` False

  it "uses lowering-provided dense index aliases to broaden explicit vectorization" $ do
    let loop =
          SLoop
            (LoopSpec ["i"] [IConst 8] Serial Nothing LoopMap)
            [ SAssign "off" (C.RAtom (C.AVar "idx"))
            , SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "off"))
            , SAssign "y" (C.RBinOp C.CAddF (C.AVar "x") (C.AFloat 1.0))
            , SArrayWrite (C.AVar "out") (C.AVar "off") (C.AVar "y")
            ]
        withoutFacts =
          vectorizeWithTypes
            [ ("arr", C.CTArray C.CTDouble)
            , ("out", C.CTArray C.CTDouble)
            , ("x", C.CTDouble)
            , ("y", C.CTDouble)
            ]
            [loop]
        withFacts =
          vectorizeWithFacts
            [ ("arr", C.CTArray C.CTDouble)
            , ("out", C.CTArray C.CTDouble)
            , ("x", C.CTDouble)
            , ("y", C.CTDouble)
            ]
            [ ("idx", VectorAccessFact (Just "i") False False False)
            , ("off", VectorAccessFact (Just "i") False False False)
            , ("out", VectorAccessFact Nothing False False True)
            ]
            [loop]
    hasRHS (\rhs -> case rhs of C.RVecLoad {} -> True; _ -> False) withoutFacts `shouldBe` False
    hasRHS (\rhs -> case rhs of C.RVecLoad {} -> True; _ -> False) withFacts `shouldBe` True
    hasRHS (\rhs -> case rhs of C.RVecStore {} -> True; _ -> False) withFacts `shouldBe` True

  it "keeps row-base dense aliases as explicit vector indices" $ do
    let loop =
          SLoop
            (LoopSpec ["j"] [IConst 8] Serial Nothing LoopMap)
            [ SAssign "flat" (C.RBinOp C.CAdd (C.AVar "row_base") (C.AVar "j"))
            , SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "flat"))
            , SAssign "y" (C.RBinOp C.CAddF (C.AVar "x") (C.AFloat 1.0))
            , SArrayWrite (C.AVar "out") (C.AVar "flat") (C.AVar "y")
            ]
        lowered =
          vectorizeWithFacts
            [ ("row_base", C.CTInt64)
            , ("arr", C.CTArray C.CTDouble)
            , ("out", C.CTArray C.CTDouble)
            , ("x", C.CTDouble)
            , ("y", C.CTDouble)
            ]
            [ ("flat", VectorAccessFact (Just "j") False False False)
            , ("out", VectorAccessFact Nothing False False True)
            ]
            [loop]
    hasRHS (\rhs -> case rhs of C.RVecLoad {} -> True; _ -> False) lowered `shouldBe` True
    hasRHS (\rhs -> case rhs of C.RVecStore {} -> True; _ -> False) lowered `shouldBe` True
    hasVecIndexVar "flat" lowered `shouldBe` True

  it "keeps non-contiguous scalar loads on the explicit path when vector ops still apply" $ do
    let loop =
          SLoop
            (LoopSpec ["j"] [IConst 8] Serial Nothing LoopMap)
            [ SAssign "offA" (C.RBinOp C.CAdd (C.AVar "row_base_a") (C.AVar "k"))
            , SAssign "a" (C.RArrayLoad (C.AVar "matA") (C.AVar "offA"))
            , SAssign "offB" (C.RBinOp C.CAdd (C.AVar "row_base_b") (C.AVar "j"))
            , SAssign "b" (C.RArrayLoad (C.AVar "matB") (C.AVar "offB"))
            , SAssign "offC" (C.RBinOp C.CAdd (C.AVar "row_base_c") (C.AVar "j"))
            , SAssign "acc" (C.RArrayLoad (C.AVar "out") (C.AVar "offC"))
            , SAssign "prod" (C.RBinOp C.CMulF (C.AVar "a") (C.AVar "b"))
            , SAssign "sum" (C.RBinOp C.CAddF (C.AVar "acc") (C.AVar "prod"))
            , SArrayWrite (C.AVar "out") (C.AVar "offC") (C.AVar "sum")
            ]
        lowered =
          vectorizeWithFacts
            [ ("row_base_a", C.CTInt64)
            , ("row_base_b", C.CTInt64)
            , ("row_base_c", C.CTInt64)
            , ("k", C.CTInt64)
            , ("matA", C.CTArray C.CTDouble)
            , ("matB", C.CTArray C.CTDouble)
            , ("out", C.CTArray C.CTDouble)
            , ("a", C.CTDouble)
            , ("b", C.CTDouble)
            , ("acc", C.CTDouble)
            , ("prod", C.CTDouble)
            , ("sum", C.CTDouble)
            ]
            [ ("offB", VectorAccessFact (Just "j") False False False)
            , ("offC", VectorAccessFact (Just "j") False False False)
            , ("out", VectorAccessFact Nothing False False True)
            ]
            [loop]
    hasRHS (\rhs -> case rhs of C.RVecLoad (C.AVar "matB") _ -> True; _ -> False) lowered `shouldBe` True
    hasRHS (\rhs -> case rhs of C.RVecLoad (C.AVar "out") _ -> True; _ -> False) lowered `shouldBe` True
    hasRHS (\rhs -> case rhs of C.RVecStore (C.AVar "out") _ _ -> True; _ -> False) lowered `shouldBe` True
    hasRHS (\rhs -> case rhs of C.RArrayLoad (C.AVar "matA") _ -> True; _ -> False) lowered `shouldBe` True
    hasRHS (\rhs -> case rhs of C.RVecBinOp C.CMulF _ _ -> True; _ -> False) lowered `shouldBe` True

  it "keeps indirect-access kernels off the explicit vector path even with dense index aliases" $ do
    let loop =
          SLoop
            (LoopSpec ["i"] [IConst 8] Serial Nothing LoopMap)
            [ SAssign "x" (C.RArrayLoad (C.AVar "src") (C.AVar "off"))
            , SArrayWrite (C.AVar "out") (C.AVar "i") (C.AVar "x")
            ]
        lowered =
          vectorizeWithFacts
            [ ("src", C.CTArray C.CTDouble)
            , ("out", C.CTArray C.CTDouble)
            , ("x", C.CTDouble)
            ]
            [ ("off", VectorAccessFact (Just "i") False False False)
            , ("src", VectorAccessFact Nothing False True False)
            , ("out", VectorAccessFact Nothing False False True)
            ]
            [loop]
    case lowered of
      [SLoop loweredSpec loweredBody] -> do
        lsExec loweredSpec `shouldBe` Vector (VectorSpec 4 TailNone)
        hasRHS (\rhs -> case rhs of C.RVecLoad {} -> True; _ -> False) loweredBody `shouldBe` False
        hasRHS (\rhs -> case rhs of C.RVecStore {} -> True; _ -> False) loweredBody `shouldBe` False
      other -> expectationFailure ("unexpected indirect-access lowering: " <> show other)

  it "keeps integer LoopMap kernels on the hint-only path" $ do
    let loop =
          SLoop
            (LoopSpec ["i"] [IConst 8] Serial Nothing LoopMap)
            [ SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "i"))
            , SAssign "y" (C.RBinOp C.CAdd (C.AVar "x") (C.AInt 1))
            , SArrayWrite (C.AVar "out") (C.AVar "i") (C.AVar "y")
            ]
    case vectorizeStmts2 [loop] of
      [SLoop loweredSpec loweredBody] -> do
        lsExec loweredSpec `shouldBe` Vector (VectorSpec 4 TailNone)
        hasRHS (\rhs -> case rhs of C.RVecLoad {} -> True; _ -> False) loweredBody `shouldBe` False
        hasRHS (\rhs -> case rhs of C.RVecStore {} -> True; _ -> False) loweredBody `shouldBe` False
      other -> expectationFailure ("unexpected statement-only fallback: " <> show other)
