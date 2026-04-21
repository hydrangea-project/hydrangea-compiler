{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.PolyhedralSpec (spec) where

import Data.Map.Strict qualified as Map
import Language.Hydrangea.CFG
import Language.Hydrangea.CFGCore qualified as C
import Language.Hydrangea.Polyhedral
import Test.Hspec

spec :: Spec
spec = describe "Polyhedral" $ do
  it "converts simple affine index expressions" $ do
    let expr =
          affineExprFromIndexExpr2
            (IAdd (IMul (IConst 4) (IVar "i")) (IAdd (IVar "n") (IConst 1)))
    case expr of
      Nothing -> expectationFailure "expected affine conversion to succeed"
      Just affine -> do
        aeConstant affine `shouldBe` 1
        Map.lookup "i" (aeTerms affine) `shouldBe` Just 4
        Map.lookup "n" (aeTerms affine) `shouldBe` Just 1

  it "extracts a single affine scop from a simple map loop" $ do
    let loop =
          SLoop
            (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopMap)
            [ SAssign "ix" (C.RBinOp C.CAdd (C.AVar "i") (C.AInt 1))
            , SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "ix"))
            , SAssign "y" (C.RBinOp C.CAddF (C.AVar "x") (C.AFloat 1.0))
            , SArrayWrite (C.AVar "out") (C.AVar "ix") (C.AVar "y")
            ]
    case extractProcScops2 (mkProc "p" ["n", "arr", "out"] [loop]) of
      [scop] -> do
        let accesses = concatMap (\stmt -> psReads stmt ++ psWrites stmt) (scStatements scop)
        scProcName scop `shouldBe` "p"
        scRootPath scop `shouldBe` [0]
        scIterators scop `shouldBe` ["i"]
        scParameters scop `shouldBe` ["n"]
        scArrays scop `shouldBe` ["arr", "out"]
        length accesses `shouldBe` 2
        case scSchedule scop of
          ScheduleLoopBand band -> lbIters band `shouldBe` ["i"]
          other -> expectationFailure ("unexpected schedule tree: " <> show other)
      other -> expectationFailure ("expected one extracted scop, got: " <> show other)

  it "reports unsupported calls as rejection diagnostics" $ do
    let loop =
          SLoop
            (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopPlain)
            [SAssign "x" (C.RCall "opaque" [C.AVar "i"])]
    case collectProcScopDiagnostics2 (mkProc "p" ["n"] [loop]) of
      [diag] -> case diag of
        ScopRejected proc path (RejectUnsupportedRHS _) -> do
          proc `shouldBe` "p"
          path `shouldBe` [0]
        other -> expectationFailure ("unexpected diagnostic: " <> show other)
      other -> expectationFailure ("expected one diagnostic, got: " <> show other)

  it "falls back to nested loop roots when an outer loop is not a scop" $ do
    let inner =
          SLoop
            (LoopSpec ["j"] [IVar "m"] Serial Nothing LoopMap)
            [ SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "j"))
            , SArrayWrite (C.AVar "out") (C.AVar "j") (C.AVar "x")
            ]
        outer =
          SLoop
            (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopPlain)
            [ SAssign "opaque" (C.RCall "f" [C.AVar "i"])
            , inner
            ]
        diagnostics = collectProcScopDiagnostics2 (mkProc "p" ["n", "m", "arr", "out"] [outer])
        isOuterReject diag = case diag of
          ScopRejected _ [0] (RejectUnsupportedRHS _) -> True
          _ -> False
        isInnerScop diag = case diag of
          ScopExtracted scop -> scRootPath scop == [0, 1]
          _ -> False
    any isOuterReject diagnostics `shouldBe` True
    any isInnerScop diagnostics `shouldBe` True

  it "collects simple affine RAW dependences inside a scop" $ do
    let loop =
          SLoop
            (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopPlain)
            [ SAssign "ix" (C.RBinOp C.CAdd (C.AVar "i") (C.AInt 1))
            , SArrayWrite (C.AVar "arr") (C.AVar "i") (C.AInt 0)
            , SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "ix"))
            ]
    case extractProcScops2 (mkProc "p" ["n", "arr"] [loop]) of
      [scop] ->
        collectScopDependences2 scop `shouldBe`
          [ PolyhedralDependence
              { pdKind = PolyDepRAW
              , pdArray = "arr"
              , pdSourceStmt = [0, 1]
              , pdTargetStmt = [0, 2]
              , pdDirection = PolyDepForward
              , pdIsLoopCarried = True
              , pdDistance = Just [1]
              }
          ]
      other -> expectationFailure ("expected one extracted scop, got: " <> show other)

  it "reifies an identity schedule back into the original loop nest" $ do
    let inner =
          SLoop
            (LoopSpec ["j"] [IVar "m"] Serial Nothing LoopMap)
            [ SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "j"))
            , SArrayWrite (C.AVar "out") (C.AVar "j") (C.AVar "x")
            ]
        loop =
          SLoop
            (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopPlain)
            [ SAssign "base" (C.RAtom (C.AVar "i"))
            , inner
            ]
    case extractProcScops2 (mkProc "p" ["n", "m", "arr", "out"] [loop]) of
      [scop] ->
        reifyScheduledScop2 (buildIdentitySchedule2 scop) `shouldBe` Just [loop]
      other -> expectationFailure ("expected one extracted scop, got: " <> show other)

  it "rewrites nested scop roots without changing surrounding CFG" $ do
    let inner =
          SLoop
            (LoopSpec ["j"] [IVar "m"] Serial Nothing LoopMap)
            [ SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "j"))
            , SArrayWrite (C.AVar "out") (C.AVar "j") (C.AVar "x")
            ]
        outer =
          SLoop
            (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopPlain)
            [ SAssign "opaque" (C.RCall "f" [C.AVar "i"])
            , inner
            ]
        proc = mkProc "p" ["n", "m", "arr", "out"] [outer]
    polyhedralProgram2 (Program [proc]) `shouldBe` Program [proc]
