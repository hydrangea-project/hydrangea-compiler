{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.PolyhedralSpec (spec) where

import Control.Applicative ((<|>))
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Map.Strict qualified as Map
import Language.Hydrangea.CFG
import Language.Hydrangea.CFGPipeline
  ( PipelineOptions(..)
  , defaultPipelineOptions
  , preparePolyhedralProgramWithOptions
  )
import Language.Hydrangea.CFGCore qualified as C
import Language.Hydrangea.Frontend (lowerToCFG2WithTypesWithOptions, readDecs)
import Language.Hydrangea.Infer (defaultInferOptions)
import Language.Hydrangea.Polyhedral
import Language.Hydrangea.Tile (tileStmts2)
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

  it "extracts and reifies a tile-friendly ND loop band" $ do
    let loop =
          SLoop
            (LoopSpec ["i", "j"] [IVar "n", IAdd (IVar "m") (IConst 4)] Serial Nothing LoopPlain)
            [ SAssign "ij" (C.RTuple [C.AVar "i", C.AVar "j"])
            , SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "ij"))
            , SArrayWrite (C.AVar "out") (C.AVar "ij") (C.AVar "x")
            ]
    case extractProcScops2 (mkProc "p" ["n", "m", "arr", "out"] [loop]) of
      [scop] -> do
        scIterators scop `shouldBe` ["i", "j"]
        scParameters scop `shouldBe` ["m", "n"]
        scArrays scop `shouldBe` ["arr", "out"]
        reifyScheduledScop2 (buildIdentitySchedule2 scop) `shouldBe` Just [loop]
        case scSchedule scop of
          ScheduleLoopBand band -> do
            lbIters band `shouldBe` ["i", "j"]
            lbRole band `shouldBe` LoopPlain
          other -> expectationFailure ("unexpected schedule tree: " <> show other)
      other -> expectationFailure ("expected one extracted scop, got: " <> show other)

  it "extracts a map-reduction nest that matches today's tiling target" $ do
    let inner =
          SLoop
            (LoopSpec ["k"] [IVar "m"] Serial (Just (ReductionSpec "acc" (IConst 0) C.RAdd)) LoopReduction)
            [ SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "k"))
            , SAssign "acc" (C.RBinOp C.CAdd (C.AVar "acc") (C.AVar "x"))
            ]
        outer =
          SLoop
            (LoopSpec ["j"] [IVar "n"] Serial Nothing LoopMapReduction)
            [ SAssign "acc" (C.RAtom (C.AInt 0))
            , inner
            , SArrayWrite (C.AVar "out") (C.AVar "j") (C.AVar "acc")
            ]
    case extractProcScops2 (mkProc "p" ["n", "m", "arr", "out"] [outer]) of
      [scop] -> do
        scIterators scop `shouldBe` ["j", "k"]
        scParameters scop `shouldBe` ["m", "n"]
        scArrays scop `shouldBe` ["arr", "out"]
        reifyScheduledScop2 (buildIdentitySchedule2 scop) `shouldBe` Just [outer]
        case scSchedule scop of
          ScheduleLoopBand outerBand -> do
            lbIters outerBand `shouldBe` ["j"]
            lbRole outerBand `shouldBe` LoopMapReduction
            case lbBody outerBand of
              ScheduleSequence [ScheduleStmtRef [0, 0], ScheduleLoopBand innerBand, ScheduleStmtRef [0, 2]] -> do
                lbIters innerBand `shouldBe` ["k"]
                lbRole innerBand `shouldBe` LoopReduction
                lbReduction innerBand `shouldBe` Just (ReductionSpec "acc" (IConst 0) C.RAdd)
              other -> expectationFailure ("unexpected nested schedule tree: " <> show other)
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

  it "extracts and reifies tile-count style ceil-div bounds" $ do
    let tileCountBound = IDiv (IAdd (IVar "n") (IConst 31)) (IConst 32)
        loop =
          SLoop
            (LoopSpec ["i"] [tileCountBound] Serial Nothing LoopPlain)
            [SArrayWrite (C.AVar "out") (C.AVar "i") (C.AInt 0)]
    case extractProcScops2 (mkProc "p" ["n", "out"] [loop]) of
      [scop] -> do
        scParameters scop `shouldBe` ["n"]
        reifyScheduledScop2 (buildIdentitySchedule2 scop) `shouldBe` Just [loop]
        case scSchedule scop of
          ScheduleLoopBand band -> lbBounds band `shouldBe` [tileCountBound]
          other -> expectationFailure ("unexpected schedule tree: " <> show other)
      other -> expectationFailure ("expected one extracted scop, got: " <> show other)

  it "reifies strip-mined ND schedules like the legacy tiler" $ do
    let loop =
          SLoop
            (LoopSpec ["i", "j"] [IConst 64, IConst 48] Serial Nothing LoopPlain)
            [SArrayWrite (C.AVar "out") (C.AVar "i") (C.AVar "j")]
    case extractProcScops2 (mkProc "p" ["out"] [loop]) of
      [scop] ->
        reifyScheduledScop2 (tileScop2 scop) `shouldBe` Just (tileStmts2 [loop])
      other -> expectationFailure ("expected one extracted scop, got: " <> show other)

  it "reifies strip-mined map-reduction schedules like the legacy tiler" $ do
    let inner =
          SLoop
            (LoopSpec ["k"] [IConst 96] Serial (Just (ReductionSpec "acc" (IConst 0) C.RAdd)) LoopReduction)
            [SAssign "acc" (C.RBinOp C.CAdd (C.AVar "acc") (C.AInt 1))]
        outer =
          SLoop
            (LoopSpec ["j"] [IConst 128] Serial Nothing LoopMapReduction)
            [ SAssign "acc" (C.RAtom (C.AInt 0))
            , inner
            , SArrayWrite (C.AVar "out") (C.AVar "j") (C.AVar "acc")
            ]
    case extractProcScops2 (mkProc "p" ["out"] [outer]) of
      [scop] ->
        reifyScheduledScop2 (tileScop2 scop) `shouldBe` Just (tileStmts2 [outer])
      other -> expectationFailure ("expected one extracted scop, got: " <> show other)

  it "extracts the matmul benchmark through the supported SCoP prelude path" $ do
    diagnostics <- loadPreparedMatmulBenchmarkDiagnostics
    extractedProcNames diagnostics `shouldSatisfy` elem "matElem"
    extractedProcNames diagnostics `shouldSatisfy` elem "result"
    rejectedProcNames diagnostics `shouldNotContain` ["matElem", "result"]

  it "keeps the matmul benchmark reduction-wrapper idiom in the extracted schedule" $ do
    diagnostics <- loadPreparedMatmulBenchmarkDiagnostics
    case [scop | ScopExtracted scop <- diagnostics, scProcName scop == "matElem"] of
      (scop : _) ->
        scheduleLoopRoles (scSchedule scop) `shouldBe` [LoopReductionWrapper, LoopReduction]
      [] ->
        expectationFailure "expected an extracted matElem scop"

  it "reifies the matmul benchmark result as blocked ii/jj/kk loops with a vector-friendly update body" $ do
    diagnostics <- loadPreparedMatmulBenchmarkDiagnostics
    case [scop | ScopExtracted scop <- diagnostics, scProcName scop == "result"] of
      (scop : _) ->
        case reifyScheduledScop2 (tileScop2 scop) of
          Just stmts -> do
            let (outerI, outerJ, redK) = resultMatmulIterPrefixes scop
            hasLoopIterPrefix (outerI <> "_tile") stmts `shouldBe` True
            hasLoopIterPrefix (outerJ <> "_tile") stmts `shouldBe` True
            hasLoopIterPrefix (redK <> "_tile") stmts `shouldBe` True
            hasMapUpdateLoop stmts `shouldBe` True
          Nothing ->
            expectationFailure "expected the blocked matmul schedule to reify"
      [] ->
        expectationFailure "expected an extracted result scop"

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

  it "keeps consistent repeated iterator shifts across multiple dimensions" $ do
    let loop =
          SLoop
            (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopPlain)
            [ SAssign "srcIx" (C.RTuple [C.AVar "i", C.AVar "i"])
            , SAssign "dst0" (C.RBinOp C.CAdd (C.AVar "i") (C.AInt 1))
            , SAssign "dstIx" (C.RTuple [C.AVar "dst0", C.AVar "dst0"])
            , SArrayWrite (C.AVar "arr") (C.AVar "srcIx") (C.AInt 0)
            , SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "dstIx"))
            ]
    case extractProcScops2 (mkProc "p" ["n", "arr"] [loop]) of
      [scop] ->
        collectScopDependences2 scop `shouldBe`
          [ PolyhedralDependence
              { pdKind = PolyDepRAW
              , pdArray = "arr"
              , pdSourceStmt = [0, 3]
              , pdTargetStmt = [0, 4]
              , pdDirection = PolyDepForward
              , pdIsLoopCarried = True
              , pdDistance = Just [1]
              }
          ]
      other -> expectationFailure ("expected one extracted scop, got: " <> show other)

  it "classifies lexicographically forward mixed-sign distances as forward" $ do
    let loop =
          SLoop
            (LoopSpec ["i", "j"] [IVar "n", IVar "m"] Serial Nothing LoopPlain)
            [ SAssign "srcIx" (C.RTuple [C.AVar "i", C.AVar "j"])
            , SAssign "nextI" (C.RBinOp C.CAdd (C.AVar "i") (C.AInt 1))
            , SAssign "prevJ" (C.RBinOp C.CSub (C.AVar "j") (C.AInt 1))
            , SAssign "dstIx" (C.RTuple [C.AVar "nextI", C.AVar "prevJ"])
            , SArrayWrite (C.AVar "arr") (C.AVar "srcIx") (C.AInt 0)
            , SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "dstIx"))
            ]
    case extractProcScops2 (mkProc "p" ["n", "m", "arr"] [loop]) of
      [scop] -> do
        collectScopDependences2 scop `shouldBe`
          [ PolyhedralDependence
              { pdKind = PolyDepRAW
              , pdArray = "arr"
              , pdSourceStmt = [0, 4]
              , pdTargetStmt = [0, 5]
              , pdDirection = PolyDepForward
              , pdIsLoopCarried = True
              , pdDistance = Just [1, -1]
              }
          ]
        blockingScopDependences2 scop `shouldBe` []
      other -> expectationFailure ("expected one extracted scop, got: " <> show other)

  it "surfaces backward loop-carried dependences as blocking" $ do
    let loop =
          SLoop
            (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopPlain)
            [ SAssign "prev" (C.RBinOp C.CSub (C.AVar "i") (C.AInt 1))
            , SArrayWrite (C.AVar "arr") (C.AVar "i") (C.AInt 0)
            , SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "prev"))
            ]
    case extractProcScops2 (mkProc "p" ["n", "arr"] [loop]) of
      [scop] -> do
        collectScopDependences2 scop `shouldBe`
          [ PolyhedralDependence
              { pdKind = PolyDepRAW
              , pdArray = "arr"
              , pdSourceStmt = [0, 1]
              , pdTargetStmt = [0, 2]
              , pdDirection = PolyDepBackward
              , pdIsLoopCarried = True
               , pdDistance = Just [-1]
               }
            ]
        blockingScopDependences2 scop `shouldBe` collectScopDependences2 scop
      other -> expectationFailure ("expected one extracted scop, got: " <> show other)

  it "records carried-band metadata for mixed-sign dependences" $ do
    let loop =
          SLoop
            (LoopSpec ["i", "j"] [IVar "n", IVar "m"] Serial Nothing LoopPlain)
            [ SAssign "srcIx" (C.RTuple [C.AVar "i", C.AVar "j"])
            , SAssign "nextI" (C.RBinOp C.CAdd (C.AVar "i") (C.AInt 1))
            , SAssign "prevJ" (C.RBinOp C.CSub (C.AVar "j") (C.AInt 1))
            , SAssign "dstIx" (C.RTuple [C.AVar "nextI", C.AVar "prevJ"])
            , SArrayWrite (C.AVar "arr") (C.AVar "srcIx") (C.AInt 0)
            , SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "dstIx"))
            ]
    case extractProcScops2 (mkProc "p" ["n", "m", "arr"] [loop]) of
      [scop] ->
        collectScopDependenceRelations2 scop `shouldBe`
          [ PolyhedralDependenceRelation
              { pdrKind = PolyDepRAW
              , pdrArray = "arr"
              , pdrSourceStmt = [0, 4]
              , pdrTargetStmt = [0, 5]
              , pdrDirection = PolyDepForward
              , pdrIsLoopCarried = True
              , pdrDistance = Just [1, -1]
              , pdrCarryInfo =
                  [ PolyhedralCarryInfo "i" (PolyCarryDistance 1)
                  , PolyhedralCarryInfo "j" (PolyCarryDistance (-1))
                  ]
              , pdrBandCarry =
                  [ PolyhedralBandCarry
                      { pbcIters = ["i", "j"]
                      , pbcRole = LoopPlain
                      , pbcStatus = PolyBandForward
                      , pbcCarryInfo =
                          [ PolyhedralCarryInfo "i" (PolyCarryDistance 1)
                          , PolyhedralCarryInfo "j" (PolyCarryDistance (-1))
                          ]
                      }
                  ]
              , pdrClassification = PolyDepClassRegular
              , pdrIsBlocking = False
              , pdrSrcIndex =
                  [ AffineExpr (Map.fromList [("i", 1)]) 0
                  , AffineExpr (Map.fromList [("j", 1)]) 0
                  ]
              , pdrTgtIndex =
                  [ AffineExpr (Map.fromList [("i", 1)]) 1
                  , AffineExpr (Map.fromList [("j", 1)]) (-1)
                  ]
              }
          ]
      other -> expectationFailure ("expected one extracted scop, got: " <> show other)

  it "classifies reduction-carried dependences separately from blocking ones" $ do
    let loop =
          SLoop
            (LoopSpec ["k"] [IVar "n"] Serial (Just (ReductionSpec "acc" (IConst 0) C.RAdd)) LoopReduction)
            [ SAssign "prev" (C.RBinOp C.CSub (C.AVar "k") (C.AInt 1))
            , SArrayWrite (C.AVar "arr") (C.AVar "k") (C.AInt 0)
            , SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "prev"))
            , SAssign "acc" (C.RBinOp C.CAdd (C.AVar "acc") (C.AVar "x"))
            ]
    case extractProcScops2 (mkProc "p" ["n", "arr"] [loop]) of
      [scop] ->
        collectScopDependenceRelations2 scop `shouldBe`
          [ PolyhedralDependenceRelation
              { pdrKind = PolyDepRAW
              , pdrArray = "arr"
              , pdrSourceStmt = [0, 1]
              , pdrTargetStmt = [0, 2]
              , pdrDirection = PolyDepBackward
              , pdrIsLoopCarried = True
              , pdrDistance = Just [-1]
              , pdrCarryInfo =
                  [PolyhedralCarryInfo "k" (PolyCarryDistance (-1))]
              , pdrBandCarry =
                  [ PolyhedralBandCarry
                      { pbcIters = ["k"]
                      , pbcRole = LoopReduction
                      , pbcStatus = PolyBandBackward
                      , pbcCarryInfo =
                          [PolyhedralCarryInfo "k" (PolyCarryDistance (-1))]
                      }
                  ]
              , pdrClassification = PolyDepClassReductionLike
              , pdrIsBlocking = False
              , pdrSrcIndex = [AffineExpr (Map.fromList [("k", 1)]) 0]
              , pdrTgtIndex = [AffineExpr (Map.fromList [("k", 1)]) (-1)]
              }
          ]
      other -> expectationFailure ("expected one extracted scop, got: " <> show other)

  it "tracks carried-vs-independent bands across nested reduction contexts" $ do
    let inner =
          SLoop
            (LoopSpec ["k"] [IVar "n"] Serial (Just (ReductionSpec "acc" (IConst 0) C.RAdd)) LoopReduction)
            [ SAssign "srcIx" (C.RTuple [C.AVar "j", C.AVar "k"])
            , SAssign "prev" (C.RBinOp C.CSub (C.AVar "k") (C.AInt 1))
            , SAssign "dstIx" (C.RTuple [C.AVar "j", C.AVar "prev"])
            , SArrayWrite (C.AVar "arr") (C.AVar "srcIx") (C.AInt 0)
            , SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "dstIx"))
            , SAssign "acc" (C.RBinOp C.CAdd (C.AVar "acc") (C.AVar "x"))
            ]
        outer =
          SLoop
            (LoopSpec ["j"] [IVar "m"] Serial Nothing LoopMapReduction)
            [ SAssign "acc" (C.RAtom (C.AInt 0))
            , inner
            , SArrayWrite (C.AVar "out") (C.AVar "j") (C.AVar "acc")
            ]
    case extractProcScops2 (mkProc "p" ["m", "n", "arr", "out"] [outer]) of
      [scop] ->
        collectScopDependenceRelations2 scop `shouldBe`
          [ PolyhedralDependenceRelation
              { pdrKind = PolyDepRAW
              , pdrArray = "arr"
              , pdrSourceStmt = [0, 1, 3]
              , pdrTargetStmt = [0, 1, 4]
              , pdrDirection = PolyDepBackward
              , pdrIsLoopCarried = True
              , pdrDistance = Just [0, -1]
              , pdrCarryInfo =
                  [ PolyhedralCarryInfo "j" PolyCarryIndependent
                  , PolyhedralCarryInfo "k" (PolyCarryDistance (-1))
                  ]
              , pdrBandCarry =
                  [ PolyhedralBandCarry
                      { pbcIters = ["j"]
                      , pbcRole = LoopMapReduction
                      , pbcStatus = PolyBandIndependent
                      , pbcCarryInfo =
                          [PolyhedralCarryInfo "j" PolyCarryIndependent]
                      }
                  , PolyhedralBandCarry
                      { pbcIters = ["k"]
                      , pbcRole = LoopReduction
                      , pbcStatus = PolyBandBackward
                      , pbcCarryInfo =
                          [PolyhedralCarryInfo "k" (PolyCarryDistance (-1))]
                      }
                  ]
              , pdrClassification = PolyDepClassReductionLike
              , pdrIsBlocking = False
              , pdrSrcIndex =
                  [ AffineExpr (Map.fromList [("j", 1)]) 0
                  , AffineExpr (Map.fromList [("k", 1)]) 0
                  ]
              , pdrTgtIndex =
                  [ AffineExpr (Map.fromList [("j", 1)]) 0
                  , AffineExpr (Map.fromList [("k", 1)]) (-1)
                  ]
              }
          ]
      other -> expectationFailure ("expected one extracted scop, got: " <> show other)

  it "keeps dependence relations on quasi-affine tile-count bounds" $ do
    let tileCountBound = IDiv (IAdd (IVar "n") (IConst 31)) (IConst 32)
        loop =
          SLoop
            (LoopSpec ["i"] [tileCountBound] Serial Nothing LoopPlain)
            [ SAssign "ix" (C.RBinOp C.CAdd (C.AVar "i") (C.AInt 1))
            , SArrayWrite (C.AVar "arr") (C.AVar "i") (C.AInt 0)
            , SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "ix"))
            ]
    case extractProcScops2 (mkProc "p" ["n", "arr"] [loop]) of
      [scop] ->
        collectScopDependenceRelations2 scop `shouldBe`
          [ PolyhedralDependenceRelation
              { pdrKind = PolyDepRAW
              , pdrArray = "arr"
              , pdrSourceStmt = [0, 1]
              , pdrTargetStmt = [0, 2]
              , pdrDirection = PolyDepForward
              , pdrIsLoopCarried = True
              , pdrDistance = Just [1]
              , pdrCarryInfo =
                  [PolyhedralCarryInfo "i" (PolyCarryDistance 1)]
              , pdrBandCarry =
                  [ PolyhedralBandCarry
                      { pbcIters = ["i"]
                      , pbcRole = LoopPlain
                      , pbcStatus = PolyBandForward
                      , pbcCarryInfo =
                          [PolyhedralCarryInfo "i" (PolyCarryDistance 1)]
                      }
                  ]
              , pdrClassification = PolyDepClassRegular
              , pdrIsBlocking = False
              , pdrSrcIndex = [AffineExpr (Map.fromList [("i", 1)]) 0]
              , pdrTgtIndex = [AffineExpr (Map.fromList [("i", 1)]) 1]
              }
          ]
      other -> expectationFailure ("expected one extracted scop, got: " <> show other)

  it "synthesizes a legal band interchange when an outer dimension is independent" $ do
    let loop =
          SLoop
            (LoopSpec ["i", "j"] [IVar "n", IVar "m"] Serial Nothing LoopPlain)
            [ SAssign "srcIx" (C.RTuple [C.AVar "i", C.AVar "j"])
            , SAssign "nextI" (C.RBinOp C.CAdd (C.AVar "i") (C.AInt 1))
            , SAssign "dstIx" (C.RTuple [C.AVar "nextI", C.AVar "j"])
            , SArrayWrite (C.AVar "arr") (C.AVar "srcIx") (C.AInt 0)
            , SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "dstIx"))
            ]
    case extractProcScops2 (mkProc "p" ["n", "m", "arr"] [loop]) of
      [scop] -> do
        case ssAffineSchedule (synthesizeScopSchedule2 scop) of
          AffineSchedule { asRoot = AffineScheduleLoopBand band } ->
            map sdIter (albDims band) `shouldBe` ["j", "i"]
          other ->
            expectationFailure ("unexpected affine schedule: " <> show other)
        case reifyScheduledScop2 (synthesizeScopSchedule2 scop) of
          Just [SLoop loopSpec _] ->
            lsIters loopSpec `shouldBe` ["j", "i"]
          other ->
            expectationFailure ("unexpected reified schedule: " <> show other)
      other -> expectationFailure ("expected one extracted scop, got: " <> show other)

  it "reorders a nested inner band even when an outer band already carries the dependence" $ do
    let inner =
          SLoop
            (LoopSpec ["j", "k"] [IVar "m", IVar "p"] Serial Nothing LoopPlain)
            [ SAssign "srcIx" (C.RTuple [C.AVar "i", C.AVar "j", C.AVar "k"])
            , SAssign "nextI" (C.RBinOp C.CAdd (C.AVar "i") (C.AInt 1))
            , SAssign "nextJ" (C.RBinOp C.CAdd (C.AVar "j") (C.AInt 1))
            , SAssign "dstIx" (C.RTuple [C.AVar "nextI", C.AVar "nextJ", C.AVar "k"])
            , SArrayWrite (C.AVar "arr") (C.AVar "srcIx") (C.AInt 0)
            , SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "dstIx"))
            ]
        outer =
          SLoop
            (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopPlain)
            [inner]
    case extractProcScops2 (mkProc "p" ["n", "m", "p", "arr"] [outer]) of
      [scop] -> do
        case ssAffineSchedule (synthesizeScopSchedule2 scop) of
          AffineSchedule
            { asRoot =
                AffineScheduleLoopBand outerBand
            } ->
              case albBody outerBand of
                AffineScheduleSequence [AffineScheduleLoopBand innerBand] ->
                  map sdIter (albDims innerBand) `shouldBe` ["k", "j"]
                other ->
                  expectationFailure ("unexpected nested affine schedule: " <> show other)
          other ->
            expectationFailure ("unexpected affine schedule: " <> show other)
        case reifyScheduledScop2 (synthesizeScopSchedule2 scop) of
          Just
            [ SLoop _ [SLoop innerLoopSpec _] ] ->
              lsIters innerLoopSpec `shouldBe` ["k", "j"]
          other ->
            expectationFailure ("unexpected reified schedule: " <> show other)
      other -> expectationFailure ("expected one extracted scop, got: " <> show other)

  it "uses band interchange to turn a blocking dependence into a forward schedule" $ do
    let loop =
          SLoop
            (LoopSpec ["i", "j"] [IVar "n", IVar "m"] Serial Nothing LoopPlain)
            [ SAssign "srcIx" (C.RTuple [C.AVar "i", C.AVar "j"])
            , SAssign "prevI" (C.RBinOp C.CSub (C.AVar "i") (C.AInt 1))
            , SAssign "nextJ" (C.RBinOp C.CAdd (C.AVar "j") (C.AInt 1))
            , SAssign "dstIx" (C.RTuple [C.AVar "prevI", C.AVar "nextJ"])
            , SArrayWrite (C.AVar "arr") (C.AVar "srcIx") (C.AInt 0)
            , SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "dstIx"))
            ]
    case extractProcScops2 (mkProc "p" ["n", "m", "arr"] [loop]) of
      [scop] -> do
        blockingScopDependences2 scop `shouldBe` collectScopDependences2 scop
        case ssAffineSchedule (synthesizeScopSchedule2 scop) of
          AffineSchedule { asRoot = AffineScheduleLoopBand band } ->
            map sdIter (albDims band) `shouldBe` ["j", "i"]
          other ->
            expectationFailure ("unexpected affine schedule: " <> show other)
        case reifyScheduledScop2 (synthesizeScopSchedule2 scop) of
          Just [SLoop loopSpec body] -> do
            -- After interchange, j is outer; i is renamed to i__s by skewing
            -- (skew makes i-dep independent: i_dist + 1*j_dist = -1+1 = 0)
            lsIters loopSpec `shouldBe` ["j", "i__s"]
            -- The skew prelude assigns i = i__s - coeff*j
            let preludes = [v | SAssign v _ <- body]
            preludes `shouldSatisfy` elem "i"
          other ->
            expectationFailure ("unexpected reified schedule: " <> show other)
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
  fusionSpec
  skewingSpec

loadPreparedMatmulBenchmarkDiagnostics :: IO [ScopDiagnostic]
loadPreparedMatmulBenchmarkDiagnostics = do
  src <- BS.readFile "bench/matmul/mat_mul_bench.hyd"
  case readDecs src of
    Left perr ->
      expectationFailure ("Parse error: " ++ perr) >> pure []
    Right decs -> do
      let pipelineOpts =
            defaultPipelineOptions
              { poEnableTiling = True
              , poEnablePolyhedral = True
              , poEnableParallelization = True
              }
      prog <- lowerToCFG2WithTypesWithOptions defaultInferOptions decs
      pure (collectProgramScopDiagnostics2 (preparePolyhedralProgramWithOptions pipelineOpts prog))

extractedProcNames :: [ScopDiagnostic] -> [C.CVar]
extractedProcNames diagnostics =
  [ scProcName scop
  | ScopExtracted scop <- diagnostics
  ]

rejectedProcNames :: [ScopDiagnostic] -> [C.CVar]
rejectedProcNames diagnostics =
  [ procName
  | ScopRejected { sdProcName = procName } <- diagnostics
  ]

scheduleLoopRoles :: ScheduleTree -> [LoopRole]
scheduleLoopRoles sched = case sched of
  ScheduleSequence xs -> concatMap scheduleLoopRoles xs
  ScheduleStmtRef {} -> []
  ScheduleLoopBand band -> lbRole band : scheduleLoopRoles (lbBody band)
  ScheduleStripMine band _ -> lbRole band : scheduleLoopRoles (lbBody band)

resultMatmulIterPrefixes :: Scop -> (C.CVar, C.CVar, C.CVar)
resultMatmulIterPrefixes scop =
  case scSchedule scop of
    ScheduleLoopBand outerBand ->
      case lbIters outerBand of
        [outerI, outerJ] ->
          case findReductionIter (lbBody outerBand) of
            Just redK -> (outerI, outerJ, redK)
            Nothing -> error "expected a reduction loop inside result scop"
        _ ->
          error "expected a 2D outer loop band in result scop"
    _ ->
      error "expected the result scop to start with a loop band"
  where
    findReductionIter sched = case sched of
      ScheduleSequence xs -> foldr ((<|>) . findReductionIter) Nothing xs
      ScheduleStmtRef {} -> Nothing
      ScheduleStripMine band _ -> findReductionIter (lbBody band)
      ScheduleLoopBand band
        | lbRole band == LoopReduction ->
            case lbIters band of
              [iter] -> Just iter
              _ -> Nothing
        | otherwise ->
            findReductionIter (lbBody band)

hasLoopIterPrefix :: C.CVar -> [Stmt] -> Bool
hasLoopIterPrefix prefix = any go
  where
    go stmt = case stmt of
      SLoop loopSpec body ->
        any (BS.isPrefixOf prefix) (lsIters loopSpec) || hasLoopIterPrefix prefix body
      SIf _ thn els ->
        hasLoopIterPrefix prefix thn || hasLoopIterPrefix prefix els
      _ ->
        False

hasMapUpdateLoop :: [Stmt] -> Bool
hasMapUpdateLoop = any go
  where
    go stmt = case stmt of
      SLoop loopSpec body ->
        (lsRole loopSpec == LoopMap && loadCount body >= 2 && writeCount body >= 1)
          || hasMapUpdateLoop body
      SIf _ thn els ->
        hasMapUpdateLoop thn || hasMapUpdateLoop els
      _ ->
        False

    loadCount =
      length
        . filter (\stmt -> case stmt of SAssign _ (C.RArrayLoad _ _) -> True; _ -> False)

    writeCount =
      length
        . filter (\stmt -> case stmt of SArrayWrite {} -> True; _ -> False)

-- ---------------------------------------------------------------------------
-- Fusion tests
-- ---------------------------------------------------------------------------

-- | Count bands at the current level of a schedule sequence.
-- Used to check that fusion produced a smaller number of bands.
sequenceBandCount :: AffineScheduleTree -> Int
sequenceBandCount tree = case tree of
  AffineScheduleSequence xs -> sum (map sequenceBandCount xs)
  AffineScheduleLoopBand {} -> 1
  AffineScheduleStmtRef {} -> 0

-- | Given an outer loop containing two inner loops, extract one SCoP and
-- synthesize a schedule; return the affine schedule root.
fuseSiblingBands :: Stmt -> AffineScheduleTree
fuseSiblingBands outer =
  case extractProcScops2 (mkProc "p" ["n", "m", "a", "b"] [outer]) of
    (scop : _) -> asRoot (ssAffineSchedule (synthesizeScopSchedule2 scop))
    [] -> error "expected at least one scop"

-- | Count inner loop bands inside the body of the outermost band.
innerBandCount :: AffineScheduleTree -> Int
innerBandCount tree = case tree of
  AffineScheduleLoopBand band -> sequenceBandCount (albBody band)
  AffineScheduleSequence xs -> case filter isLoopBand xs of
    (t : _) -> innerBandCount t
    [] -> 0
  _ -> 0
  where
    isLoopBand (AffineScheduleLoopBand {}) = True
    isLoopBand _ = False

-- | Count inner loop stmts (SLoop) in the reified output.
reifiedInnerLoopCount :: Maybe [Stmt] -> Int
reifiedInnerLoopCount Nothing = -1
reifiedInnerLoopCount (Just stmts) =
  sum [length [() | SLoop {} <- body] | SLoop _ body <- stmts]

fusionSpec :: Spec
fusionSpec = describe "fusion" $ do
  it "two independent map bands with the same bound are structurally compatible" $ do
    bandsStructurallyCompatible2 (mkMapBand "i") (mkMapBand "j") `shouldBe` True
  it "two sibling inner loops with the same bound and no cross-deps fuse" $ do
    -- outer loop containing two independent inner loops
    let outer =
          SLoop (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopPlain)
            [ SLoop (LoopSpec ["j"] [IVar "m"] Serial Nothing LoopMap)
                [SArrayWrite (C.AVar "a") (C.AVar "j") (C.AVar "i")]
            , SLoop (LoopSpec ["k"] [IVar "m"] Serial Nothing LoopMap)
                [SArrayWrite (C.AVar "b") (C.AVar "k") (C.AVar "i")]
            ]
    innerBandCount (fuseSiblingBands outer) `shouldBe` 1
  it "fused sibling loops reify to a single inner loop with both writes" $ do
    let outer =
          SLoop (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopPlain)
            [ SLoop (LoopSpec ["j"] [IVar "m"] Serial Nothing LoopMap)
                [SArrayWrite (C.AVar "a") (C.AVar "j") (C.AVar "i")]
            , SLoop (LoopSpec ["k"] [IVar "m"] Serial Nothing LoopMap)
                [SArrayWrite (C.AVar "b") (C.AVar "k") (C.AVar "i")]
            ]
    case extractProcScops2 (mkProc "p" ["n", "m", "a", "b"] [outer]) of
      (scop : _) -> do
        let scheduled = synthesizeScopSchedule2 scop
        case reifyScheduledScop2 scheduled of
          Just [SLoop _ [SLoop _ innerBody]] -> do
            let writes = [() | SArrayWrite {} <- innerBody]
            length writes `shouldBe` 2
          Just other ->
            expectationFailure ("expected outer->single inner loop, got: " <> show other)
          Nothing ->
            expectationFailure "reification failed"
      other ->
        expectationFailure ("expected scops, got: " <> show other)
  it "sibling loops with backward cross-dep do not fuse" $ do
    -- inner1 writes a[j], inner2 reads a[j+1] — backward dep blocks fusion
    let outer =
          SLoop (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopPlain)
            [ SLoop (LoopSpec ["j"] [IVar "m"] Serial Nothing LoopMap)
                [SArrayWrite (C.AVar "a") (C.AVar "j") (C.AVar "i")]
            , SLoop (LoopSpec ["k"] [IVar "m"] Serial Nothing LoopMap)
                [ SAssign "nextK" (C.RBinOp C.CAdd (C.AVar "k") (C.AInt 1))
                , SAssign "x" (C.RArrayLoad (C.AVar "a") (C.AVar "nextK"))
                , SArrayWrite (C.AVar "b") (C.AVar "k") (C.AVar "x")
                ]
            ]
    innerBandCount (fuseSiblingBands outer) `shouldBe` 2
  it "sibling loops with forward cross-dep (a[j-1]) do fuse" $ do
    -- inner1 writes a[j], inner2 reads a[j-1] — forward dep allows fusion
    let outer =
          SLoop (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopPlain)
            [ SLoop (LoopSpec ["j"] [IVar "m"] Serial Nothing LoopMap)
                [SArrayWrite (C.AVar "a") (C.AVar "j") (C.AVar "i")]
            , SLoop (LoopSpec ["k"] [IVar "m"] Serial Nothing LoopMap)
                [ SAssign "prevK" (C.RBinOp C.CSub (C.AVar "k") (C.AInt 1))
                , SAssign "x" (C.RArrayLoad (C.AVar "a") (C.AVar "prevK"))
                , SArrayWrite (C.AVar "b") (C.AVar "k") (C.AVar "x")
                ]
            ]
    innerBandCount (fuseSiblingBands outer) `shouldBe` 1
  it "tryFuseBands2 returns Nothing for bands with mismatched bounds" $ do
    let b2 = (mkMapBand "j") { albDims = [ScheduleDim "j" (IVar "m")] }
    tryFuseBands2 0 [] (AffineScheduleLoopBand (mkMapBand "i")) (AffineScheduleLoopBand b2)
      `shouldBe` Nothing
  where
    mkMapBand iter =
      AffineLoopBand
        { albDims = [ScheduleDim iter (IVar "n")]
        , albExec = Serial
        , albReduction = Nothing
        , albRole = LoopMap
        , albSkew = []
        , albBody = AffineScheduleStmtRef [0]
        }

-- ---------------------------------------------------------------------------
-- Skewing tests
-- ---------------------------------------------------------------------------

skewingSpec :: Spec
skewingSpec = describe "skewing" $ do
  it "suggestBandSkew2 returns Nothing for a loop with no backward deps" $ do
    let dims = [ScheduleDim "i" (IVar "n"), ScheduleDim "j" (IVar "m")]
        relations = []
    suggestBandSkew2 0 dims relations `shouldBe` Nothing
  it "suggestBandSkew2 returns Nothing for a single-dim band" $ do
    let dims = [ScheduleDim "i" (IVar "n")]
        rel = forwardRel "i" 1
    suggestBandSkew2 0 dims [rel] `shouldBe` Nothing
  it "suggestBandSkew2 suggests a skew when inner dim has backward dep and outer has forward dep" $ do
    let dims = [ScheduleDim "i" (IVar "n"), ScheduleDim "j" (IVar "m")]
        rel = backwardInnerRel "i" "j" 1 (-1)
    case suggestBandSkew2 0 dims [rel] of
      Nothing -> expectationFailure "expected a skew suggestion"
      Just skew -> do
        skewTarget skew `shouldBe` "j"
        skewSource skew `shouldBe` "i"
        skewCoeff skew `shouldSatisfy` (>= 1)
  it "skewCoeff >= ceil(innerDist / outerDist) ensures new inner dist is non-negative" $ do
    let dims = [ScheduleDim "i" (IVar "n"), ScheduleDim "j" (IVar "m")]
        rel = backwardInnerRel "i" "j" 2 (-3)
    case suggestBandSkew2 0 dims [rel] of
      Nothing -> expectationFailure "expected a skew suggestion"
      Just skew -> do
        let newInnerDist = (-3) + skewCoeff skew * 2
        newInnerDist `shouldSatisfy` (>= 0)
  it "suggestBandSkew2 takes the maximum coefficient when multiple deps require skewing" $ do
    let dims = [ScheduleDim "i" (IVar "n"), ScheduleDim "j" (IVar "m")]
        rel1 = backwardInnerRel "i" "j" 1 (-1)
        rel2 = backwardInnerRel "i" "j" 1 (-3)
    case suggestBandSkew2 0 dims [rel1, rel2] of
      Nothing -> expectationFailure "expected a skew suggestion"
      Just skew -> do
        let newInnerDist1 = (-1) + skewCoeff skew * 1
            newInnerDist2 = (-3) + skewCoeff skew * 1
        newInnerDist1 `shouldSatisfy` (>= 0)
        newInnerDist2 `shouldSatisfy` (>= 0)
  where
    forwardRel iter dist =
      PolyhedralDependenceRelation
        { pdrKind = PolyDepRAW
        , pdrArray = "a"
        , pdrSourceStmt = [0]
        , pdrTargetStmt = [0]
        , pdrDirection = PolyDepForward
        , pdrIsLoopCarried = True
        , pdrDistance = Just [dist]
        , pdrCarryInfo = [PolyhedralCarryInfo iter (PolyCarryDistance dist)]
        , pdrBandCarry =
            [ PolyhedralBandCarry
                { pbcIters = [iter]
                , pbcRole = LoopMap
                , pbcStatus = if dist > 0 then PolyBandForward else PolyBandIndependent
                , pbcCarryInfo = [PolyhedralCarryInfo iter (PolyCarryDistance dist)]
                }
            ]
        , pdrClassification = PolyDepClassRegular
        , pdrIsBlocking = False
        , pdrSrcIndex = []
        , pdrTgtIndex = []
        }
    backwardInnerRel outerIter innerIter outerDist innerDist =
      PolyhedralDependenceRelation
        { pdrKind = PolyDepRAW
        , pdrArray = "a"
        , pdrSourceStmt = [0]
        , pdrTargetStmt = [0]
        , pdrDirection = PolyDepForward
        , pdrIsLoopCarried = True
        , pdrDistance = Just [outerDist, innerDist]
        , pdrCarryInfo =
            [ PolyhedralCarryInfo outerIter (PolyCarryDistance outerDist)
            , PolyhedralCarryInfo innerIter (PolyCarryDistance innerDist)
            ]
        , pdrBandCarry =
            [ PolyhedralBandCarry
                { pbcIters = [outerIter, innerIter]
                , pbcRole = LoopMap
                , pbcStatus = PolyBandForward
                , pbcCarryInfo =
                    [ PolyhedralCarryInfo outerIter (PolyCarryDistance outerDist)
                    , PolyhedralCarryInfo innerIter (PolyCarryDistance innerDist)
                    ]
                }
            ]
        , pdrClassification = PolyDepClassRegular
        , pdrIsBlocking = True
        , pdrSrcIndex = []
        , pdrTgtIndex = []
        }
