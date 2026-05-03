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
            (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopMap [])
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
            (LoopSpec ["i", "j"] [IVar "n", IAdd (IVar "m") (IConst 4)] Serial Nothing LoopPlain [])
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

  it "profitability prefers row-major loop order for legal tuple-indexed map nests" $ do
    let loop =
          SLoop
            (LoopSpec ["j", "i"] [IVar "m", IVar "n"] Serial Nothing LoopMap [])
            [ SAssign "ix" (C.RTuple [C.AVar "i", C.AVar "j"])
            , SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "ix"))
            , SArrayWrite (C.AVar "out") (C.AVar "ix") (C.AVar "x")
            ]
    case extractProcScops2 (mkProc "p" ["n", "m", "arr", "out"] [loop]) of
      [scop] ->
        case ssSchedule (synthesizeScopSchedule2 scop) of
          ScheduleLoopBand band -> lbIters band `shouldBe` ["i", "j"]
          other -> expectationFailure ("unexpected schedule tree: " <> show other)
      other -> expectationFailure ("expected one extracted scop, got: " <> show other)

  it "profitability sees lowered RNdToFlat accesses as row-major-friendly" $ do
    let loop =
          SLoop
            (LoopSpec ["j", "i"] [IVar "m", IVar "n"] Serial Nothing LoopMap [])
            [ SAssign "shape" (C.RTuple [C.AVar "n", C.AVar "m"])
            , SAssign "ix" (C.RTuple [C.AVar "i", C.AVar "j"])
            , SAssign "flat" (C.RNdToFlat (C.AVar "ix") (C.AVar "shape"))
            , SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "flat"))
            , SArrayWrite (C.AVar "out") (C.AVar "flat") (C.AVar "x")
            ]
    case extractProcScops2 (mkProc "p" ["n", "m", "arr", "out"] [loop]) of
      [scop] ->
        case ssSchedule (synthesizeScopSchedule2 scop) of
          ScheduleLoopBand band -> lbIters band `shouldBe` ["i", "j"]
          other -> expectationFailure ("unexpected schedule tree: " <> show other)
      other -> expectationFailure ("expected one extracted scop, got: " <> show other)

  it "profitability counts invariant reads for reuse" $ do
    let loop =
          SLoop
            (LoopSpec ["j", "i"] [IVar "m", IVar "n"] Serial Nothing LoopMap [])
            [ SAssign "flat" (C.RBinOp C.CAdd (C.AVar "i") (C.AVar "j"))
            , SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "i"))
            , SArrayWrite (C.AVar "out") (C.AVar "flat") (C.AVar "x")
            ]
    case extractProcScops2 (mkProc "p" ["n", "m", "arr", "out"] [loop]) of
      [scop] -> do
        let facts = collectScopProfitabilityFacts2 scop
        fmap ipInvariantReadHits (Map.lookup "j" facts) `shouldBe` Just 1
        fmap ipInvariantReadHits (Map.lookup "i" facts) `shouldBe` Just 0
      other -> expectationFailure ("expected one extracted scop, got: " <> show other)

  it "profitability prefers inner placement for iterators with invariant-read reuse when locality ties" $ do
    let dims =
          [ ScheduleDim "j" (IConst 32)
          , ScheduleDim "i" (IConst 32)
          ]
        facts =
          Map.fromList
            [ ("j", IteratorProfitability { ipAccessDimHits = Map.empty, ipUnitStrideLastHits = 0, ipReadTouchHits = 0, ipInvariantReadHits = 3 })
            , ("i", IteratorProfitability { ipAccessDimHits = Map.empty, ipUnitStrideLastHits = 0, ipReadTouchHits = 0, ipInvariantReadHits = 0 })
            ]
    chooseBandPermutation2 facts 0 dims [] `shouldBe` [1, 0]

  it "extracts a map-reduction nest that matches today's tiling target" $ do
    let inner =
          SLoop
            (LoopSpec ["k"] [IVar "m"] Serial (Just (ReductionSpec "acc" (IConst 0) C.RAdd)) LoopReduction [])
            [ SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "k"))
            , SAssign "acc" (C.RBinOp C.CAdd (C.AVar "acc") (C.AVar "x"))
            ]
        outer =
          SLoop
            (LoopSpec ["j"] [IVar "n"] Serial Nothing LoopMapReduction [])
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
            (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopPlain [])
            [SAssign "x" (C.RCall "opaque" [C.AVar "i"])]
    case collectProcScopDiagnostics2 (mkProc "p" ["n"] [loop]) of
      [diag] -> case diag of
        ScopRejected proc path (RejectUnsupportedRHS _) -> do
          proc `shouldBe` "p"
          path `shouldBe` [0]
        other -> expectationFailure ("unexpected diagnostic: " <> show other)
      other -> expectationFailure ("expected one diagnostic, got: " <> show other)

  it "extracts the affine interior of clamp stencils while leaving the boundary fallback rejected" $ do
    diagnostics <-
      loadPreparedDiagnosticsFromSource $
        BS.pack $
          unlines
            [ "let input = generate [8] (let f [i] = i + 1 in f)"
            , "let result = stencil clamp"
            , "  (fn acc => acc (-1) + acc 0 + acc 1)"
            , "  input"
            ]
    extractedProcNames diagnostics `shouldSatisfy` elem "result"
    rejectedProcNames diagnostics `shouldSatisfy` elem "result"

  it "extracts the affine interior of constant-boundary stencils too" $ do
    diagnostics <-
      loadPreparedDiagnosticsFromSource $
        BS.pack $
          unlines
            [ "let input = generate [6] (let f [i] = i + 1 in f)"
            , "let result = stencil (constant 0)"
            , "  (fn acc => acc (-1) + acc 0 + acc 1)"
            , "  input"
            ]
    extractedProcNames diagnostics `shouldSatisfy` elem "result"
    rejectedProcNames diagnostics `shouldSatisfy` elem "result"

  it "extracts and reifies tile-count style ceil-div bounds" $ do
    let tileCountBound = IDiv (IAdd (IVar "n") (IConst 31)) (IConst 32)
        loop =
          SLoop
            (LoopSpec ["i"] [tileCountBound] Serial Nothing LoopPlain [])
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
            (LoopSpec ["i", "j"] [IConst 64, IConst 48] Serial Nothing LoopPlain [])
            [SArrayWrite (C.AVar "out") (C.AVar "i") (C.AVar "j")]
    case extractProcScops2 (mkProc "p" ["out"] [loop]) of
      [scop] ->
        reifyScheduledScop2 (tileScop2 scop) `shouldBe` Just (tileStmts2 [loop])
      other -> expectationFailure ("expected one extracted scop, got: " <> show other)

  it "reifies strip-mined map-reduction schedules like the legacy tiler" $ do
    let inner =
          SLoop
            (LoopSpec ["k"] [IConst 96] Serial (Just (ReductionSpec "acc" (IConst 0) C.RAdd)) LoopReduction [])
            [SAssign "acc" (C.RBinOp C.CAdd (C.AVar "acc") (C.AInt 1))]
        outer =
          SLoop
            (LoopSpec ["j"] [IConst 128] Serial Nothing LoopMapReduction [])
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
            (LoopSpec ["j"] [IVar "m"] Serial Nothing LoopMap [])
            [ SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "j"))
            , SArrayWrite (C.AVar "out") (C.AVar "j") (C.AVar "x")
            ]
        outer =
          SLoop
            (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopPlain [])
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
            (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopPlain [])
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
            (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopPlain [])
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
            (LoopSpec ["i", "j"] [IVar "n", IVar "m"] Serial Nothing LoopPlain [])
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
            (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopPlain [])
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
            (LoopSpec ["i", "j"] [IVar "n", IVar "m"] Serial Nothing LoopPlain [])
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
            (LoopSpec ["k"] [IVar "n"] Serial (Just (ReductionSpec "acc" (IConst 0) C.RAdd)) LoopReduction [])
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
            (LoopSpec ["k"] [IVar "n"] Serial (Just (ReductionSpec "acc" (IConst 0) C.RAdd)) LoopReduction [])
            [ SAssign "srcIx" (C.RTuple [C.AVar "j", C.AVar "k"])
            , SAssign "prev" (C.RBinOp C.CSub (C.AVar "k") (C.AInt 1))
            , SAssign "dstIx" (C.RTuple [C.AVar "j", C.AVar "prev"])
            , SArrayWrite (C.AVar "arr") (C.AVar "srcIx") (C.AInt 0)
            , SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "dstIx"))
            , SAssign "acc" (C.RBinOp C.CAdd (C.AVar "acc") (C.AVar "x"))
            ]
        outer =
          SLoop
            (LoopSpec ["j"] [IVar "m"] Serial Nothing LoopMapReduction [])
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
            (LoopSpec ["i"] [tileCountBound] Serial Nothing LoopPlain [])
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
            (LoopSpec ["i", "j"] [IVar "n", IVar "m"] Serial Nothing LoopPlain [])
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
            (LoopSpec ["j", "k"] [IVar "m", IVar "p"] Serial Nothing LoopPlain [])
            [ SAssign "srcIx" (C.RTuple [C.AVar "i", C.AVar "j", C.AVar "k"])
            , SAssign "nextI" (C.RBinOp C.CAdd (C.AVar "i") (C.AInt 1))
            , SAssign "nextJ" (C.RBinOp C.CAdd (C.AVar "j") (C.AInt 1))
            , SAssign "dstIx" (C.RTuple [C.AVar "nextI", C.AVar "nextJ", C.AVar "k"])
            , SArrayWrite (C.AVar "arr") (C.AVar "srcIx") (C.AInt 0)
            , SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "dstIx"))
            ]
        outer =
          SLoop
            (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopPlain [])
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
            (LoopSpec ["i", "j"] [IVar "n", IVar "m"] Serial Nothing LoopPlain [])
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
          Just [SLoop loopSpec _] -> do
            -- After interchange, j is outer; i is inner (within-band skew
            -- is deferred; band permutation alone makes the schedule legal)
            lsIters loopSpec `shouldBe` ["j", "i"]
          other ->
            expectationFailure ("unexpected reified schedule: " <> show other)
      other -> expectationFailure ("expected one extracted scop, got: " <> show other)

  it "reifies an identity schedule back into the original loop nest" $ do
    let inner =
          SLoop
            (LoopSpec ["j"] [IVar "m"] Serial Nothing LoopMap [])
            [ SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "j"))
            , SArrayWrite (C.AVar "out") (C.AVar "j") (C.AVar "x")
            ]
        loop =
          SLoop
            (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopPlain [])
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
            (LoopSpec ["j"] [IVar "m"] Serial Nothing LoopMap [])
            [ SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "j"))
            , SArrayWrite (C.AVar "out") (C.AVar "j") (C.AVar "x")
            ]
        outer =
          SLoop
            (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopPlain [])
            [ SAssign "opaque" (C.RCall "f" [C.AVar "i"])
            , inner
            ]
        proc = mkProc "p" ["n", "m", "arr", "out"] [outer]
    polyhedralProgram2 (Program [proc]) `shouldBe` Program [proc]
  fusionSpec
  skewingSpec
  temporalSkewingSpec
  wavefrontCollapseSpec

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

loadPreparedDiagnosticsFromSource :: BS.ByteString -> IO [ScopDiagnostic]
loadPreparedDiagnosticsFromSource src =
  case readDecs src of
    Left perr ->
      expectationFailure ("Parse error: " ++ perr) >> pure []
    Right decs -> do
      let pipelineOpts =
            defaultPipelineOptions
              { poEnableTiling = True
              , poEnablePolyhedral = True
              , poEnableParallelization = False
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
          SLoop (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopPlain [])
            [ SLoop (LoopSpec ["j"] [IVar "m"] Serial Nothing LoopMap [])
                [SArrayWrite (C.AVar "a") (C.AVar "j") (C.AVar "i")]
            , SLoop (LoopSpec ["k"] [IVar "m"] Serial Nothing LoopMap [])
                [SArrayWrite (C.AVar "b") (C.AVar "k") (C.AVar "i")]
            ]
    innerBandCount (fuseSiblingBands outer) `shouldBe` 1
  it "fused sibling loops reify to a single inner loop with both writes" $ do
    let outer =
          SLoop (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopPlain [])
            [ SLoop (LoopSpec ["j"] [IVar "m"] Serial Nothing LoopMap [])
                [SArrayWrite (C.AVar "a") (C.AVar "j") (C.AVar "i")]
            , SLoop (LoopSpec ["k"] [IVar "m"] Serial Nothing LoopMap [])
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
          SLoop (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopPlain [])
            [ SLoop (LoopSpec ["j"] [IVar "m"] Serial Nothing LoopMap [])
                [SArrayWrite (C.AVar "a") (C.AVar "j") (C.AVar "i")]
            , SLoop (LoopSpec ["k"] [IVar "m"] Serial Nothing LoopMap [])
                [ SAssign "nextK" (C.RBinOp C.CAdd (C.AVar "k") (C.AInt 1))
                , SAssign "x" (C.RArrayLoad (C.AVar "a") (C.AVar "nextK"))
                , SArrayWrite (C.AVar "b") (C.AVar "k") (C.AVar "x")
                ]
            ]
    innerBandCount (fuseSiblingBands outer) `shouldBe` 2
  it "sibling loops with forward cross-dep (a[j-1]) do fuse" $ do
    -- inner1 writes a[j], inner2 reads a[j-1] — forward dep allows fusion
    let outer =
          SLoop (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopPlain [])
            [ SLoop (LoopSpec ["j"] [IVar "m"] Serial Nothing LoopMap [])
                [SArrayWrite (C.AVar "a") (C.AVar "j") (C.AVar "i")]
            , SLoop (LoopSpec ["k"] [IVar "m"] Serial Nothing LoopMap [])
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

-- ---------------------------------------------------------------------------
-- Temporal skewing tests
-- ---------------------------------------------------------------------------

temporalSkewingSpec :: Spec
temporalSkewingSpec = describe "temporal skewing" $ do
  it "detectTemporalAlias finds the swap at the end of a LoopIterate body" $ do
    let body =
          [ SAssign "arr_next" (C.RArrayAlloc (C.AVar "shp"))
          , SAssign "shp" (C.RArrayShape (C.AVar "arr_cur"))
          , SLoop (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopMap [])
              [ SAssign "x" (C.RArrayLoad (C.AVar "arr_cur") (C.AVar "i"))
              , SArrayWrite (C.AVar "arr_next") (C.AVar "i") (C.AVar "x")
              ]
          , SAssign "arr_cur" (C.RAtom (C.AVar "arr_next"))
          ]
    detectTemporalAlias body `shouldBe` Just ("arr_cur", "arr_next")

  it "detectTemporalAlias returns Nothing when there is no swap" $ do
    let body =
          [ SArrayWrite (C.AVar "out") (C.AVar "i") (C.AVar "x")
          ]
    detectTemporalAlias body `shouldBe` Nothing

  it "augmentWithTemporalDeps bumps iter_t distance from 0 to 1 for WAR deps with negative spatial distance" $ do
    -- A WAR dep as produced by the extractor for a LoopIterate stencil:
    -- iter_t carry is PolyCarryIndependent (0), i distance = -1.
    -- After augmentation iter_t should become +1 so skewing can fire.
    let iterateDep = PolyhedralDependenceRelation
          { pdrKind = PolyDepWAR
          , pdrArray = "arr_next"
          , pdrSourceStmt = [0]
          , pdrTargetStmt = [1]
          , pdrDirection = PolyDepForward
          , pdrIsLoopCarried = True
          , pdrDistance = Just [0, -1]
          , pdrCarryInfo =
              [ PolyhedralCarryInfo "iter_t" PolyCarryIndependent
              , PolyhedralCarryInfo "i" (PolyCarryDistance (-1))
              ]
          , pdrBandCarry =
              [ PolyhedralBandCarry
                  { pbcIters = ["iter_t"]
                  , pbcRole = LoopIterate
                  , pbcStatus = PolyBandIndependent
                  , pbcCarryInfo = [PolyhedralCarryInfo "iter_t" PolyCarryIndependent]
                  }
              , PolyhedralBandCarry
                  { pbcIters = ["i"]
                  , pbcRole = LoopMap
                  , pbcStatus = PolyBandBackward
                  , pbcCarryInfo = [PolyhedralCarryInfo "i" (PolyCarryDistance (-1))]
                  }
              ]
          , pdrClassification = PolyDepClassRegular
          , pdrIsBlocking = True
          , pdrSrcIndex = []
          , pdrTgtIndex = []
          }
        augmented = augmentWithTemporalDeps "iter_t" [iterateDep]
    case augmented of
      [rel] -> do
        -- iter_t distance should now be +1, i distance still -1
        pdrDistance rel `shouldBe` Just [1, -1]
        -- First carryInfo entry should be iter_t: +1
        case pdrCarryInfo rel of
          (ci : _) -> do
            pciIter ci `shouldBe` "iter_t"
            pciStatus ci `shouldBe` PolyCarryDistance 1
          [] -> expectationFailure "expected non-empty carryInfo"
        -- The LoopIterate band carry should now be forward
        case pdrBandCarry rel of
          (bc : _) -> do
            pbcRole bc `shouldBe` LoopIterate
            pbcStatus bc `shouldBe` PolyBandForward
          [] -> expectationFailure "expected non-empty bandCarry"
        -- After augmentation the dep should be non-blocking (outer is forward)
        pdrIsBlocking rel `shouldBe` False
      other -> expectationFailure ("expected one dep, got: " <> show (length other))

  it "augmentWithTemporalDeps leaves forward-only deps unchanged" $ do
    let fwdDep = PolyhedralDependenceRelation
          { pdrKind = PolyDepRAW
          , pdrArray = "arr"
          , pdrSourceStmt = [0]
          , pdrTargetStmt = [1]
          , pdrDirection = PolyDepForward
          , pdrIsLoopCarried = True
          , pdrDistance = Just [1]
          , pdrCarryInfo = [PolyhedralCarryInfo "i" (PolyCarryDistance 1)]
          , pdrBandCarry =
              [ PolyhedralBandCarry
                  { pbcIters = ["i"]
                  , pbcRole = LoopMap
                  , pbcStatus = PolyBandForward
                  , pbcCarryInfo = [PolyhedralCarryInfo "i" (PolyCarryDistance 1)]
                  }
              ]
          , pdrClassification = PolyDepClassRegular
          , pdrIsBlocking = False
          , pdrSrcIndex = []
          , pdrTgtIndex = []
          }
        augmented = augmentWithTemporalDeps "iter_t" [fwdDep]
    augmented `shouldBe` [fwdDep]

  it "suggestBandSkew2 is triggered for the augmented temporal stencil dep" $ do
    -- Simulate the dep produced by augmentWithTemporalDeps for a 1D stencil:
    -- outer band (iter_t: LoopIterate []), inner band (i: LoopMap [])
    -- distance: [+1, -1]  → outerDist(iter_t)=+1, innerDist(i)=-1
    let dims = [ScheduleDim "iter_t" (IVar "T"), ScheduleDim "i" (IVar "n")]
        rel = PolyhedralDependenceRelation
          { pdrKind = PolyDepWAR
          , pdrArray = "arr_next"
          , pdrSourceStmt = [0]
          , pdrTargetStmt = [1]
          , pdrDirection = PolyDepForward
          , pdrIsLoopCarried = True
          , pdrDistance = Just [1, -1]
          , pdrCarryInfo =
              [ PolyhedralCarryInfo "iter_t" (PolyCarryDistance 1)
              , PolyhedralCarryInfo "i" (PolyCarryDistance (-1))
              ]
          , pdrBandCarry =
              [ PolyhedralBandCarry
                  { pbcIters = ["iter_t", "i"]
                  , pbcRole = LoopIterate
                  , pbcStatus = PolyBandForward
                  , pbcCarryInfo =
                      [ PolyhedralCarryInfo "iter_t" (PolyCarryDistance 1)
                      , PolyhedralCarryInfo "i" (PolyCarryDistance (-1))
                      ]
                  }
              ]
          , pdrClassification = PolyDepClassRegular
          , pdrIsBlocking = False
          , pdrSrcIndex = []
          , pdrTgtIndex = []
          }
    case suggestBandSkew2 0 dims [rel] of
      Nothing -> expectationFailure "expected a skew suggestion for temporal stencil dep"
      Just skew -> do
        skewTarget skew `shouldBe` "i"
        skewSource skew `shouldBe` "iter_t"
        skewCoeff skew `shouldSatisfy` (>= 1)

  it "LoopIterate scop with stencil pattern extracts and produces a dep suitable for skewing" $ do
    -- Build a synthetic 1D iterate loop body:
    --   iter_t ∈ [0, T):
    --     arr_next = alloc(shp)
    --     shp = shape(arr_cur)
    --     i ∈ [0, n):
    --       x = load arr_cur[i+1]   ← reads from i+1 (will be substituted → arr_next)
    --       write arr_next[i] x
    --     arr_cur = arr_next         ← temporal swap
    let innerLoop =
          SLoop (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopMap [])
            [ SAssign "ip1" (C.RBinOp C.CAdd (C.AVar "i") (C.AInt 1))
            , SAssign "x" (C.RArrayLoad (C.AVar "arr_cur") (C.AVar "ip1"))
            , SArrayWrite (C.AVar "arr_next") (C.AVar "i") (C.AVar "x")
            ]
        iterateBody =
          [ SAssign "arr_next" (C.RArrayAlloc (C.AVar "shp"))
          , SAssign "shp" (C.RArrayShape (C.AVar "arr_cur"))
          , innerLoop
          , SAssign "arr_cur" (C.RAtom (C.AVar "arr_next"))
          ]
        outerLoop =
          SLoop (LoopSpec ["iter_t"] [IVar "T"] Serial Nothing LoopIterate []) iterateBody
        proc = mkProc "p" ["T", "n", "arr_cur"] [outerLoop]
    case extractProcScops2 proc of
      [scop] -> do
        let deps = collectScopDependenceRelations2 scop
        -- There should be at least one dep involving arr_next (or arr_cur substituted)
        -- that has a negative inner distance augmented with iter_t: +1
        let temporalDeps =
              [ rel
              | rel <- deps
              , case pdrDistance rel of
                  Just (d0 : d1 : _) -> d0 > 0 && d1 < 0
                  _ -> False
              ]
        length temporalDeps `shouldSatisfy` (>= 1)
      other -> expectationFailure ("expected one extracted scop, got: " <> show (length other))

  wavefrontSkewingSpec

wavefrontSkewingSpec :: Spec
wavefrontSkewingSpec = describe "wavefront skewing" $ do
  it "suggestCrossBandSkew2 detects temporal stencil pattern" $ do
    -- Augmented dep from the stencil: iter_t=+1, i=-1
    let rel = PolyhedralDependenceRelation
          { pdrKind = PolyDepWAR
          , pdrArray = "arr"
          , pdrSourceStmt = [0]
          , pdrTargetStmt = [1]
          , pdrDirection = PolyDepForward
          , pdrIsLoopCarried = True
          , pdrDistance = Just [1, -1]
          , pdrCarryInfo =
              [ PolyhedralCarryInfo "iter_t" (PolyCarryDistance 1)
              , PolyhedralCarryInfo "i" (PolyCarryDistance (-1))
              ]
          , pdrBandCarry =
              [ PolyhedralBandCarry
                  { pbcIters = ["iter_t"]
                  , pbcRole = LoopIterate
                  , pbcStatus = PolyBandForward
                  , pbcCarryInfo = [PolyhedralCarryInfo "iter_t" (PolyCarryDistance 1)]
                  }
              , PolyhedralBandCarry
                  { pbcIters = ["i"]
                  , pbcRole = LoopMap
                  , pbcStatus = PolyBandBackward
                  , pbcCarryInfo = [PolyhedralCarryInfo "i" (PolyCarryDistance (-1))]
                  }
              ]
          , pdrClassification = PolyDepClassRegular
          , pdrIsBlocking = False
          , pdrSrcIndex = []
          , pdrTgtIndex = []
          }
    case suggestCrossBandSkew2 0 ["iter_t"] ["i"] [rel] of
      [] -> expectationFailure "expected wavefront skew for (+1, -1) dep"
      skew : _ -> do
        skewTarget skew `shouldBe` "i"
        skewSource skew `shouldBe` "iter_t"
        skewCoeff skew `shouldBe` 1

  it "synthesizeScopSchedule2 applies cross-band skew to LoopIterate+LoopMap" $ do
    -- Build a 1D iterate loop similar to the stencil pattern
    let innerLoop =
          SLoop (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopMap [])
            [ SAssign "ip1" (C.RBinOp C.CAdd (C.AVar "i") (C.AInt 1))
            , SAssign "x" (C.RArrayLoad (C.AVar "arr_cur") (C.AVar "ip1"))
            , SArrayWrite (C.AVar "arr_next") (C.AVar "i") (C.AVar "x")
            ]
        iterateBody =
          [ SAssign "arr_next" (C.RArrayAlloc (C.AVar "shp"))
          , SAssign "shp" (C.RArrayShape (C.AVar "arr_cur"))
          , innerLoop
          , SAssign "arr_cur" (C.RAtom (C.AVar "arr_next"))
          ]
        outerLoop =
          SLoop (LoopSpec ["iter_t"] [IVar "T"] Serial Nothing LoopIterate []) iterateBody
        proc = mkProc "p" ["T", "n", "arr_cur"] [outerLoop]
    case extractProcScops2 proc of
      [scop] -> do
        let scheduled = synthesizeScopSchedule2 scop
            sched = ssSchedule scheduled
        -- Walk the schedule tree to find the inner LoopMap band.
        -- After synthesis the body may be a sequence (prelude + inner band + epilogue)
        -- or directly the inner band if prelude/epilogue were absorbed.
        let findInner :: [ScheduleTree] -> Maybe LoopBand
            findInner candidates = case candidates of
              [ScheduleLoopBand inner]
                | lbRole inner == LoopMap -> Just inner
              [ScheduleSequence xs] ->
                foldl (\acc x -> acc <|> findInner [x]) Nothing xs
              _ -> Nothing
        case sched of
          ScheduleLoopBand outer
            | lbRole outer == LoopIterate ->
                case findInner [lbBody outer] of
                  Just inner -> do
                    length (lbSkew inner) `shouldSatisfy` (>= 1)
                    let firstSkew = head (lbSkew inner)
                    skewTarget firstSkew `shouldBe` "i"
                    skewSource firstSkew `shouldBe` "iter_t"
                    skewCoeff firstSkew `shouldBe` 1
                    length (lbOrigins inner) `shouldSatisfy` (>= 1)
                    case head (lbOrigins inner) of
                      IMul (IConst c) (IVar s) -> do
                        c `shouldBe` 1
                        s `shouldBe` "iter_t"
                      other ->
                        expectationFailure ("unexpected origin: " ++ show other)
                  Nothing -> expectationFailure "could not find inner LoopMap band"
            | otherwise -> expectationFailure "outer band is not LoopIterate"
          _ -> expectationFailure "expected ScheduleLoopBand"
      other -> expectationFailure ("expected one scop, got: " <> show (length other))

  it "reifyScheduledScop2 preserves skew through ScheduleStripMine" $ do
    let tupleStmt = SAssign "ij" (C.RTuple [C.AVar "i", C.AVar "j"])
        i1Stmt = SAssign "i1" (C.RBinOp C.CAdd (C.AVar "i") (C.AInt 1))
        j1Stmt = SAssign "j1" (C.RBinOp C.CAdd (C.AVar "j") (C.AInt 1))
        downStmt = SAssign "ij_down" (C.RTuple [C.AVar "i1", C.AVar "j"])
        rightStmt = SAssign "ij_right" (C.RTuple [C.AVar "i", C.AVar "j1"])
        readStmt = SAssign "x" (C.RArrayLoad (C.AVar "arr") (C.AVar "ij"))
        readDownStmt = SAssign "y" (C.RArrayLoad (C.AVar "arr") (C.AVar "ij_down"))
        readRightStmt = SAssign "z" (C.RArrayLoad (C.AVar "arr") (C.AVar "ij_right"))
        stmt = SArrayWrite (C.AVar "out") (C.AVar "ij") (C.AVar "i")
        loop =
          SLoop (LoopSpec ["i", "j"] [IVar "n", IConst 16] Serial Nothing LoopMap [])
            [tupleStmt, i1Stmt, j1Stmt, downStmt, rightStmt, readStmt, readDownStmt, readRightStmt, stmt]
        proc = mkProc "p" ["t", "n", "arr", "out"] [loop]
    case extractProcScops2 proc of
      [scop] ->
        case ssSchedule (tileScop2 scop) of
          ScheduleStripMine band plans ->
            let scheduled =
                  (tileScop2 scop)
                    { ssSchedule =
                        ScheduleStripMine
                          band
                            { lbOrigins = [IMul (IConst 1) (IVar "t")]
                            , lbSkew = [SkewSpec "i" "t" 1]
                            }
                          plans
                    }
            in
              case reifyScheduledScop2 scheduled of
                Just
                  [ SAssign skewOrigin (C.RBinOp C.CMul (C.AInt 1) (C.AVar "t"))
                  , SLoop _ outerBody
                  ] -> do
                    skewOrigin `shouldBe` "i__skew_origin"
                    case reverse outerBody of
                      (SLoop _ innerBody : _) -> do
                        innerBody `shouldContain`
                          [ SAssign "i__s" (C.RBinOp C.CAdd (C.AVar "i") (C.AVar "i__skew_origin"))
                          , SAssign "i" (C.RBinOp C.CSub (C.AVar "i__s") (C.AVar "i__skew_origin"))
                          ]
                        innerBody `shouldContain`
                          [tupleStmt, i1Stmt, j1Stmt, downStmt, rightStmt, readStmt, readDownStmt, readRightStmt, stmt]
                      _ ->
                        expectationFailure ("expected inner local loop, got: " <> show outerBody)
                other ->
                  expectationFailure ("unexpected reified strip-mined skew tree: " <> show other)
          other ->
            expectationFailure ("expected strip-mined schedule, got: " <> show other)
      other ->
        expectationFailure ("expected one scop, got: " <> show (length other))

wavefrontCollapseSpec :: Spec
wavefrontCollapseSpec = describe "wavefront collapse" $ do
  it "collapses constant-trip iterate/map kernels into a ring-buffer wavefront on the polyhedral path" $ do
    let innerLoop =
          SLoop (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopMap [])
            [ SAssign "ip1" (C.RBinOp C.CAdd (C.AVar "i") (C.AInt 1))
            , SAssign "x" (C.RArrayLoad (C.AVar "arr_cur") (C.AVar "ip1"))
            , SArrayWrite (C.AVar "arr_next") (C.AVar "i") (C.AVar "x")
            ]
        outerLoop =
          SLoop (LoopSpec ["iter_t"] [IConst 2] Serial Nothing LoopIterate [])
            [ SAssign "shp" (C.RArrayShape (C.AVar "arr_cur"))
            , SAssign "arr_next" (C.RArrayAlloc (C.AVar "shp"))
            , innerLoop
            , SAssign "arr_cur" (C.RAtom (C.AVar "arr_next"))
            ]
        proc = mkProc "p" ["n", "arr_cur"] [outerLoop]
    case polyhedralProgram2 (Program [proc]) of
      Program [rewritten] -> do
        loopRolesInStmts (procBody rewritten) `shouldNotContain` [LoopIterate]
        hasParallelLoopInStmts (procBody rewritten) `shouldBe` True
        length [() | SAssign "__hyd_discard" (C.RArrayFree _) <- procBody rewritten] `shouldSatisfy` (>= 1)
      other ->
        expectationFailure ("expected one proc, got: " <> show other)

  it "collapses dynamic-trip iterate/map kernels into a ring-buffer wavefront too" $ do
    let innerLoop =
          SLoop (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopMap [])
            [ SAssign "ip1" (C.RBinOp C.CAdd (C.AVar "i") (C.AInt 1))
            , SAssign "x" (C.RArrayLoad (C.AVar "arr_cur") (C.AVar "ip1"))
            , SArrayWrite (C.AVar "arr_next") (C.AVar "i") (C.AVar "x")
            ]
        outerLoop =
          SLoop (LoopSpec ["iter_t"] [IVar "T"] Serial Nothing LoopIterate [])
            [ SAssign "shp" (C.RArrayShape (C.AVar "arr_cur"))
            , SAssign "arr_next" (C.RArrayAlloc (C.AVar "shp"))
            , innerLoop
            , SAssign "arr_cur" (C.RAtom (C.AVar "arr_next"))
            ]
        proc = mkProc "p" ["T", "n", "arr_cur"] [outerLoop]
    case polyhedralProgram2 (Program [proc]) of
      Program [rewritten] -> do
        loopRolesInStmts (procBody rewritten) `shouldNotContain` [LoopIterate]
        hasParallelLoopInStmts (procBody rewritten) `shouldBe` True
      other ->
        expectationFailure ("expected one proc, got: " <> show other)

loopRolesInStmts :: [Stmt] -> [LoopRole]
loopRolesInStmts = concatMap go
  where
    go stmt = case stmt of
      SLoop spec body ->
        lsRole spec : loopRolesInStmts body
      SIf _ thn els ->
        loopRolesInStmts thn ++ loopRolesInStmts els
      _ ->
        []

hasParallelLoopInStmts :: [Stmt] -> Bool
hasParallelLoopInStmts = any go
  where
    go stmt = case stmt of
      SLoop spec body ->
        case lsExec spec of
          Parallel {} -> True
          _ -> hasParallelLoopInStmts body
      SIf _ thn els ->
        hasParallelLoopInStmts thn || hasParallelLoopInStmts els
      _ ->
        False
