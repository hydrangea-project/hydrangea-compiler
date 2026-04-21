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
import Language.Hydrangea.Infer (InferOptions(..), defaultInferOptions)
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

  it "reifies the matmul benchmark result as blocked ii/jj/kk loops" $ do
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

loadPreparedMatmulBenchmarkDiagnostics :: IO [ScopDiagnostic]
loadPreparedMatmulBenchmarkDiagnostics = do
  src <- BS.readFile "bench/matmul/mat_mul_bench.hyd"
  case readDecs src of
    Left perr ->
      expectationFailure ("Parse error: " ++ perr) >> pure []
    Right decs -> do
      let inferOpts = defaultInferOptions { inferSolveRefinements = False }
          pipelineOpts =
            defaultPipelineOptions
              { poEnableTiling = True
              , poEnablePolyhedral = True
              , poEnableParallelization = True
              }
      prog <- lowerToCFG2WithTypesWithOptions inferOpts decs
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
      SLoop spec body ->
        any (BS.isPrefixOf prefix) (lsIters spec) || hasLoopIterPrefix prefix body
      SIf _ thn els ->
        hasLoopIterPrefix prefix thn || hasLoopIterPrefix prefix els
      _ ->
        False

hasMapUpdateLoop :: [Stmt] -> Bool
hasMapUpdateLoop = any go
  where
    go stmt = case stmt of
      SLoop spec body ->
        (lsRole spec == LoopMap && loadCount body >= 2 && writeCount body >= 1)
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
