{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.ParallelizeSpec (spec) where

import Data.Map.Strict qualified as Map
import Language.Hydrangea.CFG
import Language.Hydrangea.CFGCore (Atom(..), BinOp(..), CType(..), RHS(..), Redop(..))
import Language.Hydrangea.Parallelize (parallelizeProc2, parallelizeStmts2)
import Test.Hspec

spec :: Spec
spec = describe "Parallelize" $ do
  it "preserves lowering-provided reduction metadata" $ do
    let inner = SLoop (LoopSpec ["k"] [IVar "n"] Serial (Just (ReductionSpec "acc" (IConst 1) RMul)) LoopReduction)
                  [SAssign "acc" (RBinOp CMul (AVar "acc") (AInt 2))]
    case parallelizeStmts2 [inner] of
      [SLoop spec' _] -> do
        lsExec spec' `shouldBe` Parallel (ParallelSpec ParallelGeneric Nothing Nothing)
        lsRed spec' `shouldBe` Just (ReductionSpec "acc" (IConst 1) RMul)
      _ -> expectationFailure "Expected a single parallelized loop"

  it "parallelizes simple single-level map loops without nested-loop heuristics" $ do
    let loop = SLoop (LoopSpec ["i"] [IConst 8] Serial Nothing LoopPlain)
                 [SArrayWrite (AVar "out") (AVar "i") (AInt 1)]
    case parallelizeStmts2 [loop] of
      [SLoop spec' _] -> lsExec spec' `shouldBe` Parallel (ParallelSpec ParallelGeneric Nothing Nothing)
      _ -> expectationFailure "Expected a single parallelized loop"

  it "parallelizes simple LoopMap kernels at top level" $ do
    let loop = SLoop (LoopSpec ["i"] [IConst 8] Serial Nothing LoopMap)
                 [SArrayWrite (AVar "out") (AVar "i") (AInt 1)]
    case parallelizeStmts2 [loop] of
      [SLoop spec' _] -> lsExec spec' `shouldBe` Parallel (ParallelSpec ParallelGeneric Nothing Nothing)
      _ -> expectationFailure "Expected a single parallelized map loop"

  it "uses procArrayFacts to parallelize conservative LoopMap bodies" $ do
    -- After the read-read dependence fix, the fallback path no longer treats
    -- two opaque reads from the same source array as blocking (reads never
    -- create races).  Both the fallback path and the fact-guided path now
    -- parallelize this loop; the difference is that the fact-guided path can
    -- handle loops where the fallback would block on a true write hazard.
    let loop =
          SLoop
            (LoopSpec ["i"] [IConst 8] Serial Nothing LoopMap)
            [ SAssign "left" (RArrayLoad (AVar "src") (AVar "leftIx"))
            , SAssign "right" (RArrayLoad (AVar "src") (AVar "rightIx"))
            , SArrayWrite (AVar "out") (AVar "dstIx") (AInt 1)
            ]
        proc =
          (mkProc "p" ["src"] [loop])
            { procArrayFacts =
                Map.fromList
                  [ ("src", ArrayFact False False True)
                  , ("out", ArrayFact True True False)
                  ]
            }
    case parallelizeStmts2 [loop] of
      [SLoop spec' _] -> lsExec spec' `shouldBe` Parallel (ParallelSpec ParallelGeneric Nothing Nothing)
      _ -> expectationFailure "Expected a single parallelized loop"
    case procBody (parallelizeProc2 proc) of
      [SLoop spec' _] -> lsExec spec' `shouldBe` Parallel (ParallelSpec ParallelGeneric Nothing Nothing)
      _ -> expectationFailure "Expected a single fact-parallelized loop"

  it "does not use procArrayFacts to justify read-write updates of the same array" $ do
    let loop =
          SLoop
            (LoopSpec ["i"] [IConst 8] Serial Nothing LoopMap)
            [ SAssign "x" (RArrayLoad (AVar "out") (AVar "readIx"))
            , SArrayWrite (AVar "out") (AVar "writeIx") (AVar "x")
            ]
        proc =
          (mkProc "p" [] [loop])
            { procArrayFacts =
                Map.fromList
                  [("out", ArrayFact True True False)]
            }
    case procBody (parallelizeProc2 proc) of
      [SLoop spec' _] -> lsExec spec' `shouldBe` Serial
      _ -> expectationFailure "Expected a single conservative read-write loop"

  it "keeps fallback parallelization off colliding scatter-style updates" $ do
    let loop =
          SLoop
            (LoopSpec ["i"] [IConst 8] Serial Nothing LoopPlain)
            [ SAssign "idx" (RArrayLoad (AVar "routes") (AVar "i"))
            , SAssign "val" (RArrayLoad (AVar "vals") (AVar "i"))
            , SAssign "old" (RArrayLoad (AVar "out") (AVar "idx"))
            , SAssign "new" (RBinOp CAdd (AVar "val") (AVar "old"))
            , SArrayWrite (AVar "out") (AVar "idx") (AVar "new")
            ]
    case parallelizeStmts2 [loop] of
      [SLoop spec' _] -> lsExec spec' `shouldBe` Serial
      _ -> expectationFailure "Expected scatter-style read/write loop to remain serial"

  it "parallelizes injective scatter kernels when route analysis proves unique writes" $ do
    let loop =
          SLoop
            (LoopSpec ["i"] [IConst 8] Serial Nothing LoopPlain)
            [ SAssign "nd" (RFlatToNd (AVar "i") (AVar "shp"))
            , SAssign "idx" (RProj 0 (AVar "nd"))
            , SAssign "val" (RArrayLoad (AVar "vals") (AVar "i"))
            , SAssign "old" (RArrayLoad (AVar "out") (AVar "idx"))
            , SAssign "new" (RBinOp CAdd (AVar "val") (AVar "old"))
            , SArrayWrite (AVar "out") (AVar "idx") (AVar "new")
            ]
        proc =
          (mkProc "p" [] [loop])
            { procArrayFacts =
                Map.fromList
                  [ ("out", ArrayFact True True False)
                  , ("vals", ArrayFact False False True)
                  ]
            }
    case procBody (parallelizeProc2 proc) of
      [SLoop spec' _] -> lsExec spec' `shouldBe` Parallel (ParallelSpec ParallelScatterDirect Nothing Nothing)
      _ -> expectationFailure "Expected injective scatter loop to parallelize"

  it "keeps non-injective scatter kernels serial even with fresh destinations" $ do
    let loop =
          SLoop
            (LoopSpec ["i"] [IConst 8] Serial Nothing LoopPlain)
            [ SAssign "idx" (RArrayLoad (AVar "routes") (AVar "i"))
            , SAssign "val" (RArrayLoad (AVar "vals") (AVar "i"))
            , SAssign "old" (RArrayLoad (AVar "out") (AVar "idx"))
            , SAssign "new" (RBinOp CAdd (AVar "val") (AVar "old"))
            , SArrayWrite (AVar "out") (AVar "idx") (AVar "new")
            ]
        proc =
          (mkProc "p" [] [loop])
            { procArrayFacts =
                Map.fromList
                  [ ("out", ArrayFact True True False)
                  , ("routes", ArrayFact False False True)
                  , ("vals", ArrayFact False False True)
                  ]
            , procTypeEnv =
                Map.fromList
                  [ ("out", CTArray CTInt64)
                  , ("idx", CTInt64)
                  , ("val", CTInt64)
                  , ("old", CTInt64)
                  , ("new", CTInt64)
                  ]
            }
    case procBody (parallelizeProc2 proc) of
      [SLoop spec' _] -> lsExec spec' `shouldBe` Parallel (ParallelSpec ParallelScatterAtomicAddInt Nothing Nothing)
      _ -> expectationFailure "Expected non-injective integer scatter-add loop to use atomic strategy"

  it "keeps unsupported colliding scatter combines serial" $ do
    let loop =
          SLoop
            (LoopSpec ["i"] [IConst 8] Serial Nothing LoopPlain)
            [ SAssign "idx" (RArrayLoad (AVar "routes") (AVar "i"))
            , SAssign "val" (RArrayLoad (AVar "vals") (AVar "i"))
            , SAssign "old" (RArrayLoad (AVar "out") (AVar "idx"))
            , SAssign "new" (RBinOp CMul (AVar "val") (AVar "old"))
            , SArrayWrite (AVar "out") (AVar "idx") (AVar "new")
            ]
        proc =
          (mkProc "p" [] [loop])
            { procArrayFacts =
                Map.fromList
                  [ ("out", ArrayFact True True False)
                  , ("routes", ArrayFact False False True)
                  , ("vals", ArrayFact False False True)
                  ]
            , procTypeEnv =
                Map.fromList
                  [ ("out", CTArray CTInt64)
                  , ("idx", CTInt64)
                  , ("val", CTInt64)
                  , ("old", CTInt64)
                  , ("new", CTInt64)
                  ]
            }
    case procBody (parallelizeProc2 proc) of
      [SLoop spec' _] -> lsExec spec' `shouldBe` Serial
      _ -> expectationFailure "Expected unsupported colliding scatter loop to stay serial"

  it "uses atomic scatter for guarded colliding integer add kernels" $ do
    let loop =
          SLoop
            (LoopSpec ["i"] [IConst 8] Serial Nothing LoopPlain)
            [ SAssign "guard" (RArrayLoad (AVar "guards") (AVar "i"))
            , SIf (AVar "guard")
                [ SAssign "idx" (RArrayLoad (AVar "routes") (AVar "i"))
                , SAssign "val" (RArrayLoad (AVar "vals") (AVar "i"))
                , SAssign "old" (RArrayLoad (AVar "out") (AVar "idx"))
                , SAssign "new" (RBinOp CAdd (AVar "val") (AVar "old"))
                , SArrayWrite (AVar "out") (AVar "idx") (AVar "new")
                ]
                []
            ]
        proc =
          (mkProc "p" [] [loop])
            { procArrayFacts =
                Map.fromList
                  [ ("out", ArrayFact True True False)
                  , ("routes", ArrayFact False False True)
                  , ("vals", ArrayFact False False True)
                  , ("guards", ArrayFact False False True)
                  ]
            , procTypeEnv =
                Map.fromList
                  [ ("out", CTArray CTInt64)
                  , ("idx", CTInt64)
                  , ("val", CTInt64)
                  , ("old", CTInt64)
                  , ("new", CTInt64)
                  ]
            }
    case procBody (parallelizeProc2 proc) of
      [SLoop spec' _] -> lsExec spec' `shouldBe` Parallel (ParallelSpec ParallelScatterAtomicAddInt Nothing Nothing)
      _ -> expectationFailure "Expected guarded integer scatter-add loop to use atomic strategy"

  it "uses atomic scatter for colliding floating-point add kernels" $ do
    let loop =
          SLoop
            (LoopSpec ["i"] [IConst 8] Serial Nothing LoopPlain)
            [ SAssign "idx" (RArrayLoad (AVar "routes") (AVar "i"))
            , SAssign "val" (RArrayLoad (AVar "vals") (AVar "i"))
            , SAssign "old" (RArrayLoad (AVar "out") (AVar "idx"))
            , SAssign "new" (RBinOp CAddF (AVar "val") (AVar "old"))
            , SArrayWrite (AVar "out") (AVar "idx") (AVar "new")
            ]
        proc =
          (mkProc "p" [] [loop])
            { procArrayFacts =
                Map.fromList
                  [ ("out", ArrayFact True True False)
                  , ("routes", ArrayFact False False True)
                  , ("vals", ArrayFact False False True)
                  ]
            , procTypeEnv =
                Map.fromList
                  [ ("out", CTArray CTDouble)
                  , ("idx", CTInt64)
                  , ("val", CTDouble)
                  , ("old", CTDouble)
                  , ("new", CTDouble)
                  ]
            }
    case procBody (parallelizeProc2 proc) of
      [SLoop spec' _] -> lsExec spec' `shouldBe` Parallel (ParallelSpec ParallelScatterAtomicAddFloat Nothing Nothing)
      _ -> expectationFailure "Expected floating-point scatter-add loop to use atomic strategy"

  it "uses privatized scatter for dense colliding integer add kernels with enough work" $ do
    let loop =
          SLoop
            (LoopSpec ["i"] [IConst 64] Serial Nothing LoopPlain)
            [ SAssign "idx" (RArrayLoad (AVar "routes") (AVar "i"))
            , SAssign "val" (RArrayLoad (AVar "vals") (AVar "i"))
            , SAssign "old" (RArrayLoad (AVar "out") (AVar "idx"))
            , SAssign "new" (RBinOp CAdd (AVar "val") (AVar "old"))
            , SArrayWrite (AVar "out") (AVar "idx") (AVar "new")
            ]
        proc =
          (mkProc "p" [] [SAssign "shp" (RTuple [AInt 8]), SAssign "out" (RArrayAlloc (AVar "shp")), loop])
            { procArrayFacts =
                Map.fromList
                  [ ("out", ArrayFact True True False)
                  , ("routes", ArrayFact False False True)
                  , ("vals", ArrayFact False False True)
                  ]
            , procTypeEnv =
                Map.fromList
                  [ ("out", CTArray CTInt64)
                  , ("idx", CTInt64)
                  , ("val", CTInt64)
                  , ("old", CTInt64)
                  , ("new", CTInt64)
                  ]
            }
    case procBody (parallelizeProc2 proc) of
      [_, _, SLoop spec' _] -> lsExec spec' `shouldBe` Parallel (ParallelSpec ParallelScatterPrivatizedIntAdd Nothing Nothing)
      _ -> expectationFailure "Expected dense colliding scatter loop to use privatized strategy"

  it "keeps inner reduction loops serial under a map-reduction outer loop" $ do
    let inner = SLoop (LoopSpec ["k"] [IVar "n"] Serial (Just (ReductionSpec "acc" (IConst 0) RAdd)) LoopReduction)
                  [SAssign "acc" (RBinOp CAdd (AVar "acc") (AInt 1))]
        outer = SLoop (LoopSpec ["j"] [IConst 8] Serial Nothing LoopMapReduction)
                  [SAssign "acc" (RAtom (AInt 0)), inner]
    case parallelizeStmts2 [outer] of
      [SLoop outerSpec [_, SLoop innerSpec _]] -> do
        lsExec outerSpec `shouldBe` Parallel (ParallelSpec ParallelGeneric Nothing Nothing)
        lsExec innerSpec `shouldBe` Serial
      _ -> expectationFailure "Expected nested map-reduction structure"

  it "does not parallelize scalar reduction wrapper loops" $ do
    let inner = SLoop (LoopSpec ["k"] [IVar "n"] Serial (Just (ReductionSpec "acc" (IConst 0) RAdd)) LoopReduction)
                  [SAssign "acc" (RBinOp CAdd (AVar "acc") (AInt 1))]
        wrapper = SLoop (LoopSpec ["j"] [IVar "one"] Serial Nothing LoopReductionWrapper)
                    [SAssign "acc" (RAtom (AInt 0)), inner]
    case parallelizeStmts2 [wrapper] of
      [SLoop wrapperSpec [_, SLoop innerSpec _]] -> do
        lsExec wrapperSpec `shouldBe` Serial
        lsExec innerSpec `shouldBe` Serial
      _ -> expectationFailure "Expected scalar reduction wrapper structure"

  it "keeps nested reduction loops serial under an outer plain parallel loop" $ do
    let inner = SLoop (LoopSpec ["k"] [IVar "n"] Serial (Just (ReductionSpec "acc" (IConst 0) RAdd)) LoopReduction)
                  [SAssign "acc" (RBinOp CAdd (AVar "acc") (AInt 1))]
        outer = SLoop (LoopSpec ["i"] [IConst 8] Serial Nothing LoopPlain)
                  [SAssign "acc" (RAtom (AInt 0)), inner]
    case parallelizeStmts2 [outer] of
      [SLoop outerSpec [_, SLoop innerSpec _]] -> do
        lsExec outerSpec `shouldBe` Parallel (ParallelSpec ParallelGeneric Nothing Nothing)
        lsExec innerSpec `shouldBe` Serial
      _ -> expectationFailure "Expected outer loop with nested reduction"
