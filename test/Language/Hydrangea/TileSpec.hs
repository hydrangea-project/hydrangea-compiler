{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.TileSpec (spec) where

import Language.Hydrangea.CFG
import Language.Hydrangea.CFGCore (Atom(..), BinOp(..), RHS(..), Redop(..))
import Language.Hydrangea.Tile (tileStmts2)
import Test.Hspec

spec :: Spec
spec = describe "Tile" $ do
  it "keeps pure streaming ND loops untiled when there is no reuse signal" $ do
    let loop =
          SLoop
            (LoopSpec ["i", "j"] [IConst 64, IConst 48] Serial Nothing LoopPlain [])
            [ SAssign "ij" (RTuple [AVar "i", AVar "j"])
            , SAssign "x" (RArrayLoad (AVar "arr") (AVar "ij"))
            , SArrayWrite (AVar "out") (AVar "ij") (AVar "x")
            ]

    tileStmts2 [loop] `shouldBe` [loop]

  it "tiles reuse-heavy ND loops by strip-mining useful dimensions" $ do
    let loop =
          SLoop
            (LoopSpec ["i", "j"] [IConst 64, IConst 48] Serial Nothing LoopPlain [])
            [ SAssign "ij" (RTuple [AVar "i", AVar "j"])
            , SAssign "i1" (RBinOp CAdd (AVar "i") (AInt 1))
            , SAssign "j1" (RBinOp CAdd (AVar "j") (AInt 1))
            , SAssign "ij_down" (RTuple [AVar "i1", AVar "j"])
            , SAssign "ij_right" (RTuple [AVar "i", AVar "j1"])
            , SAssign "x" (RArrayLoad (AVar "arr") (AVar "ij"))
            , SAssign "y" (RArrayLoad (AVar "arr") (AVar "ij_down"))
            , SAssign "z" (RArrayLoad (AVar "arr") (AVar "ij_right"))
            , SAssign "sum" (RBinOp CAdd (AVar "x") (AVar "y"))
            , SAssign "sum2" (RBinOp CAdd (AVar "sum") (AVar "z"))
            , SArrayWrite (AVar "out") (AVar "ij") (AVar "sum2")
            ]

    let tiled = tileStmts2 [loop]
        loopSpecs = collectLoopSpecs tiled

    length loopSpecs `shouldSatisfy` (> 1)

  it "skips strip-mining when a constant band only adds a tiny cleanup tile" $ do
    let loop =
          SLoop
            (LoopSpec ["i", "j"] [IConst 33, IConst 40] Serial Nothing LoopPlain [])
            [SArrayWrite (AVar "out") (AVar "i") (AVar "j")]

    tileStmts2 [loop] `shouldBe` [loop]

  it "tiles nested reduction loops without program-specific accumulator reloads" $ do
    let inner =
          SLoop
            (LoopSpec ["k"] [IConst 48] Serial (Just (ReductionSpec "acc" (IConst 0) RAdd)) LoopReduction [])
            [SAssign "acc" (RBinOp CAdd (AVar "acc") (AInt 1))]
        outer =
          SLoop
            (LoopSpec ["j"] [IConst 64] Serial Nothing LoopPlain [])
            [ SAssign "acc" (RAtom (AInt 0))
            , inner
            , SArrayWrite (AVar "out") (AVar "j") (AVar "acc")
            ]

    let tiled = tileStmts2 [outer]
        loopSpecs = collectLoopSpecs tiled

    map lsRole loopSpecs `shouldBe` [LoopPlain, LoopReductionWrapper, LoopReduction]
    map lsBounds loopSpecs `shouldSatisfy` \bounds ->
      case bounds of
        [[IConst 64], [IConst 2], [IVar _]] -> True
        _ -> False
    lsRed (loopSpecs !! 1) `shouldBe` Just (ReductionSpec "acc" (IConst 0) RAdd)
    lsRed (last loopSpecs) `shouldBe` Just (ReductionSpec "acc" (IConst 0) RAdd)
    hasAccumulatorReload tiled `shouldBe` False

  it "tiles top-level reduction loops into a wrapper plus local reduction" $ do
    let loop =
          SLoop
            (LoopSpec ["k"] [IConst 4096] Serial (Just (ReductionSpec "acc" (IConst 0) RAdd)) LoopReduction [])
            [ SAssign "x" (RArrayLoad (AVar "arr") (AVar "k"))
            , SAssign "acc" (RBinOp CAdd (AVar "acc") (AVar "x"))
            ]

    let tiled = tileStmts2 [loop]
        loopSpecs = collectLoopSpecs tiled

    map lsRole loopSpecs `shouldBe` [LoopReductionWrapper, LoopReduction]
    map lsBounds loopSpecs `shouldSatisfy` \bounds ->
      case bounds of
        [[IConst 128], [IVar _]] -> True
        _ -> False
    map lsRed loopSpecs `shouldBe`
      [ Just (ReductionSpec "acc" (IConst 0) RAdd)
      , Just (ReductionSpec "acc" (IConst 0) RAdd)
      ]

  it "leaves top-level flat loops unchanged when they are not part of a loop nest" $ do
    let loop =
          SLoop
            (LoopSpec ["i"] [IConst 128] Serial Nothing LoopPlain [])
            [SArrayWrite (AVar "out") (AVar "i") (AInt 1)]

    tileStmts2 [loop] `shouldBe` [loop]

  it "leaves small constant map-reduction kernels unchanged" $ do
    let inner =
          SLoop
            (LoopSpec ["k"] [IConst 8] Serial (Just (ReductionSpec "acc" (IConst 0) RAdd)) LoopReduction [])
            [SAssign "acc" (RBinOp CAdd (AVar "acc") (AInt 1))]
        outer =
          SLoop
            (LoopSpec ["j"] [IConst 8] Serial Nothing LoopMapReduction [])
            [ SAssign "base" (RBinOp CMul (AVar "j") (AInt 8))
            , SAssign "acc" (RAtom (AInt 0))
            , inner
            , SArrayWrite (AVar "out") (AVar "j") (AVar "acc")
            ]

    tileStmts2 [outer] `shouldBe` [outer]

  it "does not tile fold loops even with large bounds" $ do
    let loop =
          SLoop
            (LoopSpec ["i"] [IConst 256] Serial Nothing LoopFold [])
            [SAssign "acc" (RBinOp CAdd (AVar "acc") (AInt 1))]

    tileStmts2 [loop] `shouldBe` [loop]

  it "keeps tiling non-fold map-reduction nests (matmul-like shape)" $ do
    let inner =
          SLoop
            (LoopSpec ["k"] [IConst 96] Serial (Just (ReductionSpec "acc" (IConst 0) RAdd)) LoopReduction [])
            [SAssign "acc" (RBinOp CAdd (AVar "acc") (AInt 1))]
        outer =
          SLoop
            (LoopSpec ["j"] [IConst 128] Serial Nothing LoopMapReduction [])
            [ SAssign "acc" (RAtom (AInt 0))
            , inner
            , SArrayWrite (AVar "out") (AVar "j") (AVar "acc")
            ]

    let tiled = tileStmts2 [outer]
        loopSpecs = collectLoopSpecs tiled
    length loopSpecs `shouldSatisfy` (> 2)
    map lsRole loopSpecs `shouldSatisfy` \roles ->
      LoopMapReduction `elem` roles && LoopReduction `elem` roles

collectLoopSpecs :: [Stmt] -> [LoopSpec]
collectLoopSpecs = concatMap go
  where
    go stmt = case stmt of
      SLoop loopSpec body -> loopSpec : collectLoopSpecs body
      SIf _ thn els -> collectLoopSpecs thn ++ collectLoopSpecs els
      _ -> []

hasAccumulatorReload :: [Stmt] -> Bool
hasAccumulatorReload = any go
  where
    go stmt = case stmt of
      SAssign "acc" (RArrayLoad (AVar "out") _) -> True
      SLoop _ body -> hasAccumulatorReload body
      SIf _ thn els -> hasAccumulatorReload thn || hasAccumulatorReload els
      _ -> False
