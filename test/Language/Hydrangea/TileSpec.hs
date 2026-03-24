{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.TileSpec (spec) where

import Language.Hydrangea.CFG
import Language.Hydrangea.CFGCore (Atom(..), BinOp(..), RHS(..), Redop(..))
import Language.Hydrangea.Tile (tileStmts2)
import Test.Hspec

spec :: Spec
spec = describe "Tile" $ do
  it "tiles generic ND loops by strip-mining useful dimensions" $ do
    let loop =
          SLoop
            (LoopSpec ["i", "j"] [IConst 64, IConst 48] Serial Nothing LoopPlain)
            [SArrayWrite (AVar "out") (AVar "i") (AVar "j")]

    let tiled = tileStmts2 [loop]
        loopSpecs = collectLoopSpecs tiled

    map lsRole loopSpecs `shouldBe` [LoopPlain, LoopPlain]
    map lsBounds loopSpecs `shouldSatisfy` \bounds ->
      case bounds of
        [[IConst 2, IConst 2], [IVar _, IVar _]] -> True
        _ -> False
    map (length . lsIters) loopSpecs `shouldBe` [2, 2]

  it "tiles nested reduction loops without program-specific accumulator reloads" $ do
    let inner =
          SLoop
            (LoopSpec ["k"] [IConst 48] Serial (Just (ReductionSpec "acc" (IConst 0) RAdd)) LoopReduction)
            [SAssign "acc" (RBinOp CAdd (AVar "acc") (AInt 1))]
        outer =
          SLoop
            (LoopSpec ["j"] [IConst 64] Serial Nothing LoopPlain)
            [ SAssign "acc" (RAtom (AInt 0))
            , inner
            , SArrayWrite (AVar "out") (AVar "j") (AVar "acc")
            ]

    let tiled = tileStmts2 [outer]
        loopSpecs = collectLoopSpecs tiled

    map lsRole loopSpecs `shouldBe` [LoopPlain, LoopPlain, LoopMap, LoopReduction]
    map lsBounds loopSpecs `shouldSatisfy` \bounds ->
      case bounds of
        [[IConst 2], [IVar _], [IConst 2], [IVar _]] -> True
        _ -> False
    lsRed (last loopSpecs) `shouldBe` Just (ReductionSpec "acc" (IConst 0) RAdd)
    hasAccumulatorReload tiled `shouldBe` False

  it "leaves top-level flat loops unchanged when they are not part of a loop nest" $ do
    let loop =
          SLoop
            (LoopSpec ["i"] [IConst 128] Serial Nothing LoopPlain)
            [SArrayWrite (AVar "out") (AVar "i") (AInt 1)]

    tileStmts2 [loop] `shouldBe` [loop]

  it "leaves small constant map-reduction kernels unchanged" $ do
    let inner =
          SLoop
            (LoopSpec ["k"] [IConst 8] Serial (Just (ReductionSpec "acc" (IConst 0) RAdd)) LoopReduction)
            [SAssign "acc" (RBinOp CAdd (AVar "acc") (AInt 1))]
        outer =
          SLoop
            (LoopSpec ["j"] [IConst 8] Serial Nothing LoopMapReduction)
            [ SAssign "base" (RBinOp CMul (AVar "j") (AInt 8))
            , SAssign "acc" (RAtom (AInt 0))
            , inner
            , SArrayWrite (AVar "out") (AVar "j") (AVar "acc")
            ]

    tileStmts2 [outer] `shouldBe` [outer]

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
