{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.CFGPipelineSpec (spec) where

import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Map.Strict qualified as Map
import Language.Hydrangea.CFG
import Language.Hydrangea.CFGCore (Atom(..), CType(..), RHS(..), BinOp(..))
import Language.Hydrangea.CFGPipeline
  ( PipelineOptions(..)
  , defaultPipelineOptions
  , metalPipelineWithTiling
  , preparePolyhedralProgramWithOptions
  , pipelineWithOptions
  )
import Test.Hspec

spec :: Spec
spec = describe "CFGPipeline" $ do
  it "keeps tiling disabled unless opted in" $ do
    let loop =
          SLoop
            (LoopSpec ["i", "j"] [IConst 64, IConst 48] Serial Nothing LoopPlain)
            [SArrayWrite (AVar "out") (AVar "i") (AVar "j")]
        prog = Program [mkProc "p" [] [loop]]
        optsNoTiling =
          defaultPipelineOptions
            { poEnableTiling = False
            , poEnableParallelization = False
            }
        optsWithTiling =
          optsNoTiling
            { poEnableTiling = True
            }
        Program [noTileProc] = pipelineWithOptions optsNoTiling prog
        Program [tileProc] = pipelineWithOptions optsWithTiling prog
    countLoops (procBody noTileProc) `shouldBe` 1
    countLoops (procBody tileProc) `shouldSatisfy` (> 1)

  it "gates explicit vector lowering behind an opt-in" $ do
    let body =
          [ SAssign "x" (RArrayLoad (AVar "arr") (AVar "i"))
          , SAssign "y" (RBinOp CAddF (AVar "x") (AFloat 1.0))
          , SArrayWrite (AVar "out") (AVar "i") (AVar "y")
          ]
        proc =
          (mkProc "p" []
            [ SAssign "shape" (RTuple [AInt 8])
            , SAssign "arr" (RArrayAlloc (AVar "shape"))
            , SAssign "out" (RArrayAlloc (AVar "shape"))
            , SLoop (LoopSpec ["i"] [IConst 8] Serial Nothing LoopMap) body
            , SReturn (AInt 0)
            ])
            { procTypeEnv =
                Map.fromList
                  [ ("arr", CTArray CTDouble)
                  , ("out", CTArray CTDouble)
                  ]
            }
        prog = Program [proc]
        optsBase =
          defaultPipelineOptions
            { poEnableTiling = False
            , poEnableParallelization = False
            }
        Program [hintOnlyProc] =
          pipelineWithOptions (optsBase { poEnableExplicitVectorization = False }) prog
        Program [explicitProc] =
          pipelineWithOptions (optsBase { poEnableExplicitVectorization = True }) prog
    hasExplicitVectorOps (procBody hintOnlyProc) `shouldBe` False
    hasExplicitVectorOps (procBody explicitProc) `shouldBe` True

  it "applies tiling policy consistently for metal pipeline" $ do
    let loop =
          SLoop
            (LoopSpec ["i", "j"] [IConst 64, IConst 48] Serial Nothing LoopPlain)
            [SArrayWrite (AVar "out") (AVar "i") (AVar "j")]
        prog = Program [mkProc "p" [] [loop]]
        Program [noTileProc] = metalPipelineWithTiling False prog
        Program [tileProc] = metalPipelineWithTiling True prog
    countLoops (procBody noTileProc) `shouldBe` 1
    countLoops (procBody tileProc) `shouldSatisfy` (> 1)

  it "keeps polyhedral preparation on untiled canonical loops" $ do
    let loop =
          SLoop
            (LoopSpec ["i", "j"] [IConst 64, IConst 48] Serial Nothing LoopPlain)
            [SArrayWrite (AVar "out") (AVar "i") (AVar "j")]
        prog = Program [mkProc "p" [] [loop]]
        opts =
          defaultPipelineOptions
            { poEnableTiling = True
            , poEnablePolyhedral = True
            , poEnableParallelization = False
            }
        Program [preparedProc] = preparePolyhedralProgramWithOptions opts prog
    countLoops (procBody preparedProc) `shouldBe` 1

  it "routes untiled polyhedral scheduling through legal band interchange" $ do
    let loop =
          SLoop
            (LoopSpec ["i", "j"] [IVar "n", IVar "m"] Serial Nothing LoopPlain)
            [ SAssign "srcIx" (RTuple [AVar "i", AVar "j"])
            , SAssign "nextI" (RBinOp CAdd (AVar "i") (AInt 1))
            , SAssign "dstIx" (RTuple [AVar "nextI", AVar "j"])
            , SArrayWrite (AVar "arr") (AVar "srcIx") (AInt 0)
            , SAssign "x" (RArrayLoad (AVar "arr") (AVar "dstIx"))
            , SArrayWrite (AVar "out") (AVar "dstIx") (AVar "x")
            ]
        prog = Program [mkProc "p" ["n", "m", "arr", "out"] [loop]]
        opts =
          defaultPipelineOptions
            { poEnableTiling = False
            , poEnablePolyhedral = True
            , poEnableExplicitVectorization = False
            , poEnableParallelization = False
            }
        Program [polyProc] = pipelineWithOptions opts prog
    case procBody polyProc of
      [SLoop loopSpec _] -> lsIters loopSpec `shouldBe` ["j", "i"]
      other -> expectationFailure ("unexpected pipeline result: " <> show other)

  it "routes tiling through polyhedral scheduling when enabled" $ do
    let loop =
          SLoop
            (LoopSpec ["i", "j"] [IConst 64, IConst 48] Serial Nothing LoopPlain)
            [SArrayWrite (AVar "out") (AVar "i") (AVar "j")]
        prog = Program [mkProc "p" [] [loop]]
        optsNoPoly =
          defaultPipelineOptions
            { poEnableTiling = True
            , poEnablePolyhedral = False
            , poEnableExplicitVectorization = False
            , poEnableParallelization = False
            }
        optsPoly =
          optsNoPoly
            { poEnablePolyhedral = True
            }
        Program [legacyTileProc] = pipelineWithOptions optsNoPoly prog
        Program [polyTileProc] = pipelineWithOptions optsPoly prog
    procBody polyTileProc `shouldBe` procBody legacyTileProc

  it "tiles along the synthesized polyhedral loop order" $ do
    let loop =
          SLoop
            (LoopSpec ["i", "j"] [IVar "n", IVar "m"] Serial Nothing LoopPlain)
            [ SAssign "srcIx" (RTuple [AVar "i", AVar "j"])
            , SAssign "prevI" (RBinOp CSub (AVar "i") (AInt 1))
            , SAssign "nextJ" (RBinOp CAdd (AVar "j") (AInt 1))
            , SAssign "dstIx" (RTuple [AVar "prevI", AVar "nextJ"])
            , SArrayWrite (AVar "arr") (AVar "srcIx") (AInt 0)
            , SAssign "x" (RArrayLoad (AVar "arr") (AVar "dstIx"))
            , SArrayWrite (AVar "out") (AVar "dstIx") (AVar "x")
            ]
        prog = Program [mkProc "p" ["n", "m", "arr", "out"] [loop]]
        optsNoPoly =
          defaultPipelineOptions
            { poEnableTiling = True
            , poEnablePolyhedral = False
            , poEnableExplicitVectorization = False
            , poEnableParallelization = False
            }
        optsPoly =
          optsNoPoly
            { poEnablePolyhedral = True
            }
        Program [legacyTileProc] = pipelineWithOptions optsNoPoly prog
        Program [polyTileProc] = pipelineWithOptions optsPoly prog
    procBody polyTileProc `shouldNotBe` procBody legacyTileProc
    case (procBody legacyTileProc, procBody polyTileProc) of
      ([SLoop legacySpec _], [SLoop polySpec _]) -> do
        case (lsIters legacySpec, lsIters polySpec) of
          ([legacyOuter0, legacyOuter1], [polyOuter0, polyOuter1]) -> do
            legacyOuter0 `shouldSatisfy` BS.isPrefixOf "i_tile"
            legacyOuter1 `shouldSatisfy` BS.isPrefixOf "j_tile"
            polyOuter0 `shouldSatisfy` BS.isPrefixOf "j_tile"
            polyOuter1 `shouldSatisfy` BS.isPrefixOf "i_tile"
          other -> expectationFailure ("unexpected tile iter lists: " <> show other)
      other -> expectationFailure ("unexpected tiled pipeline results: " <> show other)

countLoops :: [Stmt] -> Int
countLoops = sum . map go
  where
    go stmt = case stmt of
      SLoop _ body -> 1 + countLoops body
      SIf _ thn els -> countLoops thn + countLoops els
      _ -> 0

hasExplicitVectorOps :: [Stmt] -> Bool
hasExplicitVectorOps = any go
  where
    go stmt = case stmt of
      SAssign _ rhs -> case rhs of
        RVecLoad {} -> True
        RVecStore {} -> True
        RVecBinOp {} -> True
        RVecUnOp {} -> True
        RVecSplat {} -> True
        RVecReduce {} -> True
        _ -> False
      SLoop _ body -> hasExplicitVectorOps body
      SIf _ thn els -> hasExplicitVectorOps thn || hasExplicitVectorOps els
      _ -> False
