{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.CFGPipelineSpec (spec) where

import Data.Map.Strict qualified as Map
import Language.Hydrangea.CFG
import Language.Hydrangea.CFGCore (Atom(..), CType(..), RHS(..), BinOp(..))
import Language.Hydrangea.CFGPipeline
  ( PipelineOptions(..)
  , defaultPipelineOptions
  , metalPipelineWithTiling
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
