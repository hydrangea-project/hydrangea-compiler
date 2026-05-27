{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.CFGSpec (spec) where

import Control.Monad.Writer.Strict (execWriter, tell)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Map.Strict qualified as Map
import Language.Hydrangea.CFG
import Language.Hydrangea.CFGCore (Atom(..), RHS(..))
import Test.Hspec

suffix :: Int -> BS.ByteString
suffix depth = "_d" <> BS.pack (show depth)

spec :: Spec
spec = describe "CFG" $ do
  describe "simplifyIndexExpr" $ do
    it "flattens sums and keeps the constant term last" $ do
      simplifyIndexExpr (IAdd (IConst 1) (IAdd (IVar "i") (IConst 2)))
        `shouldBe` IAdd (IVar "i") (IConst 3)

    it "normalizes subtraction into addition with a negated constant" $ do
      simplifyIndexExpr (ISub (IVar "n") (IConst 2))
        `shouldBe` IAdd (IVar "n") (IConst (-2))

    it "sorts multiplicative terms before the trailing constant factor" $ do
      simplifyIndexExpr (IMul (IConst 4) (IVar "i"))
        `shouldBe` IMul (IVar "i") (IConst 4)

  describe "rewriteStmtsWith" $ do
    it "descends through loops, parallel regions, and conditionals" $ do
      let loopSpec = LoopSpec ["i"] [IConst 4] Serial Nothing LoopPlain []
          stmts =
            [ SAssign "root" (RAtom (AInt 0))
            , SLoop loopSpec
                [ SAssign "loop" (RAtom (AInt 1))
                , SParallelRegion
                    [ SIf (ABool True)
                        [SAssign "then_branch" (RAtom (AInt 2))]
                        [SAssign "else_branch" (RAtom (AInt 3))]
                    ]
                ]
            ]
          rewrite depth stmt = case stmt of
            SAssign v rhs -> [SAssign (v <> suffix depth) rhs]
            other -> [other]
          rewritten = rewriteStmtsWith 0 (+ 1) rewrite stmts
      rewritten `shouldBe`
        [ SAssign "root_d0" (RAtom (AInt 0))
        , SLoop loopSpec
            [ SAssign "loop_d1" (RAtom (AInt 1))
            , SParallelRegion
                [ SIf (ABool True)
                    [SAssign "then_branch_d1" (RAtom (AInt 2))]
                    [SAssign "else_branch_d1" (RAtom (AInt 3))]
                ]
            ]
        ]

    it "constructs procedures with empty type and fact tables" $ do
      let proc = mkProc "p" ["x"] [SReturn (AVar "x")]
      procTypeEnv proc `shouldBe` Map.empty
      procArrayFacts proc `shouldBe` Map.empty
      procVectorAccessFacts proc `shouldBe` Map.empty

  describe "rewriteStmtsWithM" $ do
    it "sequences rewrites left-to-right" $ do
      let stmts =
            [ SAssign "a" (RAtom (AInt 1))
            , SAssign "b" (RAtom (AInt 2))
            ]
          visitOrder =
            execWriter $
              rewriteStmtsWithM () id (\_ stmt -> tell [stmt] >> pure [stmt]) stmts
      visitOrder `shouldBe` stmts
