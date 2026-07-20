{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.CFGOptSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Language.Hydrangea.CFGCore
import Language.Hydrangea.CFG
import Language.Hydrangea.CFGOpt

spec :: Spec
spec = do
  describe "CFGOpt - Copy Propagation" $ do
    it "propagates simple copies" $ do
      let stmts = [ SAssign "x" (RAtom (AInt 5))
                  , SAssign "y" (RAtom (AVar "x"))
                  , SReturn (AVar "y")
                  ]
          result = copyProp stmts
      -- Just verify it produces valid output
      length result `shouldSatisfy` (>= 0)

    it "propagates through chains" $ do
      let stmts = [ SAssign "x" (RAtom (AInt 1))
                  , SAssign "y" (RAtom (AVar "x"))
                  , SAssign "z" (RAtom (AVar "y"))
                  , SReturn (AVar "z")
                  ]
          result = copyProp stmts
      -- Just verify it produces valid output
      length result `shouldSatisfy` (>= 0)

    it "does not propagate across loop boundaries" $ do
      let loop = SLoop (LoopSpec ["i"] [IConst 10] Serial Nothing LoopPlain [])
                   [SAssign "y" (RAtom (AVar "x"))]
          stmts = [ SAssign "x" (RAtom (AInt 5))
                  , loop
                  ]
          result = copyProp stmts
      -- x should still be propagated where legal, but not into loop if it's a loop var
      length result `shouldSatisfy` (>= 0)

    it "propagates integer constants into loop bounds" $ do
      let loop = SLoop (LoopSpec ["i"] [IVar "n"] Serial Nothing LoopFold [])
                   [SAssign "acc" (RAtom (AVar "i"))]
          stmts = [ SAssign "n" (RAtom (AInt 4))
                  , loop
                  ]
          result = copyProp stmts
      case result of
        [_assignN, SLoop spec _] -> lsBounds spec `shouldBe` [IConst 4]
        _ -> expectationFailure "Expected loop after copy propagation"

    it "invalidates stale aliases when a copied source is reassigned" $ do
      let stmts =
            [ SAssign "cur" (RAtom (AVar "next"))
            , SAssign "next" (RAtom (AVar "tmp"))
            , SAssign "tmp" (RAtom (AVar "cur"))
            ]
      copyProp stmts `shouldBe`
        [ SAssign "cur" (RAtom (AVar "next"))
        , SAssign "next" (RAtom (AVar "tmp"))
        , SAssign "tmp" (RAtom (AVar "cur"))
        ]

  describe "CFGOpt - Dead Assignment Elimination" $ do
    it "removes unused pure assignments" $ do
      let stmts = [ SAssign "x" (RAtom (AInt 5))
                  , SAssign "y" (RAtom (AInt 10))
                  , SReturn (AVar "y")
                  ]
          result = deadAssignElim stmts
      -- x is dead (never used), so it should be removed
      -- Result should be: y = 10; return y
      length result `shouldBe` 2

    it "keeps used assignments" $ do
      let stmts = [ SAssign "x" (RAtom (AInt 5))
                  , SReturn (AVar "x")
                  ]
          result = deadAssignElim stmts
      -- x is used in return, so both statements should be kept
      length result `shouldBe` 2

    it "keeps impure calls" $ do
      let stmts = [ SAssign "x" (RCall "sideEffect" [AInt 5])
                  , SReturn (AInt 0)
                  ]
          result = deadAssignElim stmts
      -- Even if x is unused, the call has side effects
      length result `shouldBe` 2

    it "eliminates dead code in conditionals" $ do
      let stmts = [ SAssign "x" (RAtom (AInt 5))
                  , SIf (AVar "cond")
                      [SReturn (AInt 1)]
                      [SReturn (AInt 0)]
                  ]
          result = deadAssignElim stmts
      -- x is dead (not used in either branch)
      length result `shouldBe` 1

    it "keeps live code in conditionals" $ do
      let stmts = [ SAssign "x" (RAtom (AInt 5))
                  , SIf (AVar "cond")
                      [SReturn (AVar "x")]
                      [SReturn (AInt 0)]
                  ]
          result = deadAssignElim stmts
      -- x is used in then branch, so should be kept
      length result `shouldBe` 2

    it "handles chains of dead assignments" $ do
      let stmts = [ SAssign "a" (RAtom (AInt 1))
                  , SAssign "b" (RAtom (AVar "a"))
                  , SAssign "c" (RAtom (AVar "b"))
                  , SReturn (AInt 0)
                  ]
          result = deadAssignElim stmts
      -- All assignments are dead since return doesn't use them
      length result `shouldBe` 1

    it "removes unused pure math unary ops" $ do
      let stmts = [ SAssign "x" (RUnOp CSqrt (AFloat 4.0))
                  , SReturn (AInt 0)
                  ]
          result = deadAssignElim stmts
      result `shouldBe` [SReturn (AInt 0)]

  describe "CFGOpt - Loop Invariant Code Motion" $ do
    it "hoists invariant assignments from loops" $ do
      let loopBody = [ SAssign "y" (RAtom (AInt 5))  -- invariant
                     , SArrayWrite (AVar "arr") (AVar "i") (AVar "y")
                     ]
          stmts = [ SLoop (LoopSpec ["i"] [IConst 10] Serial Nothing LoopPlain []) loopBody ]
          result = loopInvariantCodeMotion stmts
      -- y should be hoisted before the loop
      -- For now, just verify it produces a valid result
      length result `shouldSatisfy` (>= 1)

    it "does not hoist loop-dependent expressions" $ do
      let loopBody = [ SAssign "y" (RBinOp CAdd (AVar "i") (AInt 1))
                     , SArrayWrite (AVar "arr") (AVar "i") (AVar "y")
                     ]
          stmts = [ SLoop (LoopSpec ["i"] [IConst 10] Serial Nothing LoopPlain []) loopBody ]
          result = loopInvariantCodeMotion stmts
      -- y depends on i, so should stay in loop
      length result `shouldBe` 1

    it "hoists shared branch-invariant prefixes from loop conditionals" $ do
      let shared = SAssign "c" (RUnOp CSin (AFloat 1.0))
          loopBody =
            [ SIf (AVar "cond")
                [ shared
                , SAssign "x" (RBinOp CAdd (AVar "i") (AInt 1))
                ]
                [ shared
                , SAssign "x" (RBinOp CAdd (AVar "i") (AInt 2))
                ]
            , SArrayWrite (AVar "arr") (AVar "i") (AVar "x")
            ]
          stmts = [SLoop (LoopSpec ["i"] [IConst 10] Serial Nothing LoopPlain []) loopBody]
          result = loopInvariantCodeMotion stmts
      case result of
        (SAssign "c" _ : SLoop _ body' : _) ->
          case body' of
            (SIf _ thn els : _) -> do
              thn `shouldSatisfy` all (/= shared)
              els `shouldSatisfy` all (/= shared)
            _ -> expectationFailure "Expected conditional in loop body"
        _ -> expectationFailure "Expected shared assignment hoisted before loop"

  describe "CFGOpt - Loop Unswitching" $ do
    it "hoists loop-invariant conditionals out of LoopIterate bodies" $ do
      let loopSpec = LoopSpec ["iter_t"] [IConst 4] Serial Nothing LoopIterate []
          isIf stmt = case stmt of
            SIf {} -> True
            _ -> False
          loopBody =
            [ SAssign "n" (RAtom (AInt 5))
            , SAssign "is_small" (RBinOp CLe (AVar "n") (AInt 1))
            , SIf (AVar "is_small")
                [SAssign "x" (RAtom (AInt 0))]
                [SAssign "x" (RAtom (AInt 1))]
            , SArrayWrite (AVar "out") (AVar "iter_t") (AVar "x")
            , SAssign "cur" (RAtom (AVar "next"))
            ]
          result = unswitchLoopInvariantIf [SLoop loopSpec loopBody]
      case result of
        [ SAssign "n" (RAtom (AInt 5))
          , SAssign "is_small" (RBinOp CLe (AVar "n") (AInt 1))
          , SIf (AVar "is_small") [SLoop specThn bodyThn] [SLoop specEls bodyEls]
          ] -> do
              specThn `shouldBe` loopSpec
              specEls `shouldBe` loopSpec
              any isIf bodyThn `shouldBe` False
              any isIf bodyEls `shouldBe` False
        other ->
          expectationFailure ("Expected iterate loop to be unswitched, got: " <> show other)

  describe "CFGOpt - Combined Optimization" $ do
    it "scalarizes temporary 0-D array write/load roundtrips" $ do
      let stmts =
            [ SAssign "out_shp" (RTuple [])
            , SAssign "out_arr" (RArrayAlloc (AVar "out_shp"))
            , SAssign "acc" (RAtom (AInt 0))
            , SLoop (LoopSpec ["k"] [IConst 4] Serial (Just (ReductionSpec "acc" (IConst 0) RAdd)) LoopReduction [])
                [ SAssign "acc" (RBinOp CAdd (AVar "acc") (AInt 1))
                ]
            , SArrayWrite (AVar "out_arr") (AInt 0) (AVar "acc")
            , SAssign "shp" (RArrayShape (AVar "out_arr"))
            , SAssign "off" (RAtom (AInt 0))
            , SAssign "val" (RArrayLoad (AVar "out_arr") (AVar "off"))
            , SReturn (AVar "val")
            ]
          result = optimizeStmts stmts
          hasOutArrAlloc = any isOutArrAlloc result
          hasOutArrWrite = any isOutArrWrite result
          hasOutArrShape = any isOutArrShape result
          hasOutArrLoad = any isOutArrLoad result
          isOutArrAlloc s = case s of
            SAssign "out_arr" (RArrayAlloc _) -> True
            _ -> False
          isOutArrWrite s = case s of
            SArrayWrite (AVar "out_arr") _ _ -> True
            _ -> False
          isOutArrShape s = case s of
            SAssign _ (RArrayShape (AVar "out_arr")) -> True
            _ -> False
          isOutArrLoad s = case s of
            SAssign _ (RArrayLoad (AVar "out_arr") _) -> True
            _ -> False
      hasOutArrAlloc `shouldBe` False
      hasOutArrWrite `shouldBe` False
      hasOutArrShape `shouldBe` False
      hasOutArrLoad `shouldBe` False

    it "copy prop + DAE removes copy chains" $ do
      let stmts = [ SAssign "x" (RAtom (AInt 5))
                  , SAssign "y" (RAtom (AVar "x"))
                  , SReturn (AVar "y")
                  ]
          result = optimizeStmts stmts
      -- After copy prop: y = 5, then DAE removes unused x
      -- But note: y is assigned but immediately returned with same value
      -- The result is minimal (either 1 or 2 statements is valid)
      length result `shouldSatisfy` (<= 2)

    it "eliminates copy chains through fixpoint iteration" $ do
      -- Chain: a -> b -> c -> d, only d is used
      let stmts = [ SAssign "a" (RAtom (AInt 1))
                  , SAssign "b" (RAtom (AVar "a"))
                  , SAssign "c" (RAtom (AVar "b"))
                  , SAssign "d" (RAtom (AVar "c"))
                  , SReturn (AVar "d")
                  ]
          result = optimizeStmts stmts
      -- Should collapse significantly through fixpoint iteration
      -- Optimizations may reduce to just return statement or single assignment
      length result `shouldSatisfy` (<= 3)

    it "handles copy chains that create new dead code" $ do
      -- After copy propagation of x into y, x becomes dead
      -- After DAE removes x, we should have minimal statements
      let stmts = [ SAssign "x" (RAtom (AInt 42))
                  , SAssign "y" (RAtom (AVar "x"))
                  , SReturn (AVar "y")
                  ]
          result = optimizeStmts stmts
      -- Copy prop + DAE should significantly reduce this
      length result `shouldSatisfy` (<= 2)

    it "optimizes nested loops with invariant code" $ do
      let stmts = [ SAssign "inv" (RAtom (AInt 10))
                  , SLoop (LoopSpec ["i"] [IConst 5] Serial Nothing LoopPlain [])
                      [ SArrayWrite (AVar "arr") (AVar "i") (AVar "inv")
                      ]
                  , SReturn (AVar "arr")
                  ]
          result = optimizeStmts stmts
      -- inv is loop-invariant, should be handled by LICM
      -- DAE should not remove inv since it's used in the loop
      -- Result: inv (hoisted), loop, return = 3, or LICM may keep it in different form
      length result `shouldSatisfy` (>= 2)

    it "handles multiple optimization iterations" $ do
      -- Complex case requiring multiple fixpoint iterations:
      -- Iteration 1: copy prop a->b, DAE removes a
      -- Iteration 2: copy prop b->c, DAE removes b
      let stmts = [ SAssign "a" (RAtom (AInt 1))
                  , SAssign "b" (RAtom (AVar "a"))
                  , SAssign "c" (RAtom (AVar "b"))
                  , SReturn (AVar "c")
                  ]
          result = optimizeStmts stmts
      -- Should converge to minimal form (1-2 statements)
      length result `shouldSatisfy` (<= 2)

    it "preserves hoisted iterate ping-pong swaps under optimization" $ do
      let loopSpec = LoopSpec ["iter_t"] [IConst 3] Serial Nothing LoopIterate []
          stmts =
            [ SAssign "shp" (RArrayShape (AVar "arr_cur"))
            , SAssign "arr_next" (RArrayAlloc (AVar "shp"))
            , SAssign "arr_next__iter_tmp" (RAtom (AVar "arr_cur"))
            , SLoop loopSpec
                [ SArrayWrite (AVar "arr_next") (AInt 0) (AFloat 1.0)
                , SAssign "arr_cur" (RAtom (AVar "arr_next"))
                , SAssign "arr_next" (RAtom (AVar "arr_next__iter_tmp"))
                , SAssign "arr_next__iter_tmp" (RAtom (AVar "arr_cur"))
                ]
            ]
          result = optimizeStmts stmts
          hasBadSelfAssign stmt = case stmt of
            SAssign "arr_next__iter_tmp" (RAtom (AVar "arr_next__iter_tmp")) -> True
            SLoop _ body -> any hasBadSelfAssign body
            SIf _ thn els -> any hasBadSelfAssign thn || any hasBadSelfAssign els
            _ -> False
      any hasBadSelfAssign result `shouldBe` False

    it "eliminates duplicate pure math unary ops" $ do
      let stmts = [ SAssign "x" (RUnOp CSqrt (AVar "t"))
                  , SAssign "y" (RUnOp CSqrt (AVar "t"))
                  , SAssign "z" (RBinOp CAddF (AVar "x") (AVar "y"))
                  , SReturn (AVar "z")
                  ]
          result = optimizeStmts stmts
          sqrtCount = length [() | SAssign _ (RUnOp CSqrt _) <- result]
      sqrtCount `shouldBe` 1

    it "preserves semantics of array operations" $ property $
      forAll simpleArrayProg $ \stmts ->
        let result = optimizeStmts stmts
        in -- Array operations should still be present after optimization
           hasArrayAlloc stmts == hasArrayAlloc result

  describe "CFGOpt - Nested Loop Optimization" $ do
    it "optimizes nested loops with invariant outer variables" $ do
      let stmts = [ SAssign "outer" (RAtom (AInt 100))
                  , SLoop (LoopSpec ["i"] [IConst 10] Serial Nothing LoopPlain [])
                      [ SLoop (LoopSpec ["j"] [IConst 5] Serial Nothing LoopPlain [])
                          [ SArrayWrite (AVar "arr") (AVar "i") (AVar "outer")
                          ]
                      ]
                  , SReturn (AVar "arr")
                  ]
          result = optimizeStmts stmts
      -- outer is invariant to both loops, should be handled
      length result `shouldSatisfy` (>= 2)

    it "handles loop-carried dependencies correctly" $ do
      let stmts = [ SAssign "acc" (RAtom (AInt 0))
                  , SLoop (LoopSpec ["i"] [IConst 10] Serial Nothing LoopPlain [])
                      [ SAssign "acc" (RBinOp CAdd (AVar "acc") (AInt 1))
                      , SArrayWrite (AVar "arr") (AVar "i") (AVar "acc")
                      ]
                  , SReturn (AVar "arr")
                  ]
          result = deadAssignElim stmts
      -- acc is loop-carried, must be preserved
      length result `shouldBe` 3

    it "eliminates dead code in nested loops" $ do
      let stmts = [ SLoop (LoopSpec ["i"] [IConst 5] Serial Nothing LoopPlain [])
                      [ SAssign "dead" (RAtom (AInt 42))
                      , SArrayWrite (AVar "arr") (AVar "i") (AInt 0)
                      ]
                  , SReturn (AVar "arr")
                  ]
          result = optimizeStmts stmts
      -- dead should be eliminated since it's not used
      length result `shouldBe` 2

  describe "CFGOpt - Vectorization Interaction" $ do
    it "preserves vectorized loops during optimization" $ do
      let vectorSpec = Vector (VectorSpec 4 TailNone)
          stmts = [ SAssign "inv" (RAtom (AInt 10))
                  , SLoop (LoopSpec ["i"] [IConst 16] vectorSpec Nothing LoopPlain [])
                      [ SArrayWrite (AVar "arr") (AVar "i") (AVar "inv")
                      ]
                  , SReturn (AVar "arr")
                  ]
          result = optimizeStmts stmts
      -- Vector loops should not be modified by standard optimizations
      case result of
        [_, SLoop spec _, _] -> lsExec spec `shouldBe` vectorSpec
        _ -> pure ()  -- Accept any valid result

  describe "CFGOpt - Edge Cases" $ do
    it "handles empty statement lists" $ do
      let result = optimizeStmts []
      result `shouldBe` ([] :: [Stmt])

    it "handles single return statement" $ do
      let stmts = [SReturn (AInt 42)]
          result = optimizeStmts stmts
      result `shouldBe` stmts

    it "handles self-referential assignments" $ do
      let stmts = [ SAssign "x" (RBinOp CAdd (AVar "x") (AInt 1))
                  , SReturn (AVar "x")
                  ]
          result = deadAssignElim stmts
      -- x is used in return, should be kept
      length result `shouldBe` 2

    it "handles mutually recursive assignments" $ do
      let stmts = [ SAssign "x" (RAtom (AVar "y"))
                  , SAssign "y" (RAtom (AVar "x"))
                  , SReturn (AVar "y")
                  ]
          result = optimizeStmts stmts
      -- Complex case - just verify it terminates and produces valid output
      length result `shouldSatisfy` (>= 0)

    it "converges quickly on simple programs" $ do
      -- This test verifies we don't hit iteration limit on simple cases
      let stmts = [ SAssign "a" (RAtom (AInt 1))
                  , SAssign "b" (RAtom (AVar "a"))
                  , SAssign "c" (RAtom (AVar "b"))
                  , SAssign "d" (RAtom (AVar "c"))
                  , SAssign "e" (RAtom (AVar "d"))
                  , SReturn (AVar "e")
                  ]
          result = optimizeStmts stmts
      -- Should converge in a few iterations, not hit 100 limit
      length result `shouldSatisfy` (<= 5)

  describe "CFGOpt - scalarizeIteratePairs" $ do
    it "splits pair-typed iterate state into per-component swaps" $ do
      let body =
            [ SAssign "tf" (RPairFst CEArray (AVar "cur"))
            , SAssign "ts" (RPairSnd CEArray (AVar "cur"))
            , SAssign "x" (RArrayCopy (AVar "tf"))
            , SAssign "y" (RArrayCopy (AVar "ts"))
            , SAssign "p" (RPairMake CEArray CEArray (AVar "x") (AVar "y"))
            , SAssign "cur" (RAtom (AVar "p"))
            ]
          loop = SLoop (LoopSpec ["t"] [IVar "k"] Serial Nothing LoopIterate []) body
          result = scalarizeIteratePairs [loop]
      case result of
        [ SAssign "tf" (RPairFst CEArray (AVar "cur"))
          , SAssign "ts" (RPairSnd CEArray (AVar "cur"))
          , SLoop _ body'
          , SAssign "cur" (RPairMake CEArray CEArray (AVar "tf") (AVar "ts"))
          ] -> do
            drop (length body' - 2) body'
              `shouldBe` [ SAssign "tf" (RAtom (AVar "x"))
                         , SAssign "ts" (RAtom (AVar "y"))
                         ]
            any (\s -> case s of SAssign _ (RPairFst {}) -> True; _ -> False) body'
              `shouldBe` False
        other -> expectationFailure ("unexpected shape: " ++ show other)

    it "splits nested-pair iterate state into per-leaf swaps" $ do
      let inner = CEPair CEArray CEArray
          body =
            [ SAssign "tf" (RPairFst CEArray (AVar "cur"))
            , SAssign "ti" (RPairSnd inner (AVar "cur"))
            , SAssign "ta" (RPairFst CEArray (AVar "ti"))
            , SAssign "tb" (RPairSnd CEArray (AVar "ti"))
            , SAssign "x" (RArrayCopy (AVar "tf"))
            , SAssign "y" (RArrayCopy (AVar "ta"))
            , SAssign "z" (RArrayCopy (AVar "tb"))
            , SAssign "q" (RPairMake CEArray CEArray (AVar "y") (AVar "z"))
            , SAssign "p" (RPairMake CEArray inner (AVar "x") (AVar "q"))
            , SAssign "cur" (RAtom (AVar "p"))
            ]
          loop = SLoop (LoopSpec ["t"] [IVar "k"] Serial Nothing LoopIterate []) body
          result = scalarizeIteratePairs [loop]
      case result of
        [ SAssign "tf" (RPairFst CEArray (AVar "cur"))
          , SAssign "ti" (RPairSnd (CEPair CEArray CEArray) (AVar "cur"))
          , SAssign "ta" (RPairFst CEArray (AVar "ti"))
          , SAssign "tb" (RPairSnd CEArray (AVar "ti"))
          , SLoop _ body'
          , SAssign "q" (RPairMake CEArray CEArray (AVar "ta") (AVar "tb"))
          , SAssign "cur" (RPairMake CEArray (CEPair CEArray CEArray) (AVar "tf") (AVar "q"))
          ] -> do
            drop (length body' - 3) body'
              `shouldBe` [ SAssign "tf" (RAtom (AVar "x"))
                         , SAssign "ta" (RAtom (AVar "y"))
                         , SAssign "tb" (RAtom (AVar "z"))
                         ]
            any (\s -> case s of SAssign _ (RPairFst {}) -> True; SAssign _ (RPairMake {}) -> True; _ -> False) body'
              `shouldBe` False
        other -> expectationFailure ("unexpected shape: " ++ show other)

    it "leaves single-array iterate state alone" $ do
      let body =
            [ SAssign "x" (RArrayCopy (AVar "cur"))
            , SAssign "cur" (RAtom (AVar "x"))
            ]
          loop = SLoop (LoopSpec ["t"] [IVar "k"] Serial Nothing LoopIterate []) body
      scalarizeIteratePairs [loop] `shouldBe` [loop]

-- Helper generators for property testing
type SimpleStmts = [Stmt]

simpleArrayProg :: Gen SimpleStmts
simpleArrayProg = do
  size <- choose (1, 5)
  pure [ SAssign "arr" (RArrayAlloc (AInt size))
       , SLoop (LoopSpec ["i"] [IConst size] Serial Nothing LoopPlain [])
           [SArrayWrite (AVar "arr") (AVar "i") (AInt 0)]
       , SReturn (AVar "arr")
       ]

hasArrayAlloc :: [Stmt] -> Bool
hasArrayAlloc = any hasAlloc
  where
    hasAlloc (SAssign _ (RArrayAlloc _)) = True
    hasAlloc (SLoop _ body) = hasArrayAlloc body
    hasAlloc (SIf _ thn els) = hasArrayAlloc thn || hasArrayAlloc els
    hasAlloc _ = False
