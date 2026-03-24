{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.StencilSpec (spec) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Language.Hydrangea.Frontend
import Language.Hydrangea.Interpreter
import Test.Hspec

-- | Evaluate a ByteString expression and compare with expected Value.
expectEval :: ByteString -> Value -> Expectation
expectEval src expected = do
  res <- readAndEval src
  case res of
    Left err    -> expectationFailure $ "Evaluation error: " ++ err
    Right actual -> actual `shouldBe` expected

spec :: Spec
spec = describe "stencil" $ do

  -- -------------------------------------------------------------------------
  -- 1D stencil — clamp boundary
  -- -------------------------------------------------------------------------
  describe "1D clamp" $ do
    it "3-point box filter (interior and clamped boundaries)" $
      -- arr = [1,2,3,4,5]
      -- i=0: arr[0]+arr[0]+arr[1] = 1+1+2 = 4
      -- i=1: arr[0]+arr[1]+arr[2] = 1+2+3 = 6
      -- i=2: arr[1]+arr[2]+arr[3] = 2+3+4 = 9
      -- i=3: arr[2]+arr[3]+arr[4] = 3+4+5 = 12
      -- i=4: arr[3]+arr[4]+arr[4] = 4+5+5 = 14
      expectEval
        "let arr = generate [5] (let f [i] = i + 1 in f) in \
        \stencil clamp (fn acc => acc (-1) + acc 0 + acc 1) arr"
        (VArray [5] [VInt 4, VInt 6, VInt 9, VInt 12, VInt 14])

    it "zero-offset stencil is identity" $
      expectEval
        "let arr = generate [4] (let f [i] = i * 2 in f) in \
        \stencil clamp (fn acc => acc 0) arr"
        (VArray [4] [VInt 0, VInt 2, VInt 4, VInt 6])

    it "large negative offset clamped to left boundary" $
      -- arr = [10,11,12], acc(-5) clamped to arr[0]=10 for all i
      expectEval
        "let arr = generate [3] (let f [i] = i + 10 in f) in \
        \stencil clamp (fn acc => acc (-5)) arr"
        (VArray [3] [VInt 10, VInt 10, VInt 10])

    it "large positive offset clamped to right boundary" $
      -- arr = [10,11,12], acc(5) clamped to arr[2]=12 for all i
      expectEval
        "let arr = generate [3] (let f [i] = i + 10 in f) in \
        \stencil clamp (fn acc => acc 5) arr"
        (VArray [3] [VInt 12, VInt 12, VInt 12])

    it "single-element array: all offsets clamp to same element" $
      -- acc(-1)+acc(0)+acc(1) = arr[0]+arr[0]+arr[0] = 42*3 = 126
      expectEval
        "let arr = fill [1] 42 in \
        \stencil clamp (fn acc => acc (-1) + acc 0 + acc 1) arr"
        (VArray [1] [VInt 126])

    it "body ignoring accessor returns constant" $
      expectEval
        "let arr = generate [3] (let f [i] = i in f) in \
        \stencil clamp (fn acc => 99) arr"
        (VArray [3] [VInt 99, VInt 99, VInt 99])

  -- -------------------------------------------------------------------------
  -- 1D stencil — wrap boundary
  -- -------------------------------------------------------------------------
  describe "1D wrap" $ do
    it "shift left by 1 (periodic)" $
      -- arr = [1,2,3,4]
      -- i=0: acc(-1) = arr[(0-1+4)%4]=arr[3]=4
      -- i=1: arr[0]=1, i=2: arr[1]=2, i=3: arr[2]=3
      expectEval
        "let arr = generate [4] (let f [i] = i + 1 in f) in \
        \stencil wrap (fn acc => acc (-1)) arr"
        (VArray [4] [VInt 4, VInt 1, VInt 2, VInt 3])

    it "shift right by 1 (periodic)" $
      -- arr = [1,2,3,4]
      -- i=0:arr[1]=2, i=1:arr[2]=3, i=2:arr[3]=4, i=3:arr[0]=1
      expectEval
        "let arr = generate [4] (let f [i] = i + 1 in f) in \
        \stencil wrap (fn acc => acc 1) arr"
        (VArray [4] [VInt 2, VInt 3, VInt 4, VInt 1])

    it "3-point wrap filter preserves sum (sum = 3 * original sum)" $
      -- input sum = 1+2+3+4=10; wrap filter output sum = 3*10=30
      expectEval
        "let arr = generate [4] (let f [i] = i + 1 in f) in \
        \let out = stencil wrap (fn acc => acc (-1) + acc 0 + acc 1) arr in \
        \index () (reduce (fn a b => a + b) 0 out)"
        (VInt 30)

  -- -------------------------------------------------------------------------
  -- 1D stencil — mirror boundary
  -- -------------------------------------------------------------------------
  describe "1D mirror" $ do
    it "mirror at left boundary" $
      -- arr = [1,2,3,4]; period = 2*(4-1) = 6
      -- i=0: raw=-1, mod6=5, 5>=4 → 6-5=1 → arr[1]=2
      -- i=1: raw=0 → arr[0]=1
      -- i=2: raw=1 → arr[1]=2
      -- i=3: raw=2 → arr[2]=3
      expectEval
        "let arr = generate [4] (let f [i] = i + 1 in f) in \
        \stencil mirror (fn acc => acc (-1)) arr"
        (VArray [4] [VInt 2, VInt 1, VInt 2, VInt 3])

    it "mirror at right boundary" $
      -- arr = [1,2,3,4]
      -- i=3: raw=4, mod6=4, 4>=4 → 6-4=2 → arr[2]=3
      expectEval
        "let arr = generate [4] (let f [i] = i + 1 in f) in \
        \stencil mirror (fn acc => acc 1) arr"
        (VArray [4] [VInt 2, VInt 3, VInt 4, VInt 3])

  -- -------------------------------------------------------------------------
  -- 1D stencil — constant boundary
  -- -------------------------------------------------------------------------
  describe "1D constant" $ do
    it "OOB returns zero constant (3-point filter)" $
      -- arr = [1,2,3]
      -- i=0: 0+1+2=3
      -- i=1: 1+2+3=6
      -- i=2: 2+3+0=5
      expectEval
        "let arr = generate [3] (let f [i] = i + 1 in f) in \
        \stencil (constant 0) (fn acc => acc (-1) + acc 0 + acc 1) arr"
        (VArray [3] [VInt 3, VInt 6, VInt 5])

    it "negative constant for OOB" $
      -- arr=[10,10,10]; i=0: acc(-1)=OOB→-1; i=1,2: in-bounds→10
      expectEval
        "let arr = fill [3] 10 in \
        \stencil (constant (-1)) (fn acc => acc (-1)) arr"
        (VArray [3] [VInt (-1), VInt 10, VInt 10])

    it "all OOB returns constant" $
      -- arr=[0,0]; acc(-5) for all i is OOB→99
      expectEval
        "let arr = fill [2] 0 in \
        \stencil (constant 99) (fn acc => acc (-5)) arr"
        (VArray [2] [VInt 99, VInt 99])

  -- -------------------------------------------------------------------------
  -- 2D stencil — clamp boundary
  -- -------------------------------------------------------------------------
  describe "2D clamp" $ do
    it "zero-offset 2D stencil is identity" $
      -- arr = [[0,1,2],[3,4,5]]; note: (i*3) parenthesised because + binds tighter than * in Hydrangea
      expectEval
        "let arr = generate [2,3] (let f [i,j] = (i * 3) + j in f) in \
        \stencil clamp (fn acc => acc 0 0) arr"
        (VArray [2,3] [VInt 0, VInt 1, VInt 2, VInt 3, VInt 4, VInt 5])

    it "2D shift right in column dimension (clamp)" $
      -- arr = [[1,2,3],[4,5,6]]
      -- acc(0,1): shift j by +1
      -- i=0,j=0→arr[0,1]=2; i=0,j=1→arr[0,2]=3; i=0,j=2→arr[0,2]=3
      -- i=1,j=0→arr[1,1]=5; i=1,j=1→arr[1,2]=6; i=1,j=2→arr[1,2]=6
      expectEval
        "let arr = generate [2,3] (let f [i,j] = (i * 3) + j + 1 in f) in \
        \stencil clamp (fn acc => acc 0 1) arr"
        (VArray [2,3] [VInt 2, VInt 3, VInt 3, VInt 5, VInt 6, VInt 6])

    it "2D 4-neighbour sum at corner" $
      -- arr = [[1,2,3],[4,5,6],[7,8,9]]
      -- corner (0,0): left=arr[0,0]=1, right=arr[0,1]=2, up=arr[0,0]=1, down=arr[1,0]=4 → 8
      expectEval
        "let arr = generate [3,3] (let f [i,j] = (i * 3) + j + 1 in f) in \
        \let out = stencil clamp \
        \  (fn acc => acc 0 (-1) + acc 0 1 + acc (-1) 0 + acc 1 0) arr in \
        \index [0,0] out"
        (VInt 8)

  -- -------------------------------------------------------------------------
  -- 2D stencil — constant boundary
  -- -------------------------------------------------------------------------
  describe "2D constant" $ do
    it "2D OOB returns zero constant (row above)" $
      -- arr = [[1,1],[1,1]]; acc(-1,0) at row=0 is OOB → 0
      expectEval
        "let arr = fill [2,2] 1 in \
        \stencil (constant 0) (fn acc => acc (-1) 0) arr"
        (VArray [2,2] [VInt 0, VInt 0, VInt 1, VInt 1])
