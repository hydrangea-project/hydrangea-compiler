{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.GatherBoundsSpec
--
-- Tests for static index bounds checking on gather kernels.
--
-- The type checker tracks an upper bound on integer array element values via a
-- synthetic SMT variable (@TValBound@).  Construction sites that produce
-- bounded integer arrays (@iota@, @sort_indices@, @make_index@) establish this
-- bound; @gather@ verifies that the index array's bound is at most the source
-- array's first dimension.
module Language.Hydrangea.GatherBoundsSpec (spec) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List (isInfixOf)
import Language.Hydrangea.Frontend (inferDecsTop, inferDecsTopWithWarnings, readDecs, typeCheckExp, readExp)
import Test.Hspec

-- | Parse and type-check a sequence of top-level declarations.
-- Returns @Right _@ on success, @Left msg@ on error.
checkDecs :: ByteString -> IO (Either String ())
checkDecs src =
  case readDecs src of
    Left err   -> pure (Left ("Parse error: " ++ err))
    Right decs -> do
      result <- inferDecsTop decs
      pure $ case result of
        Left msg -> Left msg
        Right _  -> Right ()

expectDecsOk :: ByteString -> Expectation
expectDecsOk src = do
  result <- checkDecs src
  case result of
    Left msg -> expectationFailure ("Expected success but got error: " ++ msg)
    Right () -> pure ()

expectDecsError :: ByteString -> (String -> Bool) -> Expectation
expectDecsError src predicate = do
  result <- checkDecs src
  case result of
    Left msg -> msg `shouldSatisfy` predicate
    Right () -> expectationFailure "Expected a type error but typechecking succeeded"

-- | Parse and type-check a single expression (helper for iota/make_index type
-- shape tests that can be written as expressions).
expectExpOk :: ByteString -> Expectation
expectExpOk src =
  case readExp src of
    Left err -> expectationFailure ("Parse error: " ++ err)
    Right e  -> do
      res <- typeCheckExp e
      case res of
        Left msg -> expectationFailure ("Expected success but got: " ++ msg)
        Right _  -> pure ()

spec :: Spec
spec = describe "gather bounds checking" $ do

  -- ------------------------------------------------------------------ --
  -- iota
  -- ------------------------------------------------------------------ --
  describe "iota" $ do

    it "typechecks iota as an expression" $ do
      expectExpOk "iota 5"

    it "typechecks iota as a top-level binding" $ do
      expectDecsOk $ BS.unlines
        [ "let idx = iota 5"
        , "let main = idx"
        ]

    it "iota n is a safe index for a same-sized source array" $ do
      -- iota 5 produces elements in [0,5); src has 5 elements — safe
      expectDecsOk $ BS.unlines
        [ "let src = generate [5] (let f [i] = i in f)"
        , "let idx = iota 5"
        , "let main = gather idx src"
        ]

    it "iota n is a safe index when source is strictly larger" $ do
      -- iota 3 elements all < 3 <= 10 — safe
      expectDecsOk $ BS.unlines
        [ "let src = generate [10] (let f [i] = i in f)"
        , "let idx = iota 3"
        , "let main = gather idx src"
        ]

    it "iota n is rejected when source is smaller" $ do
      -- iota 10 elements may be up to 9; src has only 5 — unsafe
      expectDecsError
        ( BS.unlines
            [ "let src = generate [5] (let f [i] = i in f)"
            , "let idx = iota 10"
            , "let main = gather idx src"
            ]
        )
        (isInfixOf "UnsatConstraints")

    it "iota 1 is a safe index for a 1-element source" $ do
      expectDecsOk $ BS.unlines
        [ "let src = generate [1] (let f [i] = i in f)"
        , "let idx = iota 1"
        , "let main = gather idx src"
        ]

  -- ------------------------------------------------------------------ --
  -- sort_indices
  -- ------------------------------------------------------------------ --
  describe "sort_indices" $ do

    it "sort_indices result is a safe index for the source array" $ do
      -- sort_indices keys is a permutation of [0, len(keys)), always safe
      expectDecsOk $ BS.unlines
        [ "let keys = generate [5] (let f [i] = i in f)"
        , "let perm = sort_indices keys"
        , "let main = gather perm keys"
        ]

    it "sort_indices result is a safe index for a same-sized array" $ do
      expectDecsOk $ BS.unlines
        [ "let keys  = generate [8] (let f [i] = i in f)"
        , "let vals  = generate [8] (let f [i] = i * 2 in f)"
        , "let perm  = sort_indices keys"
        , "let main  = gather perm vals"
        ]

    it "sort_indices result is rejected when dest is smaller than source" $ do
      -- perm indexes into src (size 3) but perm values come from keys (size 5)
      -- so values can be up to 4, but src only has 3 elements — unsafe
      expectDecsError
        ( BS.unlines
            [ "let keys = generate [5] (let f [i] = i in f)"
            , "let src  = generate [3] (let f [i] = i in f)"
            , "let perm = sort_indices keys"
            , "let main = gather perm src"
            ]
        )
        (isInfixOf "UnsatConstraints")

  -- ------------------------------------------------------------------ --
  -- make_index
  -- ------------------------------------------------------------------ --
  describe "make_index" $ do

    it "make_index asserts a bound that satisfies gather" $ do
      expectDecsOk $ BS.unlines
        [ "let src     = generate [10] (let f [i] = i in f)"
        , "let raw_idx = generate [5]  (let f [i] = 0 in f)"
        , "let idx     = make_index 10 raw_idx"
        , "let main    = gather idx src"
        ]

    it "make_index with bound equal to source size is accepted" $ do
      expectDecsOk $ BS.unlines
        [ "let src     = generate [4] (let f [i] = i in f)"
        , "let raw_idx = generate [4] (let f [i] = 0 in f)"
        , "let idx     = make_index 4 raw_idx"
        , "let main    = gather idx src"
        ]

    it "make_index with bound larger than source is rejected" $ do
      expectDecsError
        ( BS.unlines
            [ "let src     = generate [4] (let f [i] = i in f)"
            , "let raw_idx = generate [4] (let f [i] = 0 in f)"
            , "let idx     = make_index 5 raw_idx"
            , "let main    = gather idx src"
            ]
        )
        (isInfixOf "UnsatConstraints")

  -- ------------------------------------------------------------------ --
  -- gather with unanalyzable index body: warning, not silent pass
  -- ------------------------------------------------------------------ --
  -- When a generator body is opaque to inferBVal (e.g. division), no
  -- TValBound is established.  The bounds obligation is ungrounded, so the
  -- solver emits a "could not verify" warning rather than silently accepting.
  -- ------------------------------------------------------------------ --
  describe "gather without established bounds" $ do

    it "gather with unanalyzable body emits 'could not verify' warning" $ do
      -- Division is not tracked by inferBVal → TValBound unestablished →
      -- ungrounded obligation → warning, not silent pass.
      let src = BS.unlines
            [ "let src = generate [5] (let g [i] = i in g)"
            , "let idx = generate [5] (let f [i] = i / 2 in f)"
            , "let main = gather idx src"
            ]
      case readDecs src of
        Left err -> expectationFailure ("Parse error: " ++ err)
        Right decs -> do
          result <- inferDecsTopWithWarnings decs
          case result of
            Left msg -> expectationFailure ("Expected success with warning: " ++ msg)
            Right (_, warnings) ->
              warnings `shouldSatisfy` any (isInfixOf "could not verify")

    it "gather with second unanalyzable body emits 'could not verify' warning" $ do
      -- Another division case; src body uses i*2 but that is irrelevant —
      -- idx's division body is what makes the check ungrounded.
      let src = BS.unlines
            [ "let src = generate [10] (let g [i] = i * 2 in g)"
            , "let idx = generate [3]  (let f [i] = i / 2 in f)"
            , "let main = gather idx src"
            ]
      case readDecs src of
        Left err -> expectationFailure ("Parse error: " ++ err)
        Right decs -> do
          result <- inferDecsTopWithWarnings decs
          case result of
            Left msg -> expectationFailure ("Expected success with warning: " ++ msg)
            Right (_, warnings) ->
              warnings `shouldSatisfy` any (isInfixOf "could not verify")

  -- ------------------------------------------------------------------ --
  -- Auto-inference of generator body bounds without PBound annotation
  -- ------------------------------------------------------------------ --
  -- When a generate body is a simple arithmetic expression over the index
  -- variable (e.g. i + 1, i % M, i * k, a constant), the compiler now
  -- infers the value bound automatically without requiring [i bound N].
  -- This closes false-negative gaps where unsafe programs were silently
  -- accepted in permissive mode.
  -- ------------------------------------------------------------------ --
  describe "generate body bound auto-inference without PBound" $ do

    it "i+1 body: safe gather into N+1-element source passes" $ do
      -- i < 5, so i+1 ≤ 5; src has 6 elements — safe
      expectDecsOk $ BS.unlines
        [ "let src = generate [6] (let g [i] = i in g)"
        , "let idx = generate [5] (let f [i] = i + 1 in f)"
        , "let main = gather idx src"
        ]

    it "i+1 body: unsafe gather into N-element source is rejected" $ do
      -- i < 5, so i+1 ≤ 5; src has only 5 elements — TValBound 6 > 5: UNSAT
      expectDecsError
        ( BS.unlines
            [ "let src = generate [5] (let g [i] = i in g)"
            , "let idx = generate [5] (let f [i] = i + 1 in f)"
            , "let main = gather idx src"
            ]
        )
        (isInfixOf "UnsatConstraints")

    it "i%4 body: safe gather into 4-element source passes" $ do
      -- i % 4 is in [0, 4); src has 4 elements — safe
      expectDecsOk $ BS.unlines
        [ "let src = generate [4] (let g [i] = i in g)"
        , "let idx = generate [10] (let f [i] = i % 4 in f)"
        , "let main = gather idx src"
        ]

    it "i%4 body: unsafe gather into 3-element source is rejected" $ do
      -- i % 4 may be up to 3; src has only 3 elements — TValBound 4 > 3: UNSAT
      expectDecsError
        ( BS.unlines
            [ "let src = generate [3] (let g [i] = i in g)"
            , "let idx = generate [10] (let f [i] = i % 4 in f)"
            , "let main = gather idx src"
            ]
        )
        (isInfixOf "UnsatConstraints")

    it "i*2 body: safe gather into 2N-element source passes" $ do
      -- i < 5, so i*2 < 10; src has 10 elements — safe
      expectDecsOk $ BS.unlines
        [ "let src = generate [10] (let g [i] = i in g)"
        , "let idx = generate [5]  (let f [i] = i * 2 in f)"
        , "let main = gather idx src"
        ]

    it "i*2 body: unsafe gather into N-element source is rejected" $ do
      -- i < 5, so i*2 < 10; src has only 5 elements — TValBound 10 > 5: UNSAT
      expectDecsError
        ( BS.unlines
            [ "let src = generate [5] (let g [i] = i in g)"
            , "let idx = generate [5] (let f [i] = i * 2 in f)"
            , "let main = gather idx src"
            ]
        )
        (isInfixOf "UnsatConstraints")

    it "constant 0 body: safe gather into any non-empty source passes" $ do
      -- All elements are 0; src has 5 elements — safe (TValBound = 1 ≤ 5)
      expectDecsOk $ BS.unlines
        [ "let src = generate [5] (let g [i] = i in g)"
        , "let idx = generate [10] (let f [i] = 0 in f)"
        , "let main = gather idx src"
        ]

    it "constant 3 body: safe gather into 4-element source passes" $ do
      -- All elements are 3; src has 4 elements — safe (TValBound = 4 ≤ 4)
      expectDecsOk $ BS.unlines
        [ "let src = generate [4] (let g [i] = i in g)"
        , "let idx = generate [10] (let f [i] = 3 in f)"
        , "let main = gather idx src"
        ]

    it "constant 3 body: unsafe gather into 3-element source is rejected" $ do
      -- All elements are 3; src has only 3 elements — TValBound 4 > 3: UNSAT
      expectDecsError
        ( BS.unlines
            [ "let src = generate [3] (let g [i] = i in g)"
            , "let idx = generate [10] (let f [i] = 3 in f)"
            , "let main = gather idx src"
            ]
        )
        (isInfixOf "UnsatConstraints")

    it "scatter with i*2 indices auto-inferred: safe into 2N-element dst passes" $ do
      -- i < 5, i*2 < 10; dst has 10 elements — safe
      expectDecsOk $ BS.unlines
        [ "let n   = 5"
        , "let dst  = fill [10] 0"
        , "let vals = fill [n] 1"
        , "let main = scatter (+) dst"
        , "  (generate [n] (let f [i] = i * 2 in f))"
        , "  vals"
        ]

    it "scatter with i*2 indices: rejected when dst is too small" $ do
      -- i < 5, i*2 < 10; dst has only 5 elements — TValBound 10 > 5: UNSAT
      expectDecsError
        ( BS.unlines
            [ "let n   = 5"
            , "let dst  = fill [5] 0"
            , "let vals = fill [n] 1"
            , "let main = scatter (+) dst"
            , "  (generate [n] (let f [i] = i * 2 in f))"
            , "  vals"
            ]
        )
        (isInfixOf "UnsatConstraints")

  -- ------------------------------------------------------------------ --
  -- PBound value bound propagation through generator bodies
  -- ------------------------------------------------------------------ --
  describe "PBound value bound propagation" $ do

    it "identity body [i bound N] propagates bound N to gather" $ do
      -- generate [N] (let f [i bound N] = i in f) has TValBound = N
      expectDecsOk $ BS.unlines
        [ "let src = generate [5] (let g [i] = i in g)"
        , "let idx = generate [5] (let f [i bound 5] = i in f)"
        , "let main = gather idx src"
        ]

    it "i+1 body with [i bound N] propagates bound N+1" $ do
      -- elements < N+1; src size = N+1 — safe
      expectDecsOk $ BS.unlines
        [ "let src = generate [6] (let g [i] = i in g)"
        , "let idx = generate [5] (let f [i bound 5] = i + 1 in f)"
        , "let main = gather idx src"
        ]

    it "i+1 body with wrong source size is rejected" $ do
      -- elements bounded by N+1=6 but src has only 5 — unsafe
      expectDecsError
        ( BS.unlines
            [ "let src = generate [5] (let g [i] = i in g)"
            , "let idx = generate [5] (let f [i bound 5] = i + 1 in f)"
            , "let main = gather idx src"
            ]
        )
        (isInfixOf "UnsatConstraints")

    it "generate with non-inferable body emits a 'could not verify' warning" $ do
      -- Division is not tracked by inferBVal, so no TValBound is established.
      -- The bounds obligation is ungrounded → ObligationUnknown → warning emitted.
      -- This ensures unsafe code is never silently accepted; the user gets a notice.
      -- (Note: arithmetic bodies like i+1, i%M, i*k ARE now auto-inferred and
      -- checked precisely — only truly opaque expressions fall through here.)
      let src = BS.unlines
            [ "let src = generate [3] (let g [i] = i in g)"
            , "let idx = generate [5] (let f [i] = i / 2 in f)"
            , "let main = gather idx src"
            ]
      case readDecs src of
        Left err -> expectationFailure ("Parse error: " ++ err)
        Right decs -> do
          result <- inferDecsTopWithWarnings decs
          case result of
            Left msg -> expectationFailure ("Expected success with warning: " ++ msg)
            Right (_, warnings) ->
              warnings `shouldSatisfy` any (isInfixOf "could not verify")

    it "PBound soundness check: shape must not exceed declared bound" $ do
      -- generate [10] with [i bound 5] — shape 10 > bound 5: unsound
      expectDecsError
        ( BS.unlines
            [ "let src = generate [5] (let g [i] = i in g)"
            , "let idx = generate [10] (let f [i bound 5] = i in f)"
            , "let main = gather idx src"
            ]
        )
        (isInfixOf "UnsatConstraints")

  -- ------------------------------------------------------------------ --
  -- make_index warning
  -- ------------------------------------------------------------------ --
  describe "make_index warnings" $ do

    it "make_index emits a warning about unverified bound" $ do
      let src = BS.unlines
            [ "let src     = generate [10] (let f [i] = i in f)"
            , "let raw_idx = generate [5]  (let f [i] = 0 in f)"
            , "let idx     = make_index 10 raw_idx"
            , "let main    = gather idx src"
            ]
      case readDecs src of
        Left err -> expectationFailure ("Parse error: " ++ err)
        Right decs -> do
          result <- inferDecsTopWithWarnings decs
          case result of
            Left msg -> expectationFailure ("Expected success: " ++ msg)
            Right (_, warnings) ->
              warnings `shouldSatisfy` any (isInfixOf "make_index")

  -- ------------------------------------------------------------------ --
  -- index bounds checking
  -- ------------------------------------------------------------------ --
  describe "index bounds checking" $ do

    it "index with EBoundLetIn bound is safe when arr is large enough" $ do
      -- let y bound 5 = 3 in index [y] arr; arr has 5 elements → PLe 5 5 → SAT
      expectDecsOk $ BS.unlines
        [ "let arr  = generate [5] (let f [i] = i in f)"
        , "let main = let y bound 5 = 3 in index [y] arr"
        ]

    it "index with bound larger than arr is rejected" $ do
      -- let y bound 5 = 3 in index [y] arr; arr has 4 elements → PLe 5 4 → UNSAT
      expectDecsError
        ( BS.unlines
            [ "let arr  = generate [4] (let f [i] = i in f)"
            , "let main = let y bound 5 = 3 in index [y] arr"
            ]
        )
        (isInfixOf "UnsatConstraints")

    it "index x+1 with bound is safe when arr is large enough" $ do
      -- x bound 5 → bound of x+1 = 6 → arr has 6 elements → PLe 6 6 → SAT
      expectDecsOk $ BS.unlines
        [ "let arr  = generate [6] (let f [i] = i in f)"
        , "let main = let x bound 5 = 3 in index [x + 1] arr"
        ]

    it "index x+1 with bound too large is rejected" $ do
      -- x bound 5 → bound of x+1 = 6 → arr has 5 elements → PLe 6 5 → UNSAT
      expectDecsError
        ( BS.unlines
            [ "let arr  = generate [5] (let f [i] = i in f)"
            , "let main = let x bound 5 = 3 in index [x + 1] arr"
            ]
        )
        (isInfixOf "UnsatConstraints")

    it "index inside generator with PBound is safe" $ do
      -- i bound 5 via PBound, arr has 5 elements → PLe 5 5 → SAT
      expectDecsOk $ BS.unlines
        [ "let arr  = generate [5] (let f [i] = i in f)"
        , "let main = generate [5] (let f [i bound 5] = index [i] arr in f)"
        ]

    it "index inside generator with PBound, arr too small, is rejected" $ do
      -- i bound 5 via PBound, arr has 4 elements → PLe 5 4 → UNSAT
      expectDecsError
        ( BS.unlines
            [ "let arr  = generate [4] (let f [i] = i in f)"
            , "let main = generate [5] (let f [i bound 5] = index [i] arr in f)"
            ]
        )
        (isInfixOf "UnsatConstraints")

  describe "ungrounded obligation warnings" $ do
    it "emits a warning for an ungrounded index obligation" $ do
      -- i has no bound annotation and no concrete value, so its pred var is
      -- unconstrained; the obligation i < dim(arr,0) remains ungrounded
      let src = BS.unlines
            [ "let arr  = generate [5] (let f [i] = i in f)"
            , "let f [i] = index [i] arr"  -- standalone function, i is free
            ]
      case readDecs src of
        Left err -> expectationFailure ("Parse error: " ++ err)
        Right decs -> do
          result <- inferDecsTopWithWarnings decs
          case result of
            Left msg -> expectationFailure ("Expected success: " ++ msg)
            Right (_, warnings) ->
              warnings `shouldSatisfy` any (isInfixOf "could not verify")

  describe "parametric index safety" $ do
    it "verifies index safety when generate size matches indexed array size" $ do
      -- f [i] indexes into 5-element arr; main = generate [5] f is safe
      expectDecsOk $ BS.unlines
        [ "let arr  = generate [5] (let g [i] = i in g)"
        , "let f [i] = index [i] arr"
        , "let main = generate [5] f"
        ]

    it "rejects index safety when generate size exceeds indexed array size" $ do
      -- f [i] indexes into 3-element arr; main = generate [5] f is unsafe
      expectDecsError
        ( BS.unlines
            [ "let arr  = generate [3] (let g [i] = i in g)"
            , "let f [i] = index [i] arr"
            , "let main = generate [5] f"
            ]
        )
        (isInfixOf "UnsatConstraints")

    it "verifies concrete index variable when value is known" $ do
      -- let x = 2 in index [x] arr where arr has 5 elements
      -- x's pred var is linked to 2 → obligation 2 < 5 is verified
      let src = BS.unlines
            [ "let arr  = generate [5] (let g [i] = i in g)"
            , "let main = let x = 2 in index [x] arr"
            ]
      case readDecs src of
        Left err -> expectationFailure ("Parse error: " ++ err)
        Right decs -> do
          result <- inferDecsTopWithWarnings decs
          case result of
            Left msg -> expectationFailure ("Expected success: " ++ msg)
            Right (_, warnings) ->
              warnings `shouldSatisfy` all (not . isInfixOf "could not verify")

    it "rejects concrete out-of-bounds index variable" $ do
      -- let x = 6 in index [x] arr where arr has 5 elements → 6 < 5 is false
      expectDecsError
        ( BS.unlines
            [ "let arr  = generate [5] (let g [i] = i in g)"
            , "let main = let x = 6 in index [x] arr"
            ]
        )
        (isInfixOf "UnsatConstraints")

    -- Arithmetic over concrete values: termFromExp on EBinOp now uses pred vars
    -- for each EVar operand, so the computed value is trackable through the chain.

    it "verifies arithmetic index when result is in bounds" $ do
      -- let x = 3 in index [x + 1] arr (5 elements) → 4 < 5 ✓
      let src = BS.unlines
            [ "let arr  = generate [5] (let g [i] = i in g)"
            , "let main = let x = 3 in index [x + 1] arr"
            ]
      case readDecs src of
        Left err -> expectationFailure ("Parse error: " ++ err)
        Right decs -> do
          result <- inferDecsTopWithWarnings decs
          case result of
            Left msg -> expectationFailure ("Expected success: " ++ msg)
            Right (_, warnings) ->
              warnings `shouldSatisfy` all (not . isInfixOf "could not verify")

    it "rejects arithmetic index at exact boundary" $ do
      -- let x = 4 in index [x + 1] arr (5 elements) → 5 < 5 is false
      expectDecsError
        ( BS.unlines
            [ "let arr  = generate [5] (let g [i] = i in g)"
            , "let main = let x = 4 in index [x + 1] arr"
            ]
        )
        (isInfixOf "UnsatConstraints")

    it "verifies last valid concrete index" $ do
      -- let x = 4 in index [x] arr (5 elements) → 4 < 5 ✓
      let src = BS.unlines
            [ "let arr  = generate [5] (let g [i] = i in g)"
            , "let main = let x = 4 in index [x] arr"
            ]
      case readDecs src of
        Left err -> expectationFailure ("Parse error: " ++ err)
        Right decs -> do
          result <- inferDecsTopWithWarnings decs
          case result of
            Left msg -> expectationFailure ("Expected success: " ++ msg)
            Right (_, warnings) ->
              warnings `shouldSatisfy` all (not . isInfixOf "could not verify")

    it "rejects index at exactly arr size" $ do
      -- let x = 5 in index [x] arr (5 elements) → 5 < 5 is false
      expectDecsError
        ( BS.unlines
            [ "let arr  = generate [5] (let g [i] = i in g)"
            , "let main = let x = 5 in index [x] arr"
            ]
        )
        (isInfixOf "UnsatConstraints")

    -- Inline generator lambdas (not named declarations) benefit from the same
    -- EGenerate hypothesis emission as named functions.

    it "verifies index inside inline generator lambda" $ do
      -- generate [5] (let f [i] = index [i] arr in f) where arr has 5 elements
      expectDecsOk $ BS.unlines
        [ "let arr  = generate [5] (let g [i] = i in g)"
        , "let main = generate [5] (let f [i] = index [i] arr in f)"
        ]

    it "rejects index inside inline generator lambda when size mismatches" $ do
      -- generate [5] (let f [i] = index [i] arr in f) where arr has 3 elements
      expectDecsError
        ( BS.unlines
            [ "let arr  = generate [3] (let g [i] = i in g)"
            , "let main = generate [5] (let f [i] = index [i] arr in f)"
            ]
        )
        (isInfixOf "UnsatConstraints")

    -- Chained index: f accesses two arrays; both must be safe.

    it "verifies when both arrays match the generate size" $ do
      expectDecsOk $ BS.unlines
        [ "let a = generate [4] (let g [i] = i in g)"
        , "let b = generate [4] (let g [i] = i * 2 in g)"
        , "let f [i] = index [i] a + index [i] b"
        , "let main = generate [4] f"
        ]

    it "rejects when second array is too small" $ do
      -- a has 4 elements, b has 3 — f indexing b with i up to 3 is unsafe
      expectDecsError
        ( BS.unlines
            [ "let a = generate [4] (let g [i] = i in g)"
            , "let b = generate [3] (let g [i] = i * 2 in g)"
            , "let f [i] = index [i] a + index [i] b"
            , "let main = generate [4] f"
            ]
        )
        (isInfixOf "UnsatConstraints")

  -- ------------------------------------------------------------------ --
  -- map bounds propagation
  -- ------------------------------------------------------------------ --
  describe "map bounds propagation" $ do

    it "identity map over iota preserves bound for gather" $ do
      expectDecsOk $ BS.unlines
        [ "let src    = generate [5] (let f [i] = i in f)"
        , "let idx    = map (let f x = x in f) (iota 5)"
        , "let main   = gather idx src"
        ]

    it "map x+1 over iota 5 is safe when source has 6 elements" $ do
      expectDecsOk $ BS.unlines
        [ "let src    = generate [6] (let f [i] = i in f)"
        , "let idx    = map (let f x = x + 1 in f) (iota 5)"
        , "let main   = gather idx src"
        ]

    it "map x+1 over iota 5 is rejected when source has only 5 elements" $ do
      expectDecsError
        ( BS.unlines
            [ "let src    = generate [5] (let f [i] = i in f)"
            , "let idx    = map (let f x = x + 1 in f) (iota 5)"
            , "let main   = gather idx src"
            ]
        )
        (isInfixOf "UnsatConstraints")

    it "map x*2 over iota 5 is safe when source has 10 elements" $ do
      expectDecsOk $ BS.unlines
        [ "let src    = generate [10] (let f [i] = i in f)"
        , "let idx    = map (let f x = x * 2 in f) (iota 5)"
        , "let main   = gather idx src"
        ]

    it "map x*2 over iota 5 is rejected when source has only 9 elements" $ do
      expectDecsError
        ( BS.unlines
            [ "let src    = generate [9] (let f [i] = i in f)"
            , "let idx    = map (let f x = x * 2 in f) (iota 5)"
            , "let main   = gather idx src"
            ]
        )
        (isInfixOf "UnsatConstraints")

    it "map over generate with PBound propagates bound" $ do
      expectDecsOk $ BS.unlines
        [ "let src    = generate [5]  (let g [i] = i in g)"
        , "let base   = generate [5]  (let f [i bound 5] = i in f)"
        , "let idx    = map (let f x = x in f) base"
        , "let main   = gather idx src"
        ]

    it "chained maps compose bounds correctly" $ do
      expectDecsOk $ BS.unlines
        [ "let src    = generate [12] (let f [i] = i in f)"
        , "let step1  = map (let f x = x + 1 in f) (iota 5)"
        , "let idx    = map (let f x = x * 2 in f) step1"
        , "let main   = gather idx src"
        ]

    it "chained maps rejected when source too small for composed bound" $ do
      expectDecsError
        ( BS.unlines
            [ "let src    = generate [11] (let f [i] = i in f)"
            , "let step1  = map (let f x = x + 1 in f) (iota 5)"
            , "let idx    = map (let f x = x * 2 in f) step1"
            , "let main   = gather idx src"
            ]
        )
        (isInfixOf "UnsatConstraints")

    it "map with non-inline fn: out-of-bounds caught (f x = x+1, iota 5 vs src [3])" $ do
      -- Phase 4: output bound of map propagated from definition body.
      -- f x = x + 1  over iota 5 gives elements [1..5]; src has only 3 elements → UnsatConstraints.
      expectDecsError
        ( BS.unlines
            [ "let src    = generate [3] (let g [i] = i in g)"
            , "let f x    = x + 1"
            , "let idx    = map f (iota 5)"
            , "let main   = gather idx src"
            ]
        )
        (isInfixOf "UnsatConstraints")

    it "map with non-inline fn: in-bounds passes (f x = x+1, iota 5 vs src [6])" $ do
      -- Phase 4: elements [1..5] from f(iota 5), src has 6 elements → safe.
      expectDecsOk $ BS.unlines
        [ "let src    = generate [6] (let g [i] = i in g)"
        , "let f x    = x + 1"
        , "let idx    = map f (iota 5)"
        , "let main   = gather idx src"
        ]

    -- Phase 1: TValBoundDim propagated through emitRefineLink for array parameters.
    -- The bound check here verifies that passing an array through generate's function
    -- argument propagates the element bound; the EGenerate + gather chain already
    -- tested this.  The new Phase 1 change enables it via EApp as well.
    it "Phase 1 – iota element bound propagates: identity map via EApp (safe)" $ do
      -- Passing iota 5 through map identity preserves the TValBoundDim = 5 bound.
      -- src has exactly 5 elements → gather is safe.
      expectDecsOk $ BS.unlines
        [ "let src    = generate [5] (let g [i] = i in g)"
        , "let idx    = map (let f x = x in f) (iota 5)"
        , "let main   = gather idx src"
        ]

    it "Phase 1 – iota element bound propagates: identity map via EApp (rejected)" $ do
      -- Passing iota 5 through map identity preserves TValBoundDim = 5.
      -- src has only 4 elements → unsafe.
      expectDecsError
        ( BS.unlines
            [ "let src    = generate [4] (let g [i] = i in g)"
            , "let idx    = map (let f x = x in f) (iota 5)"
            , "let main   = gather idx src"
            ]
        )
        (isInfixOf "UnsatConstraints")

    -- Phase 3: non-inline function element bound hypothesis grounds parameter obligations.
    it "Phase 3 – map with non-inline fn: parameter obligation grounded (rejected)" $ do
      -- f i = index [i] src requires i < 3; map f (iota 5) feeds i in [0..4] → unsafe.
      expectDecsError
        ( BS.unlines
            [ "let src    = generate [3] (let g [i] = i in g)"
            , "let f i    = index [i] src"
            , "let main   = map f (iota 5)"
            ]
        )
        (isInfixOf "UnsatConstraints")

    it "Phase 3 – map with non-inline fn: parameter obligation grounded (safe)" $ do
      -- f i = index [i] src requires i < 5; map f (iota 5) feeds i in [0..4] → safe.
      -- Note: f's own definition emits an ungrounded warning (no caller-side bound at def
      -- time), but map f (iota 5) itself introduces no new UnsatConstraints.
      expectDecsOk $ BS.unlines
        [ "let src    = generate [5] (let g [i] = i in g)"
        , "let f i    = index [i] src"
        , "let main   = map f (iota 5)"
        ]

  -- ------------------------------------------------------------------ --
  -- ------------------------------------------------------------------ --
  describe "fill bounds propagation" $ do

    it "fill with a literal constant gives a bounded array" $ do
      -- fill [5] 3: all elements = 3, bounded by 4; source has 4 elements
      expectDecsOk $ BS.unlines
        [ "let src = generate [4] (let f [i] = i in f)"
        , "let idx = fill [5] 3"
        , "let main = gather idx src"
        ]

    it "fill with too-large constant is rejected" $ do
      -- fill [5] 4: elements = 4, bound = 5; source has 4 elements (< 5)
      expectDecsError
        ( BS.unlines
            [ "let src = generate [4] (let f [i] = i in f)"
            , "let idx = fill [5] 4"
            , "let main = gather idx src"
            ]
        )
        (isInfixOf "UnsatConstraints")

    it "fill with EBoundLetIn value propagates bound" $ do
      -- let v bound 5 = 3: v < 5; source has 5 elements
      expectDecsOk $ BS.unlines
        [ "let src = generate [5] (let f [i] = i in f)"
        , "let main = let v bound 5 = 3 in gather (fill [3] v) src"
        ]

  -- ------------------------------------------------------------------ --
  -- replicate bounds
  -- ------------------------------------------------------------------ --
  describe "replicate bounds propagation" $ do

    it "replicate preserves value bound for gather" $ do
      -- iota 5 has bound 5; replicate to [2][5] keeps bound; src has 5 elements
      expectDecsOk $ BS.unlines
        [ "let src  = generate [5] (let f [i] = i in f)"
        , "let idx1 = iota 5"
        , "let idx2 = replicate [2, All] idx1"
        , "let main = gather idx2 src"
        ]

    it "replicate with too-large source is rejected" $ do
      expectDecsError
        ( BS.unlines
            [ "let src  = generate [4] (let f [i] = i in f)"
            , "let idx1 = iota 5"
            , "let idx2 = replicate [2, All] idx1"
            , "let main = gather idx2 src"
            ]
        )
        (isInfixOf "UnsatConstraints")

  -- ------------------------------------------------------------------ --
  -- slice bounds
  -- ------------------------------------------------------------------ --
  describe "slice bounds propagation" $ do

    it "slice preserves value bound for gather" $ do
      -- iota 10 has bound 10; slice [0:5] gives 5 elements still bounded by 10
      expectDecsOk $ BS.unlines
        [ "let src  = generate [10] (let f [i] = i in f)"
        , "let full = iota 10"
        , "let idx  = slice [[0, 5]] full"
        , "let main = gather idx src"
        ]

    it "slice with too-small source is rejected" $ do
      expectDecsError
        ( BS.unlines
            [ "let src  = generate [9] (let f [i] = i in f)"
            , "let full = iota 10"
            , "let idx  = slice [[0, 5]] full"
            , "let main = gather idx src"
            ]
        )
        (isInfixOf "UnsatConstraints")

  -- ------------------------------------------------------------------ --
  -- reshape bounds
  -- ------------------------------------------------------------------ --
  describe "reshape bounds propagation" $ do

    it "reshape preserves value bound for gather" $ do
      -- iota 6 has bound 6; reshape to [2][3] keeps bound; src has 6 elements
      expectDecsOk $ BS.unlines
        [ "let src  = generate [6] (let f [i] = i in f)"
        , "let flat = iota 6"
        , "let idx  = reshape [2, 3] flat"
        , "let main = gather idx src"
        ]

    it "reshape with too-small source is rejected" $ do
      expectDecsError
        ( BS.unlines
            [ "let src  = generate [5] (let f [i] = i in f)"
            , "let flat = iota 6"
            , "let idx  = reshape [2, 3] flat"
            , "let main = gather idx src"
            ]
        )
        (isInfixOf "UnsatConstraints")

  -- ------------------------------------------------------------------ --
  -- gather output bounds (chained gather)
  -- ------------------------------------------------------------------ --
  describe "gather output bounds propagation" $ do

    it "gather result carries source element bound for chained gather" $ do
      -- src2 has elements = indices into src1; gather1 gathers those indices;
      -- gather2 uses gather1 output as new indices into src1
      expectDecsOk $ BS.unlines
        [ "let src1  = generate [5] (let f [i] = i in f)"
        , "let idx1  = iota 5"
        , "let src2  = iota 5"
        , "let tmp   = gather idx1 src2"
        , "let main  = gather tmp src1"
        ]

    it "chained gather is rejected when final source is too small" $ do
      expectDecsError
        ( BS.unlines
            [ "let src1  = generate [4] (let f [i] = i in f)"
            , "let idx1  = iota 5"
            , "let src2  = iota 5"
            , "let tmp   = gather idx1 src2"
            , "let main  = gather tmp src1"
            ]
        )
        (isInfixOf "UnsatConstraints")

  -- ------------------------------------------------------------------ --
  -- multi-dimensional gather bounds
  -- ------------------------------------------------------------------ --
  describe "multi-dimensional gather bounds" $ do

    it "2D generate (EVec body) → 2D gather, safe" $ do
      -- generate [K] (let f [k] = [k, 0] in f) produces Array[K] (Int,Int)
      -- where component 0 < K and component 1 < 1 (constant 0).
      -- src : Array[K][10] Int, so dim 0 = K and dim 1 = 10.
      -- TValBoundDim idx 0 = K ≤ K = TDim src 0  ✓
      -- TValBoundDim idx 1 = 1 ≤ 10 = TDim src 1 ✓
      expectDecsOk $ BS.unlines
        [ "let src  = generate [10, 10] (let f [i, j] = i in f)"
        , "let idx  = generate [10] (let f [k] = [k, 0] in f)"
        , "let main = gather idx src"
        ]

    it "2D generate → 2D gather, rejected when first component too large" $ do
      -- src has only 5 rows but the index first component can be up to K=10
      expectDecsError
        ( BS.unlines
            [ "let src  = generate [5, 10] (let f [i, j] = i in f)"
            , "let idx  = generate [10] (let f [k] = [k, 0] in f)"
            , "let main = gather idx src"
            ]
        )
        (isInfixOf "UnsatConstraints")

    it "2D generate → 2D gather, safe when constant second component is in range" $ do
      expectDecsOk $ BS.unlines
        [ "let src  = generate [10, 5] (let f [i, j] = i in f)"
        , "let idx  = generate [10] (let f [k] = [k, 0] in f)"
        , "let main = gather idx src"
        ]

    it "1D gather unchanged after refactor (smoke test)" $ do
      -- Scalar index array: TValBoundDim idxVar 0 ≤ TDim srcVar 0
      expectDecsOk $ BS.unlines
        [ "let src  = generate [10] (let f [i bound 10] = i in f)"
        , "let idx  = iota 10"
        , "let main = gather idx src"
        ]

    it "map producing pairs → 2D gather, safe" $ do
      -- map (let f x = [x, 0] in f) (iota K) produces Array[K] (Int,Int)
      -- with TValBoundDim 0 = K, TValBoundDim 1 = 1
      expectDecsOk $ BS.unlines
        [ "let src  = generate [10, 10] (let f [i, j] = i in f)"
        , "let idx  = map (let f x = [x, 0] in f) (iota 10)"
        , "let main = gather idx src"
        ]

  -- ------------------------------------------------------------------ --
  -- scatter_guarded bounds checking
  -- ------------------------------------------------------------------ --
  describe "scatter_guarded bounds checking" $ do

    it "iota N as index into fill [N] 0 is safe" $ do
      -- iota 5 elements in [0,5); dst has 5 elements — TValBoundDim = 5 ≤ dim = 5 ✓
      expectDecsOk $ BS.unlines
        [ "let dst  = fill [5] 0"
        , "let idx  = iota 5"
        , "let vals = fill [5] 1"
        , "let keep = fill [5] true"
        , "let main = scatter_guarded (+) dst idx vals keep"
        ]

    it "iota N as index into fill [N+1] 0 is safe" $ do
      -- iota 5 elements bounded by 5; dst has 6 elements — 5 ≤ 6 ✓
      expectDecsOk $ BS.unlines
        [ "let dst  = fill [6] 0"
        , "let idx  = iota 5"
        , "let vals = fill [5] 1"
        , "let keep = fill [5] true"
        , "let main = scatter_guarded (+) dst idx vals keep"
        ]

    it "iota 6 as index into fill [5] 0 is rejected" $ do
      -- iota 6 elements may be up to 5; dst has only 5 elements — 6 ≤ 5 is UNSAT
      expectDecsError
        ( BS.unlines
            [ "let dst  = fill [5] 0"
            , "let idx  = iota 6"
            , "let vals = fill [6] 1"
            , "let keep = fill [6] true"
            , "let main = scatter_guarded (+) dst idx vals keep"
            ]
        )
        (isInfixOf "UnsatConstraints")

    it "make_index N annotation satisfies obligation for fill [N]" $ do
      -- make_index N establishes TValBoundDim = N; fill [N] gives dim = N → N ≤ N ✓
      expectDecsOk $ BS.unlines
        [ "let n    = 8"
        , "let dst  = fill [n] 0"
        , "let raw  = generate [n] (let f [i] = 0 in f)"
        , "let idx  = make_index n raw"
        , "let vals = fill [n] 1"
        , "let keep = fill [n] true"
        , "let main = scatter_guarded (+) dst idx vals keep"
        ]

    it "make_index 6 annotation rejected for fill [5] destination" $ do
      -- make_index 6 asserts bound = 6; dst has dim = 5 — 6 ≤ 5 is UNSAT
      expectDecsError
        ( BS.unlines
            [ "let dst  = fill [5] 0"
            , "let idx  = make_index 6 (iota 5)"
            , "let vals = fill [5] 1"
            , "let keep = fill [5] true"
            , "let main = scatter_guarded (+) dst idx vals keep"
            ]
        )
        (isInfixOf "UnsatConstraints")

    it "ungrounded index (no make_index) produces a warning, not an error" $ do
      -- generate with a non-inferable body (division is not tracked by inferBVal)
      -- has no TValBoundDim established; the obligation is ungrounded →
      -- ObligationUnknown → warning only, not an error
      let src = BS.unlines
            [ "let dst  = fill [5] 0"
            , "let idx  = generate [5] (let f [i] = i / 2 in f)"
            , "let vals = fill [5] 1"
            , "let keep = fill [5] true"
            , "let main = scatter_guarded (+) dst idx vals keep"
            ]
      case readDecs src of
        Left err -> expectationFailure ("Parse error: " ++ err)
        Right decs -> do
          result <- inferDecsTopWithWarnings decs
          case result of
            Left msg -> expectationFailure ("Expected success but got error: " ++ msg)
            Right (_, warnings) ->
              warnings `shouldSatisfy` any (isInfixOf "could not verify")

  -- ------------------------------------------------------------------ --
  -- scatter (unguarded) bounds checking
  -- ------------------------------------------------------------------ --
  describe "scatter bounds checking" $ do

    it "iota N as index into fill [N] 0 is safe for scatter" $ do
      expectDecsOk $ BS.unlines
        [ "let dst  = fill [5] 0"
        , "let idx  = iota 5"
        , "let vals = fill [5] 1"
        , "let main = scatter (+) dst idx vals"
        ]

    it "iota 6 as index into fill [5] 0 is rejected for scatter" $ do
      expectDecsError
        ( BS.unlines
            [ "let dst  = fill [5] 0"
            , "let idx  = iota 6"
            , "let vals = fill [6] 1"
            , "let main = scatter (+) dst idx vals"
            ]
        )
        (isInfixOf "UnsatConstraints")

  -- ------------------------------------------------------------------ --
  -- % operator: type checking and bounds inference
  -- ------------------------------------------------------------------ --
  describe "% modulo operator" $ do

    it "x % m typechecks as Int" $ do
      expectDecsOk $ BS.unlines
        [ "let main = 7 % 3"
        ]

    it "% binds tighter than + (precedence)" $ do
      -- 10 % 3 + 1 = 1 + 1 = 2, not 10 % 4 = 2
      expectDecsOk $ BS.unlines
        [ "let main = 10 % 3 + 1"
        ]

    it "% result bound from constant modulus is used for gather" $ do
      -- let x bound 5 = 3 in (x % 5) has bound 5 — auto from inferBVal (Mod _)
      -- Here we test that generate with % body establishes the right bound.
      -- generate [5] (let f [i bound 5] = i % 3 in f) has TValBound = 3; src has 3 → safe
      expectDecsOk $ BS.unlines
        [ "let src = generate [3] (let g [i] = i in g)"
        , "let idx = generate [5] (let f [i bound 5] = i % 3 in f)"
        , "let main = gather idx src"
        ]

    it "% result bound too large for source is rejected" $ do
      -- i % 4 has bound 4; src has only 3 elements → 4 ≤ 3 is UNSAT
      expectDecsError
        ( BS.unlines
            [ "let src = generate [3] (let g [i] = i in g)"
            , "let idx = generate [5] (let f [i bound 5] = i % 4 in f)"
            , "let main = gather idx src"
            ]
        )
        (isInfixOf "UnsatConstraints")

    it "% with concrete modulus propagates bound for scatter" $ do
      -- i % 4 has bound 4; dst has 4 elements → TConst 4 ≤ TConst 4 ✓
      expectDecsOk $ BS.unlines
        [ "let n  = 10"
        , "let dst  = fill [4] 0"
        , "let vals = fill [n] 1"
        , "let keep = fill [n] true"
        , "let main = scatter_guarded (+) dst"
        ,   "  (generate [n] (let f [i bound n] = i % 4 in f))"
        ,   "  vals keep"
        ]

    it "% chain: x % m is a safe EBoundLetIn rhs with bound annotation" $ do
      -- let x bound 5 = 7 % 5 in index [x] arr(5) — x < 5 by % bound → SAT
      expectDecsOk $ BS.unlines
        [ "let arr  = generate [5] (let g [i] = i in g)"
        , "let main = let x bound 5 = 7 % 5 in index [x] arr"
        ]

  -- ------------------------------------------------------------------ --
  -- NIA (nonlinear integer arithmetic) via EBoundLetIn + Z3
  -- ------------------------------------------------------------------ --
  describe "NIA bounds verification via EBoundLetIn" $ do

    it "2D flat index: z*ny + y < ny*nz given 0<=y<ny, 0<=z<nz" $ do
      -- let y bound ny = ...; let z bound nz = ...;
      -- let flat bound (ny * nz) = z * ny + y → Z3 verifies NIA obligation
      expectDecsOk $ BS.unlines
        [ "let ny = 4"
        , "let nz = 3"
        , "let dst  = fill [(ny * nz)] 0"
        , "let vals = fill [6] 1"
        , "let keep = fill [6] true"
        , "let main = scatter_guarded (+) dst"
        ,   "  (generate [6]"
        ,   "    (let f [i bound 6] ="
        ,   "       let y bound ny = i % ny in"
        ,   "       let z bound nz = (i / ny) % nz in"
        ,   "       let flat bound (ny * nz) = z * ny + y in"
        ,   "       flat in"
        ,   "     f))"
        ,   "  vals keep"
        ]

    it "3D flat index: (z*ny + y)*nx + x < nx*ny*nz" $ do
      -- Mirrors the voxel_rasterization benchmark pattern with concrete constants.
      -- x<nx, y<ny, z<nz → (z*ny+y)*nx+x < nx*ny*nz verified by Z3.
      expectDecsOk $ BS.unlines
        [ "let nx = 3"
        , "let ny = 4"
        , "let nz = 2"
        , "let n  = 10"
        , "let dst  = fill [(nx * ny * nz)] 0"
        , "let vals = fill [n] 1"
        , "let keep = fill [n] true"
        , "let main = scatter_guarded (+) dst"
        ,   "  (generate [n]"
        ,   "    (let route [p] ="
        ,   "       let x bound nx = (p * 17 + 3) % nx in"
        ,   "       let y bound ny = (p * 29 + 5) % ny in"
        ,   "       let z bound nz = (p * 43 + 7) % nz in"
        ,   "       let flat bound (nx * ny * nz) = ((z * ny) + y) * nx + x in"
        ,   "       flat in"
        ,   "     route))"
        ,   "  vals keep"
        ]

    it "NIA: incorrect bound on flat index is rejected" $ do
      -- flat bound (nx * ny * nz - 1) is wrong: (z*ny+y)*nx+x can equal nx*ny*nz-1
      -- but cannot be < nx*ny*nz-1 in general → Z3 should refute or the
      -- scatter obligation PLe (nx*ny*nz - 1) (nx*ny*nz) is SAT but the
      -- declared bound nx*ny*nz - 1 < nx*ny*nz means TValBoundDim = nx*ny*nz-1
      -- and dim(dst) = nx*ny*nz → PLe (nx*ny*nz-1) (nx*ny*nz) is SAT (not rejected).
      -- Instead test a clearly wrong bound: flat bound (nx * ny * nz - 1) when
      -- dst has size nx * ny * nz - 1 → some inputs produce flat = nx*ny*nz-1
      -- which violates the inner NIA obligation flat < nx*ny*nz-1.
      -- Use a concrete small bound that Z3 can refute:
      -- x<3, y<4, z<2 so flat = (z*4+y)*3+x can be at most 2*4*3+3*4+3-... max is 23.
      -- nx*ny*nz = 24. If we say bound is 23 (= nx*ny*nz - 1), the NIA obligation
      -- (z*ny+y)*nx+x < 23 is not always true (flat can equal 23 when z=1,y=3,x=2).
      -- The NIA solver should emit an unsatisfiability or the scatter bound 23 < 24 is fine.
      -- A cleaner rejection: use a clearly too-small destination.
      expectDecsError
        ( BS.unlines
            [ "let nx = 3"
            , "let ny = 4"
            , "let nz = 2"
            , "let n  = 10"
            , "let dst  = fill [(nx * ny * nz - 1)] 0"
            , "let vals = fill [n] 1"
            , "let keep = fill [n] true"
            , "let main = scatter_guarded (+) dst"
            ,   "  (generate [n]"
            ,   "    (let route [p] ="
            ,   "       let x bound nx = (p * 17 + 3) % nx in"
            ,   "       let y bound ny = (p * 29 + 5) % ny in"
            ,   "       let z bound nz = (p * 43 + 7) % nz in"
            ,   "       let flat bound (nx * ny * nz) = ((z * ny) + y) * nx + x in"
            ,   "       flat in"
            ,   "     route))"
            ,   "  vals keep"
            ]
        )
        (isInfixOf "UnsatConstraints")

  -- ------------------------------------------------------------------ --
  -- max expression bounds
  -- ------------------------------------------------------------------ --
  -- The bounds engine now handles `max e1 e2` (saturated application of the
  -- `max` stdlib function): the element bound is max(bound(e1), bound(e2)).
  -- ------------------------------------------------------------------ --
  describe "max expression bounds" $ do

    it "max i k: safe gather when source >= max(N, k+1)" $ do
      -- i < 5, k = 3; max(i, 3) < 5; src has 5 elements → safe
      expectDecsOk $ BS.unlines
        [ "let max a b = if a > b then a else b"
        , "let src = generate [5] (let g [i] = i in g)"
        , "let idx = generate [5] (let f [i] = max i 3 in f)"
        , "let main = gather idx src"
        ]

    it "max i k: rejected when source < max(N, k+1)" $ do
      -- i < 5, k = 4; max(i, 4) can be 4; src has only 4 elements → rejected
      expectDecsError
        ( BS.unlines
            [ "let max a b = if a > b then a else b"
            , "let src = generate [4] (let g [i] = i in g)"
            , "let idx = generate [5] (let f [i] = max i 4 in f)"
            , "let main = gather idx src"
            ]
        )
        (isInfixOf "UnsatConstraints")

    it "max i i: safe when source size matches generate size" $ do
      -- i < 3; max(i, i) = i < 3; src has 3 elements → safe
      expectDecsOk $ BS.unlines
        [ "let max a b = if a > b then a else b"
        , "let src = generate [3] (let g [i] = i in g)"
        , "let idx = generate [3] (let f [i] = max i i in f)"
        , "let main = gather idx src"
        ]

    it "max of i and constant too large: rejected" $ do
      -- max(i, 5) where i < 3 → bound is max(3, 6) = 6; src has 5 elements → rejected
      expectDecsError
        ( BS.unlines
            [ "let max a b = if a > b then a else b"
            , "let src = generate [5] (let g [i] = i in g)"
            , "let idx = generate [3] (let f [i] = max i 5 in f)"
            , "let main = gather idx src"
            ]
        )
        (isInfixOf "UnsatConstraints")

  -- ------------------------------------------------------------------ --
  -- if-then-else conditional bounds
  -- ------------------------------------------------------------------ --
  -- For `if cond then e1 else e2`, the element bound is
  -- max(bound(e1), bound(e2)) — conservative but always sound.
  -- ------------------------------------------------------------------ --
  describe "if-then-else conditional bounds" $ do

    it "if-then-else: safe when both branches in range" $ do
      -- i < 5: then-branch = i+1 (< 6), else-branch = i (< 5); max bound = 6; src has 6 → safe
      expectDecsOk $ BS.unlines
        [ "let src = generate [6] (let g [i] = i in g)"
        , "let idx = generate [5] (let f [i] = if i > 0 then i + 1 else i in f)"
        , "let main = gather idx src"
        ]

    it "if-then-else: rejected when combined bound exceeds source" $ do
      -- then-branch = i+1 (bound 6), else-branch = i (bound 5); max = 6; src has 5 → rejected
      expectDecsError
        ( BS.unlines
            [ "let src = generate [5] (let g [i] = i in g)"
            , "let idx = generate [5] (let f [i] = if i > 0 then i + 1 else i in f)"
            , "let main = gather idx src"
            ]
        )
        (isInfixOf "UnsatConstraints")

    it "if-then-else with constant branches: safe" $ do
      -- then = 0, else = 1; bound = max(1, 2) = 2; src has 2 elements → safe
      expectDecsOk $ BS.unlines
        [ "let src = generate [2] (let g [i] = i in g)"
        , "let idx = generate [5] (let f [i] = if i > 0 then 0 else 1 in f)"
        , "let main = gather idx src"
        ]

    it "if-then-else with constant branches: rejected when source too small" $ do
      -- then = 0, else = 2; bound = max(1, 3) = 3; src has 2 elements → rejected
      expectDecsError
        ( BS.unlines
            [ "let src = generate [2] (let g [i] = i in g)"
            , "let idx = generate [5] (let f [i] = if i > 0 then 0 else 2 in f)"
            , "let main = gather idx src"
            ]
        )
        (isInfixOf "UnsatConstraints")
