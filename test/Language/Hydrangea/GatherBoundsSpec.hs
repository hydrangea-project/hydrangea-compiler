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
  -- gather without established bounds (permissive / no false positives)
  -- ------------------------------------------------------------------ --
  describe "gather without established bounds" $ do

    it "gather with an untracked index array is accepted (permissive)" $ do
      -- generate with an arbitrary body has no TValBound established,
      -- so the gather check is silently skipped — no false positive.
      expectDecsOk $ BS.unlines
        [ "let src = generate [5] (let f [i] = i in f)"
        , "let idx = generate [5] (let f [i] = 4 - i in f)"
        , "let main = gather idx src"
        ]

    it "gather with unknown index array is accepted (permissive)" $ do
      -- No static knowledge about idx values — passes without error.
      expectDecsOk $ BS.unlines
        [ "let src = generate [10] (let f [i] = i * 2 in f)"
        , "let idx = generate [3]  (let f [i] = i * 3 in f)"
        , "let main = gather idx src"
        ]

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

    it "generate without PBound is permissive (no TValBound)" $ do
      -- No annotation — no TValBound established, gather is unchecked.
      expectDecsOk $ BS.unlines
        [ "let src = generate [3] (let g [i] = i in g)"
        , "let idx = generate [5] (let f [i] = i + 1 in f)"
        , "let main = gather idx src"
        ]

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
