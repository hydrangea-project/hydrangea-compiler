{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.WherePrecondSpec
--
-- Tests for where-clause refinement annotations on top-level declarations.
--
-- Where clauses let a single-argument pair function declare a precondition on
-- its argument components, e.g.
--
-- > let my_gather (idx, src) where elem idx <= dim src 0 = gather idx src
--
-- The body is type-checked under the hypothesis, and call sites with pair
-- literals receive proof obligations.
module Language.Hydrangea.WherePrecondSpec (spec) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List (isInfixOf)
import Language.Hydrangea.Frontend (inferDecsTop, inferDecsTopWithWarnings, readDecs)
import Test.Hspec

-- | Parse and type-check a sequence of top-level declarations.
checkDecs :: ByteString -> IO (Either String ())
checkDecs src =
  case readDecs src of
    Left err   -> pure (Left ("Parse error: " ++ err))
    Right decs -> do
      result <- inferDecsTop decs
      pure $ case result of
        Left msg -> Left msg
        Right _  -> Right ()

checkDecsWarnings :: ByteString -> IO (Either String ((), [String]))
checkDecsWarnings src =
  case readDecs src of
    Left err   -> pure (Left ("Parse error: " ++ err))
    Right decs -> do
      result <- inferDecsTopWithWarnings decs
      pure $ case result of
        Left msg          -> Left msg
        Right (_, warns)  -> Right ((), warns)

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

expectDecsWarning :: ByteString -> (String -> Bool) -> Expectation
expectDecsWarning src predicate = do
  result <- checkDecsWarnings src
  case result of
    Left msg          -> expectationFailure ("Expected success+warning but got error: " ++ msg)
    Right ((), warns) ->
      case filter predicate warns of
        [] -> expectationFailure $
          "Expected a warning matching predicate but got: " ++ show warns
        _  -> pure ()

spec :: Spec
spec = describe "where-clause refinement annotations" $ do

  describe "pair pattern binding" $ do

    it "accepts a function with a PPair pattern and no where clause" $ do
      expectDecsOk
        "let swap (a, b) = (b, a)"

    it "accepts a function with a PPair pattern and a simple where clause" $ do
      expectDecsOk
        "let my_gather (idx, src) where elem idx <= dim src 0 = gather idx src"

    it "parses the bound shorthand" $ do
      expectDecsOk
        "let at (n, arr) where bound n (dim arr 0) = gather (iota n) arr"

    it "parses conjunctive where clauses" $ do
      expectDecsOk
        "let at2 (n, arr) where 0 <= n & n <= dim arr 0 = gather (iota n) arr"

  describe "body hypothesis: gather-wrapper" $ do

    -- The classic motivating example: the body's gather obligation should be
    -- discharged under the where-clause hypothesis.
    it "verifies the body of my_gather under the where hypothesis" $ do
      expectDecsOk
        "let my_gather (idx, src) where elem idx <= dim src 0 = gather idx src"

    it "verifies at under the bound hypothesis" $ do
      expectDecsOk
        "let at (n, arr) where bound n (dim arr 0) = gather (iota n) arr"

  describe "call-site obligation emission" $ do

    it "discharges obligations at a safe call site (iota vs generate)" $ do
      -- iota 5 produces values in [0,5), generate [10] ... has dim 10; 5 <= 10
      expectDecsOk $ BS.unlines
        [ "let my_gather (idx, src) where elem idx <= dim src 0 = gather idx src"
        , "let main = my_gather (iota 5, generate [10] (let f [i] = i in f))"
        ]

    it "reports an error at an unsafe call site (iota too large)" $ do
      expectDecsError
        ( BS.unlines
            [ "let my_gather (idx, src) where elem idx <= dim src 0 = gather idx src"
            , "let main = my_gather (iota 15, generate [10] (let f [i] = i in f))"
            ]
        )
        (const True)

    it "discharges at obligation at a safe call site" $ do
      expectDecsOk $ BS.unlines
        [ "let at (n, arr) where bound n (dim arr 0) = gather (iota n) arr"
        , "let main = at (7, generate [20] (let f [i] = i in f))"
        ]

    it "reports error for out-of-bounds at call" $ do
      expectDecsError
        ( BS.unlines
            [ "let at (n, arr) where bound n (dim arr 0) = gather (iota n) arr"
            , "let main = at (25, generate [20] (let f [i] = i in f))"
            ]
        )
        (const True)

    -- Exact-bound calls: vbound == dim should be accepted (<=, not <)
    it "accepts iota n into generate [n] (exact bound, elem <= dim)" $ do
      expectDecsOk $ BS.unlines
        [ "let my_gather (idx, src) where elem idx <= dim src 0 = gather idx src"
        , "let main = my_gather (iota 10, generate [10] (let f [i] = i in f))"
        ]

    it "reports error when iota is one over the source size" $ do
      expectDecsError
        ( BS.unlines
            [ "let my_gather (idx, src) where elem idx <= dim src 0 = gather idx src"
            , "let main = my_gather (iota 11, generate [10] (let f [i] = i in f))"
            ]
        )
        ("UnsatConstraints" `isInfixOf`)

    -- bound shorthand: 0 <= n && n < dim arr 0
    it "accepts at with index one below source size (bound shorthand)" $ do
      expectDecsOk $ BS.unlines
        [ "let at (n, arr) where bound n (dim arr 0) = gather (iota n) arr"
        , "let main = at (19, generate [20] (let f [i] = i in f))"
        ]

    it "reports error when bound index equals source size (off by one)" $ do
      -- bound n (dim arr 0) means n < dim, so n == dim is out of range
      expectDecsError
        ( BS.unlines
            [ "let at (n, arr) where bound n (dim arr 0) = gather (iota n) arr"
            , "let main = at (20, generate [20] (let f [i] = i in f))"
            ]
        )
        (const True)

    -- Zero-length iota is trivially safe
    it "accepts iota 0 into any non-empty array" $ do
      expectDecsOk $ BS.unlines
        [ "let my_gather (idx, src) where elem idx <= dim src 0 = gather idx src"
        , "let main = my_gather (iota 0, generate [5] (let f [i] = i in f))"
        ]

    -- Two independent where-clause functions in scope simultaneously
    it "accepts two where-clause functions used together safely" $ do
      expectDecsOk $ BS.unlines
        [ "let clip (idx, src) where elem idx <= dim src 0 = gather idx src"
        , "let take (n, arr)   where bound n (dim arr 0)   = gather (iota n) arr"
        , "let main = (clip (iota 3, generate [5] (let f [i] = i in f)),"
        , "            take (4, generate [10] (let g [i] = i in g)))"
        ]

    it "reports error when either of two where-clause calls is unsafe" $ do
      expectDecsError
        ( BS.unlines
            [ "let clip (idx, src) where elem idx <= dim src 0 = gather idx src"
            , "let take (n, arr)   where bound n (dim arr 0)   = gather (iota n) arr"
            , "let main = (clip (iota 6, generate [5] (let f [i] = i in f)),"
            , "            take (4, generate [10] (let g [i] = i in g)))"
            ]
        )
        (const True)

  -- Tests where sizes are symbolic (not known literal constants).
  -- The where clause provides the relational hypothesis that the solver needs.
  describe "symbolic size preconditions" $ do

    -- The outer where clause (n <= dim arr 0) provides exactly the hypothesis
    -- needed to discharge the inner where obligation for my_gather.
    it "accepts a delegation chain: outer where enables inner where obligation" $ do
      expectDecsOk $ BS.unlines
        [ "let inner (idx, src) where elem idx <= dim src 0 = gather idx src"
        , "let outer (n, arr)   where n <= dim arr 0 = inner (iota n, arr)"
        , "let main = outer (5, generate [10] (let f [i] = i in f))"
        ]

    it "reports error at unsafe call site in delegation chain" $ do
      expectDecsError
        ( BS.unlines
            [ "let inner (idx, src) where elem idx <= dim src 0 = gather idx src"
            , "let outer (n, arr)   where n <= dim arr 0 = inner (iota n, arr)"
            , "let main = outer (15, generate [10] (let f [i] = i in f))"
            ]
        )
        (const True)

    -- When the same symbolic n is used as both the generate size and the iota
    -- bound, the obligation n <= n is trivially dischargeable without a where
    -- clause on the caller.
    it "discharges n <= n trivially when symbolic n is used for both sizes" $ do
      expectDecsOk $ BS.unlines
        [ "let safe_take (n, arr) where n <= dim arr 0 = gather (iota n) arr"
        , "let fill_and_take n = safe_take (n, generate [n] (let f [i] = i in f))"
        , "let main = fill_and_take 5"
        ]

    -- The bound shorthand (0 <= n && n < dim arr 0) is strictly stronger than
    -- plain <=, so it also discharges a <= obligation in a delegated call.
    it "accepts delegation with bound shorthand implying the inner <= obligation" $ do
      expectDecsOk $ BS.unlines
        [ "let inner (idx, src) where elem idx <= dim src 0 = gather idx src"
        , "let outer (n, arr)   where bound n (dim arr 0) = inner (iota n, arr)"
        , "let main = outer (5, generate [10] (let f [i] = i in f))"
        ]

    it "reports error at unsafe call site with bound shorthand delegation" $ do
      expectDecsError
        ( BS.unlines
            [ "let inner (idx, src) where elem idx <= dim src 0 = gather idx src"
            , "let outer (n, arr)   where bound n (dim arr 0) = inner (iota n, arr)"
            , "let main = outer (10, generate [10] (let f [i] = i in f))"
            ]
        )
        (const True)

  -- A bare wrapper function (no where clause) cannot propagate component-level
  -- bounds to its callers: the pair components' refinement variables are stripped
  -- during unification, so the where-clause obligation inside the body becomes
  -- ungrounded.  The workaround is to add a where clause to the wrapper too.
  describe "indirect call: wrapper with its own where clause" $ do

    -- The workaround: give the wrapper its own where clause that matches the
    -- inner function's precondition.  Now the obligation is grounded at forward's
    -- call site and the unsafe call is caught.
    it "catches unsafe call through a wrapper that has its own where clause" $ do
      expectDecsError
        ( BS.unlines
            [ "let my_gather (idx, src) where elem idx <= dim src 0 = gather idx src"
            , "let forward (idx, src) where elem idx <= dim src 0 = my_gather (idx, src)"
            , "let main = forward (iota 15, generate [10] (let f [i] = i in f))"
            ]
        )
        (const True)

    it "accepts safe call through a wrapper that has its own where clause" $ do
      expectDecsOk $ BS.unlines
        [ "let my_gather (idx, src) where elem idx <= dim src 0 = gather idx src"
        , "let forward (idx, src) where elem idx <= dim src 0 = my_gather (idx, src)"
        , "let main = forward (iota 5, generate [10] (let f [i] = i in f))"
        ]

  -- A bare wrapper (no where clause on the wrapper itself) cannot ground the
  -- obligation coming from the inner function's body.  The WhereHyp predicates
  -- emitted inside the annotated function are NOT re-emitted at call sites, so
  -- the obligation becomes ungrounded → a "could not verify" warning is issued
  -- rather than a false-positive silent pass.
  describe "indirect call: bare wrapper without where clause" $ do

    it "issues a warning for a bare wrapper with an unsafe call" $ do
      -- The checker cannot verify safety (no path to ground the bounds), so it
      -- warns rather than silently passing.  This is case 3 of the three-case model
      -- (lacks information → warn), not case 1 (proved safe) as before the fix.
      expectDecsWarning
        ( BS.unlines
            [ "let my_gather (idx, src) where elem idx <= dim src 0 = gather idx src"
            , "let forward p = my_gather p"
            , "let main = forward (iota 15, generate [10] (let f [i] = i in f))"
            ]
        )
        ("could not verify" `isInfixOf`)

    it "issues a warning for a bare wrapper even with a safe call" $ do
      -- Without a where clause, the checker cannot distinguish safe from unsafe
      -- calls through the wrapper; it always warns (case 3).  Users should add
      -- a where clause to the wrapper to get precise checking.
      expectDecsWarning
        ( BS.unlines
            [ "let my_gather (idx, src) where elem idx <= dim src 0 = gather idx src"
            , "let forward p = my_gather p"
            , "let main = forward (iota 5, generate [10] (let f [i] = i in f))"
            ]
        )
        ("could not verify" `isInfixOf`)
