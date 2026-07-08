-- | Shared assertion combinators for pass/codegen specs.
--
-- Prefer these over ad-hoc @isInfixOf@ chains so tests pin the tokens that are
-- actually the contract (ABI names, pragmas) and tolerate incidental spacing /
-- interleaving. For pass output, prefer matching the IR ADT over any string
-- match at all.
module Language.Hydrangea.TestUtil
  ( containsAll
  , containsNone
  , containsInOrder
  ) where

import Data.List (isInfixOf, isPrefixOf, tails)

-- | All needles appear somewhere in the haystack (order-independent).
containsAll :: [String] -> String -> Bool
containsAll needles hay = all (`isInfixOf` hay) needles

-- | None of the needles appear.
containsNone :: [String] -> String -> Bool
containsNone needles hay = not (any (`isInfixOf` hay) needles)

-- | Each needle appears (as a substring) in the given order, but not
-- necessarily adjacent. Tolerant to interleaved/reformatted output between
-- the anchors.
containsInOrder :: [String] -> String -> Bool
containsInOrder [] _ = True
containsInOrder (n : ns) hay =
  case [ drop (length n) t | t <- tails hay, n `isPrefixOf` t ] of
    (rest : _) -> containsInOrder ns rest
    []         -> False
