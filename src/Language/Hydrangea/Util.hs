-- |
-- Module: Language.Hydrangea.Util
--
-- Small utilities shared across multiple Hydrangea modules.
module Language.Hydrangea.Util
  ( stripStringQuotes
  , unsnoc
  , freshUnusedName
  ) where

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Set (Set)
import qualified Data.Set as S

-- | Find the first name @mk n@, @mk (n+1)@, … that is not already in @used@,
-- returning it together with the next counter to try.  The affix scheme is
-- supplied by the caller via @mk@.
freshUnusedName :: Ord a => (Int -> a) -> Set a -> Int -> (a, Int)
freshUnusedName mk used n
  | mk n `S.member` used = freshUnusedName mk used (n + 1)
  | otherwise            = (mk n, n + 1)

-- | Split a list into its initial segment and last element, or 'Nothing' when
-- empty.  (Local reimplementation to avoid a base >= 4.19 lower-bound bump.)
unsnoc :: [a] -> Maybe ([a], a)
unsnoc xs = case reverse xs of
  []     -> Nothing
  y : ys -> Just (reverse ys, y)

-- | Remove surrounding double-quote characters from a lexer-produced string
-- literal.  The input is returned unchanged if it does not begin and end with
-- @\"@, or if it is shorter than two characters.
stripStringQuotes :: BS.ByteString -> BS.ByteString
stripStringQuotes s
  | BS.length s >= 2 && BS.head s == '"' && BS.last s == '"' = BS.tail (BS.init s)
  | otherwise = s
