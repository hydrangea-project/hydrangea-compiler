-- |
-- Module: Language.Hydrangea.Util
--
-- Small utilities shared across multiple Hydrangea modules.
module Language.Hydrangea.Util
  ( stripStringQuotes
  , unsnoc
  ) where

import qualified Data.ByteString.Lazy.Char8 as BS

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
