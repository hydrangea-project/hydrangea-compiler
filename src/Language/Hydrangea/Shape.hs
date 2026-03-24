{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: Language.Hydrangea.Shape
--
-- Helper utilities for shape/index operations using a snoc-list (trailing-axis)
-- interpretation: shapes are lists of integers where the rightmost element is
-- the innermost (fastest-varying) axis. These helpers are intentionally pure
-- and do not depend on runtime Value/Interpreter types.
module Language.Hydrangea.Shape
  ( Shape
  , shapeInit
  , shapeLast
  , generateIndicesRowMajor
  , computeOffsetRowMajor
  ) where

-- | Row-major array shape represented as a list of extents.
type Shape = [Integer]

-- | Drop the trailing/rightmost axis from a shape. For empty shapes returns [].
shapeInit :: Shape -> Shape
shapeInit [] = []
shapeInit s = init s

-- | Return the trailing/rightmost axis extent. For empty shapes returns 0.
shapeLast :: Shape -> Integer
shapeLast [] = 0
shapeLast s = last s

-- | Generate all valid index vectors for a shape in row-major order
-- (trailing/rightmost axis varies fastest).
generateIndicesRowMajor :: Shape -> [[Integer]]
generateIndicesRowMajor [] = [[]]
generateIndicesRowMajor (d:ds) = [i : rest | i <- [0 .. d - 1], rest <- generateIndicesRowMajor ds]

-- | Compute flat offset from an index in row-major (trailing-fastest) order.
-- Returns `Left` with an error message for mismatched ranks or out-of-bounds.
computeOffsetRowMajor :: Shape -> [Integer] -> Either String Int
computeOffsetRowMajor shape idx
  | length shape /= length idx = Left $ "Index dimension mismatch: shape has " ++ show (length shape) ++ " dimensions, index has " ++ show (length idx)
  | not (all (>= 0) shape) = Left "Invalid shape: negative dimension"
  | not (all (>= 0) idx) = Left "Index out of bounds"
  | not (all (\(i,s) -> i < s) (zip idx shape)) = Left "Index out of bounds"
  | otherwise = Right $ sum [fromInteger (idx !! i * product (drop (i + 1) shape)) | i <- [0 .. length shape - 1]]
