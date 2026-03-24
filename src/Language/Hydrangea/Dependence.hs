{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.Dependence
--
-- Dependence analysis operating on CFG index expressions. Conservative initial
-- implementation: exact equality and simple affine (i vs i + const) checks.
module Language.Hydrangea.Dependence
  ( ArrayAccess2(..)
  , AccessType2(..)
  , Dependence2(..)
  , DependenceDirection2(..)
  , findDependences2
  ) where

import Language.Hydrangea.CFG
import Control.Monad (guard)

-- | Kind of memory access performed at a CFG array reference.
data AccessType2 = Read2 | Write2 | ReadWrite2
  deriving (Eq, Show)

-- | Normalized array access used by dependence analysis.
data ArrayAccess2 = ArrayAccess2
  { aa2ArrayVar :: CVar
  , aa2Index :: IndexExpr
  , aa2AccessType :: AccessType2
  , aa2StmtIndex :: Int
  }
  deriving (Eq, Show)

-- | Direction classification for a dependence edge.
data DependenceDirection2 = DDForward | DDBackward | DDUnknown
  deriving (Eq, Show)

-- | Conservative dependence result between two accesses in one loop body.
data Dependence2 = Dependence2
  { depSource2 :: ArrayAccess2
  , depTarget2 :: ArrayAccess2
  , depDirection2 :: DependenceDirection2
  , depIsLoopCarried2 :: Bool
  , depDistance2 :: Maybe [Integer]
  }
  deriving (Eq, Show)

-- | High-level dependence finder for array accesses in a single loop body.
-- Conservative rules:
-- * identical index expressions -> forward
-- * simple affine difference on one iterator (i -> i + const) -> forward
-- * ND tuples compared element-wise
-- * otherwise unknown
findDependences2 :: [ArrayAccess2] -> [Dependence2]
findDependences2 accesses = do
  src <- accesses
  tgt <- accesses
  guard (aa2ArrayVar src == aa2ArrayVar tgt)
  guard (aa2StmtIndex src < aa2StmtIndex tgt)
  let (dir, dist) = analyzeIndex2 (aa2Index src) (aa2Index tgt)
      loopCarried = aa2StmtIndex src /= aa2StmtIndex tgt
  return Dependence2 { depSource2 = src, depTarget2 = tgt, depDirection2 = dir, depIsLoopCarried2 = loopCarried, depDistance2 = dist }

-- Analyze two IndexExprs conservatively.
analyzeIndex2 :: IndexExpr -> IndexExpr -> (DependenceDirection2, Maybe [Integer])
analyzeIndex2 a b
  | sa == sb = (DDForward, Just [0])
  | otherwise = analyzeAffine sa sb
  where
    sa = simplifyIndexExpr a
    sb = simplifyIndexExpr b

-- Analyze affine differences and return direction plus optional per-dimension distances.
analyzeAffine :: IndexExpr -> IndexExpr -> (DependenceDirection2, Maybe [Integer])
analyzeAffine (ITuple as) (ITuple bs)
  | length as == length bs =
      let results = zipWith analyzeAffineSingle as bs
      in if any (\r -> fst r == DDUnknown) results
           then (DDUnknown, Nothing)
           else
             case sequence (map snd results) of
               Nothing -> (DDUnknown, Nothing)
               Just dsLists ->
                 let ds = concat dsLists
                 in if all (>= 0) ds then (DDForward, Just ds)
                    else if all (<= 0) ds then (DDBackward, Just ds)
                    else (DDUnknown, Just ds)
  | otherwise = (DDUnknown, Nothing)
analyzeAffine x y = analyzeAffineSingle x y

-- Analyze a single-dimension pair and return optional distance vector (single element list).
analyzeAffineSingle :: IndexExpr -> IndexExpr -> (DependenceDirection2, Maybe [Integer])
analyzeAffineSingle x y =
  case (extractSingleVarAffine x, extractSingleVarAffine y) of
    (Just (vx, kx, cx), Just (vy, ky, cy)) | vx == vy && kx == ky ->
      let diff = cy - cx
      in if kx == 0
           then (DDUnknown, Nothing)
           else if diff `mod` kx /= 0
             then (DDUnknown, Nothing)
             else (if diff >= 0 then DDForward else DDBackward, Just [diff `div` kx])
    _ -> (DDUnknown, Nothing)

-- Try to extract a single-variable affine form: var * coeff + const
-- Returns (var, coeff, const) when successful.
extractSingleVarAffine :: IndexExpr -> Maybe (CVar, Integer, Integer)
extractSingleVarAffine ie =
  let s = simplifyIndexExpr ie
      -- collect additive terms
      collectAdd t = case t of
        IAdd a b -> let (ts, c) = collectAdd a; (us, d) = collectAdd b in (ts ++ us, c + d)
        IConst n -> ([], n)
        other -> ([other], 0)
      (terms, constSum) = collectAdd s
  in case terms of
    [] -> Nothing
    [t] -> case t of
      IVar v -> Just (v, 1, constSum)
      IMul (IConst k) (IVar v) -> Just (v, k, constSum)
      IMul (IVar v) (IConst k) -> Just (v, k, constSum)
      _ -> Nothing
    _ -> Nothing
