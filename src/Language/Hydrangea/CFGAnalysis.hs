{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.CFGAnalysis
--
-- Loop analysis for the canonical CFG loop representation.
--
-- This module performs light-weight static analyses used by CFG
-- optimisation and vectorisation passes. The analyses are intentionally
-- conservative: they produce easily-checkable information (trip-count
-- shapes, variable liveness sets and a simple vectorizability predicate)
-- that later passes use to decide safe transformations.
module Language.Hydrangea.CFGAnalysis
  ( -- * Loop Info
    LoopInfo2(..)
  , LoopBound2(..)
  , TripCount2(..)
  , isConstantTripCount2
  , isParallelTripCount2
  , loopTripCount2
  , tripCountValue2
    -- * Analysis
  , analyzeLoop2
  , analyzeStmts2
  , isVectorizableLoop2
    -- * Variable Analysis (re-exported for optimization passes)
  , definedVarsStmt2
  , definedVarsStmts2
  , usedVarsAtom2
  , usedVarsRHS2
  , usedVarsStmt2
  , usedVarsStmts2
  , usedVarsIndexExpr
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Language.Hydrangea.CFGCore (Atom(..), RHS(..))
import Language.Hydrangea.CFG

-- | Classification of a loop bound extracted from an @IndexExpr@.
--
-- * @LB2Constant n@ — constant trip count @n@.
-- * @LB2Var v@ — a variable-provided bound (e.g. function parameter).
-- * @LB2IndexExpr ie@ — a general index expression that requires further
--   analysis to extract trip-count structure.
data LoopBound2
  = LB2Constant Integer
  | LB2Var ByteString
  | LB2IndexExpr IndexExpr
  deriving (Eq, Show)

-- | Structured representation of a trip-count. The structure mirrors the
-- subset of @IndexExpr@ that can be interpreted as a (possibly
-- parametric) integer trip-count.
data TripCount2
  = TC2Unknown
  | TC2Constant Integer
  | TC2Var ByteString
  | TC2Mul TripCount2 TripCount2
  | TC2Add TripCount2 TripCount2
  deriving (Eq, Show)

-- | Light-weight summary of an analyzed loop.
--
-- @li2LoopVar@ is the first iterator name for the loop nest (most loops
-- use the first iterator as the primary induction variable). @li2Bounds@
-- records the per-dimension bounds as @LoopBound2@. @li2TripCount@ is a
-- derived trip-count for the (first) bound when possible. @li2DefinedVars@
-- and @li2UsedVars@ are the sets of variables defined/used in the loop
-- body and are used by optimisation passes.
data LoopInfo2 = LoopInfo2
  { li2LoopVar :: ByteString
  , li2Bounds :: [LoopBound2]
  , li2TripCount :: TripCount2
  , li2Body :: [Stmt]
  , li2DefinedVars :: Set ByteString
  , li2UsedVars :: Set ByteString
  , li2ExecPolicy :: ExecPolicy
  } deriving (Eq, Show)

classifyBound2 :: IndexExpr -> LoopBound2
classifyBound2 (IConst n) = LB2Constant n
classifyBound2 (IVar v) = LB2Var v
classifyBound2 ie = LB2IndexExpr ie

computeTripCount2 :: LoopBound2 -> TripCount2
computeTripCount2 (LB2Constant n) = TC2Constant n
computeTripCount2 (LB2Var v) = TC2Var v
computeTripCount2 (LB2IndexExpr ie) = tripCountFromIndexExpr ie

-- | Attempt to extract a @TripCount2@ from an @IndexExpr@ when the
-- expression is formed from the simple arithmetic subset we recognise.
tripCountFromIndexExpr :: IndexExpr -> TripCount2
tripCountFromIndexExpr = go . simplifyIndexExpr
  where
    go expr = case expr of
      IConst n -> TC2Constant n
      IVar v -> TC2Var v
      IMul a b -> mulTripCounts2 (go a) (go b)
      IAdd a b -> addTripCounts2 (go a) (go b)
      IDiv a b ->
        case (tripCountValue2 (go a), tripCountValue2 (go b)) of
          (Just x, Just y) | y /= 0 -> TC2Constant (x `div` y)
          -- Variable dividend divided by a positive constant (e.g. tile loop bounds
          -- like (n + 31) / 32): the quotient is still variable-sized.
          (Nothing, Just y) | y > 0, go a /= TC2Unknown -> go a
          _ -> TC2Unknown
      _ -> TC2Unknown

addTripCounts2 :: TripCount2 -> TripCount2 -> TripCount2
addTripCounts2 a b = fromMaybe (TC2Add a b) $ do
  x <- tripCountValue2 a
  y <- tripCountValue2 b
  pure (TC2Constant (x + y))

mulTripCounts2 :: TripCount2 -> TripCount2 -> TripCount2
mulTripCounts2 a b = fromMaybe (TC2Mul a b) $ do
  x <- tripCountValue2 a
  y <- tripCountValue2 b
  pure (TC2Constant (x * y))

-- | Predicate: is the trip-count statically a compile-time constant?
isConstantTripCount2 :: TripCount2 -> Bool
isConstantTripCount2 TC2Constant {} = True
isConstantTripCount2 (TC2Mul l r) = isConstantTripCount2 l && isConstantTripCount2 r
isConstantTripCount2 (TC2Add l r) = isConstantTripCount2 l && isConstantTripCount2 r
isConstantTripCount2 _ = False

-- | If the trip-count can be evaluated to an integer, return it.
-- This recursively evaluates @TC2Add@ / @TC2Mul@ when both sides are
-- constant; otherwise returns @Nothing@.
tripCountValue2 :: TripCount2 -> Maybe Integer
tripCountValue2 (TC2Constant n) = Just n
tripCountValue2 (TC2Mul l r) = do
  a <- tripCountValue2 l
  b <- tripCountValue2 r
  Just (a * b)
tripCountValue2 (TC2Add l r) = do
  a <- tripCountValue2 l
  b <- tripCountValue2 r
  Just (a + b)
tripCountValue2 _ = Nothing

loopTripCount2 :: LoopSpec -> TripCount2
loopTripCount2 spec =
  foldr mulTripCounts2 (TC2Constant 1) (map tripCountFromIndexExpr (lsBounds spec))

isParallelTripCount2 :: TripCount2 -> Bool
isParallelTripCount2 tc = case tripCountValue2 tc of
  Just n -> n >= 4
  Nothing -> case tc of
    TC2Var {} -> True
    TC2Add {} -> True
    TC2Mul {} -> True
    _ -> False

-- | Collect variables that are defined (assigned) by a statement.
definedVarsStmt2 :: Stmt -> Set ByteString
definedVarsStmt2 stmt = case stmt of
  SAssign v _ -> S.singleton v
  SArrayWrite (AVar arr) _ _ -> S.singleton arr
  SLoop _ body -> S.unions (map definedVarsStmt2 body)
  SIf _ thn els -> S.unions (map definedVarsStmt2 (thn ++ els))
  SReturn (AVar v) -> S.singleton v
  _ -> S.empty

-- | Defined variables across a sequence of statements.
definedVarsStmts2 :: [Stmt] -> Set ByteString
definedVarsStmts2 = S.unions . map definedVarsStmt2

-- | Variables used by an @Atom@.
usedVarsAtom2 :: Atom -> Set ByteString
usedVarsAtom2 (AVar v) = S.singleton v
usedVarsAtom2 (AVecVar v) = S.singleton v
usedVarsAtom2 _ = S.empty

-- | Variables referenced by a right-hand-side expression.
usedVarsRHS2 :: RHS -> Set ByteString
usedVarsRHS2 rhs = case rhs of
  RAtom a -> usedVarsAtom2 a
  RBinOp _ a1 a2 -> S.union (usedVarsAtom2 a1) (usedVarsAtom2 a2)
  RUnOp _ a -> usedVarsAtom2 a
  RTuple as -> S.unions (map usedVarsAtom2 as)
  RProj _ a -> usedVarsAtom2 a
  RRecord fields -> S.unions (map (usedVarsAtom2 . snd) fields)
  RRecordProj _ a -> usedVarsAtom2 a
  RArrayLoad a1 a2 -> S.union (usedVarsAtom2 a1) (usedVarsAtom2 a2)
  RArrayAlloc a -> usedVarsAtom2 a
  RArrayShape a -> usedVarsAtom2 a
  RShapeSize a -> usedVarsAtom2 a
  RShapeInit a -> usedVarsAtom2 a
  RShapeLast a -> usedVarsAtom2 a
  RFlatToNd a shp -> usedVarsAtom2 a `S.union` usedVarsAtom2 shp
  RNdToFlat a shp -> usedVarsAtom2 a `S.union` usedVarsAtom2 shp
  R2DToFlat a w -> usedVarsAtom2 a `S.union` usedVarsAtom2 w
  RCall _ args -> S.unions (map usedVarsAtom2 args)
  RVecLoad a1 a2 -> S.union (usedVarsAtom2 a1) (usedVarsAtom2 a2)
  RVecStore a1 a2 a3 -> S.unions [usedVarsAtom2 a1, usedVarsAtom2 a2, usedVarsAtom2 a3]
  RVecBinOp _ a1 a2 -> S.union (usedVarsAtom2 a1) (usedVarsAtom2 a2)
  RVecUnOp _ a -> usedVarsAtom2 a
  RVecSplat a -> usedVarsAtom2 a
  RVecReduce _ a -> usedVarsAtom2 a
  RPairMake _ _ a1 a2 -> S.union (usedVarsAtom2 a1) (usedVarsAtom2 a2)
  RPairFst _ a -> usedVarsAtom2 a
  RPairSnd _ a -> usedVarsAtom2 a


-- | Collect variables referenced by an 'IndexExpr'
usedVarsIndexExpr :: IndexExpr -> Set ByteString
usedVarsIndexExpr ie = case ie of
  IVar v -> S.singleton v
  IConst _ -> S.empty
  IAdd a b -> usedVarsIndexExpr a `S.union` usedVarsIndexExpr b
  ISub a b -> usedVarsIndexExpr a `S.union` usedVarsIndexExpr b
  IMul a b -> usedVarsIndexExpr a `S.union` usedVarsIndexExpr b
  IDiv a b -> usedVarsIndexExpr a `S.union` usedVarsIndexExpr b
  ITuple es -> S.unions (map usedVarsIndexExpr es)
  IProj _ e -> usedVarsIndexExpr e
  IFlatToNd a b -> usedVarsIndexExpr a `S.union` usedVarsIndexExpr b
  INdToFlat a b -> usedVarsIndexExpr a `S.union` usedVarsIndexExpr b
  ICall _ es -> S.unions (map usedVarsIndexExpr es)


-- | Variables referenced by a statement (reads + bounds used in loops).
usedVarsStmt2 :: Stmt -> Set ByteString
usedVarsStmt2 stmt = case stmt of
  SAssign _ rhs -> usedVarsRHS2 rhs
  SArrayWrite arr idx val -> S.unions [usedVarsAtom2 arr, usedVarsAtom2 idx, usedVarsAtom2 val]
  SLoop spec body ->
    let bodyVars = S.unions (map usedVarsStmt2 body)
        boundVars = S.unions (map usedVarsIndexExpr (lsBounds spec))
    in bodyVars `S.union` boundVars
  SIf cond thn els -> S.union (usedVarsAtom2 cond) (S.unions (map usedVarsStmt2 (thn ++ els)))
  SReturn a -> usedVarsAtom2 a

-- | Variables referenced across a sequence of statements.
usedVarsStmts2 :: [Stmt] -> Set ByteString
usedVarsStmts2 = S.unions . map usedVarsStmt2

-- | Analyse a single @SLoop@ statement and return a @LoopInfo2@ summary.
-- Returns @Nothing@ for non-loop statements.
analyzeLoop2 :: Stmt -> Maybe LoopInfo2
analyzeLoop2 (SLoop spec body) = case lsIters spec of
  [] -> Nothing
  (iter:_) -> Just $ LoopInfo2
    { li2LoopVar = iter
    , li2Bounds = map classifyBound2 (lsBounds spec)
    , li2TripCount = case lsBounds spec of
        (b:_) -> computeTripCount2 (classifyBound2 b)
        [] -> TC2Unknown
    , li2Body = body
    , li2DefinedVars = definedVarsStmts2 body
    , li2UsedVars = usedVarsStmts2 body
    , li2ExecPolicy = lsExec spec
    }
analyzeLoop2 _ = Nothing

-- | Recursively collect @LoopInfo2@ for all loops in a statement list.
-- This explores nested loops and `if` branches.
analyzeStmts2 :: [Stmt] -> [LoopInfo2]
analyzeStmts2 = concatMap go
  where
    go stmt = case analyzeLoop2 stmt of
      Just info -> info : analyzeStmts2 (li2Body info)
      Nothing -> case stmt of
        SIf _ thn els -> analyzeStmts2 thn ++ analyzeStmts2 els
        _ -> []

-- | Conservative predicate: is this loop a candidate for vectorisation?
-- The heuristic accepts fixed small-arity loops (constant trip counts >=4)
-- or parametric loops (variables) and some composed trip-count forms.
isVectorizableLoop2 :: LoopInfo2 -> Bool
isVectorizableLoop2 info = case li2TripCount info of
  TC2Constant n -> n >= 4
  TC2Var {} -> True
  TC2Mul l r -> isConstantTripCount2 l || isConstantTripCount2 r
  TC2Add l r -> isConstantTripCount2 l || isConstantTripCount2 r
  TC2Unknown -> False
