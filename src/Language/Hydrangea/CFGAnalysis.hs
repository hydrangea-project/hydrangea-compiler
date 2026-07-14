{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.CFGAnalysis
--
-- Conservative loop and variable analysis over the canonical CFG.
--
-- The helpers here summarize loop structure, estimate trip counts, and collect
-- defined/used variables for CFG statements. The results are intentionally
-- conservative: they are simple enough for later passes to trust when deciding
-- whether transformations such as vectorization, parallelization, or LICM are
-- safe to apply.
module Language.Hydrangea.CFGAnalysis
  ( -- * Loop Info
    LoopInfo(..)
  , LoopBound(..)
  , TripCount(..)
  , isConstantTripCount
  , isParallelTripCount
  , loopTripCount
  , tripCountValue
    -- * Analysis
  , analyzeLoop
  , analyzeStmts
  , isVectorizableLoop
    -- * Variable Analysis (re-exported for optimization passes)
  , definedVarsStmt
  , definedVarsStmts
  , usedVarsAtom
  , usedVarsRHS
  , usedVarsStmt
  , usedVarsStmts
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
-- * @LBConstant n@ — constant trip count @n@.
-- * @LBVar v@ — a variable-provided bound (e.g. function parameter).
-- * @LBIndexExpr ie@ — a general index expression that requires further
--   analysis to extract trip-count structure.
data LoopBound
  = LBConstant Integer
  | LBVar ByteString
  | LBIndexExpr IndexExpr
  deriving (Eq, Show)

-- | Structured representation of a trip-count. The structure mirrors the
-- subset of @IndexExpr@ that can be interpreted as a (possibly
-- parametric) integer trip-count.
data TripCount
  = TCUnknown
  | TCConstant Integer
  | TCVar ByteString
  | TCMul TripCount TripCount
  | TCAdd TripCount TripCount
  deriving (Eq, Show)

-- | Light-weight summary of an analyzed loop.
--
-- @liLoopVar@ is the first iterator name for the loop nest (most loops
-- use the first iterator as the primary induction variable). @liTripCount@
-- is a derived trip-count for the (first) bound when possible.
data LoopInfo = LoopInfo
  { liLoopVar :: ByteString
  , liTripCount :: TripCount
  , liBody :: [Stmt]
  , liExecPolicy :: ExecPolicy
  } deriving (Eq, Show)

classifyBound :: IndexExpr -> LoopBound
classifyBound (IConst n) = LBConstant n
classifyBound (IVar v) = LBVar v
classifyBound ie = LBIndexExpr ie

computeTripCount :: LoopBound -> TripCount
computeTripCount (LBConstant n) = TCConstant n
computeTripCount (LBVar v) = TCVar v
computeTripCount (LBIndexExpr ie) = tripCountFromIndexExpr ie

-- | Attempt to extract a @TripCount@ from an @IndexExpr@ when the
-- expression is formed from the simple arithmetic subset we recognise.
tripCountFromIndexExpr :: IndexExpr -> TripCount
tripCountFromIndexExpr = go . simplifyIndexExpr
  where
    go expr = case expr of
      IConst n -> TCConstant n
      IVar v -> TCVar v
      IMul a b -> mulTripCounts (go a) (go b)
      IAdd a b -> addTripCounts (go a) (go b)
      IDiv a b ->
        case (tripCountValue (go a), tripCountValue (go b)) of
          (Just x, Just y) | y /= 0 -> TCConstant (x `div` y)
          -- Variable dividend divided by a positive constant (e.g. tile loop bounds
          -- like (n + 31) / 32): the quotient is still variable-sized.
          (Nothing, Just y) | y > 0, go a /= TCUnknown -> go a
          _ -> TCUnknown
      _ -> TCUnknown

addTripCounts :: TripCount -> TripCount -> TripCount
addTripCounts a b = fromMaybe (TCAdd a b) $ do
  x <- tripCountValue a
  y <- tripCountValue b
  pure (TCConstant (x + y))

mulTripCounts :: TripCount -> TripCount -> TripCount
mulTripCounts a b = fromMaybe (TCMul a b) $ do
  x <- tripCountValue a
  y <- tripCountValue b
  pure (TCConstant (x * y))

-- | Predicate: is the trip-count statically a compile-time constant?
isConstantTripCount :: TripCount -> Bool
isConstantTripCount TCConstant {} = True
isConstantTripCount (TCMul l r) = isConstantTripCount l && isConstantTripCount r
isConstantTripCount (TCAdd l r) = isConstantTripCount l && isConstantTripCount r
isConstantTripCount _ = False

-- | If the trip-count can be evaluated to an integer, return it.
-- This recursively evaluates @TCAdd@ / @TCMul@ when both sides are
-- constant; otherwise returns @Nothing@.
tripCountValue :: TripCount -> Maybe Integer
tripCountValue (TCConstant n) = Just n
tripCountValue (TCMul l r) = do
  a <- tripCountValue l
  b <- tripCountValue r
  Just (a * b)
tripCountValue (TCAdd l r) = do
  a <- tripCountValue l
  b <- tripCountValue r
  Just (a + b)
tripCountValue _ = Nothing

-- | Combine all loop-dimension bounds into one aggregate trip-count summary.
loopTripCount :: LoopSpec -> TripCount
loopTripCount spec =
  foldr mulTripCounts (TCConstant 1) (map tripCountFromIndexExpr (lsBounds spec))

-- | Conservative heuristic for whether a trip count is large enough or symbolic
-- enough to justify parallelization work.
isParallelTripCount :: TripCount -> Bool
isParallelTripCount tc = case tripCountValue tc of
  Just n -> n >= 4
  Nothing -> case tc of
    TCVar {} -> True
    TCAdd {} -> True
    TCMul {} -> True
    _ -> False

-- | Collect variables whose storage is written by a statement.
--
-- Scalar assignments define their destination variable. Array writes count as a
-- write to the destination buffer variable. Returns are not definitions.
definedVarsStmt :: Stmt -> Set ByteString
definedVarsStmt stmt = case stmt of
  SAssign v _ -> S.singleton v
  SArrayWrite (AVar arr) _ _ -> S.singleton arr
  SLoop _ body -> S.unions (map definedVarsStmt body)
  SParallelRegion body -> S.unions (map definedVarsStmt body)
  SIf _ thn els -> S.unions (map definedVarsStmt (thn ++ els))
  _ -> S.empty

-- | Defined variables across a sequence of statements.
definedVarsStmts :: [Stmt] -> Set ByteString
definedVarsStmts = S.unions . map definedVarsStmt

-- | Variables used by an @Atom@.
usedVarsAtom :: Atom -> Set ByteString
usedVarsAtom (AVar v) = S.singleton v
usedVarsAtom (AVecVar v) = S.singleton v
usedVarsAtom _ = S.empty

-- | Variables referenced by a right-hand-side expression.
usedVarsRHS :: RHS -> Set ByteString
usedVarsRHS rhs = case rhs of
  RAtom a -> usedVarsAtom a
  RBinOp _ a1 a2 -> S.union (usedVarsAtom a1) (usedVarsAtom a2)
  RUnOp _ a -> usedVarsAtom a
  RTuple as -> S.unions (map usedVarsAtom as)
  RProj _ a -> usedVarsAtom a
  RRecord fields -> S.unions (map (usedVarsAtom . snd) fields)
  RRecordProj _ a -> usedVarsAtom a
  RArrayLoad a1 a2 -> S.union (usedVarsAtom a1) (usedVarsAtom a2)
  RArrayAlloc a -> usedVarsAtom a
  RArrayCopy a -> usedVarsAtom a
  RArrayShape a -> usedVarsAtom a
  RShapeSize a -> usedVarsAtom a
  RShapeInit a -> usedVarsAtom a
  RShapeLast a -> usedVarsAtom a
  RFlatToNd a shp -> usedVarsAtom a `S.union` usedVarsAtom shp
  RNdToFlat a shp -> usedVarsAtom a `S.union` usedVarsAtom shp
  R2DToFlat a w -> usedVarsAtom a `S.union` usedVarsAtom w
  RCall _ args -> S.unions (map usedVarsAtom args)
  RVecLoad a1 a2 -> S.union (usedVarsAtom a1) (usedVarsAtom a2)
  RVecStore a1 a2 a3 -> S.unions [usedVarsAtom a1, usedVarsAtom a2, usedVarsAtom a3]
  RVecBinOp _ a1 a2 -> S.union (usedVarsAtom a1) (usedVarsAtom a2)
  RVecUnOp _ a -> usedVarsAtom a
  RVecSplat a -> usedVarsAtom a
  RVecReduce _ a -> usedVarsAtom a
  RPairMake _ _ a1 a2 -> S.union (usedVarsAtom a1) (usedVarsAtom a2)
  RPairFst _ a -> usedVarsAtom a
  RPairSnd _ a -> usedVarsAtom a
  RArrayFree a -> usedVarsAtom a

-- | Collect variables referenced by an 'IndexExpr'.
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
usedVarsStmt :: Stmt -> Set ByteString
usedVarsStmt stmt = case stmt of
  SAssign _ rhs -> usedVarsRHS rhs
  SArrayWrite arr idx val -> S.unions [usedVarsAtom arr, usedVarsAtom idx, usedVarsAtom val]
  SLoop spec body ->
    let bodyVars = S.unions (map usedVarsStmt body)
        boundVars = S.unions (map usedVarsIndexExpr (lsBounds spec))
    in bodyVars `S.union` boundVars
  SParallelRegion body -> usedVarsStmts body
  SIf cond thn els -> S.union (usedVarsAtom cond) (S.unions (map usedVarsStmt (thn ++ els)))
  SReturn a -> usedVarsAtom a
  SBreak -> S.empty

-- | Variables referenced across a sequence of statements.
usedVarsStmts :: [Stmt] -> Set ByteString
usedVarsStmts = S.unions . map usedVarsStmt

-- | Analyse a single @SLoop@ statement and return a @LoopInfo@ summary.
-- Returns @Nothing@ for non-loop statements.
analyzeLoop :: Stmt -> Maybe LoopInfo
analyzeLoop (SLoop spec body) = case lsIters spec of
  [] -> Nothing
  (iter:_) -> Just $ LoopInfo
    { liLoopVar = iter
    , liTripCount = case lsBounds spec of
        (b:_) -> computeTripCount (classifyBound b)
        [] -> TCUnknown
    , liBody = body
    , liExecPolicy = lsExec spec
    }
analyzeLoop _ = Nothing

-- | Recursively collect @LoopInfo@ for all loops in a statement list.
-- This explores nested loops, parallel regions, and conditionals.
analyzeStmts :: [Stmt] -> [LoopInfo]
analyzeStmts = concatMap go
  where
    go stmt = case analyzeLoop stmt of
      Just info -> info : analyzeStmts (liBody info)
      Nothing -> case stmt of
        SParallelRegion body -> analyzeStmts body
        SIf _ thn els -> analyzeStmts thn ++ analyzeStmts els
        _ -> []

-- | Conservative predicate: is this loop a candidate for vectorisation?
-- The heuristic accepts fixed small-arity loops (constant trip counts >=4)
-- or parametric loops (variables) and some composed trip-count forms.
isVectorizableLoop :: LoopInfo -> Bool
isVectorizableLoop info = case liTripCount info of
  TCConstant n -> n >= 4
  TCVar {} -> True
  TCMul l r -> isConstantTripCount l || isConstantTripCount r
  TCAdd l r -> isConstantTripCount l || isConstantTripCount r
  TCUnknown -> False
