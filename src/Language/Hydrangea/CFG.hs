{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.CFG
--
-- Core IR types for the imperative CFG representation used by optimisation
-- and code-generation passes.
--
-- The key abstractions are:
--
-- * 'IndexExpr' — a structural index expression language for loop bounds
--   and dependence analysis; use 'simplifyIndexExpr' to obtain a canonical form.
-- * 'LoopSpec' \/ 'ExecPolicy' — n-dimensional loop descriptors with
--   serial, parallel, and vector execution hints.
-- * 'Stmt' \/ 'Proc' \/ 'Program' — the minimal statement IR for lowered programs.
--
-- * /Naming convention/ — functions operating directly on this CFG IR use
--   a @2@ suffix for boring historical reasons. There used to be two separate
--   CFG layers, but the first was folded into the second.
module Language.Hydrangea.CFG
  ( CVar
  , IndexExpr(..)
  , TailPolicy(..)
  , VectorSpec(..)
  , ParallelStrategy(..)
  , ParallelSpec(..)
  , ReductionSpec(..)
  , LoopRole(..)
  , ExecPolicy(..)
  , LoopSpec(..)
  , ArrayFact(..)
  , VectorAccessFact(..)
  , Stmt(..)
  , Proc(..)
  , Program(..)
  , simplifyIndexExpr
  , CType
  , mkProc
  , rewriteStmts2With
  , rewriteStmts2WithM
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Language.Hydrangea.CFGCore (Atom, RHS, Redop, CType)

-- | Canonical name for variables in the CFG.
type CVar = ByteString

-- | Structural index expression language for loop bounds and dependence analysis.
-- Use 'simplifyIndexExpr' to obtain a canonical form suitable for comparison.
data IndexExpr
  = IVar CVar
  -- ^ symbolic variable (loop iterator or named bound)
  | IConst Integer
  -- ^ integer constant
  | IAdd IndexExpr IndexExpr
  -- ^ addition; simplified/flattened by @simplifyIndexExpr@
  | ISub IndexExpr IndexExpr
  -- ^ subtraction (normalized to addition of negation)
  | IMul IndexExpr IndexExpr
  -- ^ multiplication; constant factors are collected
  | IDiv IndexExpr IndexExpr
  -- ^ division; kept syntactic (no algebraic cancellation performed)
  | ITuple [IndexExpr]
  -- ^ tuple of index expressions for ND indexing
  | IProj Int IndexExpr
  -- ^ projection from a tuple (0-based)
  | IFlatToNd IndexExpr IndexExpr
  -- ^ convert flat index -> ND index using the given shape expression
  | INdToFlat IndexExpr IndexExpr
  -- ^ convert ND index -> flat index using the given shape expression
  | ICall ByteString [IndexExpr]
  -- ^ opaque call to an index-producing function; preserved verbatim
  deriving (Eq, Show)

-- | Normalise an 'IndexExpr' into a canonical form suitable for comparison.
--
-- Normalisation rules applied recursively:
--
-- * Constant folding: sums and products of constants are evaluated.
-- * Flattening: nested @IAdd@ \/ @IMul@ trees are collected into n-ary
--   sums \/ products, then re-encoded as binary trees in a deterministic order.
-- * @ISub a b@ is rewritten to @IAdd a (IMul (IConst (-1)) b)@.
-- * Terms are sorted so that non-constant terms precede the constant term.
-- * @IDiv@ is kept syntactic (no algebraic cancellation is performed).
--
-- Example: @'simplifyIndexExpr' (IAdd (IConst 1) (IAdd (IVar "i") (IConst 2)))@
-- produces a form equivalent to @IAdd (IVar "i") (IConst 3)@.
simplifyIndexExpr :: IndexExpr -> IndexExpr
simplifyIndexExpr ie = case ie of
  IVar{} -> ie
  IConst{} -> ie
  ICall{} -> ie
  IProj i e -> IProj i (simplifyIndexExpr e)
  ITuple es -> ITuple (map simplifyIndexExpr es)
  IFlatToNd a b -> IFlatToNd (simplifyIndexExpr a) (simplifyIndexExpr b)
  INdToFlat a b -> INdToFlat (simplifyIndexExpr a) (simplifyIndexExpr b)
  ISub a b -> simplifyIndexExpr (IAdd (simplifyIndexExpr a) (IMul (IConst (-1)) (simplifyIndexExpr b)))
  IAdd a b -> simplifyAdd (simplifyIndexExpr a) (simplifyIndexExpr b)
  IMul a b -> simplifyMul (simplifyIndexExpr a) (simplifyIndexExpr b)
  IDiv a b -> IDiv (simplifyIndexExpr a) (simplifyIndexExpr b)

-- | Priority rank for canonical term ordering: lower rank sorts earlier.
-- Multiplicative factors rank before variables; opaque calls rank last.
exprRank :: IndexExpr -> Int
exprRank ex = case ex of
  IMul{} -> 0
  IVar{} -> 1
  IProj{} -> 1
  ITuple{} -> 1
  IFlatToNd{} -> 1
  INdToFlat{} -> 1
  IAdd{} -> 1
  IDiv{} -> 1
  ISub{} -> 1
  ICall{} -> 3
  _ -> 2
-- | Flatten and normalise an addition into a canonical 'IAdd' tree with the
-- constant term last.
simplifyAdd :: IndexExpr -> IndexExpr -> IndexExpr
simplifyAdd a b =
  let collectAdd :: IndexExpr -> ([IndexExpr], Integer)
      collectAdd x = case x of
        IAdd x1 x2 -> let (ts1,c1) = collectAdd x1; (ts2,c2) = collectAdd x2 in (ts1 ++ ts2, c1 + c2)
        IConst n -> ([], n)
        other -> ([other], 0)

      (termsA, constA) = collectAdd a
      (termsB, constB) = collectAdd b
      constSum = constA + constB
      sortedTerms = sortBy (comparing (\t -> (exprRank t, show t))) (termsA ++ termsB)

      build ts n = case (ts, n) of
        ([], 0) -> IConst 0
        ([], n') -> IConst n'
        ([t], 0) -> t
        (ts', 0) -> foldr1 IAdd ts'
        ([t], n') -> IAdd t (IConst n')
        (ts', n') -> IAdd (foldr1 IAdd ts') (IConst n')
  in build sortedTerms constSum

-- | Flatten and normalise a multiplication into a canonical 'IMul' tree with
-- the constant factor last.
simplifyMul :: IndexExpr -> IndexExpr -> IndexExpr
simplifyMul a b =
  let collectMul :: IndexExpr -> ([IndexExpr], Integer)
      collectMul x = case x of
        IMul x1 x2 -> let (ts1,c1) = collectMul x1; (ts2,c2) = collectMul x2 in (ts1 ++ ts2, c1 * c2)
        IConst n -> ([], n)
        other -> ([other], 1)

      (termsA, constA) = collectMul a
      (termsB, constB) = collectMul b
      constProd = constA * constB
      sortedTerms = sortBy (comparing (\t -> (exprRank t, show t))) (termsA ++ termsB)

      build ts n = case (ts, n) of
        ([], 1) -> IConst 1
        ([], n') -> IConst n'
        ([t], 1) -> t
        (ts', 1) -> foldr1 IMul ts'
        ([t], n') -> IMul t (IConst n')
        (ts', n') -> IMul (foldr1 IMul ts') (IConst n')
  in build sortedTerms constProd

-- | How the tail (sub-vector-width remainder) of a vectorised loop is handled.
data TailPolicy = TailRemainder | TailMask | TailNone
  deriving (Eq, Show)

-- | SIMD vectorisation parameters for a loop.
data VectorSpec = VectorSpec { vsWidth :: Int, vsTail :: TailPolicy }
  deriving (Eq, Show)

-- | The parallelisation strategy selected for a loop.
data ParallelStrategy
  = ParallelGeneric                  -- ^ Generic parallel map (no scatter).
  | ParallelScatterDirect            -- ^ Direct (injective) scatter.
  | ParallelScatterAtomicAddInt      -- ^ Scatter with atomic integer addition.
  | ParallelScatterAtomicAddFloat    -- ^ Scatter with atomic float addition.
  | ParallelScatterPrivatizedIntAdd  -- ^ Scatter via thread-private accumulators.
  deriving (Eq, Show)

-- | Parallel execution parameters for a loop.
--
-- 'psStrategy' records which parallelism strategy was selected so that
-- downstream passes can distinguish generic map-like parallelism from scatter
-- strategies.  'psPolicy' carries optional backend scheduling directives
-- (e.g. @schedule(static)@).
data ParallelSpec = ParallelSpec
  { psStrategy :: ParallelStrategy
  , psPolicy   :: Maybe ByteString
  }
  deriving (Eq, Show)

-- | Reduction performed across a loop nest.
data ReductionSpec = ReductionSpec
  { rsAccVar :: CVar       -- ^ accumulator variable bound inside the loop body
  , rsInit   :: IndexExpr  -- ^ initial accumulator value (constant or expression)
  , rsRedop  :: Redop      -- ^ reduction operator (see 'Redop' in "Language.Hydrangea.CFGCore")
  }
  deriving (Eq, Show)

-- | Semantic role of a loop, carried from the lowering pass.
--
-- Distinguishes structurally similar loops so that later passes can apply
-- appropriate transformations:
--
-- * 'LoopPlain' — generic loop with no special source-level intent.
-- * 'LoopMap' — per-output-element map\/generate kernel.
-- * 'LoopReductionWrapper' — scalar wrapper around a reduction, usually from a 0-D source reduction.
-- * 'LoopReduction' — the loop that performs the reduction.
-- * 'LoopMapReduction' — outer map whose body contains a per-element reduction.
data LoopRole
  = LoopPlain
  | LoopMap
  | LoopReductionWrapper
  | LoopReduction
  | LoopMapReduction
  deriving (Eq, Show)

-- | Execution policy for a loop.
data ExecPolicy
  = Serial              -- ^ Execute iterations sequentially.
  | Parallel ParallelSpec  -- ^ Execute iterations in parallel.
  | Vector VectorSpec      -- ^ Vectorise iterations with the given SIMD width.
  deriving (Eq, Show)

-- | Descriptor for an n-dimensional loop nest.
--
-- 'lsIters' and 'lsBounds' have the same length; each bound is the trip count
-- for the corresponding iterator.
data LoopSpec = LoopSpec
  { lsIters  :: [CVar]            -- ^ iterator variable names
  , lsBounds :: [IndexExpr]       -- ^ per-dimension trip counts
  , lsExec   :: ExecPolicy        -- ^ execution policy (serial\/parallel\/vector)
  , lsRed    :: Maybe ReductionSpec  -- ^ optional loop-carried reduction
  , lsRole   :: LoopRole          -- ^ semantic role from lowering
  }
  deriving (Eq, Show)

-- | Minimal statement set for the CFG, using 'Atom' and 'RHS' from "Language.Hydrangea.CFGCore".
data Stmt
  = SAssign CVar RHS           -- ^ Bind a variable to a right-hand-side expression.
  | SArrayWrite Atom Atom Atom -- ^ Write a value into an array at an index.
  | SLoop LoopSpec [Stmt]      -- ^ Execute a body over an n-dimensional iteration space.
  | SIf Atom [Stmt] [Stmt]     -- ^ Conditional control flow.
  | SReturn Atom               -- ^ Return a value from a procedure.
  deriving (Eq, Show)

-- | Walk a statement list, applying @rewrite@ to each statement after recursing
-- into loops and conditionals.  @descend@ updates the context when entering a loop.
rewriteStmts2With
  :: ctx
  -> (ctx -> ctx)
  -> (ctx -> Stmt -> [Stmt])
  -> [Stmt]
  -> [Stmt]
rewriteStmts2With initialCtx descend rewrite = go initialCtx
  where
    go ctx = concatMap (goStmt ctx)

    goStmt ctx stmt =
      let stmt' = case stmt of
            SLoop spec body -> SLoop spec (go (descend ctx) body)
            SIf cond thn els -> SIf cond (go ctx thn) (go ctx els)
            _ -> stmt
      in rewrite ctx stmt'

-- | Monadic variant of 'rewriteStmts2With'; effects from @rewrite@ are sequenced left-to-right.
rewriteStmts2WithM
  :: Monad m
  => ctx
  -> (ctx -> ctx)
  -> (ctx -> Stmt -> m [Stmt])
  -> [Stmt]
  -> m [Stmt]
rewriteStmts2WithM initialCtx descend rewrite = go initialCtx
  where
    go ctx = fmap concat . mapM (goStmt ctx)

    goStmt ctx stmt =
      case stmt of
        SLoop spec body -> do
          body' <- go (descend ctx) body
          rewrite ctx (SLoop spec body')
        SIf cond thn els -> do
          thn' <- go ctx thn
          els' <- go ctx els
          rewrite ctx (SIf cond thn' els')
        _ ->
          rewrite ctx stmt

-- | Array-level facts recorded by the lowering pass for a concrete buffer variable.
data ArrayFact = ArrayFact
  { afFreshAlloc :: Bool  -- ^ Buffer was freshly allocated in this procedure.
  , afWriteOnce  :: Bool  -- ^ Each element is written exactly once by a map-like kernel.
  , afReadOnly   :: Bool  -- ^ Buffer is only read within the relevant kernel context.
  }
  deriving (Eq, Show)

-- | Access and layout facts for a buffer variable, used by the vectorizer.
data VectorAccessFact = VectorAccessFact
  { vxfDenseLinearIndexOf :: Maybe CVar
    -- ^ The variable aliases the dense linear position of this loop iterator.
  , vxfDenseRead          :: Bool
    -- ^ Array is read via a dense linear traversal in a map-like kernel.
  , vxfIndirectRead       :: Bool
    -- ^ Array is accessed indirectly (e.g. gather); excludes it from contiguous-load vectorisation.
  , vxfContiguousWrite    :: Bool
    -- ^ Lowering writes the array contiguously in iteration order.
  }
  deriving (Eq, Show)

-- | A procedure in the CFG program.
data Proc = Proc
  { procName    :: CVar
  , procParams  :: [CVar]
  , procBody    :: [Stmt]
  , procTypeEnv :: Map CVar CType
    -- ^ Concrete C type for every variable, populated by the lowering pass.
  , procArrayFacts :: Map CVar ArrayFact
    -- ^ Buffer-level facts for array variables in this procedure.
  , procVectorAccessFacts :: Map CVar VectorAccessFact
    -- ^ Access\/layout facts for the vectorizer.
  } deriving (Eq, Show)

-- | An ordered collection of top-level procedures.
-- Procedure order is significant for code emission.
data Program = Program [Proc]
  deriving (Eq, Show)

-- | Construct a 'Proc' with empty type and fact tables.
-- Useful in tests and contexts that do not participate in the lowering pipeline.
mkProc :: CVar -> [CVar] -> [Stmt] -> Proc
mkProc name params body = Proc
  { procName    = name
  , procParams  = params
  , procBody    = body
  , procTypeEnv = Map.empty
  , procArrayFacts = Map.empty
  , procVectorAccessFacts = Map.empty
  }
