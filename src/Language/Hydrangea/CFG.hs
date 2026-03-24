{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.CFG
--
-- Canonical CFG representation (Phase 2). This module defines the
-- core IR types used by later optimisation and code-generation passes.
-- It focuses on a small, well-documented set of primitives:
--  * @IndexExpr@ — a structural index language used for loop bounds and
--    dependence analysis,
--  * @LoopSpec@ / @ExecPolicy@ — representations of ND loops and
--    execution hints (serial/parallel/vector), and
--  * @Stmt@ / @Proc@ / @Program@ — a minimal statement set for lowered
--    programs.
--
-- Design notes / invariants:
--  * @CVar@ is the canonical name type for temporaries and parameters.
--  * @IndexExpr@ is deliberately small and structural to make analysis
--    deterministic and amenable to canonicalisation (@simplifyIndexExpr@).
--  * @LoopSpec.lsBounds@ and @IndexExpr@ are used by analysis/optimisation
--    to compute iteration space sizes; they should be kept in simplified
--    form when possible.
--
-- This module provides only types and small helpers; higher-level
-- operations (bridges/adapters) belong in the CFG bridge and pipeline modules.
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

-- | Small structural index expression language for dependence analysis.
--
-- Constructor notes:
-- * @IVar name@ — a symbolic variable (typically a loop iterator or named
--   bound).
-- * @IConst n@ — integer constant.
-- * @IAdd a b@, @ISub a b@, @IMul a b@, @IDiv a b@ — binary arithmetic
--   ops. @simplifyIndexExpr@ performs folding and canonical ordering.
-- * @ITuple@ / @IProj@ — lightweight tuple projection to represent
--   multi-dimensional indices.
-- * @IFlatToNd flat sh@ / @INdToFlat nd sh@ — explicit conversions
--   between flat (linear) and n-dimensional indices; the shape is itself
--   an @IndexExpr@ describing the shape/strides.
-- * @ICall f args@ — opaque call returning an index value; treated as
--   an uninterpreted symbol by most analyses.
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

-- | Simplify/normalize an 'IndexExpr' into a small canonical form.
-- The goal is deterministic, locally-sound algebraic normalization that
-- makes expressions easier to compare and reason about in analyses.
--
-- Implemented rules (non-exhaustive):
-- * constant folding for @IAdd@ and @IMul@ (collect constant terms)
-- * flatten nested @IAdd@ / @IMul@ into n-ary forms encoded as binary
--   trees with a deterministic left/right ordering
-- * convert @ISub a b@ into @IAdd a (IMul (IConst (-1)) b)@
-- * reorder additive/multiplicative terms: non-constant (variables,
--   projections, multiplications) come before the final constant term
-- * recursively simplify sub-expressions; @IDiv@ is preserved as a
--   syntactic node (no cancellation rules)
--
-- Example:
-- > simplifyIndexExpr (IAdd (IConst 1) (IAdd (IVar "i") (IConst 2)))
-- yields a canonical form equivalent to @IAdd (IVar "i") (IConst 3)@
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

-- Helpers
-- | Rank expressions for deterministic canonical ordering used by the
-- simplifiers. Lower return value means higher priority when sorting
-- terms (so multiplicative factors come before variables, which come
-- before opaque calls).
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
-- | Collect/add terms and produce a canonical @IAdd@ tree where constant
-- terms are collected and placed last.
simplifyAdd :: IndexExpr -> IndexExpr -> IndexExpr
simplifyAdd a b =
  let collectAdd :: IndexExpr -> ([IndexExpr], Integer)
      collectAdd x = case x of
        IAdd x1 x2 -> let (ts1,c1) = collectAdd x1; (ts2,c2) = collectAdd x2 in (ts1 ++ ts2, c1 + c2)
        IConst n -> ([], n)
        other -> ([other], 0)

      (termsA, constA) = collectAdd a
      (termsB, constB) = collectAdd b
      terms = termsA ++ termsB
      constSum = constA + constB
      -- canonical ordering: prefer multiplicative/variable terms before
      -- opaque calls, then fall back to string ordering for determinism.
      sortedTerms = sortBy (comparing (\t -> (exprRank t, show t))) terms

      build ts n = case (ts, n) of
        ([], 0) -> IConst 0
        ([], n') -> IConst n'
        ([t], 0) -> t
        (ts', 0) -> foldr1 IAdd ts'
        ([t], n') -> IAdd t (IConst n')
        (ts', n') -> IAdd (foldr1 IAdd ts') (IConst n')
  in build sortedTerms constSum

-- | Collect/multiply terms and produce a canonical @IMul@ tree where
-- constant factors are multiplied together and placed last.
simplifyMul :: IndexExpr -> IndexExpr -> IndexExpr
simplifyMul a b =
  let collectMul :: IndexExpr -> ([IndexExpr], Integer)
      collectMul x = case x of
        IMul x1 x2 -> let (ts1,c1) = collectMul x1; (ts2,c2) = collectMul x2 in (ts1 ++ ts2, c1 * c2)
        IConst n -> ([], n)
        other -> ([other], 1)

      (termsA, constA) = collectMul a
      (termsB, constB) = collectMul b
      terms = termsA ++ termsB
      constProd = constA * constB
      -- prefer variables/multiplicative factors before opaque calls
      sortedTerms = sortBy (comparing (\t -> (exprRank t, show t))) terms

      build ts n = case (ts, n) of
        ([], 1) -> IConst 1
        ([], n') -> IConst n'
        ([t], 1) -> t
        (ts', 1) -> foldr1 IMul ts'
        ([t], n') -> IMul t (IConst n')
        (ts', n') -> IMul (foldr1 IMul ts') (IConst n')
  in build sortedTerms constProd

-- | Tail handling policy for vector loops
data TailPolicy = TailRemainder | TailMask | TailNone
  deriving (Eq, Show)

-- | Vectorisation parameters for a loop
data VectorSpec = VectorSpec { vsWidth :: Int, vsTail :: TailPolicy }
  deriving (Eq, Show)

data ParallelStrategy
  = ParallelGeneric
  | ParallelScatterDirect
  | ParallelScatterAtomicAddInt
  | ParallelScatterAtomicAddFloat
  | ParallelScatterPrivatizedIntAdd
  deriving (Eq, Show)

-- | Parallel execution hint.
--
-- @psStrategy@ records why the loop was parallelized at a high level, so
-- later passes and debugging output can distinguish generic map-like
-- parallelism from specific scatter strategies.
--
-- @psPolicy@ optionally carries backend-specific clauses such as
-- @schedule(static)@.
data ParallelSpec = ParallelSpec
  { psStrategy :: ParallelStrategy
  , psPolicy :: Maybe ByteString
  }
  deriving (Eq, Show)

-- | Specification for reductions performed across the loop nest.
--
-- * @rsAccVar@ is the accumulator variable used inside the loop body.
-- * @rsInit@ is the initial value of the accumulator expressed as an
--   @IndexExpr@ (allows constant or computed initialisers).
-- * @rsRedop@ is the reduction operator (see @Redop@ in CFGCore).
data ReductionSpec = ReductionSpec
  { rsAccVar :: CVar
  , rsInit :: IndexExpr
  , rsRedop :: Redop
  }
  deriving (Eq, Show)

-- | High-level loop role carried from lowering.
--
-- This distinguishes structurally similar loops that should be treated
-- differently by later passes:
-- * @LoopPlain@ — generic loop with no special source-level intent.
-- * @LoopMap@ — per-output-element map/generate style loop.
-- * @LoopReductionWrapper@ — scalar wrapper around a true reduction, usually
--   introduced by lowering 0D reductions.
-- * @LoopReduction@ — the loop that semantically performs a reduction.
-- * @LoopMapReduction@ — an outer map loop whose body contains a serial
--   per-element reduction.
data LoopRole
  = LoopPlain
  | LoopMap
  | LoopReductionWrapper
  | LoopReduction
  | LoopMapReduction
  deriving (Eq, Show)

-- | Execution policy hints for loops.
--
-- * @Serial@ — no parallelism/vectorisation is requested.
-- * @Parallel@ — a parallel policy is suggested (backend may ignore).
-- * @Vector@ — vectorisation with the given @VectorSpec@ is desired.
data ExecPolicy
  = Serial
  | Parallel ParallelSpec
  | Vector VectorSpec
  deriving (Eq, Show)

-- | LoopSpec supports ND loops via a list of iterators and bounds.
--
-- Invariants:
-- * @lsIters@ and @lsBounds@ must have the same length; each bound is an
--   @IndexExpr@ describing the trip count or shape for the corresponding
--   iterator.
-- * @lsExec@ carries execution hints (serial/parallel/vector).
-- * @lsRed@ optionally carries a reduction performed across the loop.
-- * @lsRole@ carries high-level intent from lowering.
data LoopSpec = LoopSpec
  { lsIters :: [CVar]
  -- ^ iterator variable names for each loop dimension
  , lsBounds :: [IndexExpr]
  -- ^ per-dimension bounds/limits (trip counts or shapes)
  , lsExec :: ExecPolicy
  -- ^ execution policy (vector/parallel/serial)
  , lsRed :: Maybe ReductionSpec
  -- ^ optional reduction performed across the loop nest
  , lsRole :: LoopRole
  -- ^ high-level role for the loop nest
  }
  deriving (Eq, Show)

-- | Minimal statement set for the CFG. For now reuse Atom/RHS from CFGCore.
--
-- Constructor semantics (brief):
-- * @SAssign dst rhs@ — bind the destination to the computed RHS value.
-- * @SArrayWrite arr ix val@ — write the value into the array at the index (atoms
--   used here are expected to evaluate to a memory reference/index).
-- * @SLoop loopSpec loopBody@ — execute the body over the ND iteration space
--   described by the loop spec (iterators are bound lexically inside
--   the body).
-- * @SIf cond thenStmts elseStmts@ — conditional control-flow.
-- * @SReturn atom@ — return an atom from a procedure.
data Stmt
  = SAssign CVar RHS
  | SArrayWrite Atom Atom Atom
  | SLoop LoopSpec [Stmt]
  | SIf Atom [Stmt] [Stmt]
  | SReturn Atom
  deriving (Eq, Show)

-- | Rewrite a statement tree while centralizing the standard recursive walk
-- over loops and conditionals.
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

-- | Monadic variant of 'rewriteStmts2With'.
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

-- | Buffer-level array facts preserved from lowering.
--
-- These are intentionally conservative facts about concrete CFG array
-- variables, not full aliasing proofs and not arbitrary source-level array
-- properties:
--
-- * @afFreshAlloc@ means lowering allocated the buffer in this procedure.
-- * @afWriteOnce@ means lowering knows the buffer is populated by a map-like
--   kernel that writes each logical output element once.
-- * @afReadOnly@ means the buffer is only read, not written, by the relevant
--   lowered kernel context that introduced the fact.
data ArrayFact = ArrayFact
  { afFreshAlloc :: Bool
  , afWriteOnce  :: Bool
  , afReadOnly   :: Bool
  }
  deriving (Eq, Show)

-- | Lowering-preserved access/layout facts that help the explicit vectorizer.
--
-- These facts are intentionally small and backend-oriented:
--
-- * @vxfDenseLinearIndexOf@ records that a variable denotes the same dense
--   linear traversal position as a loop iterator introduced by lowering, even
--   if that position flowed through shape/index conversion helpers.
-- * @vxfDenseRead@ records that an array is read via a dense linear traversal
--   in a map-like kernel.
-- * @vxfIndirectRead@ records that an array is read indirectly (for example via
--   gather-style indexing) and should therefore stay off the explicit
--   contiguous-load path.
-- * @vxfContiguousWrite@ records that lowering writes the array contiguously in
--   iteration order.
data VectorAccessFact = VectorAccessFact
  { vxfDenseLinearIndexOf :: Maybe CVar
  , vxfDenseRead          :: Bool
  , vxfIndirectRead       :: Bool
  , vxfContiguousWrite    :: Bool
  }
  deriving (Eq, Show)

-- | Procedure / function in the CFG program.
--
-- @procName@ is the unique identifier for the procedure, @procParams@
-- lists parameter names (each a @CVar@) and @procBody@ is the sequence of
-- @Stmt@ forming the function body. @procTypeEnv@ records the concrete
-- 'CType' of every variable produced during lowering, allowing downstream
-- passes to look up types directly instead of re-inferring them from
-- the statement structure. @procArrayFacts@ is a lowering-provided side table
-- for concrete array buffers in this procedure. @procVectorAccessFacts@
-- preserves a small amount of lowering-time access/layout information for
-- vectorization.
data Proc = Proc
  { procName    :: CVar
  , procParams  :: [CVar]
  , procBody    :: [Stmt]
  , procTypeEnv :: Map CVar CType
    -- ^ Concrete C type for every variable in this procedure.  Populated
    -- by the lowering pass; preserved (not modified) by optimisation passes.
  , procArrayFacts :: Map CVar ArrayFact
    -- ^ Lowering-provided facts for concrete array variables/buffers in this
    -- procedure. Optimisation passes may consult these conservatively.
  , procVectorAccessFacts :: Map CVar VectorAccessFact
    -- ^ Lowering-provided access/layout facts used primarily by the explicit
    -- vectorizer to distinguish dense linear traversals from indirect ones.
  } deriving (Eq, Show)

-- | A program is simply a collection of top-level procedures.
--
-- The ordering may be significant for code emission; passes that modify
-- or transform programs should preserve referential integrity of
-- procedure names.
data Program = Program [Proc]
  deriving (Eq, Show)

-- | Smart constructor for 'Proc' with an empty type environment.
-- Useful in tests and any context that doesn't participate in the
-- type-threading pipeline.
mkProc :: CVar -> [CVar] -> [Stmt] -> Proc
mkProc name params body = Proc
  { procName    = name
  , procParams  = params
  , procBody    = body
  , procTypeEnv = Map.empty
  , procArrayFacts = Map.empty
  , procVectorAccessFacts = Map.empty
  }
