{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.CFG
--
-- Statement-, loop-, and procedure-level IR for the imperative CFG pipeline.
--
-- "Language.Hydrangea.CFGCore" defines the value-level vocabulary used here.
-- This module adds index expressions, loop descriptors, statements, procedures,
-- and a few small helpers used throughout optimization, analysis, and code
-- generation.
--
-- Key abstractions:
--
-- * 'IndexExpr' — a structural index expression language for loop bounds
--   and dependence analysis; use 'simplifyIndexExpr' to obtain a canonical form.
-- * 'LoopSpec' \/ 'ExecPolicy' — n-dimensional loop descriptors with
--   serial, parallel, and vector execution hints.
-- * 'Stmt' \/ 'Proc' \/ 'Program' — the minimal statement IR for lowered programs.
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
  , emptyVectorAccessFact
  , Stmt(..)
  , Proc(..)
  , Program(..)
  , simplifyIndexExpr
  , CType
  , mkProc
  , rewriteStmtsWith
  , rewriteStmtsWithM
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Functor.Identity (Identity(..), runIdentity)
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing)
import Language.Hydrangea.CFGCore (Atom, CType, CVar, RHS, Redop)

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

sortCanonicalTerms :: [IndexExpr] -> [IndexExpr]
sortCanonicalTerms =
  sortBy (comparing (\term -> (exprRank term, show term)))

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
      sortedTerms = sortCanonicalTerms (termsA ++ termsB)

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
      sortedTerms = sortCanonicalTerms (termsA ++ termsB)

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
  = ParallelGeneric                    -- ^ Generic parallel map (no scatter).
  | ParallelScatterDirect              -- ^ Direct (injective) scatter.
  | ParallelScatterAtomicAddInt        -- ^ Scatter with atomic integer addition.
  | ParallelScatterAtomicAddFloat      -- ^ Scatter with atomic float addition.
  | ParallelScatterPrivatizedIntAdd    -- ^ int64 scatter via thread-private accumulators.
  | ParallelScatterPrivatizedFloatAdd  -- ^ double scatter via thread-private accumulators.
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
  , psSimdLen  :: Maybe Int  -- ^ When set, emit 'parallel for simd simdlen(N)' pragma.
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
-- * 'LoopFold' — sequential foldl loop; carries an accumulator, never parallelizable.
-- * 'LoopMap' — per-output-element map\/generate kernel.
-- * 'LoopReductionWrapper' — scalar wrapper around a reduction, usually from a 0-D source reduction.
-- * 'LoopReduction' — the loop that performs the reduction.
-- * 'LoopMapReduction' — outer map whose body contains a per-element reduction.
-- * 'LoopIterate' — outer temporal loop from an @iterate@ primitive; carries the array state across iterations.
data LoopRole
  = LoopPlain
  | LoopFold
  | LoopMap
  | LoopReductionWrapper
  | LoopReduction
  | LoopMapReduction
  | LoopIterate
  | LoopSegRedOuter  -- ^ Outer loop over segments in a segmented reduction;
                     --   parallelized with dynamic scheduling for load balance.
  deriving (Eq, Show)

-- | Execution policy for a loop.
data ExecPolicy
  = Serial              -- ^ Execute iterations sequentially.
  | Parallel ParallelSpec  -- ^ Execute iterations in parallel.
  | Workshare ParallelSpec -- ^ Execute iterations as worksharing within an enclosing parallel region.
  | Vector VectorSpec      -- ^ Vectorise iterations with the given SIMD width.
  deriving (Eq, Show)

-- | Descriptor for an n-dimensional loop nest.
--
-- 'lsIters' and 'lsBounds' have the same length; each bound is the trip count
-- for the corresponding iterator.  'lsOrigins' (default @IConst 0@) gives the
-- per-dimension /start/ value — loops iterate from @origin@ to @origin + bound@.
data LoopSpec = LoopSpec
  { lsIters   :: [CVar]            -- ^ iterator variable names
  , lsBounds  :: [IndexExpr]       -- ^ per-dimension trip counts
  , lsExec    :: ExecPolicy        -- ^ execution policy (serial\/parallel\/vector)
  , lsRed     :: Maybe ReductionSpec  -- ^ optional loop-carried reduction
  , lsRole    :: LoopRole          -- ^ semantic role from lowering
  , lsOrigins :: [IndexExpr]       -- ^ per-dimension start offsets (default: all IConst 0)
  }
  deriving (Eq, Show)

-- | Minimal statement set for the CFG, using 'Atom' and 'RHS' from "Language.Hydrangea.CFGCore".
data Stmt
  = SAssign CVar RHS           -- ^ Bind a variable to a right-hand-side expression.
  | SArrayWrite Atom Atom Atom -- ^ Write a value into an array at an index.
  | SLoop LoopSpec [Stmt]      -- ^ Execute a body over an n-dimensional iteration space.
  | SParallelRegion [Stmt]     -- ^ Execute a statement block inside one parallel region.
  | SIf Atom [Stmt] [Stmt]     -- ^ Conditional control flow.
  | SReturn Atom               -- ^ Return a value from a procedure.
  | SBreak                     -- ^ Break out of the enclosing loop.
  deriving (Eq, Show)

-- | Walk a statement list, applying @rewrite@ after recursing into nested
-- statements.
--
-- The traversal always descends into loop bodies, conditionals, and parallel
-- regions. @descend@ is applied only when entering a loop body, so callers can
-- track loop-sensitive context such as nesting depth or active iterators.
rewriteStmtsWith
  :: ctx
  -> (ctx -> ctx)
  -> (ctx -> Stmt -> [Stmt])
  -> [Stmt]
  -> [Stmt]
rewriteStmtsWith initialCtx descend rewrite stmts =
  runIdentity $
    rewriteStmtsWithM initialCtx descend (\ctx stmt -> Identity (rewrite ctx stmt)) stmts

-- | Monadic variant of 'rewriteStmtsWith'; effects from @rewrite@ are
-- sequenced left-to-right.
rewriteStmtsWithM
  :: Monad m
  => ctx
  -> (ctx -> ctx)
  -> (ctx -> Stmt -> m [Stmt])
  -> [Stmt]
  -> m [Stmt]
rewriteStmtsWithM initialCtx descend rewrite = go initialCtx
  where
    go ctx = fmap concat . mapM (goStmt ctx)

    goStmt ctx stmt =
      case stmt of
        SLoop spec body -> do
          body' <- go (descend ctx) body
          rewrite ctx (SLoop spec body')
        SParallelRegion body -> do
          body' <- go ctx body
          rewrite ctx (SParallelRegion body')
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

emptyVectorAccessFact :: VectorAccessFact
emptyVectorAccessFact = VectorAccessFact
  { vxfDenseLinearIndexOf = Nothing
  , vxfDenseRead = False
  , vxfIndirectRead = False
  , vxfContiguousWrite = False
  }

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
