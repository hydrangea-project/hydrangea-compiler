{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module: Language.Hydrangea.Predicate

Refinement term and predicate syntax. This module defines the small
expression language used in refinement obligations, as well as helper
functions for variable collection, substitution, and constant evaluation.

Key ideas:
- Refinement terms (@Term@) represent symbolic integer expressions.
- Predicates (@Pred@) relate terms with comparison operators.
- Array dimensions are modeled by @TDim arr i@, which projects the @i@th
  dimension of an array-bound refinement variable @arr@.
- @predVars@ / @predBindVars@ distinguish solver variables from binders.

These utilities are used by the inference engine to emit constraints and
by the solver to translate constraints into SMT queries.
-}
module Language.Hydrangea.Predicate where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Set (Set)
import Data.Set qualified as S
import GHC.Generics (Generic)
-- | Variable names used in refinement syntax.
type Var = ByteString

-- | Terms used in refinement predicates.
data Term
  = TVar Var
  | TConst Integer
  | TAdd Term Term
  | TSub Term Term
  | TMul Term Term
  | TNeg Term
  | TDim Var Int
    -- ^ @TDim arr i@ projects the @i@th dimension of the array bound to @arr@.
  | TValBound Var
    -- ^ @TValBound arr@ is the exclusive upper bound on every element of the
    --   integer array bound to @arr@.  Analogous to @TDim@ but for values.
  deriving (Eq, Ord, Show, Generic)

-- | Predicates over refinement terms.
data Pred
  = PLt  Term Term
  | PLe  Term Term
  | PEq  Term Term
  | PNeq Term Term   -- ^ Disequality: used as the negation of PEq in validity checks.
  | PGe  Term Term
  | PGt  Term Term
  deriving (Eq, Ord, Show, Generic)

-- | A predicate tagged as a hypothesis (known fact) or a safety obligation.
--
-- Hypotheses are assumed true (shape equalities, declared bounds).  Obligations
-- are conditions that must be *entailed* by the hypotheses: for each obligation
-- @S@ with hypotheses @H@, the solver checks that @H ∧ ¬S@ is UNSAT (i.e., @S@
-- is valid given @H@).  This is validity checking, not satisfiability checking.
data TaggedPred
  = Hyp Pred   -- ^ A known fact (shape equality, declared bound).
  | Obl Pred   -- ^ A safety obligation (must be entailed by hypotheses).
  deriving (Eq, Ord, Show, Generic)

-- | Extract the underlying predicate from a tagged predicate.
untagPred :: TaggedPred -> Pred
untagPred (Hyp p) = p
untagPred (Obl p) = p

-- | Negate a predicate (used to form the check @H ∧ ¬S@ for validity).
negatePred :: Pred -> Pred
negatePred (PLt  l r) = PGe l r
negatePred (PLe  l r) = PGt l r
negatePred (PEq  l r) = PNeq l r
negatePred (PNeq l r) = PEq  l r
negatePred (PGe  l r) = PLt  l r
negatePred (PGt  l r) = PLe  l r

-- | Substitute variables inside a tagged predicate.
substTaggedPredVars :: (Var -> Var) -> TaggedPred -> TaggedPred
substTaggedPredVars f (Hyp p) = Hyp (substPredVars f p)
substTaggedPredVars f (Obl p) = Obl (substPredVars f p)

-- | Convert a dimension projection into a synthetic variable name used in the solver.
dimVarName :: Var -> Int -> Var
dimVarName arr dimIx =
  arr <> "__dim__" <> fromString (show dimIx)

-- | Synthetic variable name for the element value upper bound of an integer array.
-- Used by gather-safety checking: @valBoundName arrVar@ is a symbolic integer
-- representing the exclusive upper bound on every element of the array bound to @arrVar@.
valBoundName :: Var -> Var
valBoundName arr = arr <> "__vbound__"

-- | Collect solver variables referenced by a term.
termVars :: Term -> Set Var
termVars term =
  case term of
    TVar v -> S.singleton v
    TConst _ -> S.empty
    TAdd l r -> termVars l <> termVars r
    TSub l r -> termVars l <> termVars r
    TMul l r -> termVars l <> termVars r
    TNeg t -> termVars t
    TDim arr ix -> S.singleton (dimVarName arr ix)
    TValBound arr -> S.singleton (valBoundName arr)

-- | Collect binder variables referenced by a term.
termBindVars :: Term -> Set Var
termBindVars term =
  case term of
    TVar v -> S.singleton v
    TConst _ -> S.empty
    TAdd l r -> termBindVars l <> termBindVars r
    TSub l r -> termBindVars l <> termBindVars r
    TMul l r -> termBindVars l <> termBindVars r
    TNeg t -> termBindVars t
    TDim arr _ -> S.singleton arr
    TValBound arr -> S.singleton arr

-- | Collect solver variables referenced by a predicate.
predVars :: Pred -> Set Var
predVars pred' =
  case pred' of
    PLt  l r -> termVars l <> termVars r
    PLe  l r -> termVars l <> termVars r
    PEq  l r -> termVars l <> termVars r
    PNeq l r -> termVars l <> termVars r
    PGe  l r -> termVars l <> termVars r
    PGt  l r -> termVars l <> termVars r

-- | Collect binder variables referenced by a predicate.
predBindVars :: Pred -> Set Var
predBindVars pred' =
  case pred' of
    PLt  l r -> termBindVars l <> termBindVars r
    PLe  l r -> termBindVars l <> termBindVars r
    PEq  l r -> termBindVars l <> termBindVars r
    PNeq l r -> termBindVars l <> termBindVars r
    PGe  l r -> termBindVars l <> termBindVars r
    PGt  l r -> termBindVars l <> termBindVars r

-- | Substitute variables inside a term.
substTermVars :: (Var -> Var) -> Term -> Term
substTermVars f term =
  case term of
    TVar v -> TVar (f v)
    TConst n -> TConst n
    TAdd l r -> TAdd (substTermVars f l) (substTermVars f r)
    TSub l r -> TSub (substTermVars f l) (substTermVars f r)
    TMul l r -> TMul (substTermVars f l) (substTermVars f r)
    TNeg t -> TNeg (substTermVars f t)
    TDim arr ix -> TDim (f arr) ix
    TValBound arr -> TValBound (f arr)

-- | Substitute variables inside a predicate.
substPredVars :: (Var -> Var) -> Pred -> Pred
substPredVars f pred' =
  case pred' of
    PLt  l r -> PLt  (substTermVars f l) (substTermVars f r)
    PLe  l r -> PLe  (substTermVars f l) (substTermVars f r)
    PEq  l r -> PEq  (substTermVars f l) (substTermVars f r)
    PNeq l r -> PNeq (substTermVars f l) (substTermVars f r)
    PGe  l r -> PGe  (substTermVars f l) (substTermVars f r)
    PGt  l r -> PGt  (substTermVars f l) (substTermVars f r)

-- | Evaluate a term to a constant if it contains no symbolic variables.
evalTermConst :: Term -> Maybe Integer
evalTermConst term =
  case term of
    TVar _ -> Nothing
    TConst n -> Just n
    TAdd l r -> (+) <$> evalTermConst l <*> evalTermConst r
    TSub l r -> (-) <$> evalTermConst l <*> evalTermConst r
    TMul l r -> (*) <$> evalTermConst l <*> evalTermConst r
    TNeg t -> negate <$> evalTermConst t
    TDim _ _ -> Nothing
    TValBound _ -> Nothing

-- | Evaluate a predicate to a constant if both sides are constant.
evalPredConst :: Pred -> Maybe Bool
evalPredConst pred' =
  case pred' of
    PLt  l r -> (<)  <$> evalTermConst l <*> evalTermConst r
    PLe  l r -> (<=) <$> evalTermConst l <*> evalTermConst r
    PEq  l r -> (==) <$> evalTermConst l <*> evalTermConst r
    PNeq l r -> (/=) <$> evalTermConst l <*> evalTermConst r
    PGe  l r -> (>=) <$> evalTermConst l <*> evalTermConst r
    PGt  l r -> (>)  <$> evalTermConst l <*> evalTermConst r

-- | Pack a regular string into the bytestring representation used for variables.
fromString :: String -> ByteString
fromString = BS.pack
