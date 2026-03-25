-- |
-- Module: Language.Hydrangea.Solver
--
-- SMT-backed satisfiability checking for refinement predicates.
--
-- The solver discharges constant predicates eagerly, translates the remaining
-- symbolic constraints to SBV integer formulas, and treats @Unk@ conservatively
-- as satisfiable so inference does not report false negatives when the backend
-- cannot decide a query.
module Language.Hydrangea.Solver where

import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.SBV
import Data.SBV.Control
import Language.Hydrangea.Predicate

-- | Check predicates for consistency.
--
-- Returns @Right residual@ when the predicate set is satisfiable; the
-- residual list contains non-constant (symbolic) predicates retained for
-- inclusion in the resulting @Polytype@. If the predicate set is
-- unsatisfiable the original predicate list is returned in @Left@ so the
-- caller can report an @UnsatConstraints@ error.
checkPredicates :: [Pred] -> IO (Either [Pred] [Pred])
checkPredicates preds = do
  let (constPreds, symPreds) = partitionConsts preds
  case findFalseConst constPreds of
    Just _ -> return $ Left preds
    Nothing -> do
      satResult <- checkSatisfiable symPreds
      if satResult
        then pure $ Right symPreds
        else pure $ Left preds

-- | Partition predicates into constant vs symbolic buckets.
--
-- Constant predicates are those that can be evaluated without solver
-- variables (e.g. @1 == 1@). Symbolic predicates reference solver
-- variables or array dimension variables and are forwarded to SBV.
partitionConsts :: [Pred] -> ([Pred], [Pred])
partitionConsts = foldr go ([], [])
  where
    go pred' (consts, syms) =
      case evalPredConst pred' of
        Just _ -> (pred' : consts, syms)
        Nothing -> (consts, pred' : syms)

-- | Find a constant predicate that evaluates to False.
--
-- This provides a cheap early-exit: if any fully-constant predicate is
-- provably false we can immediately reject the whole set.
findFalseConst :: [Pred] -> Maybe Pred
findFalseConst = foldr go Nothing
  where
    go pred' acc =
      case evalPredConst pred' of
        Just False -> Just pred'
        _ -> acc

-- | Check satisfiability of symbolic predicates using SBV.
--
-- Returns @True@ for @Sat@ or @Unk@ (unknown) and @False@ for @Unsat@ so
-- callers treat @Unk@ conservatively as satisfiable.
checkSatisfiable :: [Pred] -> IO Bool
checkSatisfiable [] = pure True
checkSatisfiable preds = runSMT $ do
  let vars = S.toList $ S.unions (map predVars preds)
  symVars <- buildVars vars
  mapM_ (constrain . predToSBV symVars) preds
  query $ do
    cs <- checkSat
    case cs of
      Sat -> pure True
      Unk -> pure True
      _ -> pure False

-- | Build SBV variables for each refinement variable referenced in predicates.
--
-- Each @Var@ becomes an @SInteger@ in the SBV environment and is returned as
-- a mapping for later translation of @Term@ values.
buildVars :: [Var] -> Symbolic (Map Var SInteger)
buildVars vars = do
  pairs <- mapM mk vars
  pure $ M.fromList pairs
  where
    mk v = do
      let name = BS.unpack v
      symVar <- sInteger name
      pure (v, symVar)

-- | Translate a predicate to an SBV boolean expression.
predToSBV :: Map Var SInteger -> Pred -> SBool
predToSBV env pred' =
  case pred' of
    PLt l r -> termToSBV env l .< termToSBV env r
    PLe l r -> termToSBV env l .<= termToSBV env r
    PEq l r -> termToSBV env l .== termToSBV env r
    PGe l r -> termToSBV env l .>= termToSBV env r
    PGt l r -> termToSBV env l .> termToSBV env r

-- | Translate a term to an SBV integer expression.
termToSBV :: Map Var SInteger -> Term -> SInteger
termToSBV env term =
  case term of
    TVar v -> lookupVar v
    TConst n -> literal n
    TAdd l r -> termToSBV env l + termToSBV env r
    TSub l r -> termToSBV env l - termToSBV env r
    TMul l r -> termToSBV env l * termToSBV env r
    TNeg t -> negate (termToSBV env t)
    TDim arr ix -> lookupVar (dimVarName arr ix)
    TValBound arr -> lookupVar (valBoundName arr)
  where
    lookupVar v =
      case M.lookup v env of
        Just symVar -> symVar
        Nothing -> literal 0
