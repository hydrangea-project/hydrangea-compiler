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

import Control.Monad (forM)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.SBV
import Data.SBV.Control
import Language.Hydrangea.Predicate
import Language.Hydrangea.Pretty ()
import Text.PrettyPrint.HughesPJClass (render, pPrint)

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

-- | Build a constant substitution map from hypotheses of the form
-- @TVar v = TConst n@, @TDim arr i = TConst n@, or @TValBoundDim arr i = TConst n@.
-- Used for one-pass constant propagation to detect concrete-false hypotheses
-- that reference synthetic dim/bound variables (which evalPredConst cannot see).
buildConstMap :: [Pred] -> Map Var Integer
buildConstMap preds = M.fromList (concatMap extract preds)
  where
    extract (PEq l r) = extractPair l r ++ extractPair r l
    extract _         = []
    extractPair (TVar v)      (TConst n) = [(v, n)]
    extractPair (TDim arr i)  (TConst n) = [(dimVarName arr i, n)]
    extractPair (TValBoundDim a i) (TConst n) = [(valBoundDimName a i, n)]
    extractPair _ _                      = []

-- | Substitute known constant values into a term.
applyConstMapToTerm :: Map Var Integer -> Term -> Term
applyConstMapToTerm m t =
  case t of
    TVar v       -> maybe t TConst (M.lookup v m)
    TDim arr i   -> maybe t TConst (M.lookup (dimVarName arr i) m)
    TValBoundDim a i -> maybe t TConst (M.lookup (valBoundDimName a i) m)
    TConst _     -> t
    TAdd l r     -> TAdd (f l) (f r)
    TSub l r     -> TSub (f l) (f r)
    TMul l r     -> TMul (f l) (f r)
    TNeg x       -> TNeg (f x)
    TMax l r     -> TMax (f l) (f r)
  where f = applyConstMapToTerm m

-- | Substitute known constant values into a predicate.
applyConstMapToPred :: Map Var Integer -> Pred -> Pred
applyConstMapToPred m p =
  case p of
    PLt  l r -> PLt  (f l) (f r)
    PLe  l r -> PLe  (f l) (f r)
    PEq  l r -> PEq  (f l) (f r)
    PNeq l r -> PNeq (f l) (f r)
    PGe  l r -> PGe  (f l) (f r)
    PGt  l r -> PGt  (f l) (f r)
  where f = applyConstMapToTerm m

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
    PLt  l r -> termToSBV env l .<  termToSBV env r
    PLe  l r -> termToSBV env l .<= termToSBV env r
    PEq  l r -> termToSBV env l .== termToSBV env r
    PNeq l r -> termToSBV env l ./= termToSBV env r
    PGe  l r -> termToSBV env l .>= termToSBV env r
    PGt  l r -> termToSBV env l .>  termToSBV env r

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
    TValBoundDim arr i -> lookupVar (valBoundDimName arr i)
    TMax l r -> smax (termToSBV env l) (termToSBV env r)
  where
    lookupVar v =
      case M.lookup v env of
        Just symVar -> symVar
        Nothing -> literal 0

-- ---------------------------------------------------------------------------
-- Validity checking (hypothesis / obligation architecture)
-- ---------------------------------------------------------------------------

-- | Result of checking a single obligation against its hypotheses.
data ObligationResult
  = Verified                       -- ^ H ∧ ¬S is UNSAT: obligation is entailed.
  | Counterexample [(Var, Integer)] -- ^ H ∧ ¬S is SAT: concrete witness values.
  | ObligationUnknown              -- ^ Solver returned Unknown.
  deriving (Show)

-- | Check whether obligation @obl@ is entailed by @hyps@.
--
-- Runs Z3 on @hyps ∧ ¬obl@.  Returns @Verified@ if UNSAT (obligation holds),
-- @Counterexample@ if SAT (obligation may be violated), or @ObligationUnknown@.
checkObligation :: [Pred] -> Pred -> IO ObligationResult
checkObligation hyps obl = do
  let negObl   = negatePred obl
      allPreds  = negObl : hyps
      vars      = S.toList $ S.unions (map predVars allPreds)
  runSMT $ do
    symVars <- buildVars vars
    mapM_ (constrain . predToSBV symVars) hyps
    constrain (predToSBV symVars negObl)
    query $ do
      cs <- checkSat
      case cs of
        Unsat -> return Verified
        Unk   -> return ObligationUnknown
        Sat   -> do
          vals <- mapM (\v -> (v,) <$> getValue (symVars M.! v)) vars
          return (Counterexample vals)

-- | Collect the set of SMT variables that appear in a predicate's hypotheses.
hypVars :: [TaggedPred] -> Set Var
hypVars tagged = S.unions [predVars p | Hyp p <- tagged]

-- | Check all tagged predicates.
--
-- For each @Obl p@, if every SMT variable the obligation mentions is constrained
-- by at least one hypothesis (i.e., the obligation variable set is a subset of
-- the hypothesis variable set), run @checkObligation@.  Ungrounded obligations
-- are treated as @ObligationUnknown@ (warn, not error) to preserve backward
-- compatibility with un-annotated programs.
--
-- Returns @Left (failing_preds, mWitness)@ on failure, where @mWitness@ is a
-- Z3 counterexample if available.  Returns @Right (residual, warnings)@ on
-- success, where @warnings@ contains diagnostic notes for obligations that
-- could not be statically verified.
checkTaggedPredicates :: [TaggedPred]
  -> IO (Either ([Pred], Maybe [(Var, Integer)]) ([TaggedPred], [String]))
checkTaggedPredicates tagged = do
  -- Deduplicate before solving.  Monomorphic instantiation re-emits the same
  -- predicates on every use of a zero-arg binding, so long dependency chains
  -- (e.g. coo → packed_keys → perm → sorted_* → canonical → csr) accumulate
  -- O(k) copies of every predicate for a k-step chain.  Deduplication here is
  -- O(n log n) and avoids redundant Z3 calls for duplicate Obl predicates.
  let tagged' = S.toList (S.fromList tagged)
      hyps = [p | Hyp p <- tagged']
      obls = [p | Obl p <- tagged']
      hVars = S.unions (map predVars hyps)
  -- 1. Eagerly evaluate constant hypotheses.
  let (constHyps, symHyps) = partitionConsts hyps
  case findFalseConst constHyps of
    Just p -> return (Left ([p], Nothing))
    Nothing -> do
      -- 1b. One-pass constant propagation: substitute dim/var constants from
      -- hypotheses into other hypotheses, then re-run findFalseConst.  This
      -- catches concrete-false symbolic hypotheses like
      --   PEq (TMul (TDim src 0) (TDim src 1)) (TConst 5)
      -- when there are also hyps TDim src 0 = 2 and TDim src 1 = 3.
      let cmap = buildConstMap (constHyps ++ symHyps)
          foldedHyps = map (applyConstMapToPred cmap) hyps
          (foldedConsts, _) = partitionConsts foldedHyps
      case findFalseConst foldedConsts of
        Just p -> return (Left ([p], Nothing))
        Nothing -> do
          -- 2. Check each obligation.
          -- An obligation is "grounded" only when every SMT variable it mentions is
          -- constrained by at least one hypothesis.  Ungrounded obligations have free
          -- variables that Z3 can assign arbitrarily, producing spurious counterexamples
          -- for programs that are safe in practice.  We treat them as ObligationUnknown
          -- (permissive) to preserve backward compatibility with un-annotated programs.
          results <- forM obls $ \obl -> do
            let oblVars = predVars obl
            if oblVars `S.isSubsetOf` hVars
              then do r <- checkObligation (symHyps ++ constHyps) obl
                      return (obl, r, True)
              else return (obl, ObligationUnknown, False)
          -- 3. Collect failures (obligations with a concrete counterexample).
          let failures = [(obl, w) | (obl, Counterexample w, _) <- results]
          if not (null failures)
            then do
              let failPreds        = map fst failures
                  mWitness         = Just (snd (head failures))
              return (Left (failPreds, mWitness))
            else do
              -- 4. Generate warnings for obligations that could not be verified.
              let warnings = concatMap mkWarning results
              return (Right (tagged', warnings))
  where
    mkWarning (obl, ObligationUnknown, False) =
      [ "note: could not verify '" ++ render (pPrint obl) ++ "'"
        ++ " — some index variables lack bound annotations" ]
    mkWarning (obl, ObligationUnknown, True) =
      [ "note: could not verify '" ++ render (pPrint obl) ++ "'"
        ++ " — solver returned Unknown" ]
    mkWarning _ = []
