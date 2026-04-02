{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module: Language.Hydrangea.Infer
--
-- Hindley-Milner type inference for Hydrangea, extended with refinement
-- predicates for array bounds checking.
--
-- __Types and terms.__
-- Source types are @TypeF@-fixed-points ('Type') from "Language.Hydrangea.Syntax".
-- During inference, type variables are replaced by unification metavariables
-- (@UType = UTerm TypeF IntVar@).  Refinement wrappers (@TyRefineF v t@) bind a
-- symbolic variable @v@ to a value of type @t@; they are stripped before
-- structural unification and collected as 'Pred' obligations instead.
--
-- __Entry points.__
-- 'runInfer' and 'runInferWithCtx' run an 'Infer' action and return a fully
-- generalized 'Polytype' (or a 'TypeError').  'runInferDecs' infers a sequence
-- of top-level declarations, returning the generalized scheme for each.  All
-- three entry points invoke the SMT solver to discharge refinement obligations
-- before returning; unsatisfiable obligations surface as 'UnsatConstraints'.
--
-- __Inference monad.__
-- The 'Infer' monad provides: a 'Reader' for the typing context ('Ctx'), a
-- 'State' for fresh predicate-variable generation, a 'Writer' for accumulated
-- 'Pred' obligations, 'ExceptT' for 'TypeError', and 'IntBindingT' for
-- unification state.
module Language.Hydrangea.Infer where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Unification hiding (applyBindings, (=:=))
import Control.Unification qualified as U
import Control.Unification.IntVar
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Foldable (fold)
import Data.Functor.Fixedpoint
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Set (Set, (\\))
import Data.Set qualified as S
import Language.Hydrangea.Lexer (Range)
import Language.Hydrangea.Predicate
  ( Pred (..), TaggedPred (..), Term (..)
  , predBindVars, substTaggedPredVars, untagPred
  )
import Language.Hydrangea.Solver
import Language.Hydrangea.Syntax

-- | Pattern synonym for the unification-level integer type.
pattern UTyInt :: UType
pattern UTyInt = UTerm TyIntF

-- | Pattern synonym for the unification-level floating-point type.
pattern UTyFloat :: UType
pattern UTyFloat = UTerm TyFloatF

-- | Pattern synonym for the unification-level string type.
pattern UTyString :: UType
pattern UTyString = UTerm TyStringF

-- | Pattern synonym for the unification-level unit type.
pattern UTyUnit :: UType
pattern UTyUnit = UTerm TyUnitF

-- | Pattern synonym for the unification-level boolean type.
pattern UTyBool :: UType
pattern UTyBool = UTerm TyBoolF

-- | Pattern synonym for a unification-level function type.
pattern UTyFun :: UType -> UType -> UType
pattern UTyFun t1 t2 = UTerm (TyFunF t1 t2)

-- | Pattern synonym for the cons-style tuple used to encode shapes.
pattern UTyCons :: UType -> UType -> UType
pattern UTyCons t1 t2 = UTerm (TyConsF t1 t2)

-- | Pattern synonym for a unification-level pair type.
pattern UTyPair :: UType -> UType -> UType
pattern UTyPair t1 t2 = UTerm (TyPairF t1 t2)

-- | Pattern synonym for an unification-level record type.
pattern UTyRecord :: [(Var, UType)] -> UType
pattern UTyRecord fields = UTerm (TyRecordF fields)

-- | Pattern synonym for an unification-level array type.
pattern UTyArray :: UType -> UType -> UType
pattern UTyArray shape elem = UTerm (TyArrayF shape elem)

-- | Pattern synonym for a named type variable in unification terms.
pattern UTyVar :: Var -> UType
pattern UTyVar v = UTerm (TyVarF v)

-- | Pattern synonym for a unification-level refinement wrapper.
pattern UTyRefine :: Var -> UType -> UType
pattern UTyRefine v t = UTerm (TyRefineF v t)

-- | The inference monad: reader for the typing context, state for the
-- predicate-variable supply, writer for accumulated 'Pred' obligations,
-- 'ExceptT' for 'TypeError', and 'IntBindingT' for unification state.
type Infer = ReaderT Ctx (StateT InferState (WriterT [TaggedPred] (ExceptT TypeError (IntBindingT TypeF Identity))))

-- | Typing context mapping term variables to their polymorphic types.
type Ctx = Map Var UPolytype

-- | Mutable state threaded through inference: a counter for generating
-- fresh predicate variable names, a local value-bound context for index
-- variables annotated with @[i bound E]@, accumulated warnings, and a
-- definition-body environment for propagating bounds through non-inline
-- top-level function calls.
data InferState = InferState
  { predVarCounter :: Int
  , valBoundCtx    :: Map Var Term
    -- ^ Maps variables with @[i bound E]@ annotations to their exclusive
    --   upper bound term.  Used by 'inferValBound' to compute output bounds
    --   from generator body expressions.
  , inferWarnings  :: [String]
    -- ^ Non-fatal diagnostic messages (e.g. unverifiable bound annotations).
  , defBodyEnv     :: Map Var (Var, Exp Range)
    -- ^ Maps top-level single-parameter function names to their
    --   @(paramName, bodyExp)@ pair.  Used by 'EMap' and 'EApp' to infer
    --   output value bounds for non-inline functions without requiring PBound
    --   annotations.  Only single-parameter definitions are registered.
  }

-- | Initial inference state.
initInferState :: InferState
initInferState = InferState
  { predVarCounter = 0
  , valBoundCtx    = M.empty
  , inferWarnings  = []
  , defBodyEnv     = M.empty
  }

-- | Lift a unification-library action into the full inference monad.
liftBinding :: IntBindingT TypeF Identity a -> Infer a
liftBinding = lift . lift . lift . lift

-- | Lookup table mapping variable names to their polymorphic types. The
-- elements are @UPolytype@ so they can be instantiated directly inside
-- the @Infer@ monad (also emitting any predicate obligations).
vlookup :: Range -> Var -> Infer UType
vlookup r x = do
  ctx <- ask
  case M.lookup x ctx of
    Nothing -> throwError $ UnboundVar (Just r) x
    Just ty -> instantiate ty

-- | Extend the typing context with one binding for the duration of an action.
withBinding :: (MonadReader Ctx m) => Var -> UPolytype -> m a -> m a
withBinding x ty = local (M.insert x ty)

-- | Catamorphism over a unification term.
ucata :: (Functor t) => (v -> a) -> (t a -> a) -> UTerm t v -> a
ucata f _ (UVar v) = f v
ucata f g (UTerm t) = g (fmap (ucata f g) t)

deriving instance Ord IntVar

-- | Things whose free variables can be computed in the current inference state.
class FreeVars a where
  freeVars :: a -> Infer (Set (Either Var IntVar))

instance FreeVars UType where
  freeVars ut = do
    fuvs <- fmap (S.fromList . map Right) $ liftBinding (getFreeVars ut)
    let ftvs =
          ucata
            (const S.empty)
            (\case TyVarF x -> S.singleton (Left x); f -> fold f)
            ut
    return $ fuvs `S.union` ftvs

instance FreeVars UPolytype where
  freeVars (Forall xs _ ut) = (\\ (S.fromList (map Left xs))) <$> freeVars ut

instance FreeVars Ctx where
  freeVars = fmap S.unions . mapM freeVars . M.elems

-- | Collect refinement binder names appearing in a unification type.
refineBindVars :: UType -> Set Var
refineBindVars =
  ucata
    (const S.empty)
    (\case
        TyRefineF v t -> S.insert v t
        f -> fold f
    )

-- | Rename refinement binder variables throughout a unification type.
substRefineVars :: (Var -> Var) -> UType -> UType
substRefineVars f =
  ucata
    UVar
    (\case
        TyRefineF v t -> UTerm (TyRefineF (f v) t)
        other -> UTerm other
    )

-- | Remove a single refinement wrapper (if present).
stripRefineTop :: UType -> UType
stripRefineTop ty =
  case ty of
    UTyRefine _ t -> t
    _ -> ty

-- | Remove all refinement wrappers recursively.
stripRefineDeep :: UType -> UType
stripRefineDeep =
  ucata
    UVar
    (\case
        TyRefineF _ t -> t
        f -> UTerm f
    )

-- | Separate the refinement binder (if present) from the inner type.
unwrapRefine :: UType -> (Maybe Var, UType)
unwrapRefine ty =
  case ty of
    UTyRefine v t -> (Just v, t)
    _ -> (Nothing, ty)

-- | Wrap a type with a refinement binder.
wrapRefine :: Var -> UType -> UType
wrapRefine = UTyRefine

-- | Create a fresh unification variable wrapped as a @UType@.
fresh :: Infer UType
fresh = UVar <$> liftBinding freeVar

-- | Create a fresh value type with a refinement binder.
freshValue :: Infer UType
freshValue = do
  u <- fresh
  v <- freshPredVar
  return $ wrapRefine v u

-- | Create a fresh refinement variable used in predicate obligations.
freshPredVar :: Infer Var
freshPredVar = do
  st <- lift get
  let n = predVarCounter st
  lift $ put st {predVarCounter = n + 1}
  return $ BS.pack $ "p" ++ show n

-- | Errors reported by type inference and refinement checking.
data TypeError where
  UnboundVar :: Maybe Range -> ByteString -> TypeError
  Infinite :: Maybe Range -> IntVar -> UType -> TypeError
  Mismatch :: Maybe Range -> TypeF UType -> TypeF UType -> TypeError
  InvalidPoly :: Maybe Range -> Polytype -> TypeError
  DuplicateRecordField :: Maybe Range -> ByteString -> TypeError
  MissingRecordField :: Maybe Range -> ByteString -> TypeError
  UnsatConstraints :: [Pred] -> Maybe [(Var, Integer)] -> TypeError
  MiscError :: Maybe Range -> TypeError
  deriving (Show)

data InferOptions = InferOptions
  { inferSolveRefinements :: Bool
  }

defaultInferOptions :: InferOptions
defaultInferOptions = InferOptions {inferSolveRefinements = True}

instance Fallible TypeF IntVar TypeError where
  occursFailure :: IntVar -> UTerm TypeF IntVar -> TypeError
  occursFailure = Infinite Nothing
  mismatchFailure = Mismatch Nothing

-- | Apply the current unification substitution to a type.
applyBindings :: UType -> Infer UType
applyBindings ut = lift (lift (lift (U.applyBindings ut)))

-- | Unify two types after discarding refinement wrappers for structural comparison.
(=:=) :: UType -> UType -> Infer UType
s =:= t = lift (lift (lift (stripRefineDeep s U.=:= stripRefineDeep t)))

-- | Instantiate a polymorphic scheme with fresh unification variables and emit its predicates.
instantiate :: UPolytype -> Infer UType
instantiate (Forall xs preds uty) = do
  xs' <- mapM (const fresh) xs
  (preds', uty') <-
    if null xs
      then return (preds, uty)
      else do
        let vars = S.toList $ S.unions (map (predBindVars . untagPred) preds) `S.union` refineBindVars uty
        subs <- forM vars $ \v -> do
          v' <- freshPredVar
          return (v, v')
        let env = M.fromList subs
            rename v = fromMaybe v (M.lookup v env)
        return (map (substTaggedPredVars rename) preds, substRefineVars rename uty)
  tell preds'
  return $ substU (M.fromList (zip (map Left xs) xs')) uty'

-- | Substitute named or unification variables inside a unification type.
substU :: Map (Either Var IntVar) UType -> UType -> UType
substU m =
  ucata
    (\v -> fromMaybe (UVar v) (M.lookup (Right v) m))
    ( \case
        TyVarF v -> fromMaybe (UTyVar v) (M.lookup (Left v) m)
        f -> UTerm f
    )

-- | Replace quantified variables with skolem constants for annotation checking.
skolemize :: UPolytype -> Infer UType
skolemize (Forall xs _ uty) = do
  xs' <- mapM (const fresh) xs
  return $ substU (M.fromList (zip (map Left xs) (map toSkolem xs'))) uty
  where
    toSkolem u =
      case u of
        UVar v -> UTyVar (mkVarName "s" v)
        UTerm _ -> error "toSkolem: expected a fresh unification variable"

-- | Generate a fresh variable name by appending the integer identifier to a prefix.
mkVarName :: ByteString -> IntVar -> Var
mkVarName nm (IntVar v) = nm `BS.append` BS.pack (show (v + (maxBound :: Int) + 1))

-- | Generalize the free variables of a type that are not free in the current context.
generalize :: UType -> Infer UPolytype
generalize uty = do
  uty' <- applyBindings uty
  ctx <- ask
  tmfvs <- freeVars uty'
  ctxfvs <- freeVars ctx
  let fvs = S.toList $ tmfvs \\ ctxfvs
      xs = map (either id (mkVarName "a")) fvs
  return $ Forall xs [] (substU (M.fromList (zip fvs (map UTyVar xs))) uty')

-- | Generalize a type and attach the collected tagged refinement predicates to the resulting scheme.
generalizeWithPreds :: [TaggedPred] -> UType -> Infer UPolytype
generalizeWithPreds preds uty = do
  Forall xs _ ty <- generalize uty
  return $ Forall xs preds ty

-- | Lift a surface 'Polytype' to the unification-level representation by
-- replacing fixed-point type terms with 'UTerm' nodes.
toUPolytype :: Polytype -> UPolytype
toUPolytype = fmap unfreeze

-- | Freeze a unification-based type scheme back into a surface polytype.
fromUPolytype :: UPolytype -> Polytype
fromUPolytype = fmap (fromJust . freeze)

-- | Attach a source range to an error when it does not already carry one.
addErrorRange :: Range -> TypeError -> TypeError
addErrorRange r er = case er of
  UnboundVar Nothing bs -> UnboundVar (Just r) bs
  Infinite Nothing iv ut -> Infinite (Just r) iv ut
  Mismatch Nothing ty1 ty2 -> Mismatch (Just r) ty1 ty2
  InvalidPoly Nothing pty -> InvalidPoly (Just r) pty
  _ -> er

-- | Run an inference action and annotate any thrown error with a source range.
wrange :: Range -> Infer a -> Infer a
wrange r m = catchError m (throwError . addErrorRange r)

-- | Infer an expression and unify it with an expected type.
check :: Exp Range -> UType -> Infer ()
check e ty = wrange (firstParam e) $ do
  ty' <- infer e
  void $ ty =:= ty'

-- | Build a fresh cons-tuple spine large enough to support tuple projection.
makeNFresh :: Integer -> Infer ([UType], UType)
makeNFresh 0 = do
  t1 <- fresh
  t2 <- fresh
  return ([t1], UTyCons t1 t2) -- (fresh :. fresh)
makeNFresh n = do
  t1 <- fresh
  (ts, t2) <- makeNFresh (n - 1)
  return (t1 : ts, UTyCons t1 t2)

-- | Turn a list of element types into the nested cons-tuple representation.
mkTyConsFromVec :: [UType] -> UType
mkTyConsFromVec = foldr UTyCons UTyUnit

-- | Construct a normalized unification-level record type.
uTyRecord :: [(Var, UType)] -> UType
uTyRecord = UTerm . TyRecordF . normalizeRecordFields

-- | Return the first duplicate field name in a record literal, if any.
findDuplicateField :: Ord a => [(a, b)] -> Maybe a
findDuplicateField fields = go S.empty fields
  where
    go _ [] = Nothing
    go seen ((field, _):rest)
      | field `S.member` seen = Just field
      | otherwise = go (S.insert field seen) rest

-- | Normalize a shape type to a cons-list (@TyCons elt rest … TyUnit@)
-- whose elements all unify with @expectedElt@.  A bare scalar is promoted
-- to a singleton cons-list.  Returns the normalized type.
normalizeShapeToTupleOf :: Range -> UType -> UType -> Infer UType
normalizeShapeToTupleOf r expectedElt ty = do
  ty' <- applyBindings ty
  case stripRefineTop ty' of
    UTyCons t rest -> do
      _ <- wrange r $ t =:= expectedElt
      _ <- normalizeShapeToTupleOf r expectedElt rest
      return ty'
    UTyUnit -> return UTyUnit
    _ -> do
      _ <- wrange r $ ty' =:= expectedElt
      return $ UTyCons expectedElt UTyUnit

-- | Normalize a shape expression to the cons-list-of-'Int' encoding.
normalizeShapeToTupleOfInts :: Range -> UType -> Infer UType
normalizeShapeToTupleOfInts r = normalizeShapeToTupleOf r UTyInt

-- | Build an expected tuple-of-Int shape from a vector literal. This
-- constructs a cons-list of @UTyInt@ of the same arity and checks that each
-- element's inferred type is @Int@ (unifying where necessary).
expectedShapeFromExp :: Exp Range -> Infer UType
expectedShapeFromExp (EVec _ elems) = do
  forM_ elems $ \el -> do
    ety <- infer el
    void $ wrange (firstParam el) $ ety =:= UTyInt
  return $ mkTyConsFromVec (replicate (length elems) UTyInt)
expectedShapeFromExp e = normalizeShapeToTupleOfInts (firstParam e) =<< infer e

-- | Emit a known fact (hypothesis) into the writer.
emitHyp :: Pred -> Infer ()
emitHyp p = tell [Hyp p]

-- | Emit a safety obligation into the writer.
emitObl :: Pred -> Infer ()
emitObl p = tell [Obl p]

-- | Emit a predicate as a hypothesis (alias for 'emitHyp').
emitPred :: Pred -> Infer ()
emitPred = emitHyp

-- | Emit a non-fatal diagnostic warning.
emitWarning :: String -> Infer ()
emitWarning w = modify (\s -> s { inferWarnings = inferWarnings s ++ [w] })

-- | Run an action with an extra value-bound annotation in scope for one variable.
-- Emits @0 ≤ v@ and @v < t@ as SMT hypotheses so the validity checker can use
-- them when discharging index-safety obligations.
-- The annotation is removed (restored to prior state) when the action completes.
withValBound :: Var -> Term -> Infer a -> Infer a
withValBound v t m = do
  emitHyp (PLe (TConst 0) (TVar v))
  emitHyp (PLt (TVar v) t)
  old <- gets valBoundCtx
  modify (\s -> s { valBoundCtx = M.insert v t old })
  x <- m
  modify (\s -> s { valBoundCtx = old })
  return x

-- | Look up the value-bound annotation for a variable, if any.
lookupValBound :: Var -> Infer (Maybe Term)
lookupValBound v = gets (M.lookup v . valBoundCtx)

-- | Register a top-level single-parameter function's body for later
-- bound-transfer analysis.
withDefBody :: Var -> Var -> Exp Range -> Infer a -> Infer a
withDefBody f paramName body m = do
  old <- gets defBodyEnv
  modify (\s -> s { defBodyEnv = M.insert f (paramName, body) old })
  x <- m
  modify (\s -> s { defBodyEnv = old })
  return x

-- | Look up the registered @(paramName, bodyExp)@ for a top-level function.
lookupDefBody :: Var -> Infer (Maybe (Var, Exp Range))
lookupDefBody v = gets (M.lookup v . defBodyEnv)

-- | Propagate PBound annotations from a list of patterns into valBoundCtx for
-- the duration of an action.  Called around function body inference so that
-- @[i bound N]@ patterns are visible to 'EIndex' and other bound-sensitive
-- constructs inside the body.
withBoundsFromPats :: [Pat Range] -> Infer a -> Infer a
withBoundsFromPats [] m = m
withBoundsFromPats (PBound _ v bExp : ps) m = do
  bt <- termFromExp bExp
  withValBound v bt $ withBoundsFromPats ps m
withBoundsFromPats (PVec _ vs : ps) m =
  withBoundsFromPatsFlat vs $ withBoundsFromPats ps m
withBoundsFromPats (_ : ps) m = withBoundsFromPats ps m

-- | Like 'withBoundsFromPats' but for the flat sub-pattern list inside a PVec.
withBoundsFromPatsFlat :: [Pat Range] -> Infer a -> Infer a
withBoundsFromPatsFlat [] m = m
withBoundsFromPatsFlat (PBound _ v bExp : ps) m = do
  bt <- termFromExp bExp
  withValBound v bt $ withBoundsFromPatsFlat ps m
withBoundsFromPatsFlat (_ : ps) m = withBoundsFromPatsFlat ps m

-- | Extract a refinement term from a restricted set of integer expressions.
-- Unsupported forms are conservatively replaced by fresh predicate variables.
termFromExp :: Exp Range -> Infer Term
termFromExp expr =
  case expr of
    EInt _ n -> return $ TConst n
    ENeg _ e -> TNeg <$> termFromExp e
    EBinOp _ e1 op e2 -> do
      t1 <- termFromExp e1
      t2 <- termFromExp e2
      case op of
        Plus _ -> return $ TAdd t1 t2
        Minus _ -> return $ TSub t1 t2
        Times _ -> return $ TMul t1 t2
        _ -> TVar <$> freshPredVar
    _ -> do
      -- Infer the expression type and extract its refinement variable as the
      -- SMT term, so that obligations and hypotheses share the same variable.
      ty <- infer expr
      ty' <- applyBindings ty
      case fst (unwrapRefine ty') of
        Just v  -> return (TVar v)
        Nothing -> case expr of
          EVar _ v -> return (TVar v)  -- source-name fallback if no refinement wrapper
          _        -> TVar <$> freshPredVar

-- | Interpret index expressions as a list of refinement terms.
-- Vector indices map to per-dimension terms; scalars become singleton lists.
termsFromIndexExp :: Exp Range -> Infer [Term]
termsFromIndexExp (EVec _ elems) = mapM termFromExp elems
termsFromIndexExp (EUnit _) = pure []
termsFromIndexExp (EShapeOf _ arrExp) = do
  arrTy <- infer arrExp
  (mArrVar, sTy, _eTy) <- asArrayType (firstParam arrExp) arrTy
  sTy' <- normalizeShapeToTupleOf (firstParam arrExp) UTyInt sTy
  rank <- shapeArityFromType (firstParam arrExp) sTy'
  arrVar <- maybe freshPredVar return mArrVar
  return [TDim arrVar i | i <- [0 .. rank - 1]]
termsFromIndexExp e = (: []) <$> termFromExp e

-- | Extract the refinement pred var from each Int component of a shape cons-list type.
-- Shape types have the form: UTyCons (UTyRefine v0 UTyInt) (UTyCons ...) UTyUnit.
-- Returns pred vars in dimension order; components without a refinement wrapper are skipped.
extractShapeDimVars :: UType -> [Var]
extractShapeDimVars ty =
  case stripRefineTop ty of
    UTyUnit -> []
    UTyCons h rest -> maybeToList (fst (unwrapRefine h)) ++ extractShapeDimVars rest
    _ -> []

-- | Like 'termsFromIndexExp' but pairs each term with the source expression
-- it was derived from (for use by 'inferBVal' in bounds checking).
-- Returns 'Nothing' for dimensions coming from 'EShapeOf', which are runtime
-- values with no statically analysable expression.
termsAndExpsFromIndex :: Exp Range -> Infer [(Term, Maybe (Exp Range))]
termsAndExpsFromIndex (EVec _ elems) = do
  ts <- mapM termFromExp elems
  return $ zip ts (map Just elems)
termsAndExpsFromIndex (EUnit _) = return []
termsAndExpsFromIndex e@(EShapeOf _ _) = do
  ts <- termsFromIndexExp e
  return [(t, Nothing) | t <- ts]
termsAndExpsFromIndex e = do
  t <- termFromExp e
  return [(t, Just e)]

-- ---------------------------------------------------------------------------
-- Value-bound propagation for generator body expressions
-- ---------------------------------------------------------------------------

-- | Internal representation of a value bound during propagation.
-- 'BoundOf t' means the value is strictly less than @t@ (exclusive upper bound).
-- 'ExactVal t' means the value is exactly @t@ (a constant or shape variable).
data BVal = BoundOf Term | ExactVal Term

-- | Convert a 'BVal' to an exclusive upper bound 'Term'.
-- For an exact value @c@, the exclusive bound is @c + 1@.
toBound :: BVal -> Term
toBound (BoundOf t)  = t
toBound (ExactVal t) = TAdd t (TConst 1)

-- | Combine two 'BVal's under addition.
bvalAdd :: BVal -> BVal -> BVal
bvalAdd (BoundOf b)   (ExactVal c)  = BoundOf  (TAdd b c)
bvalAdd (ExactVal c)  (BoundOf b)   = BoundOf  (TAdd b c)
bvalAdd (BoundOf b1)  (BoundOf b2)  = BoundOf  (TAdd b1 b2)
bvalAdd (ExactVal c1) (ExactVal c2) = ExactVal (TAdd c1 c2)

-- | Combine two 'BVal's under subtraction.
-- When the right operand could be non-negative (the common case for index vars),
-- subtracting it from an exact value gives the same exact value as a conservative
-- upper bound; subtracting from a 'BoundOf' term decreases the bound.
bvalSub :: BVal -> BVal -> BVal
bvalSub (BoundOf b)   (ExactVal c)  = BoundOf  (TSub b c)
bvalSub (ExactVal c)  (BoundOf _)   = ExactVal c  -- y >= 0 so c - y <= c
bvalSub (ExactVal c1) (ExactVal c2) = ExactVal (TSub c1 c2)
bvalSub (BoundOf b1)  (BoundOf _)   = BoundOf  b1 -- conservative

-- | Multiply a 'BVal' by a positive integer constant.
bvalMulConst :: Integer -> BVal -> BVal
bvalMulConst k (BoundOf b)   = BoundOf  (TMul (TConst k) b)
bvalMulConst k (ExactVal c)  = ExactVal (TMul (TConst k) c)

-- | Conservatively upper-bound the maximum of two 'BVal's.
-- If @x < ba@ and @y < bb@ then @max(x,y) < max(ba, bb)@.
-- Mixed exact/bound cases use @toBound@ to lift the exact value before taking the max.
bvalMax :: BVal -> BVal -> BVal
bvalMax (BoundOf b1)  (BoundOf b2)  = BoundOf  (TMax b1 b2)
bvalMax (ExactVal c1) (ExactVal c2) = ExactVal  (TMax c1 c2)
bvalMax (BoundOf b)   (ExactVal c)  = BoundOf   (TMax b (TAdd c (TConst 1)))
bvalMax (ExactVal c)  (BoundOf b)   = BoundOf   (TMax (TAdd c (TConst 1)) b)

-- | Conservatively compute a 'BVal' for an integer expression, given the
-- current 'valBoundCtx'.  Returns 'Nothing' if the bound cannot be determined.
inferBVal :: Exp Range -> Infer (Maybe BVal)
inferBVal expr = case expr of
  EVar _ v -> do
    mb <- lookupValBound v
    return $ Just $ maybe (ExactVal (TVar v)) BoundOf mb
  EInt _ n -> return $ Just $ ExactVal (TConst n)
  EBinOp _ e1 op e2 -> case op of
    Plus  _ -> combine bvalAdd e1 e2
    Minus _ -> combine bvalSub e1 e2
    Times _ -> do
      mb1 <- inferBVal e1
      mb2 <- inferBVal e2
      return $ case (mb1, mb2, e1, e2) of
        (Just (ExactVal (TConst k)), Just b2, _, _) | k > 0 -> Just (bvalMulConst k b2)
        (Just b1, Just (ExactVal (TConst k)), _, _) | k > 0 -> Just (bvalMulConst k b1)
        _ -> Nothing
    Mod _ -> do
      mb2 <- inferBVal e2
      return $ case mb2 of
        Just (ExactVal m) -> Just (BoundOf m)  -- x % m  <  m  (exact modulus)
        Just (BoundOf m)  -> Just (BoundOf m)  -- x % m  <  m  (upper bound on modulus)
        _                 -> Nothing
    Divide _ -> do
      -- Integer division by a positive constant k: if 0 ≤ e < b then
      -- e / k ≤ (b-1) / k, so the exclusive upper bound is (b-1)/k + 1.
      -- Only handle concrete divisors (EInt); symbolic divisors fall through.
      mb1 <- inferBVal e1
      return $ case (mb1, e2) of
        (Just (ExactVal (TConst n)), EInt _ k) | k > 0 ->
          Just (ExactVal (TConst (n `div` k)))
        (Just (BoundOf (TConst b)), EInt _ k) | k > 0 ->
          Just (BoundOf (TConst ((b - 1) `div` k + 1)))
        _ -> Nothing
    _ -> return Nothing
  EIfThenElse _ _ e1 e2 -> do
    -- Bound of an if-then-else is the max of both branch bounds (conservative).
    mb1 <- inferBVal e1
    mb2 <- inferBVal e2
    return $ bvalMax <$> mb1 <*> mb2
  EApp _ (EApp _ (EVar _ "max") e1) e2 -> do
    -- Saturated application of the built-in `max` function.
    mb1 <- inferBVal e1
    mb2 <- inferBVal e2
    return $ bvalMax <$> mb1 <*> mb2
  _ -> return Nothing
  where
    combine f e1 e2 = do
      mb1 <- inferBVal e1
      mb2 <- inferBVal e2
      return $ f <$> mb1 <*> mb2

-- | Compute an exclusive upper bound 'Term' for an integer expression.
-- Returns 'Nothing' if the bound cannot be statically determined.
inferValBound :: Exp Range -> Infer (Maybe Term)
inferValBound e = fmap toBound <$> inferBVal e

-- | Extract the 'PBound' pattern info from an inline generator lambda.
-- Handles @let f [i bound E] = body in f@ (PVec containing a single PBound)
-- and @let f (i bound E) = body in f@ (bare PBound at the top level).
-- Returns @(indexVar, boundExp)@ if found, 'Nothing' otherwise.
extractPBoundInfo :: Exp Range -> Maybe (Var, Exp Range)
extractPBoundInfo (ELetIn _ (Dec _ _ [PVec _ [PBound _ v e]] _ _) _) = Just (v, e)
extractPBoundInfo (ELetIn _ (Dec _ _ [PBound _ v e]            _ _) _) = Just (v, e)
extractPBoundInfo _ = Nothing

-- | Extract the variable from a single parameter pattern.
-- Handles bare @x@ (PVar), vec-style @[x]@ (PVec [PVar]), and PBound.
patVar :: Pat Range -> Maybe Var
patVar (PVar _ v)          = Just v
patVar (PVec _ [PVar _ v]) = Just v
patVar (PBound _ v _)      = Just v
patVar _                   = Nothing

-- | Extract ordered parameter variable names from an inline function
-- expression of the form @let f p1 p2 ... = body in f@.
-- Returns @[]@ if the expression is not in this recognizable form.
extractFnParamVars :: Exp Range -> [Var]
extractFnParamVars (ELetIn _ (Dec _ _ pats _ _) _) = mapMaybe patVar pats
extractFnParamVars _                                = []

-- | Compute the value bound of an inline generator body, with the index
-- variable already registered in the caller's 'valBoundCtx'.
-- Used by Case 1 (PBound) and by EMap/EZipWith bound propagation.
inferBodyBound :: Exp Range -> Infer (Maybe Term)
inferBodyBound (ELetIn _ (Dec _ _ _ _ body) _) = inferValBound body
inferBodyBound _ = return Nothing

-- | Infer the terminal bound from a function whose body is a chain of
-- @EBoundLetIn@ nodes (Case 3 in EGenerate).  Traverses regular @ELetIn@
-- wrappers and @EBoundLetIn@ nodes (adding their declared bounds to
-- 'valBoundCtx' without emitting hypotheses), then returns the bound of the
-- final expression only when it is a variable declared in the chain above.
-- Returns 'Nothing' for arithmetic expressions or un-annotated variables,
-- so it never emits spurious bounds for bodies that were not explicitly
-- annotated.
inferBodyBoundChain :: Exp Range -> Infer (Maybe Term)
inferBodyBoundChain (ELetIn _ (Dec _ _ _ _ chainBody) _) = chainExprBound chainBody
inferBodyBoundChain _ = return Nothing

chainExprBound :: Exp Range -> Infer (Maybe Term)
chainExprBound (EBoundLetIn _ v boundExp _ body) = do
  bt <- termFromExp boundExp
  old <- gets valBoundCtx
  modify (\s -> s { valBoundCtx = M.insert v bt old })
  result <- chainExprBound body
  modify (\s -> s { valBoundCtx = old })
  return result
chainExprBound (ELetIn _ _ body) = chainExprBound body
chainExprBound (EVar _ v) = lookupValBound v
chainExprBound _ = return Nothing

-- | Extract the body expression from an inline function @let f p = body in f@.
extractFnBody :: Exp Range -> Maybe (Exp Range)
extractFnBody (ELetIn _ (Dec _ _ _ _ body) _) = Just body
extractFnBody _                                = Nothing

-- | Infer per-component value bounds from a vector-literal body @[e0, e1, ...]@.
-- Returns @Just [b0, b1, ...]@ when every component has an inferrable bound,
-- @Nothing@ otherwise (caller silently skips multi-dim bound emission).
inferVecElemBounds :: Exp Range -> Infer (Maybe [Term])
inferVecElemBounds (EVec _ es) = sequence <$> mapM inferValBound es
inferVecElemBounds _ = return Nothing

-- | Count the number of dimensions in a shape type after normalization.
shapeArityFromType :: Range -> UType -> Infer Int
shapeArityFromType r ty = do
  ty' <- normalizeShapeToTupleOf r UTyInt ty
  let go t =
        case stripRefineTop t of
          UTyUnit -> return 0
          UTyCons _ rest -> (1 +) <$> go rest
          _ -> throwError $ MiscError (Just r)
  go ty'

-- | Count the number of consecutive Int-argument layers at the front of a
-- (possibly refined) function type.  Used to determine the rank of a stencil
-- from the accessor type it accepts.
-- E.g. Int -> Int -> Float gives 2; Int -> Float gives 1; Float gives 0.
countIntAccessorArgs :: UType -> Int
countIntAccessorArgs ty = case stripRefineTop ty of
  UTyFun arg rest -> case stripRefineTop arg of
    UTyInt -> 1 + countIntAccessorArgs rest
    _      -> 0
  _ -> 0

-- | Strip the first n function-argument layers from a type, returning the
-- result type.  peelNArgs 2 (Int -> Int -> Float) = Float.
peelNArgs :: Int -> UType -> UType
peelNArgs 0 ty = ty
peelNArgs n ty = case stripRefineTop ty of
  UTyFun _ rest -> peelNArgs (n - 1) rest
  _             -> ty

-- | Extract a refined array: returns optional array binder, shape, and element.
asArrayType :: Range -> UType -> Infer (Maybe Var, UType, UType)
asArrayType r ty = do
  ty' <- applyBindings ty
  let (mv, inner) = unwrapRefine ty'
  case stripRefineTop inner of
    UTyArray s e -> return (mv, s, e)
    UVar _ -> do
      s <- fresh
      e <- fresh
      _ <- wrange r $ inner =:= UTyArray s e
      return (mv, s, e)
    _ -> throwError $ MiscError (Just r)

-- | Create a fresh refinement binder for a type.
freshRefined :: UType -> Infer (Var, UType)
freshRefined ty = do
  v <- freshPredVar
  return (v, wrapRefine v ty)

-- | Emit refinement equalities between a function parameter type and an
-- argument type. Arrays use per-dimension equality; other refined values
-- link their refinement variables directly.
emitRefineLink :: Range -> UType -> UType -> Infer ()
emitRefineLink r expected actual = do
  expected' <- applyBindings expected
  actual' <- applyBindings actual
  let (mvExp, innerExp) = unwrapRefine expected'
      (mvAct, innerAct) = unwrapRefine actual'
  case (mvExp, mvAct) of
    (Just vExp, Just vAct) ->
      case (stripRefineTop innerExp, stripRefineTop innerAct) of
        (UTyArray s1 _, UTyArray s2 _) -> do
          _ <- wrange r $ s1 =:= s2
          rank <- shapeArityFromType r s1
          forM_ [0 .. rank - 1] $ \i -> do
            emitPred (PEq (TDim vExp i) (TDim vAct i))
            emitPred (PEq (TValBoundDim vExp i) (TValBoundDim vAct i))
        _ -> emitPred (PEq (TVar vExp) (TVar vAct))
    _ -> return ()


-- | Decompose a type into its two pair (or cons-tuple) components.
-- Handles 'UTyPair', cons-list shapes (@TyCons t1 (TyCons t2 TyUnit)@), and
-- unknown types (unified against 'UTyPair' via fresh metavariables).
extractPair :: Range -> UType -> Infer (UType, UType)
extractPair r pty = case stripRefineTop pty of
  UTyPair t1 t2 -> return (t1, t2)
  UTyCons _ _ -> do
    ty1 <- fresh; ty2 <- fresh
    _ <- wrange r $ UTyCons ty1 (UTyCons ty2 UTyUnit) =:= pty
    return (ty1, ty2)
  _ -> do
    ty1 <- fresh; ty2 <- fresh
    _ <- wrange r $ UTyPair ty1 ty2 =:= pty
    return (ty1, ty2)

-- | Infer the type of an expression, emitting any refinement obligations.
infer :: Exp Range -> Infer UType
infer (EInt _ n) = do
  v <- freshPredVar
  emitPred (PEq (TVar v) (TConst n))
  return $ wrapRefine v UTyInt
infer (EFloat _ _) = return UTyFloat
infer (EVar r x) = vlookup r x
infer (EString _ _) = do
  v <- freshPredVar
  return $ wrapRefine v UTyString
infer (EUnit _) = do
  v <- freshPredVar
  return $ wrapRefine v UTyUnit
infer (EBool _ _) = do
  v <- freshPredVar
  return $ wrapRefine v UTyBool
infer (EProj r i e) = do
  t1 <- infer e
  t1b <- applyBindings t1
  (ts, t2) <- makeNFresh i
  _ <- wrange r $ t1b =:= t2
  _ <- applyBindings t2
  let elt = ts !! fromIntegral i
  applyBindings elt
infer (ERecordProj r e field) = do
  recTy <- infer e
  recTy' <- applyBindings recTy
  case stripRefineTop recTy' of
    UTyRecord fields ->
      case lookup field fields of
        Just fieldTy -> applyBindings fieldTy
        Nothing -> throwError $ MissingRecordField (Just r) field
    _ -> do
      fieldTy <- fresh
      _ <- wrange r $ recTy =:= uTyRecord [(field, fieldTy)]
      applyBindings fieldTy
infer (EBinOp _ e1 op e2) = case op of
  Plus _    -> ii UTyInt;   Minus _   -> ii UTyInt
  Times _   -> ii UTyInt;   Divide _  -> ii UTyInt;  Mod _     -> ii UTyInt
  Eq _      -> ii UTyBool;  Neq _     -> ii UTyBool
  Lt _      -> ii UTyBool;  Le _      -> ii UTyBool
  Gt _      -> ii UTyBool;  Ge _      -> ii UTyBool
  And _     -> bb;          Or _      -> bb
  PlusF _   -> ff UTyFloat; MinusF _  -> ff UTyFloat
  TimesF _  -> ff UTyFloat; DivideF _ -> ff UTyFloat
  EqF _     -> ff UTyBool;  NeqF _    -> ff UTyBool
  LtF _     -> ff UTyBool;  LeF _     -> ff UTyBool
  GtF _     -> ff UTyBool;  GeF _     -> ff UTyBool
  where
    ii res = check e1 UTyInt   >> check e2 UTyInt   >> return res
    ff res = check e1 UTyFloat >> check e2 UTyFloat >> return res
    bb     = check e1 UTyBool  >> check e2 UTyBool  >> return UTyBool
infer (EUnOp r uop e) =
  case uop of
    Not _     -> check e UTyBool >> return UTyBool
    Fst _     -> infer e >>= applyBindings >>= fmap fst . extractPair r
    Snd _     -> infer e >>= applyBindings >>= fmap snd . extractPair r
    Sqrt _    -> check e UTyFloat >> return UTyFloat
    ExpF _    -> check e UTyFloat >> return UTyFloat
    Log  _    -> check e UTyFloat >> return UTyFloat
    Sin  _    -> check e UTyFloat >> return UTyFloat
    Cos  _    -> check e UTyFloat >> return UTyFloat
    AbsF _    -> check e UTyFloat >> return UTyFloat
    FloorF _  -> check e UTyFloat >> return UTyFloat
    CeilF  _  -> check e UTyFloat >> return UTyFloat
    Erf  _    -> check e UTyFloat >> return UTyFloat
    FloatOf _ -> check e UTyInt   >> return UTyFloat
    IntOf _   -> check e UTyFloat >> return UTyInt
infer (EApp r e1 e2) = do
  ty1 <- infer e1
  ty2 <- infer e2
  ty <- fresh
  _ <- wrange r $ ty1 =:= UTyFun ty2 ty
  ty1' <- applyBindings ty1
  case stripRefineTop ty1' of
    UTyFun argTy resTy -> do
      emitRefineLink r argTy ty2
      -- Propagate scalar value bounds from the argument to the parameter's
      -- refinement variable.  This grounds parameter-dependent obligations in
      -- the callee when the argument has a statically inferrable bound (e.g.
      -- from iota-produced values, arithmetic like i%N, or PBound indices).
      mBound <- inferBVal e2
      argTy' <- applyBindings argTy
      forM_ mBound $ \bval ->
        case fst (unwrapRefine argTy') of
          Just pParam -> do
            emitHyp (PLe (TConst 0) (TVar pParam))
            emitHyp (PLt (TVar pParam) (toBound bval))
          Nothing -> return ()
      return resTy
    _ -> return ty
infer (EIfThen _ e1 e2) = do
  check e1 UTyBool
  infer e2
infer (EIfThenElse r e1 e2 e3) = do
  check e1 UTyBool
  ty2 <- infer e2
  ty3 <- infer e3
  _ <- wrange r $ ty2 =:= ty3
  return ty2
infer (ENeg _ e) = do
  t <- infer e
  t' <- applyBindings t
  case stripRefineTop t' of
    UTyFloat -> return UTyFloat
    UTyInt   -> return UTyInt
    UVar _   -> do
      _ <- t' =:= UTyInt
      return UTyInt
    _ -> throwError $ MiscError Nothing
infer (EVec _ es) = do
  tys <- mapM infer es
  return $ mkTyConsFromVec tys
infer (EPair _ e1 e2) = UTyPair <$> infer e1 <*> infer e2
infer (ERecord r fields) = do
  case findDuplicateField fields of
    Just field -> throwError $ DuplicateRecordField (Just r) field
    Nothing -> do
      inferredFields <- mapM (\(f, e) -> (f,) <$> infer e) fields
      return $ uTyRecord inferredFields
infer (EShapeOf _ arrExp) = do
  arrTy <- infer arrExp
  (_mArrVar, sTy, _eTy) <- asArrayType (firstParam arrExp) arrTy
  normalizeShapeToTupleOf (firstParam arrExp) UTyInt sTy
infer (EGenerate _ shapeExp fn) = do
  sTy      <- infer shapeExp
  expected <- expectedShapeFromExp shapeExp
  _ <- wrange (firstParam shapeExp) $ expected =:= sTy
  sTy'   <- applyBindings expected
  fty    <- infer fn
  elemTy <- fresh
  fty'   <- applyBindings fty
  case fty' of
    UTyFun argTy _ -> do
      _ <- wrange (firstParam shapeExp) $ argTy =:= sTy'
      return ()
    _ -> return ()
  _ <- wrange (firstParam shapeExp) $ fty =:= UTyFun sTy' elemTy
  (arrVar, arrTy) <- freshRefined (UTyArray sTy' elemTy)
  shapeTerms <- termsFromIndexExp shapeExp
  forM_ (zip [0 ..] shapeTerms) $ \(i, t) -> do
    emitPred (PEq (TDim arrVar i) t)
    -- Implicit invariant: all generate dimensions are ≥ 1.  Emit as hypothesis
    -- so the validity checker can use it (e.g., index [0] on a symbolic-size
    -- array is safe when dim ≥ 1).
    emitHyp (PLe (TConst 1) (TDim arrVar i))
  -- Emit bounds hypotheses for the generator function's index argument pred vars.
  -- For each dimension i, assert that the index component is in [0, TDim arrVar i).
  -- Pred vars are extracted from the function's argument type (fty'), since after
  -- unification sTy' holds fresh pred vars distinct from those in the function's type.
  fty'' <- applyBindings fty
  let argDimVars = case stripRefineTop fty'' of
        UTyFun argTy _ -> extractShapeDimVars argTy
        _              -> []
  forM_ (zip [0 ..] argDimVars) $ \(i, dv) -> do
    emitHyp (PLe (TConst 0) (TVar dv))
    emitHyp (PLt (TVar dv) (TDim arrVar i))
  -- Attempt to infer value bound from generator body.
  -- Case 1: PBound annotation present — scalar bound with a declared index range.
  -- Case 2: EVec body — per-component bounds for tuple-producing generators.
  case extractPBoundInfo fn of
    Just (idxVar, idxBoundExp) -> do
      idxBoundTerm <- termFromExp idxBoundExp
      -- Soundness: shape must not exceed the declared input bound.
      emitObl (PLe (TDim arrVar 0) idxBoundTerm)
      -- Infer output bound from body expression.
      mOutBound <- withValBound idxVar idxBoundTerm $ inferBodyBound fn
      case mOutBound of
        Just outBound -> emitPred (PEq (TValBoundDim arrVar 0) outBound)
        Nothing -> emitWarning "note: generator body value bound not statically inferred"
    Nothing ->
      -- Try per-component bounds when the body is an EVec literal.
      -- Bind the first index variable from the parameter pattern so that
      -- components like @k@ can be resolved relative to the array dimension.
      case extractFnParamVars fn of
        (idxVar : _) -> do
          let idxBound = TDim arrVar 0
          mCompBounds <- withValBound idxVar idxBound $ do
            case extractFnBody fn of
              Just body -> inferVecElemBounds body
              Nothing   -> return Nothing
          forM_ (maybe [] (zip [0..]) mCompBounds) $ \(i, b) ->
            emitPred (PEq (TValBoundDim arrVar i) b)
          -- Case 3: EBoundLetIn chain in the body — try scalar bound propagation.
          -- Runs only when Case 2 (EVec) found no per-component bounds.
          -- Case 4: Arithmetic fallback — when the chain analysis also yields nothing,
          -- fall back to inferBodyBound which uses inferBVal to handle expressions
          -- like @i + 1@, @i % M@, @i * k@, and constant bodies without requiring a
          -- PBound annotation.  This closes the false-negative gap where, e.g.,
          -- @generate [N] (let f [i] = i + 1 in f)@ was silently accepted as a
          -- gather index into an N-element array.
          when (isNothing mCompBounds) $ do
            mOutBound <- withValBound idxVar idxBound $ do
              mChain <- inferBodyBoundChain fn
              case mChain of
                Just _  -> return mChain
                Nothing -> inferBodyBound fn   -- arithmetic fallback (Case 4)
            forM_ mOutBound $ \outBound ->
              emitPred (PEq (TValBoundDim arrVar 0) outBound)
        [] -> return ()
  return arrTy
infer (EReadArray _ shapeExp fileExp) = do
  fTy <- infer fileExp
  _ <- wrange (firstParam fileExp) $ fTy =:= UTyString
  sTy <- infer shapeExp
  sTy' <- normalizeShapeToTupleOf (firstParam shapeExp) UTyInt sTy
  (arrVar, arrTy) <- freshRefined (UTyArray sTy' UTyInt)
  shapeTerms <- termsFromIndexExp shapeExp
  forM_ (zip [0 ..] shapeTerms) $ \(i, t) ->
    emitPred (PEq (TDim arrVar i) t)
  return arrTy
infer (EReadArrayFloat _ shapeExp fileExp) = do
  fTy <- infer fileExp
  _ <- wrange (firstParam fileExp) $ fTy =:= UTyString
  sTy <- infer shapeExp
  sTy' <- normalizeShapeToTupleOf (firstParam shapeExp) UTyInt sTy
  (arrVar, arrTy) <- freshRefined (UTyArray sTy' UTyFloat)
  shapeTerms <- termsFromIndexExp shapeExp
  forM_ (zip [0 ..] shapeTerms) $ \(i, t) ->
    emitPred (PEq (TDim arrVar i) t)
  return arrTy
infer (EWriteArray _ arrExp fileExp) = do
  arrTy <- infer arrExp
  (_mArrVar, _sTy, eTy) <- asArrayType (firstParam arrExp) arrTy
  _ <- wrange (firstParam arrExp) $ eTy =:= UTyInt
  fTy <- infer fileExp
  _ <- wrange (firstParam fileExp) $ fTy =:= UTyString
  return UTyUnit
infer (EWriteArrayFloat _ arrExp fileExp) = do
  arrTy <- infer arrExp
  (_mArrVar, _sTy, eTy) <- asArrayType (firstParam arrExp) arrTy
  _ <- wrange (firstParam arrExp) $ eTy =:= UTyFloat
  fTy <- infer fileExp
  _ <- wrange (firstParam fileExp) $ fTy =:= UTyString
  return UTyUnit
infer (EGetEnvInt _ varExp) = do
  vTy <- infer varExp
  _ <- wrange (firstParam varExp) $ vTy =:= UTyString
  return UTyInt
infer (EGetEnvString _ varExp) = do
  vTy <- infer varExp
  _ <- wrange (firstParam varExp) $ vTy =:= UTyString
  return UTyString
infer (EFill _ shapeExp valExp) = do
  sTy <- infer shapeExp
  sTy' <- normalizeShapeToTupleOf (firstParam shapeExp) UTyInt sTy
  elemTy <- infer valExp
  (arrVar, arrTy) <- freshRefined (UTyArray sTy' elemTy)
  shapeTerms <- termsFromIndexExp shapeExp
  forM_ (zip [0 ..] shapeTerms) $ \(i, t) ->
    emitPred (PEq (TDim arrVar i) t)
  -- Propagate value bound from the fill value expression.
  mBound <- inferValBound valExp
  forM_ mBound $ \b -> emitPred (PEq (TValBoundDim arrVar 0) b)
  return arrTy
infer (EReplicate _ shapeDims arrExp) = do
  arrTy <- infer arrExp
  (mArrVar, sTy, eTy) <- asArrayType (firstParam arrExp) arrTy
  sTy' <- normalizeShapeToTupleOf (firstParam arrExp) UTyInt sTy
  srcRank <- shapeArityFromType (firstParam arrExp) sTy'
  srcVar <- maybe freshPredVar return mArrVar
  let step (terms, explicitFlags, srcIx) dim =
        case dim of
          ShapeAll _ ->
            if srcIx >= srcRank
              then throwError $ MiscError (Just (firstParam arrExp))
              else return (terms ++ [TDim srcVar srcIx], explicitFlags ++ [False], srcIx + 1)
          ShapeAny _ e -> do
            ty <- infer e
            _ <- wrange (firstParam e) $ ty =:= UTyInt
            t <- termFromExp e
            return (terms ++ [t], explicitFlags ++ [True], srcIx)
          ShapeDim _ e -> do
            ty <- infer e
            _ <- wrange (firstParam e) $ ty =:= UTyInt
            t <- termFromExp e
            return (terms ++ [t], explicitFlags ++ [True], srcIx)
  (outTerms, explicitFlags, srcIxEnd) <- foldM step ([], [], 0) shapeDims
  when (srcIxEnd /= srcRank) $ throwError $ MiscError (Just (firstParam arrExp))
  forM_ (zip outTerms explicitFlags) $ \(t, isExplicit) ->
    when isExplicit $ emitPred (PLe (TConst 0) t)
  let outShape = mkTyConsFromVec (replicate (length outTerms) UTyInt)
  (arrVar, arrTyOut) <- freshRefined (UTyArray outShape eTy)
  forM_ (zip [0 ..] outTerms) $ \(i, t) ->
    emitPred (PEq (TDim arrVar i) t)
  -- Propagate value bound: replicate preserves elements unchanged.
  case mArrVar of
    Just sv -> emitPred (PEq (TValBoundDim arrVar 0) (TValBoundDim sv 0))
    Nothing -> return ()
  return arrTyOut
infer (ESlice _ sliceDims arrExp) = do
  arrTy <- infer arrExp
  (mArrVar, sTy, eTy) <- asArrayType (firstParam arrExp) arrTy
  sTy' <- normalizeShapeToTupleOf (firstParam arrExp) UTyInt sTy
  srcRank <- shapeArityFromType (firstParam arrExp) sTy'
  when (length sliceDims /= srcRank) $ throwError $ MiscError (Just (firstParam arrExp))
  srcVar <- maybe freshPredVar return mArrVar
  forM_ (zip [0 ..] sliceDims) $ \(i, dim) ->
    case dim of
      SliceAll _ -> return ()
      SliceRange _ start len -> do
        startTy <- infer start
        lenTy <- infer len
        _ <- wrange (firstParam start) $ startTy =:= UTyInt
        _ <- wrange (firstParam len) $ lenTy =:= UTyInt
        tStart <- termFromExp start
        tLen <- termFromExp len
        emitObl (PLe (TConst 0) tStart)
        emitObl (PLe (TConst 0) tLen)
        emitObl (PLe (TAdd tStart tLen) (TDim srcVar i))
  let outShape = mkTyConsFromVec (replicate srcRank UTyInt)
  (arrVar, arrTyOut) <- freshRefined (UTyArray outShape eTy)
  forM_ (zip [0 ..] sliceDims) $ \(i, dim) ->
    case dim of
      SliceAll _ -> emitPred (PEq (TDim arrVar i) (TDim srcVar i))
      SliceRange _ _ len -> do
        tLen <- termFromExp len
        emitPred (PEq (TDim arrVar i) tLen)
  -- Propagate value bound: slice returns a subset of source elements.
  case mArrVar of
    Just sv -> emitPred (PEq (TValBoundDim arrVar 0) (TValBoundDim sv 0))
    Nothing -> return ()
  return arrTyOut
infer (EReshape _ shapeExp arrExp) = do
  arrTy <- infer arrExp
  (mArrVar, sTy, eTy) <- asArrayType (firstParam arrExp) arrTy
  sTy' <- normalizeShapeToTupleOf (firstParam arrExp) UTyInt sTy
  srcRank <- shapeArityFromType (firstParam arrExp) sTy'
  sOutTy <- infer shapeExp
  expected <- expectedShapeFromExp shapeExp
  _ <- wrange (firstParam shapeExp) $ expected =:= sOutTy
  sOutTy' <- applyBindings expected
  srcVar <- maybe freshPredVar return mArrVar
  let srcTerms = [TDim srcVar i | i <- [0 .. srcRank - 1]]
  outTerms <- termsFromIndexExp shapeExp
  let termProduct [] = TConst 1
      termProduct (t : ts) = foldl TMul t ts
  emitPred (PEq (termProduct srcTerms) (termProduct outTerms))
  forM_ outTerms $ \t -> emitPred (PLe (TConst 0) t)
  (arrVar, arrTyOut) <- freshRefined (UTyArray sOutTy' eTy)
  forM_ (zip [0 ..] outTerms) $ \(i, t) ->
    emitPred (PEq (TDim arrVar i) t)
  -- Propagate value bound: reshape rearranges elements without changing values.
  case mArrVar of
    Just sv -> emitPred (PEq (TValBoundDim arrVar 0) (TValBoundDim sv 0))
    Nothing -> return ()
  return arrTyOut
infer (EMap _ fn arrExp) = do
  arrTy <- infer arrExp
  (mArrVar, sTy, eTy) <- asArrayType (firstParam arrExp) arrTy
  fty <- infer fn
  outTy <- fresh
  _ <- wrange (firstParam arrExp) $ fty =:= UTyFun eTy outTy
  (arrVar, arrTyOut) <- freshRefined (UTyArray sTy outTy)
  case mArrVar of
    Nothing -> return arrTyOut
    Just srcVar -> do
      rank <- shapeArityFromType (firstParam arrExp) sTy
      forM_ [0 .. rank - 1] $ \i ->
        emitPred (PEq (TDim arrVar i) (TDim srcVar i))
      -- Propagate value bounds through the map function body.
      -- Case 1: scalar body — infer a single component-0 output bound.
      -- Case 2: EVec body — infer per-component bounds for tuple-producing maps.
      case extractFnParamVars fn of
        (paramVar : _) -> do
          let srcElemBound = TValBoundDim srcVar 0
          mOutBound <- withValBound paramVar srcElemBound $ inferBodyBound fn
          case mOutBound of
            Just outBound -> emitPred (PEq (TValBoundDim arrVar 0) outBound)
            Nothing -> do
              -- Try per-component bounds when the body is an EVec literal.
              mCompBounds <- withValBound paramVar srcElemBound $ do
                case extractFnBody fn of
                  Just body -> inferVecElemBounds body
                  Nothing   -> return Nothing
              forM_ (maybe [] (zip [0..]) mCompBounds) $ \(i, b) ->
                emitPred (PEq (TValBoundDim arrVar i) b)
        [] -> do
          -- Non-inline named function: get the parameter pred var from the
          -- function type after unification.  Using eTy directly doesn't work
          -- because asArrayType returns a concrete element type (e.g. UTyInt)
          -- with no refinement wrapper — the pred var lives in fty's argument
          -- position, not in eTy.
          fty' <- applyBindings fty
          case stripRefineTop fty' of
            UTyFun paramTy _ ->
              case fst (unwrapRefine paramTy) of
                Just elemPV -> do
                  emitHyp (PLe (TConst 0) (TVar elemPV))
                  emitHyp (PLt (TVar elemPV) (TValBoundDim srcVar 0))
                Nothing -> return ()
            _ -> return ()
          -- Independently attempt to establish the output element bound by
          -- running inferValBound on the callee's registered definition body
          -- with the parameter name bounded by the source element bound.
          -- This covers non-inline functions like `let f x = x+1`.
          case fn of
            EVar _ f -> do
              mDefBody <- lookupDefBody f
              forM_ mDefBody $ \(paramName, defBody) -> do
                mOutBound <- withValBound paramName (TValBoundDim srcVar 0) $
                             inferValBound defBody
                forM_ mOutBound $ \outBound ->
                  emitPred (PEq (TValBoundDim arrVar 0) outBound)
            _ -> return ()
      return arrTyOut
infer (EZipWith _ fn arrExp1 arrExp2) = do
  arrTy1 <- infer arrExp1
  arrTy2 <- infer arrExp2
  (mVar1, sTy1, eTy1) <- asArrayType (firstParam arrExp1) arrTy1
  (mVar2, sTy2, eTy2) <- asArrayType (firstParam arrExp2) arrTy2
  _ <- wrange (firstParam arrExp1) $ sTy1 =:= sTy2
  fty <- infer fn
  outTy <- fresh
  _ <- wrange (firstParam fn) $ fty =:= UTyFun eTy1 (UTyFun eTy2 outTy)
  sTy <- applyBindings sTy1
  (arrVar, arrTyOut) <- freshRefined (UTyArray sTy outTy)
  case (mVar1, mVar2) of
    (Just v1, Just v2) -> do
      rank <- shapeArityFromType (firstParam arrExp1) sTy
      forM_ [0 .. rank - 1] $ \i ->
        emitPred (PEq (TDim v1 i) (TDim v2 i))
      forM_ [0 .. rank - 1] $ \i ->
        emitPred (PEq (TDim arrVar i) (TDim v1 i))
      -- Propagate value bounds through the binary function body.
      case extractFnParamVars fn of
        (p1 : p2 : _) -> do
          let b1 = TValBoundDim v1 0
              b2 = TValBoundDim v2 0
          mOutBound <- withValBound p1 b1 $
                         withValBound p2 b2 $
                           inferBodyBound fn
          forM_ mOutBound $ \outBound ->
            emitPred (PEq (TValBoundDim arrVar 0) outBound)
        _ -> return ()
      return arrTyOut
    _ -> return arrTyOut
infer (EReduce _ fn initExp arrExp) = do
  arrTy <- infer arrExp
  initTy <- infer initExp
  (mArrVar, sTy, eTy) <- asArrayType (firstParam arrExp) arrTy
  sTy' <- normalizeShapeToTupleOf (firstParam arrExp) UTyInt sTy
  fty <- infer fn
  rank <- shapeArityFromType (firstParam arrExp) sTy'
  when (rank <= 0) $ throwError $ MiscError (Just (firstParam arrExp))
  let outShape = mkTyConsFromVec (replicate (rank - 1) UTyInt)
  _ <- wrange (firstParam arrExp) $ fty =:= UTyFun eTy (UTyFun eTy eTy)
  _ <- wrange (firstParam initExp) $ initTy =:= eTy
  (arrVar, arrTyOut) <- freshRefined (UTyArray outShape eTy)
  case mArrVar of
    Nothing -> return arrTyOut
    Just srcVar -> do
      forM_ [0 .. rank - 2] $ \i ->
        emitPred (PEq (TDim arrVar i) (TDim srcVar i))
      -- Conservative value bound: propagate element bound through the binary
      -- reduce function body.  Sound for monotone functions (max, min).
      case extractFnParamVars fn of
        (p1 : p2 : _) -> do
          let elemBound = TValBoundDim srcVar 0
          mOutBound <- withValBound p1 elemBound $
                         withValBound p2 elemBound $
                           inferBodyBound fn
          forM_ mOutBound $ \outBound ->
            emitPred (PEq (TValBoundDim arrVar 0) outBound)
        _ -> return ()
      return arrTyOut
infer (EReduceGenerate r fn initExp shapeExp genFn) =
  infer (EReduce r fn initExp (EGenerate r shapeExp genFn))
infer (EFoldl _ fn initExp arrExp) = do
  arrTy  <- infer arrExp
  initTy <- infer initExp
  (_mArrVar, sTy, eTy) <- asArrayType (firstParam arrExp) arrTy
  sTy' <- normalizeShapeToTupleOf (firstParam arrExp) UTyInt sTy
  fty <- infer fn
  rank <- shapeArityFromType (firstParam arrExp) sTy'
  when (rank /= 1) $ throwError $ MiscError (Just (firstParam arrExp))
  _ <- wrange (firstParam fn) $ fty =:= UTyFun initTy (UTyFun eTy initTy)
  return initTy
infer (EFoldlWhile _ predExp fn initExp arrExp) = do
  arrTy  <- infer arrExp
  initTy <- infer initExp
  (_mArrVar, sTy, eTy) <- asArrayType (firstParam arrExp) arrTy
  sTy' <- normalizeShapeToTupleOf (firstParam arrExp) UTyInt sTy
  pty <- infer predExp
  fty <- infer fn
  rank <- shapeArityFromType (firstParam arrExp) sTy'
  when (rank /= 1) $ throwError $ MiscError (Just (firstParam arrExp))
  _ <- wrange (firstParam predExp) $ pty =:= UTyFun initTy UTyBool
  _ <- wrange (firstParam fn) $ fty =:= UTyFun initTy (UTyFun eTy initTy)
  return initTy
infer (EScan _ fn initExp arrExp) = do
  arrTy <- infer arrExp
  initTy <- infer initExp
  (mArrVar, sTy, eTy) <- asArrayType (firstParam arrExp) arrTy
  sTy' <- normalizeShapeToTupleOf (firstParam arrExp) UTyInt sTy
  fty <- infer fn
  rank <- shapeArityFromType (firstParam arrExp) sTy'
  when (rank /= 1) $ throwError $ MiscError (Just (firstParam arrExp))
  _ <- wrange (firstParam fn) $ fty =:= UTyFun initTy (UTyFun eTy initTy)
  (arrVar, arrTyOut) <- freshRefined (UTyArray sTy' initTy)
  case mArrVar of
    Nothing -> return arrTyOut
    Just srcVar -> do
      emitPred (PEq (TDim arrVar 0) (TDim srcVar 0))
      -- Conservative value bound: propagate element bound through the scan
      -- function body (same approach as EReduce; sound for monotone functions).
      case extractFnParamVars fn of
        (p1 : p2 : _) -> do
          let elemBound = TValBoundDim srcVar 0
          mOutBound <- withValBound p1 elemBound $
                         withValBound p2 elemBound $
                           inferBodyBound fn
          forM_ mOutBound $ \outBound ->
            emitPred (PEq (TValBoundDim arrVar 0) outBound)
        _ -> return ()
      return arrTyOut
infer (ESegmentedReduce _ fn initExp offsetsExp valsExp) = do
  offsetsTy <- infer offsetsExp
  valsTy <- infer valsExp
  initTy <- infer initExp
  (_mOffsetsVar, sOffsets, eOffsets) <- asArrayType (firstParam offsetsExp) offsetsTy
  (_mValsVar, sVals, eVals) <- asArrayType (firstParam valsExp) valsTy
  sOffsets' <- normalizeShapeToTupleOf (firstParam offsetsExp) UTyInt sOffsets
  sVals' <- normalizeShapeToTupleOf (firstParam valsExp) UTyInt sVals
  offsetsRank <- shapeArityFromType (firstParam offsetsExp) sOffsets'
  valsRank <- shapeArityFromType (firstParam valsExp) sVals'
  when (offsetsRank /= 1 || valsRank /= 1) $
    throwError $ MiscError (Just (firstParam offsetsExp))
  _ <- wrange (firstParam offsetsExp) $ eOffsets =:= UTyInt
  fty <- infer fn
  _ <- wrange (firstParam fn) $ fty =:= UTyFun initTy (UTyFun eVals initTy)
  let outShape = mkTyConsFromVec [UTyInt]
  pure $ UTyArray outShape initTy
infer (ESortIndices _ arrExp) = do
  arrTy <- infer arrExp
  (mArrVar, sTy, eTy) <- asArrayType (firstParam arrExp) arrTy
  sTy' <- normalizeShapeToTupleOf (firstParam arrExp) UTyInt sTy
  rank <- shapeArityFromType (firstParam arrExp) sTy'
  when (rank /= 1) $ throwError $ MiscError (Just (firstParam arrExp))
  _ <- wrange (firstParam arrExp) $ eTy =:= UTyInt
  (arrVar, arrTyOut) <- freshRefined (UTyArray sTy' UTyInt)
  case mArrVar of
    Nothing -> return arrTyOut
    Just srcVar -> do
      emitPred (PEq (TDim arrVar 0) (TDim srcVar 0))
      emitPred (PEq (TValBoundDim arrVar 0) (TDim srcVar 0))
      return arrTyOut
infer (EIota _ nExp) = do
  nTy <- infer nExp
  _ <- wrange (firstParam nExp) $ nTy =:= UTyInt
  nTy' <- applyBindings nTy
  let sTy' = UTyCons nTy' UTyUnit
  (arrVar, arrTy) <- freshRefined (UTyArray sTy' UTyInt)
  shapeTerms <- termsFromIndexExp nExp
  forM_ (zip [0 ..] shapeTerms) $ \(i, t) ->
    emitPred (PEq (TDim arrVar i) t)
  emitPred (PEq (TValBoundDim arrVar 0) (TDim arrVar 0))
  return arrTy
infer (EMakeIndex _ nExp arrExp) = do
  nTy   <- infer nExp
  arrTy <- infer arrExp
  _     <- wrange (firstParam nExp) $ nTy =:= UTyInt
  (mArrVar, sTy, eTy) <- asArrayType (firstParam arrExp) arrTy
  sTy' <- normalizeShapeToTupleOf (firstParam arrExp) UTyInt sTy
  rank <- shapeArityFromType (firstParam arrExp) sTy'
  srcVar <- maybe freshPredVar return mArrVar
  -- Create a fresh refined output type so the user-declared TValBoundDim is
  -- independent of any value bound already established on arrExp.  This
  -- prevents a contradiction when arrExp is, e.g., a generate whose body
  -- has an auto-inferred bound that differs from the declared N.
  (arrVarOut, arrTyOut) <- freshRefined (UTyArray sTy' eTy)
  -- Copy shape constraints from input.
  forM_ [0 .. rank - 1] $ \i ->
    emitPred (PEq (TDim arrVarOut i) (TDim srcVar i))
  -- Assert the user-declared exclusive upper bound on element values.
  nTerm <- termFromExp nExp
  emitPred (PEq (TValBoundDim arrVarOut 0) nTerm)
  emitWarning "note: make_index bound not statically verified"
  return arrTyOut
infer (ECOOSumDuplicates _ nrowsExp ncolsExp nnzExp rowsExp colsExp valsExp) = do
  nrowsTy <- infer nrowsExp
  ncolsTy <- infer ncolsExp
  nnzTy <- infer nnzExp
  _ <- wrange (firstParam nrowsExp) $ nrowsTy =:= UTyInt
  _ <- wrange (firstParam ncolsExp) $ ncolsTy =:= UTyInt
  _ <- wrange (firstParam nnzExp) $ nnzTy =:= UTyInt
  rowsTy <- infer rowsExp
  colsTy <- infer colsExp
  valsTy <- infer valsExp
  (_mRowsVar, sRows, eRows) <- asArrayType (firstParam rowsExp) rowsTy
  (_mColsVar, sCols, eCols) <- asArrayType (firstParam colsExp) colsTy
  (_mValsVar, sVals, eVals) <- asArrayType (firstParam valsExp) valsTy
  sRows' <- normalizeShapeToTupleOf (firstParam rowsExp) UTyInt sRows
  sCols' <- normalizeShapeToTupleOf (firstParam colsExp) UTyInt sCols
  sVals' <- normalizeShapeToTupleOf (firstParam valsExp) UTyInt sVals
  rowsRank <- shapeArityFromType (firstParam rowsExp) sRows'
  colsRank <- shapeArityFromType (firstParam colsExp) sCols'
  valsRank <- shapeArityFromType (firstParam valsExp) sVals'
  when (rowsRank /= 1 || colsRank /= 1 || valsRank /= 1) $
    throwError $ MiscError (Just (firstParam rowsExp))
  _ <- wrange (firstParam colsExp) $ sCols' =:= sRows'
  _ <- wrange (firstParam valsExp) $ sVals' =:= sRows'
  _ <- wrange (firstParam rowsExp) $ eRows =:= UTyInt
  _ <- wrange (firstParam colsExp) $ eCols =:= UTyInt
  _ <- wrange (firstParam valsExp) $ eVals =:= UTyInt
  pure $ uTyRecord
    [ ("nrows", UTyInt)
    , ("ncols", UTyInt)
    , ("nnz", UTyInt)
    , ("rows", UTyArray sRows' UTyInt)
    , ("cols", UTyArray sRows' UTyInt)
    , ("vals", UTyArray sRows' UTyInt)
    ]
infer (ECSRFromSortedCOO _ nrowsExp ncolsExp nnzExp rowsExp colsExp valsExp) = do
  nrowsTy <- infer nrowsExp
  ncolsTy <- infer ncolsExp
  nnzTy <- infer nnzExp
  _ <- wrange (firstParam nrowsExp) $ nrowsTy =:= UTyInt
  _ <- wrange (firstParam ncolsExp) $ ncolsTy =:= UTyInt
  _ <- wrange (firstParam nnzExp) $ nnzTy =:= UTyInt
  rowsTy <- infer rowsExp
  colsTy <- infer colsExp
  valsTy <- infer valsExp
  (_mRowsVar, sRows, eRows) <- asArrayType (firstParam rowsExp) rowsTy
  (_mColsVar, sCols, eCols) <- asArrayType (firstParam colsExp) colsTy
  (_mValsVar, sVals, eVals) <- asArrayType (firstParam valsExp) valsTy
  sRows' <- normalizeShapeToTupleOf (firstParam rowsExp) UTyInt sRows
  sCols' <- normalizeShapeToTupleOf (firstParam colsExp) UTyInt sCols
  sVals' <- normalizeShapeToTupleOf (firstParam valsExp) UTyInt sVals
  rowsRank <- shapeArityFromType (firstParam rowsExp) sRows'
  colsRank <- shapeArityFromType (firstParam colsExp) sCols'
  valsRank <- shapeArityFromType (firstParam valsExp) sVals'
  when (rowsRank /= 1 || colsRank /= 1 || valsRank /= 1) $
    throwError $ MiscError (Just (firstParam rowsExp))
  _ <- wrange (firstParam colsExp) $ sCols' =:= sRows'
  _ <- wrange (firstParam valsExp) $ sVals' =:= sRows'
  _ <- wrange (firstParam rowsExp) $ eRows =:= UTyInt
  _ <- wrange (firstParam colsExp) $ eCols =:= UTyInt
  _ <- wrange (firstParam valsExp) $ eVals =:= UTyInt
  let rowPtrShape = mkTyConsFromVec [UTyInt]
  pure $ uTyRecord
    [ ("nrows", UTyInt)
    , ("ncols", UTyInt)
    , ("nnz", UTyInt)
    , ("row_ptr", UTyArray rowPtrShape UTyInt)
    , ("col_idx", UTyArray sRows' UTyInt)
    , ("vals", UTyArray sRows' UTyInt)
    ]
infer (EPermute _ comb defaults permFn arrExp) = do
  defaultsTy <- infer defaults
  arrTy <- infer arrExp
  (mDstVar, sDst, eDst) <- asArrayType (firstParam defaults) defaultsTy
  (_mSrcVar, sSrc, eSrc) <- asArrayType (firstParam arrExp) arrTy
  _ <- wrange (firstParam arrExp) $ eSrc =:= eDst
  combTy <- infer comb
  _ <- wrange (firstParam comb) $ combTy =:= UTyFun eDst (UTyFun eDst eDst)
  permTy <- infer permFn
  _ <- wrange (firstParam permFn) $ permTy =:= UTyFun sSrc sDst
  sDst' <- applyBindings sDst
  (arrVar, arrTyOut) <- freshRefined (UTyArray sDst' eDst)
  case mDstVar of
    Nothing -> return arrTyOut
    Just dstVar -> do
      rank <- shapeArityFromType (firstParam defaults) sDst'
      forM_ [0 .. rank - 1] $ \i ->
        emitPred (PEq (TDim arrVar i) (TDim dstVar i))
      return arrTyOut
infer (EScatter _ comb defaults idxArr vals) = do
  defaultsTy <- infer defaults
  idxArrTy <- infer idxArr
  valsTy <- infer vals
  (mDstVar, sDst, eDst) <- asArrayType (firstParam defaults) defaultsTy
  (mIdxVar, sSrc, idxTy) <- asArrayType (firstParam idxArr) idxArrTy
  (_mSrcVar', sSrc', eSrc) <- asArrayType (firstParam vals) valsTy
  _ <- wrange (firstParam idxArr) $ sSrc =:= sSrc'
  idxTy' <- normalizeShapeToTupleOf (firstParam idxArr) UTyInt idxTy
  sDstNorm <- normalizeShapeToTupleOf (firstParam defaults) UTyInt sDst
  _ <- wrange (firstParam idxArr) $ idxTy' =:= sDstNorm
  _ <- wrange (firstParam vals) $ eSrc =:= eDst
  combTy <- infer comb
  _ <- wrange (firstParam comb) $ combTy =:= UTyFun eDst (UTyFun eDst eDst)
  sDst' <- applyBindings sDst
  (arrVar, arrTyOut) <- freshRefined (UTyArray sDst' eDst)
  -- Bounds check: index values must fit inside the destination array.
  -- Mirrors EGather but checks against dstVar instead of srcVar.
  -- Ungrounded obligations (TValBoundDim not established) emit a warning, not an error.
  idxElemRank <- shapeArityFromType (firstParam idxArr) idxTy'
  case (mIdxVar, mDstVar) of
    (Just idxVar, Just dstVar) ->
      forM_ [0 .. idxElemRank - 1] $ \i ->
        emitObl (PLe (TValBoundDim idxVar i) (TDim dstVar i))
    _ -> return ()
  case mDstVar of
    Nothing -> return arrTyOut
    Just dstVar -> do
      rank <- shapeArityFromType (firstParam defaults) sDst'
      forM_ [0 .. rank - 1] $ \i ->
        emitPred (PEq (TDim arrVar i) (TDim dstVar i))
      return arrTyOut
infer (EScatterGuarded _ comb defaults idxArr vals guardArr) = do
  defaultsTy <- infer defaults
  idxArrTy <- infer idxArr
  valsTy <- infer vals
  guardTy <- infer guardArr
  (mDstVar, sDst, eDst) <- asArrayType (firstParam defaults) defaultsTy
  (mIdxVar, sSrc, idxTy) <- asArrayType (firstParam idxArr) idxArrTy
  (_mSrcVar', sSrc', eSrc) <- asArrayType (firstParam vals) valsTy
  (_mSrcVar'', sSrc'', guardElemTy) <- asArrayType (firstParam guardArr) guardTy
  _ <- wrange (firstParam idxArr) $ sSrc =:= sSrc'
  _ <- wrange (firstParam idxArr) $ sSrc =:= sSrc''
  idxTy' <- normalizeShapeToTupleOf (firstParam idxArr) UTyInt idxTy
  sDstNorm <- normalizeShapeToTupleOf (firstParam defaults) UTyInt sDst
  _ <- wrange (firstParam idxArr) $ idxTy' =:= sDstNorm
  _ <- wrange (firstParam vals) $ eSrc =:= eDst
  _ <- wrange (firstParam guardArr) $ guardElemTy =:= UTyBool
  combTy <- infer comb
  _ <- wrange (firstParam comb) $ combTy =:= UTyFun eDst (UTyFun eDst eDst)
  sDst' <- applyBindings sDst
  (arrVar, arrTyOut) <- freshRefined (UTyArray sDst' eDst)
  -- Bounds check: index values must fit inside the destination array.
  -- Mirrors EGather but checks against dstVar instead of srcVar.
  -- Ungrounded obligations (TValBoundDim not established) emit a warning, not an error.
  idxElemRank <- shapeArityFromType (firstParam idxArr) idxTy'
  case (mIdxVar, mDstVar) of
    (Just idxVar, Just dstVar) ->
      forM_ [0 .. idxElemRank - 1] $ \i ->
        emitObl (PLe (TValBoundDim idxVar i) (TDim dstVar i))
    _ -> return ()
  case mDstVar of
    Nothing -> return arrTyOut
    Just dstVar -> do
      rank <- shapeArityFromType (firstParam defaults) sDst'
      forM_ [0 .. rank - 1] $ \i ->
        emitPred (PEq (TDim arrVar i) (TDim dstVar i))
      return arrTyOut
infer (EScatterGenerate _ comb defaults idxArr valFn) =
  infer (EScatter (firstParam defaults) comb defaults idxArr
           (EGenerate (firstParam idxArr) (EShapeOf (firstParam idxArr) idxArr) valFn))
infer (EGather _ idxArr arrExp) = do
  idxArrTy <- infer idxArr
  arrTy    <- infer arrExp
  (mIdxVar, sIdx, idxTy) <- asArrayType (firstParam idxArr) idxArrTy
  (mSrcVar, sSrc, eTy) <- asArrayType (firstParam arrExp) arrTy
  idxTy' <- normalizeShapeToTupleOf (firstParam idxArr) UTyInt idxTy
  sSrc' <- normalizeShapeToTupleOf (firstParam arrExp) UTyInt sSrc
  _ <- wrange (firstParam idxArr) $ idxTy' =:= sSrc'
  sIdx' <- applyBindings sIdx
  (arrVar, arrTyOut) <- freshRefined (UTyArray sIdx' eTy)
  case mIdxVar of
    Nothing -> return ()
    Just idxVar -> do
      rank <- shapeArityFromType (firstParam idxArr) sIdx'
      forM_ [0 .. rank - 1] $ \i ->
        emitPred (PEq (TDim arrVar i) (TDim idxVar i))
  -- Element value bounds: for each component i of the index element type,
  -- require that TValBoundDim idxVar i ≤ TDim srcVar i.  For a 1D gather the
  -- index element is a scalar Int (rank 1) and we recover the original check.
  -- For a multi-dimensional gather each component is checked independently.
  idxElemRank <- shapeArityFromType (firstParam idxArr) idxTy'
  case (mIdxVar, mSrcVar) of
    (Just idxVar, Just srcVar) ->
      forM_ [0 .. idxElemRank - 1] $ \i ->
        emitObl (PLe (TValBoundDim idxVar i) (TDim srcVar i))
    _ -> return ()
  -- Propagate the source array's element value bound to the output: gathered
  -- elements are a subset of the source elements, so they share the same bound.
  -- Loop over the element arity of the source's element type (usually 1 for Int).
  srcElemRank <- (shapeArityFromType (firstParam arrExp) =<< normalizeShapeToTupleOf (firstParam arrExp) UTyInt eTy)
                   `catchError` \_ -> return 1
  case mSrcVar of
    Just srcVar ->
      forM_ [0 .. srcElemRank - 1] $ \i ->
        emitPred (PEq (TValBoundDim arrVar i) (TValBoundDim srcVar i))
    Nothing -> return ()
  return arrTyOut
infer (EIndex _ idx arrExp) = do
  idxTy <- infer idx
  arrTy <- infer arrExp
  (mArrVar, sTy, eTy) <- asArrayType (firstParam arrExp) arrTy
  sTy' <- applyBindings sTy
  idxTy' <- applyBindings idxTy
  _ <- wrange (firstParam idx) $ idxTy' =:= sTy'
  pairs  <- termsAndExpsFromIndex idx
  arrVar <- maybe freshPredVar return mArrVar
  forM_ (zip [0 ..] pairs) $ \(i, (t, mExp)) -> do
    emitObl (PLe (TConst 0) t)
    emitObl (PLt t (TDim arrVar i))
    -- If the index sub-expression has a statically known exclusive upper bound
    -- (via valBoundCtx or arithmetic over bounded variables), emit a verifiable
    -- constraint: bound ≤ dim(arr, i).  This makes index safety checkable for
    -- variables introduced with [x bound N] or let x bound N = ... in ...
    case mExp of
      Nothing -> return ()   -- EShapeOf: no static expression to analyse
      Just e  -> do
        mb <- inferBVal e
        case mb of
          Just (BoundOf b) -> emitObl (PLe b (TDim arrVar i))
          _                -> return ()  -- no useful bound; check remains permissive
  return eTy
infer (ECheckIndex _ idx defVal arrExp) = do
  idxTy <- infer idx
  defValTy <- infer defVal
  arrTy <- infer arrExp
  (_mArrVar, sTy, eTy) <- asArrayType (firstParam arrExp) arrTy
  sTy' <- applyBindings sTy
  idxTy' <- applyBindings idxTy
  eTy' <- applyBindings eTy
  defValTy' <- applyBindings defValTy
  _ <- wrange (firstParam idx)    $ idxTy'    =:= sTy'
  _ <- wrange (firstParam defVal) $ defValTy' =:= eTy'
  return eTy'
infer (EStencil _ bnd fnExp arrExp) = do
  inferBnd bnd
  arrTy <- infer arrExp
  (mArrVar, sTy, eTy) <- asArrayType (firstParam arrExp) arrTy
  -- Infer the stencil function before normalizing the array shape, so the
  -- accessor argument type determines the rank rather than prematurely
  -- constraining an unknown shape variable to rank-1.
  outTy     <- fresh
  fty       <- infer fnExp
  accessorTy <- fresh
  _ <- wrange (firstParam fnExp) $ fty =:= UTyFun accessorTy outTy
  accessorTy' <- applyBindings accessorTy
  -- Stencil is currently limited to 1-D and 2-D arrays.
  let rank = countIntAccessorArgs accessorTy'
  when (rank < 1 || rank > 2) $
    throwError $ MiscError (Just (firstParam arrExp))
  -- Build the concrete shape type and constrain the input array shape.
  let shapeTy = foldr UTyCons UTyUnit (replicate rank UTyInt)
  _ <- wrange (firstParam arrExp) $ sTy =:= shapeTy
  -- The accessor's return type must match the array element type.
  let accessorRetTy = peelNArgs rank accessorTy'
  _ <- wrange (firstParam arrExp) $ accessorRetTy =:= eTy
  (arrVar, arrTyOut) <- freshRefined (UTyArray shapeTy outTy)
  case mArrVar of
    Nothing -> return arrTyOut
    Just srcVar -> do
      forM_ [0 .. rank - 1] $ \i ->
        emitPred (PEq (TDim arrVar i) (TDim srcVar i))
      return arrTyOut
infer (ELetIn r dec e) = do
  let Dec rr v pats mty body = dec
  bodyTy    <- fresh
  skolemAnn <- mapM (skolemize . toUPolytype) mty
  (patBinds, funTy) <- buildPatFunType rr bodyTy pats skolemAnn
  -- Infer the RHS under the pattern bindings and a monomorphic self-reference,
  -- collecting refinement predicates emitted inside.  After generalization
  -- the binding is re-introduced with a polymorphic scheme for the body @e@.
  (rhsTy, rhsPreds) <- censor (const []) $ listen $
    withBoundsFromPats pats $
    bindPats patBinds $ withBinding v (Forall [] [] funTy) $ do
      inferredBodyTy <- infer body
      _ <- inferredBodyTy =:= bodyTy
      case skolemAnn of
        Just ann -> wrange rr (funTy =:= ann) >> return ann
        Nothing  -> return (if null pats then inferredBodyTy else funTy)
  -- For monomorphic inner let-bindings (no generalized type variables), emit
  -- the captured predicates exactly once via 'tell' and store a zero-predicate
  -- scheme.  This prevents exponential blowup when a variable is used multiple
  -- times: 'instantiate' re-emits predicates on every use, so storing them in
  -- the scheme would multiply the predicate count by the number of uses.
  -- For polymorphic bindings (xs non-empty) the existing behaviour is correct:
  -- each instantiation renames the predicate variables to match the fresh type
  -- variable substitution, so re-emission is intentional.
  Forall xs _ ty <- generalize rhsTy
  sch <- case xs of
    [] -> tell rhsPreds >> return (Forall [] [] ty)
    _  -> return (Forall xs rhsPreds ty)
  withBinding v sch $ wrange r $ infer e
infer (EBoundLetIn _ x boundExp rhs body) = do
  rhsTy <- infer rhs
  _ <- wrange (firstParam rhs) $ rhsTy =:= UTyInt
  boundTerm <- termFromExp boundExp
  -- Try to verify the bound statically; warn if we cannot.
  mRhsBound <- inferValBound rhs
  case mRhsBound of
    Just rhsBound -> emitObl (PLe rhsBound boundTerm)
    Nothing -> do
      -- Static inference failed; emit a nonlinear arithmetic (NIA) obligation
      -- for the SMT solver.  Z3 can often verify products of symbolic variables
      -- (e.g. z*ny*nx + y*nx + x < nx*ny*nz) given the active bound hypotheses.
      -- If Z3 returns Unknown the obligation produces a "could not verify" warning.
      rhsTerm <- termFromExp rhs
      emitObl (PLt rhsTerm boundTerm)
  -- Bind x as Int with bound in context for the body.
  withBinding x (Forall [] [] UTyInt) $
    withValBound x boundTerm $
      infer body
infer (EOp _ op) = pure $ case op of
  Plus _    -> ii2i; Minus _   -> ii2i; Times _   -> ii2i; Divide _  -> ii2i; Mod _ -> ii2i
  Eq _      -> ii2b; Neq _     -> ii2b; Lt _       -> ii2b; Le _     -> ii2b
  Gt _      -> ii2b; Ge _      -> ii2b
  And _     -> bb2b; Or _      -> bb2b
  PlusF _   -> ff2f; MinusF _  -> ff2f; TimesF _  -> ff2f; DivideF _ -> ff2f
  EqF _     -> ff2b; NeqF _    -> ff2b; LtF _      -> ff2b; LeF _    -> ff2b
  GtF _     -> ff2b; GeF _     -> ff2b
  where
    ii2i = UTyFun UTyInt   (UTyFun UTyInt   UTyInt)
    ii2b = UTyFun UTyInt   (UTyFun UTyInt   UTyBool)
    bb2b = UTyFun UTyBool  (UTyFun UTyBool  UTyBool)
    ff2f = UTyFun UTyFloat (UTyFun UTyFloat UTyFloat)
    ff2b = UTyFun UTyFloat (UTyFun UTyFloat UTyBool)

-- | Type-check a boundary condition.
-- BConst requires the constant expression to have the same type as the source
-- array's elements; the caller is responsible for unifying that. Here we just
-- infer the constant expression's type and return it so the caller can unify.
inferBnd :: BoundaryCondition Range -> Infer ()
inferBnd BClamp      = return ()
inferBnd BWrap       = return ()
inferBnd BMirror     = return ()
inferBnd (BConst e)  = void (infer e)

-- | Build the function type for a multi-argument declaration from its pattern
-- list, threading an optional skolemized annotation through each argument.
-- Returns the accumulated (variable, type) bindings and the complete function
-- type.
buildPatFunType
  :: Range          -- ^ source range for error reporting
  -> UType          -- ^ fresh result type variable
  -> [Pat Range]    -- ^ argument patterns
  -> Maybe UType    -- ^ skolemized annotation (if present)
  -> Infer ([(Var, UType)], UType)
buildPatFunType r bodyTy = go
  where
    go [] Nothing       = return ([], bodyTy)
    go [] (Just ty)     = return ([], ty)
    go [PVar _ arg] mAnn = do
      argTy <- freshValue
      funTy <- case mAnn of
        Nothing  -> return $ UTyFun argTy bodyTy
        Just ann -> wrange r (ann =:= UTyFun argTy bodyTy)
      return ([(arg, argTy)], funTy)
    go [PBound _ arg _] mAnn = do
      argTy <- freshValue
      funTy <- case mAnn of
        Nothing  -> return $ UTyFun argTy bodyTy
        Just ann -> wrange r (ann =:= UTyFun argTy bodyTy)
      return ([(arg, argTy)], funTy)
    go [PVec _ vs] mAnn = do
      tyArgs <- mapM (const freshValue) vs
      let argTy = foldr UTyCons UTyUnit tyArgs
          binds = [(v, t) | (PVar _ v, t) <- zip vs tyArgs]
                ++ [(v, t) | (PBound _ v _, t) <- zip vs tyArgs]
      funTy <- case mAnn of
        Nothing  -> return $ UTyFun argTy bodyTy
        Just ann -> wrange r (ann =:= UTyFun argTy bodyTy)
      return (binds, funTy)
    go (PVar _ arg : ps) mAnn = do
      argTy <- freshValue
      case mAnn of
        Nothing -> do
          (binds, restTy) <- go ps Nothing
          return ((arg, argTy) : binds, UTyFun argTy restTy)
        Just ann -> do
          innerTy <- fresh
          funTy <- wrange r (ann =:= UTyFun argTy innerTy)
          (binds, _) <- go ps (Just innerTy)
          return ((arg, argTy) : binds, funTy)
    go (PVec _ vs : ps) mAnn = do
      tyArgs <- mapM (const freshValue) vs
      let argTy      = foldr UTyCons UTyUnit tyArgs
          bindsLocal = [(v, t) | (PVar _ v, t) <- zip vs tyArgs]
      case mAnn of
        Nothing -> do
          (bindsRest, restTy) <- go ps Nothing
          return (bindsLocal ++ bindsRest, UTyFun argTy restTy)
        Just ann -> do
          innerTy <- fresh
          funTy <- wrange r (ann =:= UTyFun argTy innerTy)
          (bindsRest, _) <- go ps (Just innerTy)
          return (bindsLocal ++ bindsRest, funTy)

-- | Extend the context with a list of monomorphic variable bindings.
bindPats :: [(Var, UType)] -> Infer a -> Infer a
bindPats []            m = m
bindPats ((v, t) : vs) m = withBinding v (Forall [] [] t) $ bindPats vs m

-- | Infer and generalize a sequence of declarations from left to right.
inferDecs :: [Dec Range] -> Infer [(Var, UPolytype)]
inferDecs [] = return []
inferDecs (Dec r var pats mty e : rest) = do
  bodyTy    <- fresh
  skolemAnn <- mapM (skolemize . toUPolytype) mty
  (patBinds, funTy) <- buildPatFunType r bodyTy pats skolemAnn
  (inferredBodyTy, rhsPreds) <- censor (const []) $ listen $
    withBoundsFromPats pats $ bindPats patBinds $ infer e
  _ <- inferredBodyTy =:= bodyTy
  case skolemAnn of
    Just ann -> void $ wrange r (funTy =:= ann)
    Nothing  -> return ()
  -- Preserve the refinement binder from the inferred body type.  The =:= call
  -- above strips refinement wrappers before structural unification, so bodyTy
  -- (and funTy) loses the UTyRefine wrapper that carries the pred variable.
  -- For zero-pattern declarations we use inferredBodyTy directly (which still
  -- holds the wrapper); for function declarations funTy already tracks params.
  let rhsTy = if null pats then inferredBodyTy else funTy
  boundTy <- generalizeWithPreds rhsPreds =<< applyBindings rhsTy
  -- Register the definition body for single-parameter functions so that EMap
  -- and EApp can infer output value bounds for non-inline call sites.  We
  -- extract the parameter name via 'patVar'; if it cannot be extracted (e.g.
  -- PVec with multiple sub-patterns), we skip registration.
  let registerDef = case pats of
        [pat] | Just paramName <- patVar pat -> withDefBody var paramName e
        _                                    -> id
  rest' <- registerDef $ withBinding var boundTy $ inferDecs rest
  return $ (var, boundTy) : rest'

-- | Extend the typing context with many bindings for the duration of an action.
withAllBinding :: (MonadReader Ctx m) => [(Var, UPolytype)] -> m a -> m a
withAllBinding [] m = m
withAllBinding ((v, t) : ls) m = withBinding v t $ withAllBinding ls m

-- | Bind pattern variables to fresh refined types before running an inference action.
withPats :: [Pat Range] -> Infer a -> Infer a
withPats [] e = e
withPats ((PVar _ v) : ps) e = do
  tyArg <- freshValue
  withBinding v (Forall [] [] tyArg) $ withPats ps e
withPats ((PBound _ v _) : ps) e = do
  tyArg <- freshValue
  withBinding v (Forall [] [] tyArg) $ withPats ps e
withPats ((PVec r vs) : ps) e = do
  tyArgs <- mapM (const freshValue) vs
  vars <- forM vs $ \p ->
    case p of
      PVar _ v   -> return v
      PBound _ v _ -> return v
      _ -> throwError $ MiscError (Just r)
  withAllBinding (zip vars $ map (Forall [] []) tyArgs) $ withPats ps e

-- | Infer the type of a unary function body under a fresh binding for its argument.
inferFun :: Var -> Exp Range -> Infer UType
inferFun arg e1 = do
  tyArg <- freshValue
  withBinding arg (Forall [] [] tyArg) $
    UTyFun tyArg <$> infer e1

-- | Run inference from an empty context and solve the resulting refinement predicates.
runInfer :: Infer UType -> IO (Either TypeError Polytype)
runInfer = runInferWithOptions defaultInferOptions

runInferWithOptions :: InferOptions -> Infer UType -> IO (Either TypeError Polytype)
runInferWithOptions opts e = do
  let result =
        runIdentity $ evalIntBindingT $ runExceptT $ runWriterT $ runStateT (runReaderT action M.empty) initInferState
  case result of
    Left err -> return $ Left err
    Right ((sch, _preds), _st) -> do
      let poly = fromUPolytype sch
      fst <$> finalizePolyWithOptions opts poly
  where
    action = do
      (ty, preds) <- censor (const []) $ listen (e >>= applyBindings)
      generalizeWithPreds preds ty

-- | Run inference with a custom initial context (useful to inject builtins)
runInferWithCtx :: Ctx -> Infer UType -> IO (Either TypeError Polytype)
runInferWithCtx = runInferWithCtxOptions defaultInferOptions

runInferWithCtxOptions :: InferOptions -> Ctx -> Infer UType -> IO (Either TypeError Polytype)
runInferWithCtxOptions opts ctx e = do
  let result =
        runIdentity $ evalIntBindingT $ runExceptT $ runWriterT $ runStateT (runReaderT action ctx) initInferState
  case result of
    Left err -> return $ Left err
    Right ((sch, _preds), _st) -> do
      let poly = fromUPolytype sch
      fst <$> finalizePolyWithOptions opts poly
  where
    action = do
      (ty, preds) <- censor (const []) $ listen (e >>= applyBindings)
      generalizeWithPreds preds ty

-- | Infer and generalize the type of a single expression.
inferPolytype :: Exp Range -> IO (Either TypeError Polytype)
inferPolytype = runInfer . infer

-- | Rename the universally-quantified type variables of a 'Polytype' to
-- canonical @a0, a1, …@ names.
renamePolytype :: Polytype -> Polytype
renamePolytype pty = evalState (go pty) (0 :: Int, M.empty)
  where
    go (Forall vars preds t) = do
      vars' <- forM vars $ \v -> do
        (n, m) <- get
        let v' = BS.pack ("a" ++ show n)
        put (n + 1, M.insert v v' m)
        return v'
      t' <- ycataM rename t
      return (Forall vars' preds t')
    rename (TyVar v) = gets (maybe (TyVar v) TyVar . M.lookup v . snd)
    rename t         = return t

-- | Infer top-level declarations and return their generalized Polytypes.
runInferDecs :: [Dec Range] -> IO (Either TypeError ([(Var, Polytype)], [String]))
runInferDecs = runInferDecsWithOptions defaultInferOptions

runInferDecsWithOptions :: InferOptions -> [Dec Range] -> IO (Either TypeError ([(Var, Polytype)], [String]))
runInferDecsWithOptions opts decs = do
  let result =
        runIdentity $ evalIntBindingT $ runExceptT $ runWriterT $
          runStateT (flip runReaderT M.empty $ inferDecs decs) initInferState
  case result of
    Left err -> return $ Left err
    Right ((ups, st), _preds) -> do
      pairs <- forM ups $ \(v, up) -> do
        (res, solverWs) <- finalizePolyWithOptions opts (fromUPolytype up)
        return (v, res, solverWs)
      let warnings = inferWarnings st ++ concatMap (\(_, _, ws) -> ws) pairs
      return $ fmap (, warnings) $ traverse (\(v, res, _) -> fmap (v,) res) pairs

-- | Discharge the predicates attached to a generalized type scheme.
finalizePoly :: Polytype -> IO (Either TypeError Polytype)
finalizePoly poly = fst <$> finalizePolyWithOptions defaultInferOptions poly

finalizePolyWithOptions :: InferOptions -> Polytype -> IO (Either TypeError Polytype, [String])
finalizePolyWithOptions opts poly
  | not (inferSolveRefinements opts) = return (Right poly, [])
  | otherwise = do
  let Forall xs tagged ty = poly
  checked <- checkTaggedPredicates tagged
  case checked of
    Left (failing, mWitness) -> return (Left (UnsatConstraints failing mWitness), [])
    Right (residual, warnings) -> return (Right (Forall xs residual ty), warnings)
