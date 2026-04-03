{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.Uniquify
--
-- Freshen locally bound names while preserving lexical scope.
--
-- The pass runs after type inference and before fusion, so it preserves
-- user-facing source locations while reducing capture concerns for downstream
-- transformations.
--
-- Naming conventions:
--  * Fresh binders are suffixed with __uniq_<n> (e.g. x__uniq_3).
--  * Existing __fusion_* and __uniq_* names are left untouched to avoid
--    interfering with compiler-generated binders.
module Language.Hydrangea.Uniquify
  ( uniquifyExp
  , uniquifyDec
  , uniquifyDecs
  ) where

import Control.Monad.State
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Language.Hydrangea.Syntax

newtype UniqM a = UniqM {runUniqM :: State UniqState a}
  deriving (Functor, Applicative, Monad, MonadState UniqState)

data UniqState = UniqState
  { uniqCounter :: Int
  , uniqEnv :: Map Var Var
  , uniqUsed :: Set Var
  }

-- | Freshen local binders in an expression while preserving its structure.
uniquifyExp :: Exp a -> Exp a
uniquifyExp expr =
  let st = UniqState {uniqCounter = 0, uniqEnv = M.empty, uniqUsed = collectVarsExp expr}
   in evalState (runUniqM (uniqExp expr)) st

-- | Freshen local binders within a single declaration body.
uniquifyDec :: Dec a -> Dec a
uniquifyDec dec =
  let st = UniqState {uniqCounter = 0, uniqEnv = M.empty, uniqUsed = collectVarsDec dec}
   in evalState (runUniqM (uniqTopDec dec)) st

-- | Freshen local binders across a group of declarations using one shared name supply.
uniquifyDecs :: [Dec a] -> [Dec a]
uniquifyDecs decs =
  let st = UniqState {uniqCounter = 0, uniqEnv = M.empty, uniqUsed = S.unions (map collectVarsDec decs)}
   in evalState (runUniqM (mapM uniqTopDec decs)) st

uniqTopDec :: Dec a -> UniqM (Dec a)
uniqTopDec (Dec a name pats mw poly body) = do
  env <- gets uniqEnv
  (pats', patEnv) <- uniqPats pats
  withEnv (M.union patEnv env) $ do
    body' <- uniqExp body
    pure (Dec a name pats' mw poly body')

uniqExp :: Exp a -> UniqM (Exp a)
uniqExp expr =
  case expr of
    EInt {} -> pure expr
    EFloat {} -> pure expr
    EVar a v -> EVar a <$> lookupVar v
    EString {} -> pure expr
    EUnit {} -> pure expr
    EBool {} -> pure expr
    EVec a es -> EVec a <$> mapM uniqExp es
    EApp a f x -> EApp a <$> uniqExp f <*> uniqExp x
    EIfThen a c t -> EIfThen a <$> uniqExp c <*> uniqExp t
    EIfThenElse a c t f -> EIfThenElse a <$> uniqExp c <*> uniqExp t <*> uniqExp f
    ENeg a e -> ENeg a <$> uniqExp e
    EBinOp a l op r -> EBinOp a <$> uniqExp l <*> pure op <*> uniqExp r
    EUnOp a op e -> EUnOp a op <$> uniqExp e
    EOp {} -> pure expr
    ELetIn a dec body -> do
      (dec', decEnv) <- uniqLocalDec dec
      body' <- withEnv decEnv (uniqExp body)
      pure (ELetIn a dec' body')
    EProj a i e -> EProj a i <$> uniqExp e
    EPair a e1 e2 -> EPair a <$> uniqExp e1 <*> uniqExp e2
    ERecord a fields -> ERecord a <$> mapM (\(field, fieldExp) -> do
      fieldExp' <- uniqExp fieldExp
      pure (field, fieldExp')) fields
    ERecordProj a e field -> ERecordProj a <$> uniqExp e <*> pure field
    EGenerate a sz f -> EGenerate a <$> uniqExp sz <*> uniqExp f
    EMap a f arr -> EMap a <$> uniqExp f <*> uniqExp arr
    EZipWith a f a1 a2 -> EZipWith a <$> uniqExp f <*> uniqExp a1 <*> uniqExp a2
    EReduce a f z arr -> EReduce a <$> uniqExp f <*> uniqExp z <*> uniqExp arr
    EReduceGenerate a f z shape gen -> EReduceGenerate a <$> uniqExp f <*> uniqExp z <*> uniqExp shape <*> uniqExp gen
    EFoldl a f z arr -> EFoldl a <$> uniqExp f <*> uniqExp z <*> uniqExp arr
    EFoldlWhile a p f z arr -> EFoldlWhile a <$> uniqExp p <*> uniqExp f <*> uniqExp z <*> uniqExp arr
    EScan a f z arr -> EScan a <$> uniqExp f <*> uniqExp z <*> uniqExp arr
    ESegmentedReduce a f z offsets vals ->
      ESegmentedReduce a <$> uniqExp f <*> uniqExp z <*> uniqExp offsets <*> uniqExp vals
    ESortIndices a arr -> ESortIndices a <$> uniqExp arr
    EIota a n -> EIota a <$> uniqExp n
    EMakeIndex a n arr -> EMakeIndex a <$> uniqExp n <*> uniqExp arr
    ECOOSumDuplicates a nrows ncols nnz rows cols vals ->
      ECOOSumDuplicates a <$> uniqExp nrows <*> uniqExp ncols <*> uniqExp nnz
        <*> uniqExp rows <*> uniqExp cols <*> uniqExp vals
    ECSRFromSortedCOO a nrows ncols nnz rows cols vals ->
      ECSRFromSortedCOO a <$> uniqExp nrows <*> uniqExp ncols <*> uniqExp nnz
        <*> uniqExp rows <*> uniqExp cols <*> uniqExp vals
    EPermute a c d p arr -> EPermute a <$> uniqExp c <*> uniqExp d <*> uniqExp p <*> uniqExp arr
    EScatter a c d idx v -> EScatter a <$> uniqExp c <*> uniqExp d <*> uniqExp idx <*> uniqExp v
    EScatterGuarded a c d idx v g -> EScatterGuarded a <$> uniqExp c <*> uniqExp d <*> uniqExp idx <*> uniqExp v <*> uniqExp g
    EScatterGenerate a c d idx f -> EScatterGenerate a <$> uniqExp c <*> uniqExp d <*> uniqExp idx <*> uniqExp f
    EGather a idx arr -> EGather a <$> uniqExp idx <*> uniqExp arr
    EIndex a idx arr -> EIndex a <$> uniqExp idx <*> uniqExp arr
    ECheckIndex a idx def arr -> ECheckIndex a <$> uniqExp idx <*> uniqExp def <*> uniqExp arr
    EFill a s v -> EFill a <$> uniqExp s <*> uniqExp v
    EShapeOf a arr -> EShapeOf a <$> uniqExp arr
    EReplicate a dims arr -> EReplicate a <$> mapM uniqShapeDim dims <*> uniqExp arr
    ESlice a dims arr -> ESlice a <$> mapM uniqSliceDim dims <*> uniqExp arr
    EReshape a s arr -> EReshape a <$> uniqExp s <*> uniqExp arr
    EReadArray a s f -> EReadArray a <$> uniqExp s <*> uniqExp f
    EReadArrayFloat a s f -> EReadArrayFloat a <$> uniqExp s <*> uniqExp f
    EWriteArray a arr f -> EWriteArray a <$> uniqExp arr <*> uniqExp f
    EWriteArrayFloat a arr f -> EWriteArrayFloat a <$> uniqExp arr <*> uniqExp f
    EGetEnvInt a e -> EGetEnvInt a <$> uniqExp e
    EGetEnvString a e -> EGetEnvString a <$> uniqExp e
    EStencil a bnd f arr -> EStencil a <$> uniqBnd bnd <*> uniqExp f <*> uniqExp arr
    EBoundLetIn a x boundExp rhs body -> do
      env <- gets uniqEnv
      x' <- freshenBinder x
      boundExp' <- uniqExp boundExp
      rhs' <- uniqExp rhs
      body' <- withEnv (M.insert x x' env) (uniqExp body)
      pure (EBoundLetIn a x' boundExp' rhs' body')

uniqBnd :: BoundaryCondition a -> UniqM (BoundaryCondition a)
uniqBnd BClamp        = pure BClamp
uniqBnd BWrap         = pure BWrap
uniqBnd BMirror       = pure BMirror
uniqBnd (BConst e)    = BConst <$> uniqExp e

uniqLocalDec :: Dec a -> UniqM (Dec a, Map Var Var)
uniqLocalDec (Dec a name pats mw poly body) = do
  env <- gets uniqEnv
  name' <- freshenBinder name
  (pats', patEnv) <- uniqPats pats
  let decEnv = M.insert name name' patEnv
  body' <- withEnv (M.union decEnv env) (uniqExp body)
  pure (Dec a name' pats' mw poly body', M.union decEnv env)

uniqPats :: [Pat a] -> UniqM ([Pat a], Map Var Var)
uniqPats pats = do
  (pats', envs) <- unzip <$> mapM uniqPat pats
  pure (pats', M.unions envs)

uniqPat :: Pat a -> UniqM (Pat a, Map Var Var)
uniqPat pat =
  case pat of
    PVar a v -> do
      v' <- freshenBinder v
      pure (PVar a v', M.singleton v v')
    PBound a v boundExp -> do
      v' <- freshenBinder v
      -- boundExp is not a binder site; rename free uses inside it too
      boundExp' <- uniqExp boundExp
      pure (PBound a v' boundExp', M.singleton v v')
    PVec a ps -> do
      (ps', envs) <- unzip <$> mapM uniqPat ps
      pure (PVec a ps', M.unions envs)
    PPair a p1 p2 -> do
      (p1', env1) <- uniqPat p1
      (p2', env2) <- uniqPat p2
      pure (PPair a p1' p2', M.union env1 env2)

uniqShapeDim :: ShapeDim a -> UniqM (ShapeDim a)
uniqShapeDim dim =
  case dim of
    ShapeAll a -> pure (ShapeAll a)
    ShapeAny a e -> ShapeAny a <$> uniqExp e
    ShapeDim a e -> ShapeDim a <$> uniqExp e

uniqSliceDim :: SliceDim a -> UniqM (SliceDim a)
uniqSliceDim dim =
  case dim of
    SliceAll a -> pure (SliceAll a)
    SliceRange a s l -> SliceRange a <$> uniqExp s <*> uniqExp l

lookupVar :: Var -> UniqM Var
lookupVar v = do
  env <- gets uniqEnv
  pure (M.findWithDefault v v env)

withEnv :: Map Var Var -> UniqM a -> UniqM a
withEnv env' action = do
  st <- get
  put st {uniqEnv = env'}
  result <- action
  modify' (\s -> s {uniqEnv = uniqEnv st})
  pure result

freshenBinder :: Var -> UniqM Var
freshenBinder v
  | shouldSkip v = pure v
  | otherwise = freshName v

shouldSkip :: Var -> Bool
shouldSkip v = BS.isPrefixOf "__fusion_" v || BS.isPrefixOf "__uniq_" v

freshName :: Var -> UniqM Var
freshName base = do
  st <- get
  let n = uniqCounter st
      candidate = base <> "__uniq_" <> BS.pack (show n)
  if candidate `S.member` uniqUsed st
    then put st {uniqCounter = n + 1} >> freshName base
    else do
      put st {uniqCounter = n + 1, uniqUsed = S.insert candidate (uniqUsed st)}
      pure candidate

collectVarsExp :: Exp a -> Set Var
collectVarsExp expr =
  case expr of
    EInt _ _ -> S.empty
    EFloat _ _ -> S.empty
    EVar _ v -> S.singleton v
    EString _ _ -> S.empty
    EUnit _ -> S.empty
    EBool _ _ -> S.empty
    EVec _ es -> S.unions (map collectVarsExp es)
    EApp _ f x -> collectVarsExp f `S.union` collectVarsExp x
    EIfThen _ c t -> collectVarsExp c `S.union` collectVarsExp t
    EIfThenElse _ c t f -> collectVarsExp c `S.union` collectVarsExp t `S.union` collectVarsExp f
    ENeg _ e -> collectVarsExp e
    EBinOp _ l _ r -> collectVarsExp l `S.union` collectVarsExp r
    EUnOp _ _ e -> collectVarsExp e
    EOp _ _ -> S.empty
    ELetIn _ d e -> collectVarsDec d `S.union` collectVarsExp e
    EProj _ _ e -> collectVarsExp e
    EPair _ e1 e2 -> collectVarsExp e1 `S.union` collectVarsExp e2
    ERecord _ fields -> S.unions (map (collectVarsExp . snd) fields)
    ERecordProj _ e _ -> collectVarsExp e
    EGenerate _ sz f -> collectVarsExp sz `S.union` collectVarsExp f
    EMap _ f arr -> collectVarsExp f `S.union` collectVarsExp arr
    EZipWith _ f a b -> collectVarsExp f `S.union` collectVarsExp a `S.union` collectVarsExp b
    EReduce _ f z arr -> collectVarsExp f `S.union` collectVarsExp z `S.union` collectVarsExp arr
    EReduceGenerate _ f z shape gen -> collectVarsExp f `S.union` collectVarsExp z `S.union` collectVarsExp shape `S.union` collectVarsExp gen
    EFoldl _ f z arr -> collectVarsExp f `S.union` collectVarsExp z `S.union` collectVarsExp arr
    EFoldlWhile _ p f z arr -> collectVarsExp p `S.union` collectVarsExp f `S.union` collectVarsExp z `S.union` collectVarsExp arr
    EScan _ f z arr -> collectVarsExp f `S.union` collectVarsExp z `S.union` collectVarsExp arr
    ESegmentedReduce _ f z offsets vals ->
      collectVarsExp f `S.union` collectVarsExp z `S.union` collectVarsExp offsets `S.union` collectVarsExp vals
    ESortIndices _ arr -> collectVarsExp arr
    EIota _ n -> collectVarsExp n
    EMakeIndex _ n arr -> collectVarsExp n `S.union` collectVarsExp arr
    ECOOSumDuplicates _ nrows ncols nnz rows cols vals ->
      collectVarsExp nrows `S.union` collectVarsExp ncols `S.union` collectVarsExp nnz
        `S.union` collectVarsExp rows `S.union` collectVarsExp cols `S.union` collectVarsExp vals
    ECSRFromSortedCOO _ nrows ncols nnz rows cols vals ->
      collectVarsExp nrows `S.union` collectVarsExp ncols `S.union` collectVarsExp nnz
        `S.union` collectVarsExp rows `S.union` collectVarsExp cols `S.union` collectVarsExp vals
    EPermute _ c d p a -> collectVarsExp c `S.union` collectVarsExp d `S.union` collectVarsExp p `S.union` collectVarsExp a
    EScatter _ c d idx v -> collectVarsExp c `S.union` collectVarsExp d `S.union` collectVarsExp idx `S.union` collectVarsExp v
    EScatterGuarded _ c d idx v g -> collectVarsExp c `S.union` collectVarsExp d `S.union` collectVarsExp idx `S.union` collectVarsExp v `S.union` collectVarsExp g
    EScatterGenerate _ c d idx f -> collectVarsExp c `S.union` collectVarsExp d `S.union` collectVarsExp idx `S.union` collectVarsExp f
    EGather _ idx a -> collectVarsExp idx `S.union` collectVarsExp a
    EIndex _ i a -> collectVarsExp i `S.union` collectVarsExp a
    ECheckIndex _ i def a -> collectVarsExp i `S.union` collectVarsExp def `S.union` collectVarsExp a
    EFill _ s v -> collectVarsExp s `S.union` collectVarsExp v
    EShapeOf _ a -> collectVarsExp a
    EReplicate _ dims a -> S.unions (collectVarsShapeDim <$> dims) `S.union` collectVarsExp a
    ESlice _ dims a -> S.unions (collectVarsSliceDim <$> dims) `S.union` collectVarsExp a
    EReshape _ s a -> collectVarsExp s `S.union` collectVarsExp a
    EReadArray _ s f -> collectVarsExp s `S.union` collectVarsExp f
    EReadArrayFloat _ s f -> collectVarsExp s `S.union` collectVarsExp f
    EWriteArray _ arr f -> collectVarsExp arr `S.union` collectVarsExp f
    EWriteArrayFloat _ arr f -> collectVarsExp arr `S.union` collectVarsExp f
    EGetEnvInt _ e -> collectVarsExp e
    EGetEnvString _ e -> collectVarsExp e
    EStencil _ bnd f arr -> collectVarsBnd bnd `S.union` collectVarsExp f `S.union` collectVarsExp arr
    EBoundLetIn _ x boundExp rhs body ->
      S.insert x (collectVarsExp boundExp `S.union` collectVarsExp rhs `S.union` collectVarsExp body)

collectVarsBnd :: BoundaryCondition a -> Set Var
collectVarsBnd BClamp     = S.empty
collectVarsBnd BWrap      = S.empty
collectVarsBnd BMirror    = S.empty
collectVarsBnd (BConst e) = collectVarsExp e

collectVarsDec :: Dec a -> Set Var
collectVarsDec (Dec _ name pats _ _ body) =
  S.insert name (S.unions (map collectVarsPat pats) `S.union` collectVarsExp body)

collectVarsPat :: Pat a -> Set Var
collectVarsPat pat =
  case pat of
    PVar _ v -> S.singleton v
    PBound _ v _ -> S.singleton v
    PVec _ ps -> S.unions (map collectVarsPat ps)
    PPair _ p1 p2 -> collectVarsPat p1 `S.union` collectVarsPat p2

collectVarsShapeDim :: ShapeDim a -> Set Var
collectVarsShapeDim dim =
  case dim of
    ShapeAll _ -> S.empty
    ShapeAny _ e -> collectVarsExp e
    ShapeDim _ e -> collectVarsExp e

collectVarsSliceDim :: SliceDim a -> Set Var
collectVarsSliceDim dim =
  case dim of
    SliceAll _ -> S.empty
    SliceRange _ s l -> collectVarsExp s `S.union` collectVarsExp l
