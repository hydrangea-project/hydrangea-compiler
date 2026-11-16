{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.TemporalTiling
--
-- Conservative source-level temporal rewrites that expose tiling-friendly
-- structure before lowering.  The first rewrite implemented here is a 1-D
-- halo decomposition for constant-boundary stencil iterates with a statically
-- known trip count and source length.
module Language.Hydrangea.TemporalTiling
  ( rewriteTemporalDecs
  ) where

import Control.Monad.State.Strict
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Language.Hydrangea.Lexer (Range)
import Language.Hydrangea.Syntax
import Language.Hydrangea.Uniquify (collectVarsDec)
import Language.Hydrangea.Util (freshUnusedName)

type DecEnv = Map Var (Dec Range)

data RewriteState = RewriteState
  { rsCounter :: Int
  , rsUsed :: Set Var
  }

type RewriteM = State RewriteState

data StepInfo = StepInfo
  { siRadius :: Integer
  }

rewriteTemporalDecs :: [Dec Range] -> [Dec Range]
rewriteTemporalDecs decs =
  evalState (rewriteTopDecs M.empty decs) initState
  where
    initState =
      RewriteState
        { rsCounter = 0
        , rsUsed = S.unions (map collectVarsDec decs)
        }

rewriteTopDecs :: DecEnv -> [Dec Range] -> RewriteM [Dec Range]
rewriteTopDecs _ [] = pure []
rewriteTopDecs env (dec : rest) = do
  dec' <- rewriteDec env dec
  let env' = M.insert (decName dec') dec' env
  (dec' :) <$> rewriteTopDecs env' rest

rewriteDec :: DecEnv -> Dec Range -> RewriteM (Dec Range)
rewriteDec env (Dec a name pats mw poly body) =
  Dec a name pats mw poly <$> rewriteExp env body

rewriteExp :: DecEnv -> Exp Range -> RewriteM (Exp Range)
rewriteExp env expr = do
  expr' <- case expr of
    EInt {} -> pure expr
    EFloat {} -> pure expr
    EVar {} -> pure expr
    EString {} -> pure expr
    EUnit {} -> pure expr
    EBool {} -> pure expr
    EOp {} -> pure expr
    EVec a es -> EVec a <$> mapM (rewriteExp env) es
    EApp a f x -> EApp a <$> rewriteExp env f <*> rewriteExp env x
    EIfThen a c t -> EIfThen a <$> rewriteExp env c <*> rewriteExp env t
    EIfThenElse a c t f ->
      EIfThenElse a <$> rewriteExp env c <*> rewriteExp env t <*> rewriteExp env f
    ENeg a e -> ENeg a <$> rewriteExp env e
    EBinOp a l op r -> EBinOp a <$> rewriteExp env l <*> pure op <*> rewriteExp env r
    EUnOp a op e -> EUnOp a op <$> rewriteExp env e
    ELetIn a dec body -> do
      dec' <- rewriteDec env dec
      let env' = M.insert (decName dec') dec' env
      ELetIn a dec' <$> rewriteExp env' body
    EProj a i e -> EProj a i <$> rewriteExp env e
    EPair a e1 e2 -> EPair a <$> rewriteExp env e1 <*> rewriteExp env e2
    ERecord a fields ->
      ERecord a <$> mapM (\(field, fieldExp) -> do fieldExp' <- rewriteExp env fieldExp
                                                   pure (field, fieldExp')) fields
    ERecordProj a e field -> ERecordProj a <$> rewriteExp env e <*> pure field
    EGenerate a sz f -> EGenerate a <$> rewriteExp env sz <*> rewriteExp env f
    EMap a f arr -> EMap a <$> rewriteExp env f <*> rewriteExp env arr
    EZipWith a f a1 a2 ->
      EZipWith a <$> rewriteExp env f <*> rewriteExp env a1 <*> rewriteExp env a2
    EAppend a a1 a2 -> EAppend a <$> rewriteExp env a1 <*> rewriteExp env a2
    EReduce a f z arr -> EReduce a <$> rewriteExp env f <*> rewriteExp env z <*> rewriteExp env arr
    EReduceGenerate a f z shape gen ->
      EReduceGenerate a <$> rewriteExp env f <*> rewriteExp env z <*> rewriteExp env shape <*> rewriteExp env gen
    EIterate a n initArr f ->
      EIterate a <$> rewriteExp env n <*> rewriteExp env initArr <*> rewriteExp env f
    EFoldl a f z arr -> EFoldl a <$> rewriteExp env f <*> rewriteExp env z <*> rewriteExp env arr
    EFoldlWhile a p f z arr ->
      EFoldlWhile a <$> rewriteExp env p <*> rewriteExp env f <*> rewriteExp env z <*> rewriteExp env arr
    EScan a f z arr -> EScan a <$> rewriteExp env f <*> rewriteExp env z <*> rewriteExp env arr
    EScanInclusive a f z arr ->
      EScanInclusive a <$> rewriteExp env f <*> rewriteExp env z <*> rewriteExp env arr
    EScanR a f z arr -> EScanR a <$> rewriteExp env f <*> rewriteExp env z <*> rewriteExp env arr
    EScanRInclusive a f z arr ->
      EScanRInclusive a <$> rewriteExp env f <*> rewriteExp env z <*> rewriteExp env arr
    ESegmentedReduce a f z offsets vals ->
      ESegmentedReduce a <$> rewriteExp env f <*> rewriteExp env z <*> rewriteExp env offsets <*> rewriteExp env vals
    ESortIndices a arr -> ESortIndices a <$> rewriteExp env arr
    EIota a n -> EIota a <$> rewriteExp env n
    EMakeIndex a n arr -> EMakeIndex a <$> rewriteExp env n <*> rewriteExp env arr
    ECOOSumDuplicates a nrows ncols nnz rows cols vals ->
      ECOOSumDuplicates a <$> rewriteExp env nrows <*> rewriteExp env ncols <*> rewriteExp env nnz
        <*> rewriteExp env rows <*> rewriteExp env cols <*> rewriteExp env vals
    ECSRFromSortedCOO a nrows ncols nnz rows cols vals ->
      ECSRFromSortedCOO a <$> rewriteExp env nrows <*> rewriteExp env ncols <*> rewriteExp env nnz
        <*> rewriteExp env rows <*> rewriteExp env cols <*> rewriteExp env vals
    EPermute a c d p arr ->
      EPermute a <$> rewriteExp env c <*> rewriteExp env d <*> rewriteExp env p <*> rewriteExp env arr
    EScatter a c d idx vals ->
      EScatter a <$> rewriteExp env c <*> rewriteExp env d <*> rewriteExp env idx <*> rewriteExp env vals
    EScatterGuarded a c d idx vals guardArr ->
      EScatterGuarded a <$> rewriteExp env c <*> rewriteExp env d <*> rewriteExp env idx <*> rewriteExp env vals <*> rewriteExp env guardArr
    EScatterGenerate a c d idx f ->
      EScatterGenerate a <$> rewriteExp env c <*> rewriteExp env d <*> rewriteExp env idx <*> rewriteExp env f
    EScatterChain a c d phases -> do
      c' <- rewriteExp env c
      d' <- rewriteExp env d
      phases' <-
        mapM
          (\p -> ScatterPhase <$> rewriteExp env (spIndex p)
                              <*> rewriteExp env (spValues p)
                              <*> mapM (rewriteExp env) (spGuard p))
          phases
      pure (EScatterChain a c' d' phases')
    EScatterGen a c d phases -> do
      c' <- rewriteExp env c
      d' <- rewriteExp env d
      phases' <-
        mapM
          (\p -> ScatterGenPhase <$> rewriteExp env (sgpShape p)
                                 <*> rewriteExp env (sgpIndexFn p)
                                 <*> rewriteExp env (sgpValueFn p)
                                 <*> mapM (rewriteExp env) (sgpGuardFn p))
          phases
      pure (EScatterGen a c' d' phases')
    EGather a idx arr -> EGather a <$> rewriteExp env idx <*> rewriteExp env arr
    EIndex a idx arr -> EIndex a <$> rewriteExp env idx <*> rewriteExp env arr
    ECheckIndex a idx def arr ->
      ECheckIndex a <$> rewriteExp env idx <*> rewriteExp env def <*> rewriteExp env arr
    ECompact a m z g xs ->
      ECompact a m <$> rewriteExp env z <*> rewriteExp env g <*> rewriteExp env xs
    EFill a s v -> EFill a <$> rewriteExp env s <*> rewriteExp env v
    EShapeOf a arr -> EShapeOf a <$> rewriteExp env arr
    EReplicate a dims arr -> EReplicate a <$> mapM (rewriteShapeDim env) dims <*> rewriteExp env arr
    ESlice a dims arr -> ESlice a <$> mapM (rewriteSliceDim env) dims <*> rewriteExp env arr
    EReshape a s arr -> EReshape a <$> rewriteExp env s <*> rewriteExp env arr
    EReadArray a s f -> EReadArray a <$> rewriteExp env s <*> rewriteExp env f
    EReadArrayFloat a s f -> EReadArrayFloat a <$> rewriteExp env s <*> rewriteExp env f
    EWriteArray a arr f -> EWriteArray a <$> rewriteExp env arr <*> rewriteExp env f
    EWriteArrayFloat a arr f -> EWriteArrayFloat a <$> rewriteExp env arr <*> rewriteExp env f
    EGetEnvInt a e -> EGetEnvInt a <$> rewriteExp env e
    EGetEnvString a e -> EGetEnvString a <$> rewriteExp env e
    EStencil a bnd f arr -> EStencil a <$> rewriteBnd env bnd <*> rewriteExp env f <*> rewriteExp env arr
    EBoundLetIn a x boundExp rhs body ->
      EBoundLetIn a x <$> rewriteExp env boundExp <*> rewriteExp env rhs <*> rewriteExp env body
    EReify a e -> EReify a <$> rewriteExp env e
  rewriteTemporalIterate env expr'

rewriteBnd :: DecEnv -> BoundaryCondition Range -> RewriteM (BoundaryCondition Range)
rewriteBnd _ BClamp = pure BClamp
rewriteBnd _ BWrap = pure BWrap
rewriteBnd _ BMirror = pure BMirror
rewriteBnd env (BConst e) = BConst <$> rewriteExp env e

rewriteShapeDim :: DecEnv -> ShapeDim Range -> RewriteM (ShapeDim Range)
rewriteShapeDim _ (ShapeAll a) = pure (ShapeAll a)
rewriteShapeDim env (ShapeAny a e) = ShapeAny a <$> rewriteExp env e
rewriteShapeDim env (ShapeDim a e) = ShapeDim a <$> rewriteExp env e

rewriteSliceDim :: DecEnv -> SliceDim Range -> RewriteM (SliceDim Range)
rewriteSliceDim _ (SliceAll a) = pure (SliceAll a)
rewriteSliceDim env (SliceRange a s l) = SliceRange a <$> rewriteExp env s <*> rewriteExp env l

rewriteTemporalIterate :: DecEnv -> Exp Range -> RewriteM (Exp Range)
rewriteTemporalIterate env expr = case expr of
  EIterate a n initArr stepFn
    | Just tripCount <- staticIntExp n
    , tripCount > 0
    , Just initLen <- knownOneDimLength env initArr
    , Just StepInfo {siRadius = radius} <- resolveStepInfo env stepFn
    , radius > 0 ->
        let halo = tripCount * radius
            split = initLen `div` 2
            rightLen = initLen - split
        in if halo <= split && halo <= rightLen
             then buildHaloRewrite a tripCount initArr stepFn split rightLen halo
             else pure expr
  _ ->
    pure expr

resolveStepInfo :: DecEnv -> Exp Range -> Maybe StepInfo
resolveStepInfo env stepExp = do
  (arrVar, body) <- extractSingleArgFn env stepExp
  case body of
    EStencil _ bnd stencilFn (EVar _ arrRef)
      | arrRef == arrVar
      , isSupportedBoundary bnd -> do
          (accVar, stencilBody) <- extractSingleArgFn env stencilFn
          offsets <- collectAccessorCalls accVar stencilBody
          (backward, forward) <- symmetricOneDimRadius offsets
          if backward == forward && backward > 0
            then Just StepInfo {siRadius = backward}
            else Nothing
    _ ->
      Nothing

isSupportedBoundary :: BoundaryCondition Range -> Bool
isSupportedBoundary BConst {} = True
isSupportedBoundary _ = False

extractSingleArgFn :: DecEnv -> Exp Range -> Maybe (Var, Exp Range)
extractSingleArgFn env expr = case expr of
  ELetIn _ (Dec _ _ [PVar _ argVar] _ _ body) _ ->
    Just (argVar, body)
  EVar _ name ->
    case M.lookup name env of
      Just (Dec _ _ [PVar _ argVar] _ _ body) -> Just (argVar, body)
      _ -> Nothing
  _ ->
    Nothing

collectAccessorCalls :: Var -> Exp Range -> Maybe [[Integer]]
collectAccessorCalls accVar = go
  where
    go expr = case expr of
      EApp _ (EVar _ v) d
        | v == accVar -> (\x -> [[x]]) <$> staticIntExp d
      EApp _ (EApp _ (EVar _ v) d0) d1
        | v == accVar -> (\xs -> [xs]) <$> sequence [staticIntExp d0, staticIntExp d1]
      EVec _ es -> concat <$> mapM go es
      EApp _ f x -> (++) <$> go f <*> go x
      EIfThen _ c t -> (++) <$> go c <*> go t
      EIfThenElse _ c t f -> (\x y z -> x ++ y ++ z) <$> go c <*> go t <*> go f
      ENeg _ e -> go e
      EBinOp _ l _ r -> (++) <$> go l <*> go r
      EUnOp _ _ e -> go e
      ELetIn _ (Dec _ _ _ _ _ body) e -> (++) <$> go body <*> go e
      EProj _ _ e -> go e
      EPair _ e1 e2 -> (++) <$> go e1 <*> go e2
      ERecord _ fields -> concat <$> mapM (go . snd) fields
      ERecordProj _ e _ -> go e
      EGenerate _ sz f -> (++) <$> go sz <*> go f
      EMap _ f arr -> (++) <$> go f <*> go arr
      EZipWith _ f a1 a2 -> (\x y z -> x ++ y ++ z) <$> go f <*> go a1 <*> go a2
      EAppend _ a1 a2 -> (++) <$> go a1 <*> go a2
      EReduce _ f z arr -> (\x y z' -> x ++ y ++ z') <$> go f <*> go z <*> go arr
      EReduceGenerate _ f z shape gen ->
        (\w x y z' -> w ++ x ++ y ++ z') <$> go f <*> go z <*> go shape <*> go gen
      EIterate _ n initArr f -> (\x y z -> x ++ y ++ z) <$> go n <*> go initArr <*> go f
      EFoldl _ f z arr -> (\x y z' -> x ++ y ++ z') <$> go f <*> go z <*> go arr
      EFoldlWhile _ p f z arr ->
        (\w x y z' -> w ++ x ++ y ++ z') <$> go p <*> go f <*> go z <*> go arr
      EScan _ f z arr -> (\x y z' -> x ++ y ++ z') <$> go f <*> go z <*> go arr
      EScanInclusive _ f z arr -> (\x y z' -> x ++ y ++ z') <$> go f <*> go z <*> go arr
      EScanR _ f z arr -> (\x y z' -> x ++ y ++ z') <$> go f <*> go z <*> go arr
      EScanRInclusive _ f z arr -> (\x y z' -> x ++ y ++ z') <$> go f <*> go z <*> go arr
      ESegmentedReduce _ f z offsets vals ->
        (\w x y z' -> w ++ x ++ y ++ z') <$> go f <*> go z <*> go offsets <*> go vals
      ESortIndices _ arr -> go arr
      EIota _ n -> go n
      EMakeIndex _ n arr -> (++) <$> go n <*> go arr
      ECOOSumDuplicates _ nrows ncols nnz rows cols vals ->
        concat <$> mapM go [nrows, ncols, nnz, rows, cols, vals]
      ECSRFromSortedCOO _ nrows ncols nnz rows cols vals ->
        concat <$> mapM go [nrows, ncols, nnz, rows, cols, vals]
      EPermute _ c d p arr ->
        concat <$> mapM go [c, d, p, arr]
      EScatter _ c d idx vals ->
        concat <$> mapM go [c, d, idx, vals]
      EScatterGuarded _ c d idx vals guardArr ->
        concat <$> mapM go [c, d, idx, vals, guardArr]
      EScatterGenerate _ c d idx f ->
        concat <$> mapM go [c, d, idx, f]
      EScatterChain _ c d phases -> do
        phaseCalls <-
          concat <$> mapM (\p -> concat <$> mapM go ([spIndex p, spValues p] ++ maybeToList (spGuard p))) phases
        rest <- concat <$> mapM go [c, d]
        pure (rest ++ phaseCalls)
      EScatterGen _ c d phases -> do
        phaseCalls <-
          concat
            <$> mapM
                  (\p -> concat <$> mapM go ([sgpShape p, sgpIndexFn p, sgpValueFn p] ++ maybeToList (sgpGuardFn p)))
                  phases
        rest <- concat <$> mapM go [c, d]
        pure (rest ++ phaseCalls)
      EGather _ idx arr -> (++) <$> go idx <*> go arr
      EIndex _ idx arr -> (++) <$> go idx <*> go arr
      ECheckIndex _ idx def arr -> (\x y z -> x ++ y ++ z) <$> go idx <*> go def <*> go arr
      ECompact _ _ z g xs -> (\x y w -> x ++ y ++ w) <$> go z <*> go g <*> go xs
      EFill _ s v -> (++) <$> go s <*> go v
      EShapeOf _ arr -> go arr
      EReplicate _ dims arr -> (++) <$> (concat <$> mapM goShapeDim dims) <*> go arr
      ESlice _ dims arr -> (++) <$> (concat <$> mapM goSliceDim dims) <*> go arr
      EReshape _ s arr -> (++) <$> go s <*> go arr
      EReadArray _ s f -> (++) <$> go s <*> go f
      EReadArrayFloat _ s f -> (++) <$> go s <*> go f
      EWriteArray _ arr f -> (++) <$> go arr <*> go f
      EWriteArrayFloat _ arr f -> (++) <$> go arr <*> go f
      EGetEnvInt _ e -> go e
      EGetEnvString _ e -> go e
      EStencil _ bnd f arr ->
        let goBnd = case bnd of
              BConst e -> go e
              _ -> pure []
        in (\x y z -> x ++ y ++ z) <$> goBnd <*> go f <*> go arr
      EBoundLetIn _ _ boundExp rhs body ->
        (\x y z -> x ++ y ++ z) <$> go boundExp <*> go rhs <*> go body
      EReify _ e -> go e
      EInt {} -> pure []
      EFloat {} -> pure []
      EVar {} -> pure []
      EString {} -> pure []
      EUnit {} -> pure []
      EBool {} -> pure []
      EOp {} -> pure []

    goShapeDim dim = case dim of
      ShapeAll _ -> pure []
      ShapeAny _ e -> go e
      ShapeDim _ e -> go e

    goSliceDim dim = case dim of
      SliceAll _ -> pure []
      SliceRange _ s l -> (++) <$> go s <*> go l

    maybeToList = maybe [] pure

symmetricOneDimRadius :: [[Integer]] -> Maybe (Integer, Integer)
symmetricOneDimRadius offsets = do
  off <- sequence [expectOneDim os | os <- offsets]
  let lo = minimum (0 : off)
      hi = maximum (0 : off)
      backward = max 0 (negate lo)
      forward = max 0 hi
  pure (backward, forward)
  where
    expectOneDim [x] = Just x
    expectOneDim _ = Nothing

knownOneDimLength :: DecEnv -> Exp Range -> Maybe Integer
knownOneDimLength env expr = case expr of
  EVar _ v ->
    case M.lookup v env of
      Just (Dec _ _ [] _ _ body) -> knownOneDimLength env body
      Nothing -> Nothing
      _ -> Nothing
  EGenerate _ shape _ -> oneDimStaticShape shape
  EFill _ shape _ -> oneDimStaticShape shape
  EReadArray _ shape _ -> oneDimStaticShape shape
  EReadArrayFloat _ shape _ -> oneDimStaticShape shape
  EReplicate _ _ arr -> knownOneDimLength env arr
  EReshape _ shape _ -> oneDimStaticShape shape
  EReify _ e -> knownOneDimLength env e
  _ -> Nothing

oneDimStaticShape :: Exp Range -> Maybe Integer
oneDimStaticShape shape = case shape of
  EVec _ [dim] -> staticIntExp dim
  _ -> Nothing

staticIntExp :: Exp Range -> Maybe Integer
staticIntExp expr = case expr of
  EInt _ n -> Just n
  ENeg _ e -> negate <$> staticIntExp e
  EBinOp _ lhs op rhs -> do
    x <- staticIntExp lhs
    y <- staticIntExp rhs
    case op of
      Plus _ -> Just (x + y)
      Minus _ -> Just (x - y)
      Times _ -> Just (x * y)
      Divide _ | y /= 0 -> Just (x `div` y)
      Mod _ | y /= 0 -> Just (x `mod` y)
      _ -> Nothing
  _ -> Nothing

buildHaloRewrite
  :: Range
  -> Integer
  -> Exp Range
  -> Exp Range
  -> Integer
  -> Integer
  -> Integer
  -> RewriteM (Exp Range)
buildHaloRewrite a tripCount initArr stepFn split rightLen halo = do
  initName <- freshName "__temporal_halo_init"
  leftName <- freshName "__temporal_halo_left"
  rightName <- freshName "__temporal_halo_right"
  leftIterName <- freshName "__temporal_halo_left_iter"
  rightIterName <- freshName "__temporal_halo_right_iter"
  leftResName <- freshName "__temporal_halo_left_res"
  rightResName <- freshName "__temporal_halo_right_res"
  let initVar = EVar a initName
      leftVar = EVar a leftName
      rightVar = EVar a rightName
      leftIterVar = EVar a leftIterName
      rightIterVar = EVar a rightIterName
      leftResVar = EVar a leftResName
      rightResVar = EVar a rightResName
      countExp = EInt a tripCount
      mkSlice start len arr =
        ESlice a [SliceRange a (EInt a start) (EInt a len)] arr
      result =
        EAppend a leftResVar rightResVar
  pure $
    mkLet a initName initArr $
      mkLet a leftName (mkSlice 0 (split + halo) initVar) $
        mkLet a rightName (mkSlice (split - halo) (rightLen + halo) initVar) $
          mkLet a leftIterName (EIterate a countExp leftVar stepFn) $
            mkLet a rightIterName (EIterate a countExp rightVar stepFn) $
              mkLet a leftResName (mkSlice 0 split leftIterVar) $
                mkLet a rightResName (mkSlice halo rightLen rightIterVar) result

mkLet :: Range -> Var -> Exp Range -> Exp Range -> Exp Range
mkLet a name rhs body =
  ELetIn a (Dec a name [] Nothing Nothing rhs) body

freshName :: Var -> RewriteM Var
freshName prefix = do
  st <- get
  let (name, nextCounter) =
        freshUnusedName
          (\n -> prefix <> BS.pack (show n))
          (rsUsed st)
          (rsCounter st)
  put st { rsCounter = nextCounter, rsUsed = S.insert name (rsUsed st) }
  pure name
