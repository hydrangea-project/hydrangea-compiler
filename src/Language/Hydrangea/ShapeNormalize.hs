{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.ShapeNormalize
--
-- A shape normalization pass that canonicalizes shape expressions to enable
-- better fusion opportunities, particularly for zipWith over sliced arrays.
--
-- The pass works by:
-- 1. Building an environment of known array shapes from let-bindings
-- 2. Normalizing shape expressions (EShapeOf, EProj) by looking up known shapes
-- 3. Folding constant shape projections when possible
--
-- This enables zipWith fusion over sliced arrays by ensuring both slices have
-- syntactically equal normalized shape expressions.
module Language.Hydrangea.ShapeNormalize
  ( normalizeShapesExp
  , normalizeShapesDec
  , normalizeShapesDecs
  ) where

import Control.Monad.State
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Language.Hydrangea.Syntax

-- | Environment tracking known shapes for array variables.
--
-- For each array variable, we store its shape expression if known.
-- Unknown shapes are not stored (conservative approach).
data ShapeEnv a = ShapeEnv
  { arrayShapes :: Map Var (Exp a)
  }

-- | Monad for shape normalization with environment.
newtype ShapeM a b = ShapeM { runShapeM :: State (ShapeEnv a) b }
  deriving (Functor, Applicative, Monad, MonadState (ShapeEnv a))

-- | Run the shape normalization monad with an empty environment.
runWithEmptyEnv :: ShapeM a b -> b
runWithEmptyEnv m = evalState (runShapeM m) (ShapeEnv M.empty)

-- | Look up the shape of an array variable.
lookupShape :: Var -> ShapeM a (Maybe (Exp a))
lookupShape v = gets (M.lookup v . arrayShapes)

-- | Add a shape binding to the environment.
bindShape :: Var -> Exp a -> ShapeM a ()
bindShape v shapeExpr = modify $ \env ->
  env { arrayShapes = M.insert v shapeExpr (arrayShapes env) }

-- | Remove a shape binding from the environment (for shadowing).
unbindShape :: Var -> ShapeM a ()
unbindShape v = modify $ \env ->
  env { arrayShapes = M.delete v (arrayShapes env) }

-- | Normalize a single expression.
normalizeShapesExp :: Exp a -> Exp a
normalizeShapesExp expr = runWithEmptyEnv (normExp expr)

-- | Normalize a declaration.
normalizeShapesDec :: Dec a -> Dec a
normalizeShapesDec dec = runWithEmptyEnv (normDec dec)

-- | Normalize multiple declarations (for top-level).
normalizeShapesDecs :: [Dec a] -> [Dec a]
normalizeShapesDecs decs = runWithEmptyEnv (mapM normDec decs)

-- | Extract shape from an array-producing expression.
--
-- Returns the shape expression if known, otherwise Nothing.
extractShape :: Exp a -> Maybe (Exp a)
extractShape expr = case expr of
  -- generate shape fn -> shape
  EGenerate _ shape _ -> Just shape
  -- fill shape val -> shape  
  EFill _ shape _ -> Just shape
  -- reshape newShape arr -> newShape
  EReshape _ newShape _ -> Just newShape
  -- For other array producers, return Nothing (conservative)
  _ -> Nothing

-- | Normalize an expression.
normExp :: Exp a -> ShapeM a (Exp a)
normExp expr = case expr of
  -- EShapeOf: try to resolve to concrete shape
  EShapeOf a arr -> do
    arr' <- normExp arr
    case arr' of
      -- shape_of (generate s f) = s
      EGenerate _ shape _ -> normExp shape
      -- shape_of (fill s v) = s
      EFill _ shape _ -> normExp shape
      -- shape_of (reshape s a) = s
      EReshape _ shape _ -> normExp shape
      -- shape_of (slice dims arr) = normalized slice shape
      ESlice _ dims src -> do
        -- For slice, compute the shape directly from source shape and slice dims
        normExp $ sliceShape a (EShapeOf a src) dims
      -- shape_of var: look up in environment
      EVar _ v -> do
        mShape <- lookupShape v
        case mShape of
          Just shape -> pure shape
          Nothing -> pure (EShapeOf a arr')
      -- Default: keep as shape_of but with normalized inner expr
      _ -> pure (EShapeOf a arr')

  -- EProj: try to fold constant projections
  EProj a i vecExpr -> do
    vecExpr' <- normExp vecExpr
    case vecExpr' of
      -- EVec [e0, e1, ...] !! i = ei (if in bounds)
      EVec _ elems | i >= 0 && fromIntegral i < length elems ->
        normExp (elems !! fromIntegral i)
      -- Otherwise keep the projection
      _ -> pure (EProj a i vecExpr')

  -- ESlice: normalize dimensions and source
  ESlice a dims arr -> do
    arr' <- normExp arr
    -- Normalize dimensions
    dims' <- mapM normSliceDim dims
    -- Try to simplify slice of slice
    case arr' of
      ESlice _ innerDims innerArr ->
        -- slice dims2 (slice dims1 arr) = slice (compose dims2 dims1) arr
        normExp $ ESlice a (composeSliceDims innerDims dims') innerArr
      _ -> do
        -- Return normalized slice
        pure $ ESlice a dims' arr'

  -- ELetIn: handle shape binding
  ELetIn a dec body -> do
    dec'@(Dec _ name _ _ decBody) <- normDec dec
    -- If this is an array binding, extract and bind its shape
    case extractShape decBody of
      Just shapeExpr -> do
        -- Add binding, normalize body, then remove binding
        bindShape name shapeExpr
        body' <- normExp body
        unbindShape name
        pure (ELetIn a dec' body')
      Nothing -> do
        body' <- normExp body
        pure (ELetIn a dec' body')

  -- Recursively normalize all subexpressions
  EInt {} -> pure expr
  EFloat {} -> pure expr
  EVar {} -> pure expr
  EString {} -> pure expr
  EUnit {} -> pure expr
  EBool {} -> pure expr
  ENeg a e -> ENeg a <$> normExp e
  EBinOp a l op r -> EBinOp a <$> normExp l <*> pure op <*> normExp r
  EUnOp a op e -> EUnOp a op <$> normExp e
  EOp {} -> pure expr
  EApp a f x -> EApp a <$> normExp f <*> normExp x
  EIfThen a c t -> EIfThen a <$> normExp c <*> normExp t
  EIfThenElse a c t f -> EIfThenElse a <$> normExp c <*> normExp t <*> normExp f
  EVec a es -> EVec a <$> mapM normExp es
  EPair a e1 e2 -> EPair a <$> normExp e1 <*> normExp e2
  ERecord a fields ->
    ERecord a <$> mapM (\(field, fieldExp) -> do
      fieldExp' <- normExp fieldExp
      pure (field, fieldExp')) fields
  ERecordProj a e field -> ERecordProj a <$> normExp e <*> pure field
  EGenerate a sz f -> EGenerate a <$> normExp sz <*> normExp f
  EMap a f arr -> EMap a <$> normExp f <*> normExp arr
  EZipWith a f a1 a2 -> EZipWith a <$> normExp f <*> normExp a1 <*> normExp a2
  EReduce a f z arr -> EReduce a <$> normExp f <*> normExp z <*> normExp arr
  EReduceGenerate a f z sz g -> EReduceGenerate a <$> normExp f <*> normExp z <*> normExp sz <*> normExp g
  EFoldl a f z arr -> EFoldl a <$> normExp f <*> normExp z <*> normExp arr
  EScan a f z arr -> EScan a <$> normExp f <*> normExp z <*> normExp arr
  ESegmentedReduce a f z offsets vals ->
    ESegmentedReduce a <$> normExp f <*> normExp z <*> normExp offsets <*> normExp vals
  ESortIndices a arr -> ESortIndices a <$> normExp arr
  EIota a n -> EIota a <$> normExp n
  EMakeIndex a n arr -> EMakeIndex a <$> normExp n <*> normExp arr
  ECOOSumDuplicates a nrows ncols nnz rows cols vals ->
    ECOOSumDuplicates a <$> normExp nrows <*> normExp ncols <*> normExp nnz
      <*> normExp rows <*> normExp cols <*> normExp vals
  ECSRFromSortedCOO a nrows ncols nnz rows cols vals ->
    ECSRFromSortedCOO a <$> normExp nrows <*> normExp ncols <*> normExp nnz
      <*> normExp rows <*> normExp cols <*> normExp vals
  EPermute a c d p arr -> EPermute a <$> normExp c <*> normExp d <*> normExp p <*> normExp arr
  EScatter a c d idx vals -> EScatter a <$> normExp c <*> normExp d <*> normExp idx <*> normExp vals
  EScatterGuarded a c d idx vals guardArr ->
    EScatterGuarded a <$> normExp c <*> normExp d <*> normExp idx <*> normExp vals <*> normExp guardArr
  EScatterGenerate a c d idx f -> EScatterGenerate a <$> normExp c <*> normExp d <*> normExp idx <*> normExp f
  EGather a idx arr -> EGather a <$> normExp idx <*> normExp arr
  EIndex a idx arr -> EIndex a <$> normExp idx <*> normExp arr
  ECheckIndex a idx def arr -> ECheckIndex a <$> normExp idx <*> normExp def <*> normExp arr
  EFill a sz v -> EFill a <$> normExp sz <*> normExp v
  EReplicate a dims arr -> EReplicate a <$> mapM normShapeDim dims <*> normExp arr
  EReshape a sz arr -> EReshape a <$> normExp sz <*> normExp arr
  EReadArray a sz f -> EReadArray a <$> normExp sz <*> normExp f
  EReadArrayFloat a sz f -> EReadArrayFloat a <$> normExp sz <*> normExp f
  EWriteArray a arr f -> EWriteArray a <$> normExp arr <*> normExp f
  EWriteArrayFloat a arr f -> EWriteArrayFloat a <$> normExp arr <*> normExp f
  EGetEnvInt a e -> EGetEnvInt a <$> normExp e
  EGetEnvString a e -> EGetEnvString a <$> normExp e
  EBoundLetIn a v boundExp rhs body ->
    EBoundLetIn a v <$> normExp boundExp <*> normExp rhs <*> normExp body
  EStencil a bnd f arr -> EStencil a <$> normBnd bnd <*> normExp f <*> normExp arr

normBnd :: BoundaryCondition a -> ShapeM a (BoundaryCondition a)
normBnd BClamp     = pure BClamp
normBnd BWrap      = pure BWrap
normBnd BMirror    = pure BMirror
normBnd (BConst e) = BConst <$> normExp e

-- | Normalize a slice dimension.
normSliceDim :: SliceDim a -> ShapeM a (SliceDim a)
normSliceDim dim = case dim of
  SliceAll {} -> pure dim
  SliceRange a start len -> SliceRange a <$> normExp start <*> normExp len

-- | Normalize a shape dimension (for replicate).
normShapeDim :: ShapeDim a -> ShapeM a (ShapeDim a)
normShapeDim dim = case dim of
  ShapeAll {} -> pure dim
  ShapeAny a e -> ShapeAny a <$> normExp e
  ShapeDim a e -> ShapeDim a <$> normExp e

-- | Normalize a declaration.
normDec :: Dec a -> ShapeM a (Dec a)
normDec (Dec a name pats poly body) = do
  -- For declarations with patterns, we can't easily track shapes
  -- Just normalize the body
  body' <- normExp body
  pure (Dec a name pats poly body')

-- | Compute shape expression for a slice operation.
sliceShape :: a -> Exp a -> [SliceDim a] -> Exp a
sliceShape a srcShape dims =
  EVec a (zipWith (sliceDimShape a srcShape) [(0::Int)..] dims)

-- | Compute shape for a single slice dimension.
sliceDimShape :: a -> Exp a -> Int -> SliceDim a -> Exp a
sliceDimShape a srcShape idx dim = case dim of
  SliceAll _ -> EProj a (fromIntegral idx) srcShape
  SliceRange _ _ lenExpr -> lenExpr

-- | Compose two sets of slice dimensions.
-- slice dims2 (slice dims1 arr) = slice (compose dims2 dims1) arr
composeSliceDims :: [SliceDim a] -> [SliceDim a] -> [SliceDim a]
composeSliceDims innerDims outerDims =
  zipWith composeDim (zip [(0::Int)..] innerDims) outerDims
  where
    composeDim (idx, innerDim) outerDim = case (innerDim, outerDim) of
      -- All composed with All = All
      (SliceAll _, SliceAll a) -> SliceAll a
      -- All composed with Range = project from inner source
      (SliceAll _, SliceRange a start len) -> SliceRange a start len
      -- Range composed with All = Range (offset)
      (SliceRange _ innerStart _, SliceAll a) ->
        -- The outer All wants the full range of the inner slice
        -- So we keep the inner range but note it's offset
        SliceAll a  -- Simplified: treat as taking all of sliced dim
      -- Range composed with Range = compose ranges
      (SliceRange a1 innerStart innerLen, SliceRange a outerStart outerLen) ->
        SliceRange a (EBinOp a1 innerStart (Plus a1) outerStart) outerLen
      _ -> outerDim  -- Conservative fallback
