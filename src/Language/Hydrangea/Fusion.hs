{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.Fusion
--
-- A semantics-preserving array fusion pass based on symbolic producers.
--
-- The pass walks the AST and, when possible, interprets array terms as
-- symbolic producers:
--
--   * a symbolic shape expression, and
--   * a symbolic element kernel represented with a small lambda-like IR
--     (@KExpr@) using de Bruijn indices.
--
-- This enables generic composition across chains of @map@/@zipwith@
-- without hard-coding every nested syntactic pattern.
--
-- Producer interpretation currently covers core array combinators and selected
-- index-transforming operations:
-- @generate@, @fill@, @map@, @zipwith@, @gather@, @replicate@, @slice@,
-- with emission into @generate@ or @fill@ forms, and reduce fusion into
-- @reduce_generate@ when applicable.
--
-- The pass still keeps conservative local rewrites (let-hoisting/inlining
-- and selected combinator rewrites) for cases that are not represented by
-- producers.
--
-- Binder strategy (let + patterns):
--  * Inline a let-bound array expression when it is used exactly once and
--    inlining cannot capture variables (checked via free/bound-variable sets).
--  * Hoist a let-expression through array combinators when the bound names
--    do not appear free in the other operands (scope-preserving move).
--  * Avoid general alpha-renaming; instead, skip a rewrite when capture risk
--    is detected. Fresh names are only introduced for synthesized functions.
--
-- Naming conventions:
--  * __fusion_* names are generated locally by this pass and are safe to treat
--    as compiler-internal binders (not user-defined).
--  * __uniq_* names come from the Uniquify pass and indicate hygienically
--    freshened binders. Fusion never relies on the exact suffix.
--
-- Termination strategy:
--  * Fusion runs as a bounded fixed-point iteration (@fuseFixWithLimit@).
--  * Stabilization checks both structural equality and alpha-equivalence
--    under normalization of internal @__fusion_*@ names. This prevents
--    non-progress loops caused only by fresh binder renaming.
module Language.Hydrangea.Fusion
  ( fuseExp
  , fuseDec
  , fuseDecs
  ) where

import Control.Monad.State
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Language.Hydrangea.Syntax
import Language.Hydrangea.ShapeNormalize (normalizeShapesExp)

-- | Track used variables to avoid capture, plus a fresh counter.
data FusionState = FusionState
  { freshCounter :: Int
  , usedVars :: Set Var
  }

newtype FusionM a = FusionM {runFusionM :: State FusionState a}
  deriving (Functor, Applicative, Monad, MonadState FusionState)

-- | Collect all variable names appearing in an expression.
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
    EStencil _ bnd f arr ->
      collectVarsBndFusion bnd `S.union` collectVarsExp f `S.union` collectVarsExp arr
    EBoundLetIn _ x boundExp rhs body ->
      S.insert x (collectVarsExp boundExp `S.union` collectVarsExp rhs `S.union` collectVarsExp body)

collectVarsBndFusion :: BoundaryCondition a -> Set Var
collectVarsBndFusion BClamp     = S.empty
collectVarsBndFusion BWrap      = S.empty
collectVarsBndFusion BMirror    = S.empty
collectVarsBndFusion (BConst e) = collectVarsExp e

collectVarsDec :: Dec a -> Set Var
collectVarsDec (Dec _ name pats _ body) =
  S.insert name (S.unions (map collectVarsPat pats) `S.union` collectVarsExp body)

collectVarsPat :: Pat a -> Set Var
collectVarsPat pat =
  case pat of
    PVar _ v -> S.singleton v
    PBound _ v _ -> S.singleton v
    PVec _ ps -> S.unions (map collectVarsPat ps)

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

patVars :: Pat a -> Set Var
patVars pat =
  case pat of
    PVar _ v -> S.singleton v
    PBound _ v _ -> S.singleton v
    PVec _ ps -> S.unions (map patVars ps)

decBoundVars :: Dec a -> Set Var
decBoundVars (Dec _ name pats _ _) = S.insert name (S.unions (map patVars pats))

freeVarsExp :: Exp a -> Set Var
freeVarsExp expr =
  case expr of
    EInt _ _ -> S.empty
    EFloat _ _ -> S.empty
    EVar _ v -> S.singleton v
    EString _ _ -> S.empty
    EUnit _ -> S.empty
    EBool _ _ -> S.empty
    EVec _ es -> S.unions (map freeVarsExp es)
    EApp _ f x -> freeVarsExp f `S.union` freeVarsExp x
    EIfThen _ c t -> freeVarsExp c `S.union` freeVarsExp t
    EIfThenElse _ c t f -> freeVarsExp c `S.union` freeVarsExp t `S.union` freeVarsExp f
    ENeg _ e -> freeVarsExp e
    EBinOp _ l _ r -> freeVarsExp l `S.union` freeVarsExp r
    EUnOp _ _ e -> freeVarsExp e
    EOp _ _ -> S.empty
    ELetIn _ dec body ->
      let bound = decBoundVars dec
       in freeVarsDec dec `S.union` (freeVarsExp body S.\\ bound)
    EProj _ _ e -> freeVarsExp e
    EPair _ e1 e2 -> freeVarsExp e1 `S.union` freeVarsExp e2
    ERecord _ fields -> S.unions (map (freeVarsExp . snd) fields)
    ERecordProj _ e _ -> freeVarsExp e
    EGenerate _ sz f -> freeVarsExp sz `S.union` freeVarsExp f
    EMap _ f arr -> freeVarsExp f `S.union` freeVarsExp arr
    EZipWith _ f a b -> freeVarsExp f `S.union` freeVarsExp a `S.union` freeVarsExp b
    EReduce _ f z arr -> freeVarsExp f `S.union` freeVarsExp z `S.union` freeVarsExp arr
    EReduceGenerate _ f z shape gen -> freeVarsExp f `S.union` freeVarsExp z `S.union` freeVarsExp shape `S.union` freeVarsExp gen
    EFoldl _ f z arr -> freeVarsExp f `S.union` freeVarsExp z `S.union` freeVarsExp arr
    EScan _ f z arr -> freeVarsExp f `S.union` freeVarsExp z `S.union` freeVarsExp arr
    ESegmentedReduce _ f z offsets vals ->
      freeVarsExp f `S.union` freeVarsExp z `S.union` freeVarsExp offsets `S.union` freeVarsExp vals
    ESortIndices _ arr -> freeVarsExp arr
    EIota _ n -> freeVarsExp n
    EMakeIndex _ n arr -> freeVarsExp n `S.union` freeVarsExp arr
    ECOOSumDuplicates _ nrows ncols nnz rows cols vals ->
      freeVarsExp nrows `S.union` freeVarsExp ncols `S.union` freeVarsExp nnz
        `S.union` freeVarsExp rows `S.union` freeVarsExp cols `S.union` freeVarsExp vals
    ECSRFromSortedCOO _ nrows ncols nnz rows cols vals ->
      freeVarsExp nrows `S.union` freeVarsExp ncols `S.union` freeVarsExp nnz
        `S.union` freeVarsExp rows `S.union` freeVarsExp cols `S.union` freeVarsExp vals
    EPermute _ c d p a -> freeVarsExp c `S.union` freeVarsExp d `S.union` freeVarsExp p `S.union` freeVarsExp a
    EScatter _ c d idx v -> freeVarsExp c `S.union` freeVarsExp d `S.union` freeVarsExp idx `S.union` freeVarsExp v
    EScatterGuarded _ c d idx v g -> freeVarsExp c `S.union` freeVarsExp d `S.union` freeVarsExp idx `S.union` freeVarsExp v `S.union` freeVarsExp g
    EScatterGenerate _ c d idx f -> freeVarsExp c `S.union` freeVarsExp d `S.union` freeVarsExp idx `S.union` freeVarsExp f
    EGather _ idx a -> freeVarsExp idx `S.union` freeVarsExp a
    EIndex _ i a -> freeVarsExp i `S.union` freeVarsExp a
    ECheckIndex _ i def a -> freeVarsExp i `S.union` freeVarsExp def `S.union` freeVarsExp a
    EFill _ s v -> freeVarsExp s `S.union` freeVarsExp v
    EShapeOf _ a -> freeVarsExp a
    EReplicate _ dims a -> S.unions (map freeVarsShapeDim dims) `S.union` freeVarsExp a
    ESlice _ dims a -> S.unions (map freeVarsSliceDim dims) `S.union` freeVarsExp a
    EReshape _ s a -> freeVarsExp s `S.union` freeVarsExp a
    EReadArray _ s f -> freeVarsExp s `S.union` freeVarsExp f
    EReadArrayFloat _ s f -> freeVarsExp s `S.union` freeVarsExp f
    EWriteArray _ arr f -> freeVarsExp arr `S.union` freeVarsExp f
    EWriteArrayFloat _ arr f -> freeVarsExp arr `S.union` freeVarsExp f
    EGetEnvInt _ e -> freeVarsExp e
    EGetEnvString _ e -> freeVarsExp e
    EStencil _ bnd f arr ->
      freeVarsBnd bnd `S.union` freeVarsExp f `S.union` freeVarsExp arr
    EBoundLetIn _ x boundExp rhs body ->
      collectVarsExp boundExp `S.union` freeVarsExp rhs
        `S.union` S.delete x (freeVarsExp body)

freeVarsBnd :: BoundaryCondition a -> Set Var
freeVarsBnd BClamp     = S.empty
freeVarsBnd BWrap      = S.empty
freeVarsBnd BMirror    = S.empty
freeVarsBnd (BConst e) = freeVarsExp e

freeVarsDec :: Dec a -> Set Var
freeVarsDec (Dec _ _ pats _ body) = freeVarsExp body S.\\ S.unions (map patVars pats)

freeVarsShapeDim :: ShapeDim a -> Set Var
freeVarsShapeDim dim =
  case dim of
    ShapeAll _ -> S.empty
    ShapeAny _ e -> freeVarsExp e
    ShapeDim _ e -> freeVarsExp e

freeVarsSliceDim :: SliceDim a -> Set Var
freeVarsSliceDim dim =
  case dim of
    SliceAll _ -> S.empty
    SliceRange _ s l -> freeVarsExp s `S.union` freeVarsExp l

boundVarsExp :: Exp a -> Set Var
boundVarsExp expr =
  case expr of
    EInt _ _ -> S.empty
    EFloat _ _ -> S.empty
    EVar _ _ -> S.empty
    EString _ _ -> S.empty
    EUnit _ -> S.empty
    EBool _ _ -> S.empty
    EVec _ es -> S.unions (map boundVarsExp es)
    EApp _ f x -> boundVarsExp f `S.union` boundVarsExp x
    EIfThen _ c t -> boundVarsExp c `S.union` boundVarsExp t
    EIfThenElse _ c t f -> boundVarsExp c `S.union` boundVarsExp t `S.union` boundVarsExp f
    ENeg _ e -> boundVarsExp e
    EBinOp _ l _ r -> boundVarsExp l `S.union` boundVarsExp r
    EUnOp _ _ e -> boundVarsExp e
    EOp _ _ -> S.empty
    ELetIn _ dec body -> decBoundVars dec `S.union` boundVarsExp body `S.union` boundVarsDecBody dec
    EProj _ _ e -> boundVarsExp e
    EPair _ e1 e2 -> boundVarsExp e1 `S.union` boundVarsExp e2
    ERecord _ fields -> S.unions (map (boundVarsExp . snd) fields)
    ERecordProj _ e _ -> boundVarsExp e
    EGenerate _ sz f -> boundVarsExp sz `S.union` boundVarsExp f
    EMap _ f arr -> boundVarsExp f `S.union` boundVarsExp arr
    EZipWith _ f a b -> boundVarsExp f `S.union` boundVarsExp a `S.union` boundVarsExp b
    EReduce _ f z arr -> boundVarsExp f `S.union` boundVarsExp z `S.union` boundVarsExp arr
    EReduceGenerate _ f z shape gen -> boundVarsExp f `S.union` boundVarsExp z `S.union` boundVarsExp shape `S.union` boundVarsExp gen
    EFoldl _ f z arr -> boundVarsExp f `S.union` boundVarsExp z `S.union` boundVarsExp arr
    EScan _ f z arr -> boundVarsExp f `S.union` boundVarsExp z `S.union` boundVarsExp arr
    ESegmentedReduce _ f z offsets vals ->
      boundVarsExp f `S.union` boundVarsExp z `S.union` boundVarsExp offsets `S.union` boundVarsExp vals
    ESortIndices _ arr -> boundVarsExp arr
    EIota _ n -> boundVarsExp n
    EMakeIndex _ n arr -> boundVarsExp n `S.union` boundVarsExp arr
    ECOOSumDuplicates _ nrows ncols nnz rows cols vals ->
      boundVarsExp nrows `S.union` boundVarsExp ncols `S.union` boundVarsExp nnz
        `S.union` boundVarsExp rows `S.union` boundVarsExp cols `S.union` boundVarsExp vals
    ECSRFromSortedCOO _ nrows ncols nnz rows cols vals ->
      boundVarsExp nrows `S.union` boundVarsExp ncols `S.union` boundVarsExp nnz
        `S.union` boundVarsExp rows `S.union` boundVarsExp cols `S.union` boundVarsExp vals
    EPermute _ c d p a -> boundVarsExp c `S.union` boundVarsExp d `S.union` boundVarsExp p `S.union` boundVarsExp a
    EScatter _ c d idx v -> boundVarsExp c `S.union` boundVarsExp d `S.union` boundVarsExp idx `S.union` boundVarsExp v
    EScatterGuarded _ c d idx v g -> boundVarsExp c `S.union` boundVarsExp d `S.union` boundVarsExp idx `S.union` boundVarsExp v `S.union` boundVarsExp g
    EScatterGenerate _ c d idx f -> boundVarsExp c `S.union` boundVarsExp d `S.union` boundVarsExp idx `S.union` boundVarsExp f
    EGather _ idx a -> boundVarsExp idx `S.union` boundVarsExp a
    EIndex _ i a -> boundVarsExp i `S.union` boundVarsExp a
    ECheckIndex _ i def a -> boundVarsExp i `S.union` boundVarsExp def `S.union` boundVarsExp a
    EFill _ s v -> boundVarsExp s `S.union` boundVarsExp v
    EShapeOf _ a -> boundVarsExp a
    EReplicate _ dims a -> S.unions (map boundVarsShapeDim dims) `S.union` boundVarsExp a
    ESlice _ dims a -> S.unions (map boundVarsSliceDim dims) `S.union` boundVarsExp a
    EReshape _ s a -> boundVarsExp s `S.union` boundVarsExp a
    EReadArray _ _ _ -> S.empty
    EReadArrayFloat _ _ _ -> S.empty
    EWriteArray _ _ _ -> S.empty
    EWriteArrayFloat _ _ _ -> S.empty
    EGetEnvInt _ _ -> S.empty
    EGetEnvString _ _ -> S.empty
    EStencil _ bnd f arr ->
      boundVarsBnd bnd `S.union` boundVarsExp f `S.union` boundVarsExp arr
    EBoundLetIn _ x _ rhs body ->
      S.insert x (boundVarsExp rhs `S.union` boundVarsExp body)

boundVarsBnd :: BoundaryCondition a -> Set Var
boundVarsBnd BClamp     = S.empty
boundVarsBnd BWrap      = S.empty
boundVarsBnd BMirror    = S.empty
boundVarsBnd (BConst e) = boundVarsExp e

boundVarsDecBody :: Dec a -> Set Var
boundVarsDecBody (Dec _ _ pats _ body) = boundVarsExp body `S.union` S.unions (map patVars pats)

boundVarsShapeDim :: ShapeDim a -> Set Var
boundVarsShapeDim dim =
  case dim of
    ShapeAll _ -> S.empty
    ShapeAny _ e -> boundVarsExp e
    ShapeDim _ e -> boundVarsExp e

boundVarsSliceDim :: SliceDim a -> Set Var
boundVarsSliceDim dim =
  case dim of
    SliceAll _ -> S.empty
    SliceRange _ s l -> boundVarsExp s `S.union` boundVarsExp l

countVarExp :: Var -> Exp a -> Int
countVarExp v expr =
  case expr of
    EInt _ _ -> 0
    EFloat _ _ -> 0
    EVar _ v' -> if v == v' then 1 else 0
    EString _ _ -> 0
    EUnit _ -> 0
    EBool _ _ -> 0
    EVec _ es -> sum (map (countVarExp v) es)
    EApp _ f x -> countVarExp v f + countVarExp v x
    EIfThen _ c t -> countVarExp v c + countVarExp v t
    EIfThenElse _ c t f -> countVarExp v c + countVarExp v t + countVarExp v f
    ENeg _ e -> countVarExp v e
    EBinOp _ l _ r -> countVarExp v l + countVarExp v r
    EUnOp _ _ e -> countVarExp v e
    EOp _ _ -> 0
    ELetIn _ dec body ->
      let name = decName dec
          patBound = S.unions (map patVars (case dec of Dec _ _ ps _ _ -> ps))
          inDec = if v == name || v `S.member` patBound then 0 else countVarExp v (case dec of Dec _ _ _ _ b -> b)
          inBody = if v == name then 0 else countVarExp v body
       in inDec + inBody
    EProj _ _ e -> countVarExp v e
    EPair _ e1 e2 -> countVarExp v e1 + countVarExp v e2
    ERecord _ fields -> sum (map (countVarExp v . snd) fields)
    ERecordProj _ e _ -> countVarExp v e
    EGenerate _ sz f -> countVarExp v sz + countVarExp v f
    EMap _ f arr -> countVarExp v f + countVarExp v arr
    EZipWith _ f a b -> countVarExp v f + countVarExp v a + countVarExp v b
    EReduce _ f z arr -> countVarExp v f + countVarExp v z + countVarExp v arr
    EReduceGenerate _ f z shape gen -> countVarExp v f + countVarExp v z + countVarExp v shape + countVarExp v gen
    EFoldl _ f z arr -> countVarExp v f + countVarExp v z + countVarExp v arr
    EScan _ f z arr -> countVarExp v f + countVarExp v z + countVarExp v arr
    ESegmentedReduce _ f z offsets vals ->
      countVarExp v f + countVarExp v z + countVarExp v offsets + countVarExp v vals
    ESortIndices _ arr -> countVarExp v arr
    EIota _ n -> countVarExp v n
    EMakeIndex _ n arr -> countVarExp v n + countVarExp v arr
    ECOOSumDuplicates _ nrows ncols nnz rows cols vals ->
      countVarExp v nrows + countVarExp v ncols + countVarExp v nnz
        + countVarExp v rows + countVarExp v cols + countVarExp v vals
    ECSRFromSortedCOO _ nrows ncols nnz rows cols vals ->
      countVarExp v nrows + countVarExp v ncols + countVarExp v nnz
        + countVarExp v rows + countVarExp v cols + countVarExp v vals
    EPermute _ c d p a -> countVarExp v c + countVarExp v d + countVarExp v p + countVarExp v a
    EScatter _ c d idx val -> countVarExp v c + countVarExp v d + countVarExp v idx + countVarExp v val
    EScatterGuarded _ c d idx val g -> countVarExp v c + countVarExp v d + countVarExp v idx + countVarExp v val + countVarExp v g
    EScatterGenerate _ c d idx f -> countVarExp v c + countVarExp v d + countVarExp v idx + countVarExp v f
    EGather _ idx a -> countVarExp v idx + countVarExp v a
    EIndex _ i a -> countVarExp v i + countVarExp v a
    ECheckIndex _ i def a -> countVarExp v i + countVarExp v def + countVarExp v a
    EFill _ s val -> countVarExp v s + countVarExp v val
    EShapeOf _ a -> countVarExp v a
    EReplicate _ dims a -> sum (map (countVarShapeDim v) dims) + countVarExp v a
    ESlice _ dims a -> sum (map (countVarSliceDim v) dims) + countVarExp v a
    EReshape _ s a -> countVarExp v s + countVarExp v a
    EReadArray _ s f -> countVarExp v s + countVarExp v f
    EReadArrayFloat _ s f -> countVarExp v s + countVarExp v f
    EWriteArray _ arr f -> countVarExp v arr + countVarExp v f
    EWriteArrayFloat _ arr f -> countVarExp v arr + countVarExp v f
    EGetEnvInt _ e -> countVarExp v e
    EGetEnvString _ e -> countVarExp v e
    EStencil _ bnd f arr ->
      countVarBnd v bnd + countVarExp v f + countVarExp v arr
    EBoundLetIn _ _ boundExp rhs body ->
      countVarExp v boundExp + countVarExp v rhs + countVarExp v body

countVarBnd :: Var -> BoundaryCondition a -> Int
countVarBnd _ BClamp     = 0
countVarBnd _ BWrap      = 0
countVarBnd _ BMirror    = 0
countVarBnd v (BConst e) = countVarExp v e

countVarShapeDim :: Var -> ShapeDim a -> Int
countVarShapeDim v dim =
  case dim of
    ShapeAll _ -> 0
    ShapeAny _ e -> countVarExp v e
    ShapeDim _ e -> countVarExp v e

countVarSliceDim :: Var -> SliceDim a -> Int
countVarSliceDim v dim =
  case dim of
    SliceAll _ -> 0
    SliceRange _ s l -> countVarExp v s + countVarExp v l

substExp :: Var -> Exp a -> Exp a -> Exp a
substExp v replacement expr =
  case expr of
    EInt {} -> expr
    EFloat {} -> expr
    EVar a v' -> if v == v' then replacement else EVar a v'
    EString {} -> expr
    EUnit {} -> expr
    EBool {} -> expr
    EVec a es -> EVec a (map (substExp v replacement) es)
    EApp a f x -> EApp a (substExp v replacement f) (substExp v replacement x)
    EIfThen a c t -> EIfThen a (substExp v replacement c) (substExp v replacement t)
    EIfThenElse a c t f -> EIfThenElse a (substExp v replacement c) (substExp v replacement t) (substExp v replacement f)
    ENeg a e -> ENeg a (substExp v replacement e)
    EBinOp a l op r -> EBinOp a (substExp v replacement l) op (substExp v replacement r)
    EUnOp a op e -> EUnOp a op (substExp v replacement e)
    EOp {} -> expr
    ELetIn a dec body ->
      let Dec da name pats poly decBody = dec
          patBound = S.unions (map patVars pats)
          decBody' = if v == name || v `S.member` patBound then decBody else substExp v replacement decBody
          body' = if v == name then body else substExp v replacement body
       in ELetIn a (Dec da name pats poly decBody') body'
    EProj a i e -> EProj a i (substExp v replacement e)
    EPair a e1 e2 -> EPair a (substExp v replacement e1) (substExp v replacement e2)
    ERecord a fields ->
      ERecord a [(field, substExp v replacement fieldExp) | (field, fieldExp) <- fields]
    ERecordProj a e field -> ERecordProj a (substExp v replacement e) field
    EGenerate a sz f -> EGenerate a (substExp v replacement sz) (substExp v replacement f)
    EMap a f arr -> EMap a (substExp v replacement f) (substExp v replacement arr)
    EZipWith a f a1 a2 -> EZipWith a (substExp v replacement f) (substExp v replacement a1) (substExp v replacement a2)
    EReduce a f z arr -> EReduce a (substExp v replacement f) (substExp v replacement z) (substExp v replacement arr)
    EReduceGenerate a f z shape gen -> EReduceGenerate a (substExp v replacement f) (substExp v replacement z) (substExp v replacement shape) (substExp v replacement gen)
    EFoldl a f z arr -> EFoldl a (substExp v replacement f) (substExp v replacement z) (substExp v replacement arr)
    EScan a f z arr -> EScan a (substExp v replacement f) (substExp v replacement z) (substExp v replacement arr)
    ESegmentedReduce a f z offsets vals ->
      ESegmentedReduce a (substExp v replacement f) (substExp v replacement z)
        (substExp v replacement offsets) (substExp v replacement vals)
    ESortIndices a arr -> ESortIndices a (substExp v replacement arr)
    EIota a n -> EIota a (substExp v replacement n)
    EMakeIndex a n arr -> EMakeIndex a (substExp v replacement n) (substExp v replacement arr)
    ECOOSumDuplicates a nrows ncols nnz rows cols vals ->
      ECOOSumDuplicates a
        (substExp v replacement nrows)
        (substExp v replacement ncols)
        (substExp v replacement nnz)
        (substExp v replacement rows)
        (substExp v replacement cols)
        (substExp v replacement vals)
    ECSRFromSortedCOO a nrows ncols nnz rows cols vals ->
      ECSRFromSortedCOO a
        (substExp v replacement nrows)
        (substExp v replacement ncols)
        (substExp v replacement nnz)
        (substExp v replacement rows)
        (substExp v replacement cols)
        (substExp v replacement vals)
    EPermute a c d p arr -> EPermute a (substExp v replacement c) (substExp v replacement d) (substExp v replacement p) (substExp v replacement arr)
    EScatter a c d idx val -> EScatter a (substExp v replacement c) (substExp v replacement d) (substExp v replacement idx) (substExp v replacement val)
    EScatterGuarded a c d idx val g ->
      EScatterGuarded a (substExp v replacement c) (substExp v replacement d) (substExp v replacement idx) (substExp v replacement val) (substExp v replacement g)
    EScatterGenerate a c d idx f -> EScatterGenerate a (substExp v replacement c) (substExp v replacement d) (substExp v replacement idx) (substExp v replacement f)
    EGather a idx arr -> EGather a (substExp v replacement idx) (substExp v replacement arr)
    EIndex a idx arr -> EIndex a (substExp v replacement idx) (substExp v replacement arr)
    ECheckIndex a idx def arr -> ECheckIndex a (substExp v replacement idx) (substExp v replacement def) (substExp v replacement arr)
    EFill a s val -> EFill a (substExp v replacement s) (substExp v replacement val)
    EShapeOf a arr -> EShapeOf a (substExp v replacement arr)
    EReplicate a dims arr -> EReplicate a (map (substShapeDim v replacement) dims) (substExp v replacement arr)
    ESlice a dims arr -> ESlice a (map (substSliceDim v replacement) dims) (substExp v replacement arr)
    EReshape a s arr -> EReshape a (substExp v replacement s) (substExp v replacement arr)
    EReadArray a s f -> EReadArray a (substExp v replacement s) (substExp v replacement f)
    EReadArrayFloat a s f -> EReadArrayFloat a (substExp v replacement s) (substExp v replacement f)
    EWriteArray a arr f -> EWriteArray a (substExp v replacement arr) (substExp v replacement f)
    EWriteArrayFloat a arr f -> EWriteArrayFloat a (substExp v replacement arr) (substExp v replacement f)
    EGetEnvInt a e -> EGetEnvInt a (substExp v replacement e)
    EGetEnvString a e -> EGetEnvString a (substExp v replacement e)
    EStencil a bnd f arr ->
      EStencil a (substBnd v replacement bnd)
                 (substExp v replacement f)
                 (substExp v replacement arr)
    EBoundLetIn a x boundExp rhs body ->
      -- Don't substitute into body if x shadows v
      let rhs' = substExp v replacement rhs
          boundExp' = substExp v replacement boundExp
          body' = if x == v then body else substExp v replacement body
      in EBoundLetIn a x boundExp' rhs' body'

substBnd :: Var -> Exp a -> BoundaryCondition a -> BoundaryCondition a
substBnd _ _ BClamp     = BClamp
substBnd _ _ BWrap      = BWrap
substBnd _ _ BMirror    = BMirror
substBnd v r (BConst e) = BConst (substExp v r e)

substShapeDim :: Var -> Exp a -> ShapeDim a -> ShapeDim a
substShapeDim v replacement dim =
  case dim of
    ShapeAll a -> ShapeAll a
    ShapeAny a e -> ShapeAny a (substExp v replacement e)
    ShapeDim a e -> ShapeDim a (substExp v replacement e)

substSliceDim :: Var -> Exp a -> SliceDim a -> SliceDim a
substSliceDim v replacement dim =
  case dim of
    SliceAll a -> SliceAll a
    SliceRange a s l -> SliceRange a (substExp v replacement s) (substExp v replacement l)

-- | Apply a pattern-binding to an argument expression, substituting each
-- pattern variable for the corresponding projection of the argument.
-- Returns the body with the argument substituted, or Nothing if the pattern
-- is complex and cannot be safely reduced here.
substPat :: Pat a -> Exp a -> Exp a -> Maybe (Exp a)
substPat (PVar _ v) arg body = Just (substExp v arg body)
substPat (PVec pa ps) arg body =
  -- For a vector/tuple pattern [x, y, ...], substitute arg_i for each p_i
  -- using EProj to extract each component. Use the PVec's own annotation for
  -- synthesized EProj nodes (avoids needing to extract it from the argument).
  let go acc (p, i) = do
        acc' <- acc
        substPat p (EProj pa i arg) acc'
  in foldl go (Just body) (zip ps [0..])

-- | True when an expression represents a function value (has remaining
-- parameters in a local let-binding), so that inlining it at application
-- sites enables further beta reduction.
isFunctionExp :: Exp a -> Bool
isFunctionExp (ELetIn _ (Dec _ _ pats _ _) (EVar _ _)) = not (null pats)
isFunctionExp (ELetIn _ _ body) = isFunctionExp body
isFunctionExp _ = False

-- | Decompose a chain of applications @f a1 a2 ...@ into @(f, [a1, a2, ...])@.
peelApps :: Exp a -> (Exp a, [Exp a])
peelApps expr = go expr []
  where
    go (EApp _ fn arg) acc = go fn (arg : acc)
    go fn acc = (fn, acc)

isArrayExp :: Exp a -> Bool
isArrayExp expr =
  case expr of
    EGenerate {} -> True
    EMap {} -> True
    EZipWith {} -> True
    EReduce {} -> True
    EReduceGenerate {} -> True
    EFoldl {} -> False
    EScan {} -> True
    ESegmentedReduce {} -> False
    ESortIndices {} -> True
    EIota {} -> True
    EMakeIndex {} -> True
    ECOOSumDuplicates {} -> False
    ECSRFromSortedCOO {} -> False
    EPermute {} -> True
    EScatter {} -> True
    EScatterGuarded {} -> True
    EScatterGenerate {} -> True
    EGather {} -> True
    EFill {} -> True
    EShapeOf {} -> False
    EReplicate {} -> True
    ESlice {} -> True
    EReshape {} -> True
    ELetIn _ _ body -> isArrayExp body
    _ -> False

freshVar :: ByteString -> FusionM Var
freshVar prefix = do
  st <- get
  let n = freshCounter st
      candidate = prefix <> BS.pack (show n)
  if candidate `S.member` usedVars st
    then put st {freshCounter = n + 1} >> freshVar prefix
    else do
      put st {freshCounter = n + 1, usedVars = S.insert candidate (usedVars st)}
      pure candidate

mkLet1 :: a -> FusionM (Var, Var)
mkLet1 _ = do
  fn <- freshVar "__fusion_f"
  x <- freshVar "__fusion_x"
  pure (fn, x)

mkLet2 :: a -> FusionM (Var, Var, Var)
mkLet2 _ = do
  fn <- freshVar "__fusion_f"
  x <- freshVar "__fusion_x"
  y <- freshVar "__fusion_y"
  pure (fn, x, y)

mkDec1 :: a -> Var -> Var -> Exp a -> Dec a
mkDec1 a fn x body = Dec a fn [PVar a x] Nothing body

mkDec2 :: a -> Var -> Var -> Var -> Exp a -> Dec a
mkDec2 a fn x y body = Dec a fn [PVar a x, PVar a y] Nothing body

-- | Small internal lambda-like kernel language with de Bruijn variables.
--
-- KExpr is a tiny, internal expression language used to represent the
-- element computation of a symbolic array producer. It intentionally mirrors
-- a one-argument lambda using de Bruijn indices: @KVar 0@ is the logical
-- element index for a producer. KExpr is not exported outside the pass and
-- is kept minimal to make composition (compose/instantiate) straightforward.
--
-- KVar 0 denotes the current index for array producers.
data KExpr a
  = KVar Int
  | KConst (Exp a)
  | KApp (KExpr a) (KExpr a)
  | KIndex (KExpr a) (KExpr a)
  | KVec [KExpr a]
  | KProj Integer (KExpr a)
  | KBinOp (Operator a) (KExpr a) (KExpr a)

-- | Symbolic array producer: shape plus element kernel.
--
-- Producer encapsulates the information we need to fuse array pipelines:
-- the @producerShape@ field is an expression describing the output shape and
-- @producerElem@ is a @KExpr@ describing how to compute each element from
-- a logical index. Many fusion rewrites operate by converting array terms
-- into @Producer@ values, composing their kernels, and emitting a
-- simplified array form.
--
-- The kernel may mention de Bruijn variable 0 as the logical index.
data Producer a = Producer
  { producerShape :: Exp a
  , producerElem :: KExpr a
  }

-- | Abstract representation of a scatter-style write operation.
--
-- Mirrors the Rocq @scatter_kernel@ record in @theory/Fusion/Scatter.v@.
-- Both @EScatter@/@EScatterGuarded@ and @EPermute@ can be described by this
-- structure:
--
--   - @EScatter c d idxArr valsArr@:
--       @skCombine = c, skDefault = d, skIndex = idxArr,
--        skValues = valsArr, skGuard = Nothing@
--   - @EScatterGuarded c d idxArr valsArr guardArr@:
--       @skCombine = c, skDefault = d, skIndex = idxArr,
--        skValues = valsArr, skGuard = Just guardArr@
--   - @EPermute c d permFn srcArr@:
--       @skCombine = c, skDefault = d,
--        skIndex = EGenerate (EShapeOf a srcArr) permFn, skValues = srcArr,
--        skGuard = Nothing@
--     (permFn maps source index → destination index, so we represent the
--     index side as a generate rather than an explicit function.)
--
-- The shared fields make it possible to apply common fusion rules
-- (map absorption on values, let-hoisting) to both scatter forms without
-- duplicating code.
data ScatterKernel a = ScatterKernel
  { skCombine :: Exp a  -- ^ Associative combine: new_val -> existing_val -> result
  , skDefault :: Exp a  -- ^ Default output array (initial values at every position)
  , skIndex   :: Exp a  -- ^ Array of destination indices (skIndex[i] = dest for source pos i)
  , skValues  :: Exp a  -- ^ Array of values to scatter (skValues[i] = value at source pos i)
  , skGuard   :: Maybe (Exp a)  -- ^ Optional boolean guard for each source position
  }

kUsesVar :: Int -> KExpr a -> Bool
kUsesVar target expr =
  case expr of
    KVar i -> i == target
    KConst _ -> False
    KApp f x -> kUsesVar target f || kUsesVar target x
    KIndex i arr -> kUsesVar target i || kUsesVar target arr
    KVec es -> any (kUsesVar target) es
    KProj _ e -> kUsesVar target e
    KBinOp _ l r -> kUsesVar target l || kUsesVar target r

kApply :: a -> [Exp a] -> KExpr a -> Exp a
kApply a args expr =
  case expr of
    KVar i -> args !! i
    KConst e -> e
    KApp f x -> EApp a (kApply a args f) (kApply a args x)
    KIndex i arr -> EIndex a (kApply a args i) (kApply a args arr)
    KVec es -> EVec a (map (kApply a args) es)
    KProj i e -> EProj a i (kApply a args e)
    KBinOp op l r -> EBinOp a (kApply a args l) op (kApply a args r)

kComposeUnary :: KExpr a -> KExpr a -> KExpr a
kComposeUnary outer inner = subst outer
  where
    subst e =
      case e of
        KVar 0 -> inner
        KVar i -> KVar i
        KConst c -> KConst c
        KApp f x -> KApp (subst f) (subst x)
        KIndex i arr -> KIndex (subst i) (subst arr)
        KVec es -> KVec (map subst es)
        KProj i x -> KProj i (subst x)
        KBinOp op l r -> KBinOp op (subst l) (subst r)

kAsUnaryFunction :: KExpr a -> Maybe (Exp a)
kAsUnaryFunction expr =
  case expr of
    KApp (KConst fn) (KVar 0) -> Just fn
    _ -> Nothing

-- | Try to interpret an expression as a symbolic producer.
--
-- This performs the abstract-interpretation step for fusible producer forms,
-- composing kernels for nested @map@/@zipwith@ pipelines and encoding
-- index transforms for operations such as @gather@, @replicate@, and @slice@.
asProducer :: (Eq a) => Exp a -> Maybe (Producer a)
asProducer expr =
  case expr of
    EGenerate _ shape genFn ->
      Just (Producer shape (KApp (KConst genFn) (KVar 0)))
    EFill _ shape val ->
      Just (Producer shape (KConst val))
    EMap _ f arr -> do
      p <- asProducer arr
      pure p {producerElem = KApp (KConst f) (producerElem p)}
    EZipWith _ f a1 a2 -> do
      p1 <- asProducer a1
      p2 <- asProducer a2
      if producerShape p1 == producerShape p2
        then pure $ Producer (producerShape p1) (KApp (KApp (KConst f) (producerElem p1)) (producerElem p2))
        else Nothing
    EGather a idx arr -> do
      pArr <- asProducer arr
      let pIdx = asProducer idx
          idxAtOut =
            case pIdx of
              Just q -> producerElem q
              Nothing -> KIndex (KVar 0) (KConst idx)
          shape =
            case pIdx of
              Just q -> producerShape q
              Nothing -> EShapeOf a idx
      pure $ Producer shape (kComposeUnary (producerElem pArr) idxAtOut)
    EReplicate a dims arr -> do
      pArr <- asProducer arr
      let srcPositions = [j | (j, d) <- zip [0 :: Int ..] dims, isShapeAll d]
          srcIdx = KVec [KProj (fromIntegral j) (KVar 0) | j <- srcPositions]
          shape = EShapeOf a (EReplicate a dims arr)
      pure $ Producer shape (kComposeUnary (producerElem pArr) srcIdx)
    ESlice a dims arr -> do
      pArr <- asProducer arr
      let comps = zipWith (sliceComponent a) [0 ..] dims
          srcIdx = KVec comps
          -- Compute slice shape from source shape
          rawShape = sliceShape a (producerShape pArr) dims
          -- Normalize to enable fusion with other slices of same dimensions
          shape = normalizeShapesExp rawShape
      pure $ Producer shape (kComposeUnary (producerElem pArr) srcIdx)
    -- iota n: generates [0..n-1]; element at index i is i itself (KVar 0)
    EIota a nExp ->
      Just (Producer (EVec a [nExp]) (KVar 0))
    -- make_index is a type annotation; transparent for fusion
    EMakeIndex _ _ arr -> asProducer arr
    _ -> Nothing

asScatterProducer :: (Eq a) => Exp a -> Maybe (Producer a)
asScatterProducer expr =
  case expr of
    EVar a _ ->
      Just (Producer (EShapeOf a expr) (KIndex (KVar 0) (KConst expr)))
    EMap a f arr -> do
      p <- asScatterProducer arr
      pure p {producerShape = EShapeOf a expr, producerElem = KApp (KConst f) (producerElem p)}
    EZipWith a f a1 a2 -> do
      p1 <- asScatterProducer a1
      p2 <- asScatterProducer a2
      pure $ Producer (EShapeOf a expr) (KApp (KApp (KConst f) (producerElem p1)) (producerElem p2))
    EGather a idx arr -> do
      pArr <- asScatterProducer arr
      let pIdx = asScatterProducer idx
          idxAtOut =
            case pIdx of
              Just q -> producerElem q
              Nothing -> KIndex (KVar 0) (KConst idx)
      pure $ Producer (EShapeOf a expr) (kComposeUnary (producerElem pArr) idxAtOut)
    EReplicate a dims arr -> do
      pArr <- asScatterProducer arr
      let srcPositions = [j | (j, d) <- zip [0 :: Int ..] dims, isShapeAll d]
          srcIdx = KVec [KProj (fromIntegral j) (KVar 0) | j <- srcPositions]
          shape = EShapeOf a expr
      pure $ Producer shape (kComposeUnary (producerElem pArr) srcIdx)
    ESlice a dims arr -> do
      pArr <- asScatterProducer arr
      let comps = zipWith (sliceComponent a) [0 ..] dims
          srcIdx = KVec comps
          shape = EShapeOf a expr
      pure $ Producer shape (kComposeUnary (producerElem pArr) srcIdx)
    _ -> asProducer expr

scatterSourceAt :: a -> Var -> Exp a -> Maybe (Exp a, [Exp a])
scatterSourceAt a i expr =
  case expr of
    EVar {} ->
      Just (EIndex a (EVar a i) expr, [expr])
    EMap _ f arr ->
      Just (EApp a f (EIndex a (EVar a i) arr), [arr])
    EZipWith _ f x y ->
      Just
        ( EApp a
            (EApp a f (EIndex a (EVar a i) x))
            (EIndex a (EVar a i) y)
        , [x, y]
        )
    _ -> Nothing

sameExpShape :: Exp a -> Exp a -> Bool
sameExpShape x y = fmap (const ()) x == fmap (const ()) y

mergeScatterSources :: [Exp a] -> [Exp a] -> [Exp a]
mergeScatterSources xs ys = foldl step xs ys
  where
    step acc e
      | any (`sameExpShape` e) acc = acc
      | otherwise = acc ++ [e]

isShapeAll :: ShapeDim a -> Bool
isShapeAll dim =
  case dim of
    ShapeAll _ -> True
    _ -> False

sliceComponent :: a -> Int -> SliceDim a -> KExpr a
sliceComponent a outPos dim =
  case dim of
    SliceAll _ -> KProj (fromIntegral outPos) (KVar 0)
    SliceRange _ start _ ->
      KBinOp (Plus a) (KConst start) (KProj (fromIntegral outPos) (KVar 0))

-- | Compute the shape expression for a sliced array.
-- For each dimension, either keep the original size (SliceAll) or use the slice length.
sliceShape :: a -> Exp a -> [SliceDim a] -> Exp a
sliceShape a srcShape dims =
  EVec a (zipWith (sliceDimShape a srcShape) [0..] dims)

sliceDimShape :: a -> Exp a -> Int -> SliceDim a -> Exp a
sliceDimShape a srcShape idx dim =
  case dim of
    SliceAll _ -> EProj a (fromIntegral idx) srcShape
    SliceRange _ _ lenExpr -> lenExpr

-- | Returns True for producer expressions that are not yet in atomic emitted
-- form (EGenerate / EFill). Used as a convergence guard when trying to simplify
-- the index side of EScatter: we only attempt simplification when the index
-- expression is composite (EMap, EZipWith) so that emitting the producer as a
-- generate actually produces a different, simpler term.
isCompositeProducerExp :: Exp a -> Bool
isCompositeProducerExp expr =
  case expr of
    EMap {} -> True
    EZipWith {} -> True
    _ -> False

isDerivedProducerExp :: Exp a -> Bool
isDerivedProducerExp expr =
  case expr of
    EMap {} -> True
    EZipWith {} -> True
    EGather {} -> True
    EReplicate {} -> True
    ESlice {} -> True
    _ -> False

emitKernel1 :: a -> KExpr a -> FusionM (Dec a, Exp a)
emitKernel1 a kernel = do
  (fn, i) <- mkLet1 a
  let body = kApply a [EVar a i] kernel
  pure (mkDec1 a fn i body, EVar a fn)

-- | Emit a symbolic producer back to concrete AST.
--
-- Constant kernels become @fill@; index-dependent kernels become @generate@
-- (possibly with a synthesized helper function).
emitProducer :: a -> Producer a -> FusionM (Exp a)
emitProducer a p =
  if not (kUsesVar 0 (producerElem p))
    then pure (EFill a (producerShape p) (kApply a [] (producerElem p)))
    else
      case kAsUnaryFunction (producerElem p) of
        Just fn -> pure (EGenerate a (producerShape p) fn)
        Nothing -> do
          (dec, fnExp) <- emitKernel1 a (producerElem p)
          pure (ELetIn a dec (EGenerate a (producerShape p) fnExp))

-- | Recognise an expression as a scatter kernel (Rocq: @as_scatter@).
--
-- Handles both @EScatter@ and @EPermute@. An @EPermute c d permFn srcArr@
-- is equivalent to @EScatter c d (generate (shape_of srcArr) permFn) srcArr@,
-- so we represent its index side as a @generate@ node.
asScatter :: a -> Exp a -> Maybe (ScatterKernel a)
asScatter a expr =
  case expr of
    EScatter _ c d idx vals ->
      Just ScatterKernel { skCombine = c, skDefault = d, skIndex = idx, skValues = vals, skGuard = Nothing }
    EScatterGuarded _ c d idx vals guardArr ->
      Just ScatterKernel { skCombine = c, skDefault = d, skIndex = idx, skValues = vals, skGuard = Just guardArr }
    EPermute _ c d permFn arr ->
      -- permute c d permFn arr  ≡  scatter c d (generate (shape arr) permFn) arr
      Just ScatterKernel { skCombine = c, skDefault = d
                         , skIndex = EGenerate a (EShapeOf a arr) permFn
                         , skValues = arr
                         , skGuard = Nothing }
    _ -> Nothing

-- | Emit a scatter kernel back to concrete AST (Rocq: @emit_scatter@).
-- Produces either @EScatter@ or @EScatterGuarded@ depending on whether the
-- kernel carries a guard; if the input came from @EPermute@ the equivalence
-- @scatter c d (generate (shape arr) permFn) arr@ is preserved.
emitScatter :: a -> ScatterKernel a -> Exp a
emitScatter a sk =
  case skGuard sk of
    Nothing -> EScatter a (skCombine sk) (skDefault sk) (skIndex sk) (skValues sk)
    Just guardArr -> EScatterGuarded a (skCombine sk) (skDefault sk) (skIndex sk) (skValues sk) guardArr

-- | Absorb an inner @EMap g@ on the values side into the combine function
-- (Rocq: @FuseScatterMap@, @scatter_combine_map_fun@).
--
-- @scatter c d idx (map g xs)  =>  let c' x y = c (g x) y in scatter c' d idx xs@
--
-- Returns @Nothing@ if @vals@ is not an @EMap@.  On success, returns the
-- fresh let-binding @Dec@ and the updated @ScatterKernel@ (caller wraps
-- the emitted scatter expression in @ELetIn a dec ...@).
scatterAbsorbMapValues :: a -> ScatterKernel a -> Maybe (FusionM (Dec a, ScatterKernel a))
scatterAbsorbMapValues a sk =
  case skValues sk of
    EMap _ g xs -> Just $ do
      (fn, x, y) <- mkLet2 a
      let body = EApp a (EApp a (skCombine sk) (EApp a g (EVar a x))) (EVar a y)
          dec = mkDec2 a fn x y body
      pure (dec, sk { skCombine = EVar a fn, skValues = xs })
    _ -> Nothing

isFusionInternalVar :: Var -> Bool
isFusionInternalVar = BS.isPrefixOf "__fusion_"

data NormFusionState = NormFusionState
  { normFusionCounter :: Int
  , normFusionEnv :: Map Var Var
  }

-- | Normalize compiler-generated fusion binders and compare for convergence.
--
-- Two iterations are considered equivalent if they differ only in fresh
-- @__fusion_*@ names.
normalizeFusionInternalVars :: Exp a -> Exp a
normalizeFusionInternalVars expr = evalState (normExp expr) (NormFusionState 0 M.empty)

normUseVar :: Var -> State NormFusionState Var
normUseVar v = do
  env <- gets normFusionEnv
  pure (M.findWithDefault v v env)

normBindVar :: Var -> State NormFusionState Var
normBindVar v
  | isFusionInternalVar v = do
      st <- get
      let n = normFusionCounter st
          v' = "__fusion_norm" <> BS.pack (show n)
      put st {normFusionCounter = n + 1, normFusionEnv = M.insert v v' (normFusionEnv st)}
      pure v'
  | otherwise = do
      modify' (\s -> s {normFusionEnv = M.insert v v (normFusionEnv s)})
      pure v

withNormEnv :: Map Var Var -> State NormFusionState b -> State NormFusionState b
withNormEnv env action = do
  st <- get
  put st {normFusionEnv = env}
  out <- action
  modify' (\s -> s {normFusionEnv = normFusionEnv st})
  pure out

normPat :: Pat a -> State NormFusionState (Pat a)
normPat pat =
  case pat of
    PVar a v -> PVar a <$> normBindVar v
    PBound a v e -> PBound a <$> normBindVar v <*> normExp e
    PVec a ps -> PVec a <$> mapM normPat ps

normShapeDim :: ShapeDim a -> State NormFusionState (ShapeDim a)
normShapeDim dim =
  case dim of
    ShapeAll a -> pure (ShapeAll a)
    ShapeAny a e -> ShapeAny a <$> normExp e
    ShapeDim a e -> ShapeDim a <$> normExp e

normSliceDim :: SliceDim a -> State NormFusionState (SliceDim a)
normSliceDim dim =
  case dim of
    SliceAll a -> pure (SliceAll a)
    SliceRange a s l -> SliceRange a <$> normExp s <*> normExp l

normDec :: Dec a -> State NormFusionState (Dec a, Map Var Var)
normDec (Dec a name pats poly body) = do
  env0 <- gets normFusionEnv
  name' <- normBindVar name
  pats' <- mapM normPat pats
  env1 <- gets normFusionEnv
  body' <- normExp body
  modify' (\s -> s {normFusionEnv = env0})
  pure (Dec a name' pats' poly body', env1)

normExp :: Exp a -> State NormFusionState (Exp a)
normExp expr =
  case expr of
    EInt {} -> pure expr
    EFloat {} -> pure expr
    EVar a v -> EVar a <$> normUseVar v
    EString {} -> pure expr
    EUnit {} -> pure expr
    EBool {} -> pure expr
    EVec a es -> EVec a <$> mapM normExp es
    EApp a f x -> EApp a <$> normExp f <*> normExp x
    EIfThen a c t -> EIfThen a <$> normExp c <*> normExp t
    EIfThenElse a c t f -> EIfThenElse a <$> normExp c <*> normExp t <*> normExp f
    ENeg a e -> ENeg a <$> normExp e
    EBinOp a l op r -> EBinOp a <$> normExp l <*> pure op <*> normExp r
    EUnOp a op e -> EUnOp a op <$> normExp e
    EOp {} -> pure expr
    ELetIn a d e -> do
      (d', env') <- normDec d
      e' <- withNormEnv env' (normExp e)
      pure (ELetIn a d' e')
    EProj a i e -> EProj a i <$> normExp e
    EPair a e1 e2 -> EPair a <$> normExp e1 <*> normExp e2
    ERecord a fields ->
      ERecord a <$> mapM (\(field, fieldExp) -> do
        fieldExp' <- normExp fieldExp
        pure (field, fieldExp')) fields
    ERecordProj a e field -> ERecordProj a <$> normExp e <*> pure field
    EGenerate a s f -> EGenerate a <$> normExp s <*> normExp f
    EMap a f arr -> EMap a <$> normExp f <*> normExp arr
    EZipWith a f x y -> EZipWith a <$> normExp f <*> normExp x <*> normExp y
    EReduce a f z arr -> EReduce a <$> normExp f <*> normExp z <*> normExp arr
    EReduceGenerate a f z s g -> EReduceGenerate a <$> normExp f <*> normExp z <*> normExp s <*> normExp g
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
    EFill a s v -> EFill a <$> normExp s <*> normExp v
    EShapeOf a arr -> EShapeOf a <$> normExp arr
    EReplicate a dims arr -> EReplicate a <$> mapM normShapeDim dims <*> normExp arr
    ESlice a dims arr -> ESlice a <$> mapM normSliceDim dims <*> normExp arr
    EReshape a s arr -> EReshape a <$> normExp s <*> normExp arr
    EReadArray a s f -> EReadArray a <$> normExp s <*> normExp f
    EReadArrayFloat a s f -> EReadArrayFloat a <$> normExp s <*> normExp f
    EWriteArray a arr f -> EWriteArray a <$> normExp arr <*> normExp f
    EWriteArrayFloat a arr f -> EWriteArrayFloat a <$> normExp arr <*> normExp f
    EGetEnvInt a e -> EGetEnvInt a <$> normExp e
    EGetEnvString a e -> EGetEnvString a <$> normExp e
    EStencil a bnd f arr -> EStencil a <$> normBndFusion bnd <*> normExp f <*> normExp arr
    EBoundLetIn a x boundExp rhs body -> do
      env <- gets normFusionEnv
      x' <- normBindVar x
      boundExp' <- normExp boundExp
      rhs' <- normExp rhs
      body' <- withNormEnv (M.insert x x' env) (normExp body)
      pure (EBoundLetIn a x' boundExp' rhs' body')

normBndFusion :: BoundaryCondition a -> State NormFusionState (BoundaryCondition a)
normBndFusion BClamp     = pure BClamp
normBndFusion BWrap      = pure BWrap
normBndFusion BMirror    = pure BMirror
normBndFusion (BConst e) = BConst <$> normExp e

-- | Run the fusion pass on a whole expression.
--
-- This applies the fusion driver (@fuseFix@) with a fresh @FusionState@
-- initialized from the expression's free variables. The result is a
-- semantics-preserving, potentially-fused expression suitable for
-- downstream lowering and code generation.
fuseExp :: (Eq a) => Exp a -> Exp a
fuseExp expr =
  let initState = FusionState {freshCounter = 0, usedVars = collectVarsExp expr}
   in evalState (runFusionM (fuseFix expr)) initState

-- | Run fusion over a single top-level declaration.
--
-- Initializes fusion state from the declaration's bound variables and
-- applies the fixed-point driver to the declaration body.
fuseDec :: (Eq a) => Dec a -> Dec a
fuseDec dec =
  let initState = FusionState {freshCounter = 0, usedVars = collectVarsDec dec}
   in evalState (runFusionM (fuseDecM dec)) initState

-- | Run fusion for a list of top-level declarations.
--
-- Useful for processing a whole program's top-level definitions in one
-- pass; a single @FusionState@ is created covering all declarations so
-- generated helper names remain unique across them.
fuseDecs :: (Eq a) => [Dec a] -> [Dec a]
fuseDecs decs =
  -- First pass: fuse each declaration independently.
  let initState0 = FusionState {freshCounter = 0, usedVars = S.unions (map collectVarsDec decs)}
      fused0 = evalState (runFusionM (mapM fuseDecM decs)) initState0
      -- Second pass: inline single-use function-valued declarations that are
      -- now visible as function expressions after the first fusion round.
      inlined = inlineFnValueDecs fused0
      -- Third pass: fuse again to beta-reduce the newly inlined applications.
      initState1 = FusionState {freshCounter = 0, usedVars = S.unions (map collectVarsDec inlined)}
   in evalState (runFusionM (mapM fuseDecM inlined)) initState1

-- | Inline function-valued top-level declarations into their single use sites.
--
-- A top-level declaration @let f = (let g x = body in g)@ has a function as
-- its value (no array produced, just a partially-applied function). When @f@
-- appears exactly once in a subsequent declaration, we substitute the body
-- directly so that beta reduction can proceed during fusion. This eliminates
-- the "cross-declaration closure" pattern that the per-declaration fusion pass
-- cannot see through.
inlineFnValueDecs :: (Eq a) => [Dec a] -> [Dec a]
inlineFnValueDecs = go []
  where
    go env [] = []
    go env (Dec a name [] poly body : rest) =
      let body' = applyEnv env body
          totalUsage = sum (map (\(Dec _ _ _ _ b) -> countVarExp name (applyEnv env b)) rest)
          eligible = isFunctionExp body' && totalUsage == 1
      in if eligible
           then go ((name, body') : env) rest
           else Dec a name [] poly body' : go env rest
    go env (Dec a name pats poly body : rest) =
      let body' = applyEnv env body
      in Dec a name pats poly body' : go env rest

    applyEnv env e = foldl (\acc (v, r) -> substExp v r acc) e env

fuseFix :: (Eq a) => Exp a -> FusionM (Exp a)
fuseFix = fuseFixWithLimit 200

-- | Bounded fixed-point driver for fusion rewrites.
--
-- The bound protects against accidental non-progress cycles. Normal execution
-- converges much earlier; alpha-normalized equality avoids spinning on fresh
-- binder renaming.
fuseFixWithLimit :: (Eq a) => Int -> Exp a -> FusionM (Exp a)
fuseFixWithLimit iters0 e0 = go iters0 e0
  where
    go 0 e = pure e
    go n e = do
      e' <- fuseOnce e
      let stable = e' == e || normalizeFusionInternalVars e' == normalizeFusionInternalVars e
      if stable
        then pure e'
        else go (n - 1) e'

fuseOnce :: (Eq a) => Exp a -> FusionM (Exp a)
fuseOnce expr =
  case expr of
    EInt {} -> pure expr
    EFloat {} -> pure expr
    EVar {} -> pure expr
    EString {} -> pure expr
    EUnit {} -> pure expr
    EBool {} -> pure expr
    EVec a es -> EVec a <$> mapM fuseOnce es
    EApp a f x -> do
      f' <- fuseOnce f
      x' <- fuseOnce x
      -- Beta reduction: (let fn p pats... = fbody in fn) arg
      -- → substitute arg for p in fbody, keeping remaining pats.
      case f' of
        ELetIn _ (Dec db fname (p:pats) poly fbody) (EVar _ fname')
          | fname == fname' ->
              case substPat p x' fbody of
                Just fbody' ->
                  let reduced = if null pats
                        then fbody'
                        else ELetIn db (Dec db fname pats poly fbody') (EVar db fname')
                  in fuseOnce reduced
                Nothing -> pure (EApp a f' x')
        _ -> pure (EApp a f' x')
    EIfThen a c t -> EIfThen a <$> fuseOnce c <*> fuseOnce t
    EIfThenElse a c t f -> EIfThenElse a <$> fuseOnce c <*> fuseOnce t <*> fuseOnce f
    ENeg a e -> ENeg a <$> fuseOnce e
    EBinOp a l op r -> EBinOp a <$> fuseOnce l <*> pure op <*> fuseOnce r
    EUnOp a op e -> EUnOp a op <$> fuseOnce e
    EOp {} -> pure expr
    ELetIn a d e -> do
      d'@(Dec da name pats _ body) <- fuseDecM d
      e' <- fuseOnce e
      -- Beta reduction: (let f p1...pn = body in f a1...am) where 1 ≤ m.
      -- Consume as many leading arguments as we have patterns, substituting each.
      let (retFn, retArgs) = peelApps e'
          maybeBeta = case (pats, retFn) of
            (_:_, EVar _ fname') | fname' == name && not (null retArgs) ->
              let n = min (length pats) (length retArgs)
                  (consumedPats, remainingPats) = splitAt n pats
                  (consumedArgs, remainingArgs) = splitAt n retArgs
                  applyPats acc (p, arg) = acc >>= substPat p arg
                  mbody' = foldl applyPats (Just body) (zip consumedPats consumedArgs)
              in case mbody' of
                   Just body' ->
                     let inner = if null remainingPats
                                   then body'
                                   else ELetIn da (Dec da name remainingPats Nothing body') (EVar da name)
                     in Just (foldl (EApp a) inner remainingArgs)
                   Nothing -> Nothing
            _ -> Nothing
      case maybeBeta of
        Just reduced -> fuseOnce reduced
        Nothing ->
          let inlineable = null pats
              usage = countVarExp name e'
              captureRisk = not $ S.null (freeVarsExp body `S.intersection` boundVarsExp e')
              -- Inline array expressions (fusion), or function-valued expressions
              -- that appear exactly once (enables cross-binding beta reduction).
              shouldInline = inlineable && usage == 1 && not captureRisk
                          && (isArrayExp body || isFunctionExp body)
          in if shouldInline
               then fuseOnce (substExp name body e')
               else pure (ELetIn a d' e')
    EProj a i e -> EProj a i <$> fuseOnce e
    EPair a e1 e2 -> EPair a <$> fuseOnce e1 <*> fuseOnce e2
    ERecord a fields ->
      ERecord a <$> mapM (\(field, fieldExp) -> do
        fieldExp' <- fuseOnce fieldExp
        pure (field, fieldExp')) fields
    ERecordProj a e field -> ERecordProj a <$> fuseOnce e <*> pure field
    EGenerate a sz f -> EGenerate a <$> fuseOnce sz <*> fuseOnce f
    EMap a f arr -> do
      f' <- fuseOnce f
      arr' <- fuseOnce arr
      fuseMap a f' arr'
    EZipWith a f a1 a2 -> do
      f' <- fuseOnce f
      a1' <- fuseOnce a1
      a2' <- fuseOnce a2
      fuseZipWith a f' a1' a2'
    EReduce a f z arr -> do
      f' <- fuseOnce f
      z' <- fuseOnce z
      arr' <- fuseOnce arr
      fuseReduce a f' z' arr'
    EReduceGenerate a f z shape gen ->
      EReduceGenerate a <$> fuseOnce f <*> fuseOnce z <*> fuseOnce shape <*> fuseOnce gen
    EFoldl a f z arr -> EFoldl a <$> fuseOnce f <*> fuseOnce z <*> fuseOnce arr
    EScan a f z arr -> EScan a <$> fuseOnce f <*> fuseOnce z <*> fuseOnce arr
    ESegmentedReduce a f z offsets vals ->
      ESegmentedReduce a <$> fuseOnce f <*> fuseOnce z <*> fuseOnce offsets <*> fuseOnce vals
    ESortIndices a arr -> ESortIndices a <$> fuseOnce arr
    EIota a n -> EIota a <$> fuseOnce n
    EMakeIndex a n arr -> EMakeIndex a <$> fuseOnce n <*> fuseOnce arr
    ECOOSumDuplicates a nrows ncols nnz rows cols vals ->
      ECOOSumDuplicates a <$> fuseOnce nrows <*> fuseOnce ncols <*> fuseOnce nnz
        <*> fuseOnce rows <*> fuseOnce cols <*> fuseOnce vals
    ECSRFromSortedCOO a nrows ncols nnz rows cols vals ->
      ECSRFromSortedCOO a <$> fuseOnce nrows <*> fuseOnce ncols <*> fuseOnce nnz
        <*> fuseOnce rows <*> fuseOnce cols <*> fuseOnce vals
    EPermute a c d p arr -> do
      c' <- fuseOnce c
      d' <- fuseOnce d
      p' <- fuseOnce p
      arr' <- fuseOnce arr
      fusePermute a c' d' p' arr'
    EScatter a c d idx vals -> do
      c' <- fuseOnce c
      d' <- fuseOnce d
      idx' <- fuseOnce idx
      vals' <- fuseOnce vals
      fuseScatter a c' d' idx' vals'
    EScatterGuarded a c d idx vals guardArr -> do
      c' <- fuseOnce c
      d' <- fuseOnce d
      idx' <- fuseOnce idx
      vals' <- fuseOnce vals
      guardArr' <- fuseOnce guardArr
      let sk0 = ScatterKernel
            { skCombine = c'
            , skDefault = d'
            , skIndex = idx'
            , skValues = vals'
            , skGuard = Just guardArr'
            }
      fuseScatterKernel a sk0
    EScatterGenerate a c d idx valFn -> do
      c' <- fuseOnce c
      d' <- fuseOnce d
      idx' <- fuseOnce idx
      valFn' <- fuseOnce valFn
      -- Index-side simplification: if the index array is a composite producer,
      -- simplify it.  Everything else is already fused.
      case asProducer idx' of
        Just pIdx | isCompositeProducerExp idx' -> do
          idx'' <- emitProducer a pIdx
          pure (EScatterGenerate a c' d' idx'' valFn')
        _ -> pure (EScatterGenerate a c' d' idx' valFn')
    EGather a idx arr -> do
      idx' <- fuseOnce idx
      arr' <- fuseOnce arr
      fuseGather a idx' arr'
    EIndex a idx arr -> do
      idx' <- fuseOnce idx
      arr' <- fuseOnce arr
      fuseIndex a idx' arr'
    ECheckIndex a idx def arr -> ECheckIndex a <$> fuseOnce idx <*> fuseOnce def <*> fuseOnce arr
    EFill a s v -> EFill a <$> fuseOnce s <*> fuseOnce v
    EShapeOf a arr -> EShapeOf a <$> fuseOnce arr
    EReplicate a dims arr -> do
      dims' <- mapM fuseShapeDim dims
      arr' <- fuseOnce arr
      fuseReplicate a dims' arr'
    ESlice a dims arr -> do
      dims' <- mapM fuseSliceDim dims
      arr' <- fuseOnce arr
      fuseSlice a dims' arr'
    EReadArray a s f -> EReadArray a <$> fuseOnce s <*> fuseOnce f
    EReadArrayFloat a s f -> EReadArrayFloat a <$> fuseOnce s <*> fuseOnce f
    EWriteArray a arr f -> EWriteArray a <$> fuseOnce arr <*> fuseOnce f
    EWriteArrayFloat a arr f -> EWriteArrayFloat a <$> fuseOnce arr <*> fuseOnce f
    EGetEnvInt a e -> EGetEnvInt a <$> fuseOnce e
    EGetEnvString a e -> EGetEnvString a <$> fuseOnce e
    EStencil a bnd f arr -> do
      bnd' <- fuseBnd bnd
      f'   <- fuseOnce f
      arr' <- fuseOnce arr
      pure (EStencil a bnd' f' arr')
    EReshape a s arr -> do
      s' <- fuseOnce s
      arr' <- fuseOnce arr
      fuseReshape a s' arr'
    EBoundLetIn a x boundExp rhs body ->
      EBoundLetIn a x <$> fuseOnce boundExp <*> fuseOnce rhs <*> fuseOnce body

fuseDecM :: (Eq a) => Dec a -> FusionM (Dec a)
fuseDecM (Dec a name pats poly body) = do
  body' <- fuseFix body
  pure (Dec a name pats poly body')

fuseBnd :: (Eq a) => BoundaryCondition a -> FusionM (BoundaryCondition a)
fuseBnd BClamp     = pure BClamp
fuseBnd BWrap      = pure BWrap
fuseBnd BMirror    = pure BMirror
fuseBnd (BConst e) = BConst <$> fuseOnce e

fuseShapeDim :: (Eq a) => ShapeDim a -> FusionM (ShapeDim a)
fuseShapeDim dim =
  case dim of
    ShapeAll a -> pure (ShapeAll a)
    ShapeAny a e -> ShapeAny a <$> fuseOnce e
    ShapeDim a e -> ShapeDim a <$> fuseOnce e

fuseSliceDim :: (Eq a) => SliceDim a -> FusionM (SliceDim a)
fuseSliceDim dim =
  case dim of
    SliceAll a -> pure (SliceAll a)
    SliceRange a s l -> SliceRange a <$> fuseOnce s <*> fuseOnce l

fuseMap :: (Eq a) => a -> Exp a -> Exp a -> FusionM (Exp a)
fuseMap a f arr =
  case arr of
    -- map f (let x = e in body) => let x = e in map f body
    ELetIn _ dec body | isArrayExp body ->
      let bound = decBoundVars dec
       in if S.null (bound `S.intersection` freeVarsExp f)
            then pure (ELetIn a dec (EMap a f body))
            else pure (EMap a f arr)
    _ ->
      case asProducer arr of
        Just p -> emitProducer a p {producerElem = KApp (KConst f) (producerElem p)}
        Nothing ->
          case arr of
            -- map f (zipwith g xs ys) => zipwith (\x y -> f (g x y)) xs ys
            EZipWith _ g xs ys -> do
              (fn, x, y) <- mkLet2 a
              let body = EApp a f (EApp a (EApp a g (EVar a x)) (EVar a y))
                  dec = mkDec2 a fn x y body
              pure $ ELetIn a dec (EZipWith a (EVar a fn) xs ys)
            _ -> pure (EMap a f arr)

fuseZipWith :: (Eq a) => a -> Exp a -> Exp a -> Exp a -> FusionM (Exp a)
fuseZipWith a f a1 a2 =
  case (a1, a2) of
    -- zipwith f (let x = e in body) ys => let x = e in zipwith f body ys
    (ELetIn _ dec body, ys) | isArrayExp body ->
      let bound = decBoundVars dec
          fv = freeVarsExp f `S.union` freeVarsExp ys
       in if S.null (bound `S.intersection` fv)
            then pure (ELetIn a dec (EZipWith a f body ys))
            else pure (EZipWith a f a1 a2)
    -- zipwith f xs (let x = e in body) => let x = e in zipwith f xs body
    (xs, ELetIn _ dec body) | isArrayExp body ->
      let bound = decBoundVars dec
          fv = freeVarsExp f `S.union` freeVarsExp xs
       in if S.null (bound `S.intersection` fv)
            then pure (ELetIn a dec (EZipWith a f xs body))
            else pure (EZipWith a f a1 a2)
    _ ->
      case (asProducer a1, asProducer a2) of
        (Just p1, Just p2) -> do
          let s1 = producerShape p1
              s2 = producerShape p2
          -- Compare shapes modulo annotations so equal shapes still fuse when
          -- they carry different source ranges.
          if fmap (const ()) s1 == fmap (const ()) s2
            then emitProducer a $ Producer s1 (KApp (KApp (KConst f) (producerElem p1)) (producerElem p2))
            else do
              -- Fall back to local rewrites when producer fusion does not apply.
              case (a1, a2) of
                -- zipwith f (map g xs) ys => zipwith (\x y -> f (g x) y) xs ys
                (EMap _ g xs, ys) -> do
                  (fn, x, y) <- mkLet2 a
                  let body = EApp a (EApp a f (EApp a g (EVar a x))) (EVar a y)
                      dec = mkDec2 a fn x y body
                  pure $ ELetIn a dec (EZipWith a (EVar a fn) xs ys)
                -- zipwith f xs (map g ys) => zipwith (\x y -> f x (g y)) xs ys
                (xs, EMap _ g ys) -> do
                  (fn, x, y) <- mkLet2 a
                  let body = EApp a (EApp a f (EVar a x)) (EApp a g (EVar a y))
                      dec = mkDec2 a fn x y body
                  pure $ ELetIn a dec (EZipWith a (EVar a fn) xs ys)
                -- zipwith f (fill s x) ys => map (\y -> f x y) ys
                (EFill _ _ x, ys) -> do
                  (fn, y) <- mkLet1 a
                  let body = EApp a (EApp a f x) (EVar a y)
                      dec = mkDec1 a fn y body
                  pure $ ELetIn a dec (EMap a (EVar a fn) ys)
                -- zipwith f xs (fill s y) => map (\x -> f x y) xs
                (xs, EFill _ _ y) -> do
                  (fn, x) <- mkLet1 a
                  let body = EApp a (EApp a f (EVar a x)) y
                      dec = mkDec1 a fn x body
                  pure $ ELetIn a dec (EMap a (EVar a fn) xs)
                _ -> pure (EZipWith a f a1 a2)
        _ ->
          -- Neither operand is a producer, try structural rewrites
          case (a1, a2) of
            -- zipwith f (map g xs) ys => zipwith (\x y -> f (g x) y) xs ys
            (EMap _ g xs, ys) -> do
              (fn, x, y) <- mkLet2 a
              let body = EApp a (EApp a f (EApp a g (EVar a x))) (EVar a y)
                  dec = mkDec2 a fn x y body
              pure $ ELetIn a dec (EZipWith a (EVar a fn) xs ys)
            -- zipwith f xs (map g ys) => zipwith (\x y -> f x (g y)) xs ys
            (xs, EMap _ g ys) -> do
              (fn, x, y) <- mkLet2 a
              let body = EApp a (EApp a f (EVar a x)) (EApp a g (EVar a y))
                  dec = mkDec2 a fn x y body
              pure $ ELetIn a dec (EZipWith a (EVar a fn) xs ys)
            -- zipwith f (fill s x) ys => map (\y -> f x y) ys
            (EFill _ _ x, ys) -> do
              (fn, y) <- mkLet1 a
              let body = EApp a (EApp a f x) (EVar a y)
                  dec = mkDec1 a fn y body
              pure $ ELetIn a dec (EMap a (EVar a fn) ys)
            -- zipwith f xs (fill s y) => map (\x -> f x y) xs
            (xs, EFill _ _ y) -> do
              (fn, x) <- mkLet1 a
              let body = EApp a (EApp a f (EVar a x)) y
                  dec = mkDec1 a fn x body
              pure $ ELetIn a dec (EMap a (EVar a fn) xs)
            _ -> pure (EZipWith a f a1 a2)

fusePermute :: (Eq a) => a -> Exp a -> Exp a -> Exp a -> Exp a -> FusionM (Exp a)
fusePermute a c d p arr =
  case arr of
    -- Hoist let-bindings out of the source-array position so that subsequent
    -- iterations can see through to the underlying producer (mirrors the
    -- ELetIn-hoisting rule added to fuseIndex / fuseMap).
    ELetIn _ dec body | isArrayExp body ->
      let bound = decBoundVars dec
          fv = freeVarsExp c `S.union` freeVarsExp d `S.union` freeVarsExp p
       in if S.null (bound `S.intersection` fv)
            then pure (ELetIn a dec (EPermute a c d p body))
            else pure (EPermute a c d p arr)
    _ ->
      -- Apply the shared values-side map absorption rule first.
       let sk0 = ScatterKernel { skCombine = c, skDefault = d
                                , skIndex = p, skValues = arr, skGuard = Nothing }
        in case scatterAbsorbMapValues a sk0 of
             Just m -> do
               (dec, sk') <- m
               -- Keep the @EPermute@ node and update only its operands.
               pure $ ELetIn a dec (EPermute a (skCombine sk') d p (skValues sk'))
             Nothing ->
               -- Otherwise, try simplifying the source array as a producer.
               case asProducer arr of
                 Just pArr -> do
                   arr' <- emitProducer a pArr
                   pure (EPermute a c d p arr')
                 Nothing -> pure (EPermute a c d p arr)

fuseScatter :: (Eq a) => a -> Exp a -> Exp a -> Exp a -> Exp a -> FusionM (Exp a)
fuseScatter a c d idx vals =
  let sk0 = ScatterKernel { skCombine = c, skDefault = d, skIndex = idx, skValues = vals, skGuard = Nothing }
   in fuseScatterKernel a sk0

-- | Unified scatter fusion: applies all scatter fusion rules to a
-- @ScatterKernel@ and emits the result as @EScatter@.
--
-- Rules are applied in priority order:
--
-- * Hoist @ELetIn@ from the values position to expose inner producers.
-- * Hoist @ELetIn@ from the index position for the same reason.
-- * Absorb an @EMap g@ on values into the combine function
--   (Rocq: @FuseScatterMap@).
-- * Simplify a composite index producer to a single @generate@
--   (Rocq: @scatter_reindex@ / T4 scatter index-side).
-- * When both index and values are @EMap@ over the same source array, convert
--   to @EScatterGenerate@ so values are computed at scatter time instead of
--   materializing an intermediate mapped array:
--
--       scatter c d (map t1 src) (map t2 src)
--       => let valFn i = t2 (index i src)
--          in scatter_generate c d (map t1 src) valFn
--
--   (Rocq: @scatter_fold_reindex@, @scatter_reindex@, PushUpdate.v / Scatter.v)
fuseScatterKernel :: (Eq a) => a -> ScatterKernel a -> FusionM (Exp a)
fuseScatterKernel a sk =
  case skValues sk of
    -- Rule 1: hoist ELetIn from values position.
    ELetIn _ dec body | isArrayExp body ->
      let bound = decBoundVars dec
          fv = freeVarsExp (skCombine sk) `S.union` freeVarsExp (skDefault sk)
                `S.union` freeVarsExp (skIndex sk)
       in if S.null (bound `S.intersection` fv)
            then pure (ELetIn a dec (emitScatter a sk { skValues = body }))
            else pure (emitScatter a sk)
    _ ->
      case skIndex sk of
        -- Rule 2: hoist ELetIn from index position.
        ELetIn _ dec body | isArrayExp body ->
          let bound = decBoundVars dec
              fv = freeVarsExp (skCombine sk) `S.union` freeVarsExp (skDefault sk)
                    `S.union` freeVarsExp (skValues sk)
                    `S.union` maybe S.empty freeVarsExp (skGuard sk)
           in if S.null (bound `S.intersection` fv)
                then pure (ELetIn a dec (emitScatter a sk { skIndex = body }))
                else pure (emitScatter a sk)
        _ ->
          -- Rule 6: scatter_reindex — when both index and values are EMap over
          -- the same source array (possibly with different mapping functions),
          -- avoid materialising the intermediate mapped-values array by converting
          -- to EScatterGenerate with an inline value generator.
          --
          --   scatter c d (map t1 src) (map t2 src)
          --   => let valFn i = t2 (index i src)
          --      in scatter_generate c d (map t1 src) valFn
          --
          -- This is more general than the strict Rocq scatter_reindex (which
          -- requires t1 = t2); it fires whenever both arrays share the same
          -- source, covering the common histogram/segmented-reduction pattern:
          --   scatter (+) zeros (map bin arr) (map weight arr)
          case (skGuard sk, skIndex sk, skValues sk) of
            (Nothing, EMap _ _t1 rawIdx, EMap _ t2 rawVals)
              | fmap (const ()) rawIdx == fmap (const ()) rawVals -> do
                  (fn, i) <- mkLet1 a
                  let body = EApp a t2 (EIndex a (EVar a i) rawVals)
                      dec  = mkDec1 a fn i body
                  pure $ ELetIn a dec
                           (EScatterGenerate a (skCombine sk) (skDefault sk)
                                               (skIndex sk) (EVar a fn))
            (Nothing, EZipWith _ routeFn xs ys, EZipWith _ valueFn xs' ys')
              | fmap (const ()) xs == fmap (const ()) xs'
              , fmap (const ()) ys == fmap (const ()) ys' -> do
                  (idxFn, idxI) <- mkLet1 a
                  (valFn, valI) <- mkLet1 a
                  let idxBody =
                        EApp a
                          (EApp a routeFn (EIndex a (EVar a idxI) xs))
                          (EIndex a (EVar a idxI) ys)
                      valBody =
                        EApp a
                          (EApp a valueFn (EIndex a (EVar a valI) xs))
                          (EIndex a (EVar a valI) ys)
                      idxDec = mkDec1 a idxFn idxI idxBody
                      valDec = mkDec1 a valFn valI valBody
                      idxGen = EGenerate a (EShapeOf a xs) (EVar a idxFn)
                  pure $
                    ELetIn a idxDec
                      (ELetIn a valDec
                        (EScatterGenerate a (skCombine sk) (skDefault sk)
                          idxGen (EVar a valFn)))
            (Nothing, idxExp, valsExp)
              | Just (idxBody, idxSources) <- scatterSourceAt a "__scatter_idx" idxExp
              , Just (valBody, valSources) <- scatterSourceAt a "__scatter_val" valsExp
              , iterSrc : _ <- mergeScatterSources idxSources valSources
              -> do
                  (idxFn, idxI) <- mkLet1 a
                  (valFn, valI) <- mkLet1 a
                  let idxDec = mkDec1 a idxFn idxI (substExp "__scatter_idx" (EVar a idxI) idxBody)
                      valDec = mkDec1 a valFn valI (substExp "__scatter_val" (EVar a valI) valBody)
                      idxGen = EGenerate a (EShapeOf a iterSrc) (EVar a idxFn)
                  pure $
                     ELetIn a idxDec
                       (ELetIn a valDec
                         (EScatterGenerate a (skCombine sk) (skDefault sk)
                           idxGen (EVar a valFn)))
            (Just guardExp, idxExp, valsExp)
              | Just (idxBody, idxSources) <- scatterSourceAt a "__scatter_idx" idxExp
              , Just (valBody, valSources) <- scatterSourceAt a "__scatter_val" valsExp
              , Just (guardBody, guardSources) <- scatterSourceAt a "__scatter_guard" guardExp
              , iterSrc : _ <- mergeScatterSources idxSources
                                 (mergeScatterSources valSources guardSources)
              -> do
                  (idxFn, idxI) <- mkLet1 a
                  (valFn, valI) <- mkLet1 a
                  (guardFn, guardI) <- mkLet1 a
                  let idxDec =
                        mkDec1 a idxFn idxI
                          (substExp "__scatter_idx" (EVar a idxI) idxBody)
                      valDec =
                        mkDec1 a valFn valI
                          (substExp "__scatter_val" (EVar a valI) valBody)
                      guardDec =
                        mkDec1 a guardFn guardI
                          (substExp "__scatter_guard" (EVar a guardI) guardBody)
                      iterShape = EShapeOf a iterSrc
                      idxGen = EGenerate a iterShape (EVar a idxFn)
                      valGen = EGenerate a iterShape (EVar a valFn)
                      guardGen = EGenerate a iterShape (EVar a guardFn)
                  pure $
                    ELetIn a idxDec
                      (ELetIn a valDec
                        (ELetIn a guardDec
                          (EScatterGuarded a (skCombine sk) (skDefault sk)
                            idxGen valGen guardGen)))
            (Nothing, idxExp, valsExp)
              | isDerivedProducerExp idxExp || isDerivedProducerExp valsExp
              , Just pIdx <- asScatterProducer idxExp
              , Just pVals <- asScatterProducer valsExp
              , fmap (const ()) (producerShape pIdx) == fmap (const ()) (producerShape pVals) -> do
                  idx' <- emitProducer a pIdx
                  (valDec, valFnExp) <- emitKernel1 a (producerElem pVals)
                  let emitWith idxBody = ELetIn a valDec
                        (EScatterGenerate a (skCombine sk) (skDefault sk) idxBody valFnExp)
                  pure $
                    case idx' of
                      ELetIn _ dec idxBody | isArrayExp idxBody ->
                        ELetIn a dec (emitWith idxBody)
                      _ ->
                        emitWith idx'
            _ ->
              -- Rule 3: absorb EMap on values side into combine.
              case scatterAbsorbMapValues a sk of
                Just m -> do
                  (dec, sk') <- m
                  pure $ ELetIn a dec (emitScatter a sk')
                Nothing ->
                  -- Rule 4: simplify composite index producer.
                  case asProducer (skIndex sk) of
                    Just pIdx | isCompositeProducerExp (skIndex sk) -> do
                      idx' <- emitProducer a pIdx
                      pure (emitScatter a sk { skIndex = idx' })
                    _ ->
                      -- Rule 5: values side is a generate → emit EScatterGenerate to
                      -- avoid materialising the intermediate values array.
                      case (skGuard sk, skValues sk) of
                        (Nothing, EGenerate _ _sh valFn) ->
                          pure (EScatterGenerate a (skCombine sk) (skDefault sk) (skIndex sk) valFn)
                        _ -> pure (emitScatter a sk)

fuseReduce :: (Eq a) => a -> Exp a -> Exp a -> Exp a -> FusionM (Exp a)
fuseReduce a f z arr =
  case asProducer arr of
    Just p -> do
      let shape = producerShape p
          kernel = producerElem p
      case kAsUnaryFunction kernel of
        Just genFn -> pure (EReduceGenerate a f z shape genFn)
        Nothing -> do
          (dec, genFnExp) <- emitKernel1 a kernel
          pure (ELetIn a dec (EReduceGenerate a f z shape genFnExp))
    Nothing ->
      case arr of
        -- reduce f z (zipwith g xs ys) => reduce_generate f z (shape_of xs) (\i -> g (index i xs) (index i ys))
        EZipWith _ g xs ys -> do
          (fn, idx) <- mkLet1 a
          let body = EApp a (EApp a g (EIndex a (EVar a idx) xs)) (EIndex a (EVar a idx) ys)
              dec = mkDec1 a fn idx body
              shape = EShapeOf a xs
          pure $ ELetIn a dec (EReduceGenerate a f z shape (EVar a fn))
        -- reduce f z (let x = e in body) => let x = e in reduce f z body
        ELetIn _ dec body | isArrayExp body ->
          let bound = decBoundVars dec
              fv = freeVarsExp f `S.union` freeVarsExp z
           in if S.null (bound `S.intersection` fv)
                then pure (ELetIn a dec (EReduce a f z body))
                else pure (EReduce a f z arr)
        -- reduce f z (map g xs) => reduce (\acc x -> f acc (g x)) z xs
        EMap _ g xs -> do
          (fn, acc, x) <- mkLet2 a
          let body = EApp a (EApp a f (EVar a acc)) (EApp a g (EVar a x))
              dec = mkDec2 a fn acc x body
          pure $ ELetIn a dec (EReduce a (EVar a fn) z xs)
        -- reduce f z (gather idx xs) => reduce_generate f z (shape_of idx) (\i -> index (index i idx) xs)
        EGather _ idx xs -> do
          (fn, i) <- mkLet1 a
          let body = EIndex a (EIndex a (EVar a i) idx) xs
              dec = mkDec1 a fn i body
              shape = EShapeOf a idx
          pure $ ELetIn a dec (EReduceGenerate a f z shape (EVar a fn))
        _ -> pure (EReduce a f z arr)

fuseGather :: (Eq a) => a -> Exp a -> Exp a -> FusionM (Exp a)
fuseGather a idx arr =
  case asProducer arr of
    -- gather idx producer => map (\j -> producerElem j) idx
    Just p -> do
      (fn, i) <- mkLet1 a
      let body = kApply a [EVar a i] (producerElem p)
          dec = mkDec1 a fn i body
      pure $ ELetIn a dec (EMap a (EVar a fn) idx)
    Nothing ->
      case arr of
        -- gather idx (map f xs) => map f (gather idx xs)
        EMap _ f xs -> pure (EMap a f (EGather a idx xs))
        _ -> pure (EGather a idx arr)

fuseIndex :: (Eq a) => a -> Exp a -> Exp a -> FusionM (Exp a)
fuseIndex a idx arr =
  case arr of
    -- Hoist let-bindings out of the array position so that subsequent iterations
    -- can see through to the underlying producer.  This mirrors the equivalent
    -- rule in fuseMap / fuseZipWith:
    --   index i (let x = e in body) => let x = e in index i body
    ELetIn _ dec body | isArrayExp body ->
      let bound = decBoundVars dec
       in if S.null (bound `S.intersection` freeVarsExp idx)
            then pure (ELetIn a dec (EIndex a idx body))
            else pure (EIndex a idx arr)
    _ ->
      case asProducer arr of
        -- If the array is a recognizable producer, evaluate its kernel at the index
        -- directly. This eliminates intermediate arrays for generate, fill, map,
        -- zipWith, gather, replicate, and slice without needing separate cases.
        --   index i (generate shape f)    => f i
        --   index i (fill shape v)        => v
        --   index i (map f xs)            => f (xs[i])     [when xs is a producer]
        --   index i (gather idx arr)      => arr[idx[i]]   [when both are producers]
        Just p -> pure (kApply a [idx] (producerElem p))
        -- Fallback rules for when the outer combinator is known but the inner array
        -- is opaque (a variable, etc.).  These float the scalar lookup outward so
        -- that subsequent fusion iterations can simplify further.
        Nothing ->
          case arr of
            -- index i (map f xs) => f (index i xs)
            EMap _ f xs -> pure (EApp a f (EIndex a idx xs))
            -- index i (zipWith f xs ys) => f (index i xs) (index i ys)
            EZipWith _ f xs ys -> pure (EApp a (EApp a f (EIndex a idx xs)) (EIndex a idx ys))
            _ -> pure (EIndex a idx arr)

fuseReplicate :: (Eq a) => a -> [ShapeDim a] -> Exp a -> FusionM (Exp a)
fuseReplicate a dims arr =
  case arr of
    -- replicate dims (map f xs) => map f (replicate dims xs)
    EMap _ f xs -> pure (EMap a f (EReplicate a dims xs))
    _ -> pure (EReplicate a dims arr)

fuseSlice :: (Eq a) => a -> [SliceDim a] -> Exp a -> FusionM (Exp a)
fuseSlice a dims arr =
  case arr of
    -- slice dims (map f xs) => map f (slice dims xs)
    EMap _ f xs -> pure (EMap a f (ESlice a dims xs))
    _ -> pure (ESlice a dims arr)

fuseReshape :: (Eq a) => a -> Exp a -> Exp a -> FusionM (Exp a)
fuseReshape a s arr =
  case arr of
    -- reshape s (map f xs) => map f (reshape s xs)
    EMap _ f xs -> pure (EMap a f (EReshape a s xs))
    _ -> pure (EReshape a s arr)
