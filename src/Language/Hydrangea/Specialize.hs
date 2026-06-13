{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.Specialize
--
-- Whole-program monomorphization (specialization) of polymorphic top-level
-- functions.
--
-- Hydrangea uses Hindley–Milner inference, so a top-level helper such as
--
-- > let dup a = zipwith (fn x y => x) a a
--
-- is given the polymorphic scheme @forall a. Array a -> Array a@.  The C
-- backend, however, needs a concrete element type for every array and every
-- combine-function parameter.  Inlining during lowering recovers concrete
-- types for /directly/ supplied arguments, but the binders inside a
-- polymorphic function body remain type variables, which can surface as
-- @CTUnknown@ at code generation time.
--
-- This pass eliminates that problem at the source level.  For every syntactic
-- reference to a polymorphic top-level function it produces a fresh, dedicated
-- clone of that function and rewrites the reference to point at the clone.
-- Because each clone is then referenced from exactly one site, re-running type
-- inference over the cloned program assigns ground types to every binder in
-- the clone (each clone is used at a single instantiation).  The binder types
-- recovered this way (see "Language.Hydrangea.Infer".'runInferDecsBinderTypes')
-- are threaded into lowering as the authoritative source of concrete types.
--
-- Cloning is performed recursively: a clone's body may itself reference other
-- polymorphic functions, which are cloned in turn.  Self-recursive polymorphic
-- functions cannot be monomorphized this way; such references are left
-- untouched (and the original definition is retained) so the pass always
-- terminates and never produces dangling references.
--
-- The pass is the identity on programs that contain no polymorphic top-level
-- functions, so fully monomorphic programs are unaffected.
module Language.Hydrangea.Specialize
  ( monomorphizeDecs
  , polymorphicFnNames
  ) where

import Control.Monad.State
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as S

import Language.Hydrangea.Lexer (Range)
import Language.Hydrangea.Syntax
import Language.Hydrangea.Uniquify (collectVarsDec, collectVarsExp)

-- | Given the inferred top-level schemes and the declaration list, return the
-- set of top-level /function/ declarations (those with parameters) whose
-- scheme is polymorphic (has quantified type variables).  These are the
-- declarations the pass specializes.
polymorphicFnNames :: [(Var, Polytype)] -> [Dec Range] -> Set Var
polymorphicFnNames schemes decs =
  let hasParams = S.fromList [decName d | d <- decs, decHasParams d]
      poly = S.fromList [v | (v, Forall qs _ _) <- schemes, not (null qs)]
  in poly `S.intersection` hasParams
  where
    decHasParams (Dec _ _ pats _ _ _) = not (null pats)

-- | Monomorphize a program by specializing the named polymorphic top-level
-- functions.  Clones are appended after the (rewritten) non-polymorphic
-- declarations; any polymorphic original that remains referenced (only
-- possible through self-recursion) is retained.
--
-- The resulting declarations may temporarily share binder names between a
-- clone and its original; callers should run 'uniquifyDecsForce' afterwards to
-- restore globally unique binders before re-inference and lowering.
monomorphizeDecs :: Set Var -> [Dec Range] -> [Dec Range]
monomorphizeDecs poly decs
  | S.null poly = decs
  | otherwise =
      let fnDecs = M.fromList [(decName d, d) | d <- decs, decName d `S.member` poly]
          kept   = [d | d <- decs, not (decName d `S.member` poly)]
          st0    = SpecState { ssCounter = 0, ssClones = [], ssFnDecs = fnDecs, ssPoly = poly }
          (keptRewritten, stFinal) = runState (mapM (rewriteDec []) kept) st0
          clones = reverse (ssClones stFinal)
          -- A polymorphic original is dropped once it has been fully specialized
          -- away.  Two situations require keeping the original:
          --   * it is never called anywhere (an exported / library function with
          --     no call site to specialize against), or
          --   * it is still referenced after rewriting (only possible through
          --     self-recursion, which cannot be monomorphized).
          -- We keep those roots plus every polymorphic original transitively
          -- reachable from them, so no reference is left dangling.
          calledNames     = S.unions [collectVarsExp body | Dec _ _ _ _ _ body <- decs]
          referencedAfter = S.unions (map collectVarsDec (keptRewritten ++ clones))
          uncalledPoly    = S.filter (`S.notMember` calledNames) poly
          recursionPoly   = S.filter (`S.member` referencedAfter) poly
          keepPoly        = closureOverPoly (uncalledPoly `S.union` recursionPoly)
          keptPolyOrigs   = [d | d <- decs, decName d `S.member` keepPoly]
      in keptRewritten ++ clones ++ keptPolyOrigs
  where
    bodyOf = M.fromList [(name, body) | (Dec _ name _ _ _ body) <- decs]
    -- Transitive closure of @start@ over references to polymorphic functions in
    -- the original (un-rewritten) declaration bodies.
    closureOverPoly start = go start start
      where
        go acc frontier
          | S.null frontier = acc
          | otherwise =
              let refs  = S.unions [maybe S.empty collectVarsExp (M.lookup n bodyOf) | n <- S.toList frontier]
                  fresh = (refs `S.intersection` poly) `S.difference` acc
              in go (acc `S.union` fresh) fresh

data SpecState = SpecState
  { ssCounter :: !Int
  , ssClones  :: [Dec Range]            -- ^ accumulated clones (reverse order)
  , ssFnDecs  :: Map Var (Dec Range)    -- ^ polymorphic originals by name
  , ssPoly    :: Set Var                -- ^ names being specialized
  }

type SpecM = State SpecState

freshCloneName :: Var -> SpecM Var
freshCloneName base = do
  n <- gets ssCounter
  modify' $ \s -> s { ssCounter = n + 1 }
  pure $ base <> "__spec_" <> BS.pack (show n)

-- | The list argument is the path of original function names currently being
-- cloned, used to detect (and break) self-recursive references.
rewriteDec :: [Var] -> Dec Range -> SpecM (Dec Range)
rewriteDec path (Dec a name pats mw poly body) = do
  pats' <- mapM (rewritePat path) pats
  body' <- rewriteExp path body
  pure (Dec a name pats' mw poly body')

rewritePat :: [Var] -> Pat Range -> SpecM (Pat Range)
rewritePat path pat = case pat of
  PVar {} -> pure pat
  PBound a v e -> PBound a v <$> rewriteExp path e
  PVec a ps -> PVec a <$> mapM (rewritePat path) ps
  PPair a p1 p2 -> PPair a <$> rewritePat path p1 <*> rewritePat path p2

-- | Produce (or, for recursion, decline to produce) a clone of polymorphic
-- function @name@ and return the name to reference at this site.
specializeRef :: [Var] -> Var -> SpecM Var
specializeRef path name = do
  isPoly <- gets (S.member name . ssPoly)
  if not isPoly || name `elem` path
    then pure name           -- monomorphic, or self-recursive: leave as-is
    else do
      mDec <- gets (M.lookup name . ssFnDecs)
      case mDec of
        Nothing -> pure name
        Just (Dec a _ pats mw poly body) -> do
          cloneName <- freshCloneName name
          pats' <- mapM (rewritePat (name : path)) pats
          body' <- rewriteExp (name : path) body
          let clone = Dec a cloneName pats' mw poly body'
          modify' $ \s -> s { ssClones = clone : ssClones s }
          pure cloneName

rewriteExp :: [Var] -> Exp Range -> SpecM (Exp Range)
rewriteExp path = go
  where
    go expr = case expr of
      EInt {} -> pure expr
      EFloat {} -> pure expr
      EVar a v -> EVar a <$> specializeRef path v
      EString {} -> pure expr
      EUnit {} -> pure expr
      EBool {} -> pure expr
      EOp {} -> pure expr
      EVec a es -> EVec a <$> mapM go es
      EApp a f x -> EApp a <$> go f <*> go x
      EIfThen a c t -> EIfThen a <$> go c <*> go t
      EIfThenElse a c t f -> EIfThenElse a <$> go c <*> go t <*> go f
      ENeg a e -> ENeg a <$> go e
      EBinOp a l op r -> EBinOp a <$> go l <*> pure op <*> go r
      EUnOp a op e -> EUnOp a op <$> go e
      ELetIn a dec body -> ELetIn a <$> rewriteDec path dec <*> go body
      EProj a i e -> EProj a i <$> go e
      EPair a e1 e2 -> EPair a <$> go e1 <*> go e2
      ERecord a fields -> ERecord a <$> mapM (\(f, fe) -> (,) f <$> go fe) fields
      ERecordProj a e f -> ERecordProj a <$> go e <*> pure f
      EGenerate a sz f -> EGenerate a <$> go sz <*> go f
      EMap a f arr -> EMap a <$> go f <*> go arr
      EZipWith a f a1 a2 -> EZipWith a <$> go f <*> go a1 <*> go a2
      EAppend a a1 a2 -> EAppend a <$> go a1 <*> go a2
      EReduce a f z arr -> EReduce a <$> go f <*> go z <*> go arr
      EReduceGenerate a f z shape gen ->
        EReduceGenerate a <$> go f <*> go z <*> go shape <*> go gen
      EIterate a n initArr f -> EIterate a <$> go n <*> go initArr <*> go f
      EFoldl a f z arr -> EFoldl a <$> go f <*> go z <*> go arr
      EFoldlWhile a p f z arr -> EFoldlWhile a <$> go p <*> go f <*> go z <*> go arr
      EScan a f z arr -> EScan a <$> go f <*> go z <*> go arr
      EScanInclusive a f z arr -> EScanInclusive a <$> go f <*> go z <*> go arr
      EScanR a f z arr -> EScanR a <$> go f <*> go z <*> go arr
      EScanRInclusive a f z arr -> EScanRInclusive a <$> go f <*> go z <*> go arr
      ESegmentedReduce a f z offsets vals ->
        ESegmentedReduce a <$> go f <*> go z <*> go offsets <*> go vals
      ESortIndices a arr -> ESortIndices a <$> go arr
      EIota a n -> EIota a <$> go n
      EMakeIndex a n arr -> EMakeIndex a <$> go n <*> go arr
      ECOOSumDuplicates a nrows ncols nnz rows cols vals ->
        ECOOSumDuplicates a <$> go nrows <*> go ncols <*> go nnz
          <*> go rows <*> go cols <*> go vals
      ECSRFromSortedCOO a nrows ncols nnz rows cols vals ->
        ECSRFromSortedCOO a <$> go nrows <*> go ncols <*> go nnz
          <*> go rows <*> go cols <*> go vals
      EPermute a c d p arr -> EPermute a <$> go c <*> go d <*> go p <*> go arr
      EScatter a c d idx v -> EScatter a <$> go c <*> go d <*> go idx <*> go v
      EScatterGuarded a c d idx v g ->
        EScatterGuarded a <$> go c <*> go d <*> go idx <*> go v <*> go g
      EScatterGenerate a c d idx f ->
        EScatterGenerate a <$> go c <*> go d <*> go idx <*> go f
      EScatterChain a c d phases -> do
        c' <- go c
        d' <- go d
        phases' <- mapM (\p -> ScatterPhase <$> go (spIndex p)
                                            <*> go (spValues p)
                                            <*> mapM go (spGuard p)) phases
        pure (EScatterChain a c' d' phases')
      EScatterGen a c d phases -> do
        c' <- go c
        d' <- go d
        phases' <- mapM (\p -> ScatterGenPhase <$> go (sgpShape p)
                                               <*> go (sgpIndexFn p)
                                               <*> go (sgpValueFn p)
                                               <*> mapM go (sgpGuardFn p)) phases
        pure (EScatterGen a c' d' phases')
      EGather a idx arr -> EGather a <$> go idx <*> go arr
      EIndex a idx arr -> EIndex a <$> go idx <*> go arr
      ECheckIndex a idx def arr -> ECheckIndex a <$> go idx <*> go def <*> go arr
      EFill a s v -> EFill a <$> go s <*> go v
      EShapeOf a arr -> EShapeOf a <$> go arr
      EReplicate a dims arr -> EReplicate a <$> mapM (rewriteShapeDim path) dims <*> go arr
      ESlice a dims arr -> ESlice a <$> mapM (rewriteSliceDim path) dims <*> go arr
      EReshape a s arr -> EReshape a <$> go s <*> go arr
      EReadArray a s f -> EReadArray a <$> go s <*> go f
      EReadArrayFloat a s f -> EReadArrayFloat a <$> go s <*> go f
      EWriteArray a arr f -> EWriteArray a <$> go arr <*> go f
      EWriteArrayFloat a arr f -> EWriteArrayFloat a <$> go arr <*> go f
      EGetEnvInt a e -> EGetEnvInt a <$> go e
      EGetEnvString a e -> EGetEnvString a <$> go e
      EStencil a bnd f arr -> EStencil a <$> rewriteBnd path bnd <*> go f <*> go arr
      EBoundLetIn a x boundExp rhs body ->
        EBoundLetIn a x <$> go boundExp <*> go rhs <*> go body
      EReify a e -> EReify a <$> go e

rewriteBnd :: [Var] -> BoundaryCondition Range -> SpecM (BoundaryCondition Range)
rewriteBnd path bnd = case bnd of
  BClamp -> pure BClamp
  BWrap -> pure BWrap
  BMirror -> pure BMirror
  BConst e -> BConst <$> rewriteExp path e

rewriteShapeDim :: [Var] -> ShapeDim Range -> SpecM (ShapeDim Range)
rewriteShapeDim path dim = case dim of
  ShapeAll a -> pure (ShapeAll a)
  ShapeAny a e -> ShapeAny a <$> rewriteExp path e
  ShapeDim a e -> ShapeDim a <$> rewriteExp path e

rewriteSliceDim :: [Var] -> SliceDim Range -> SpecM (SliceDim Range)
rewriteSliceDim path dim = case dim of
  SliceAll a -> pure (SliceAll a)
  SliceRange a s l -> SliceRange a <$> rewriteExp path s <*> rewriteExp path l
