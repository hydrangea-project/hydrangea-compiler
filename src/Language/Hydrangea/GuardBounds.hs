{-# LANGUAGE OverloadedStrings #-}

-- | Guard-derived loop bounds.
--
-- A loop whose body is a run of pure assignments followed by a one-armed
-- @if c then ...@ does nothing in an iteration where @c@ is false.  When a
-- conjunct of @c@ bounds an iterator, those iterations can be skipped
-- instead of tested.  The body, and every strategy the parallelizer chose
-- for it, is left alone; this module only says over which iterations the
-- loop may run, and 'Language.Hydrangea.CodegenC' emits the tighter loop.
--
-- Two shapes are recognised.
--
-- * A 1-D loop @for i < B@ with a conjunct @i < e@, @e@ loop-invariant:
--   the loop runs @i < min(B, e)@.
--
-- * A flat loop over a 2-D rectangle, @for i < N*M@ with
--   @nd = flat_to_nd(i, (N, M))@, @j = nd.0@, @k = nd.1@ -- the shape every
--   2-D scatter lowers to.  A conjunct @k < e(j)@, where @e@ depends only on
--   @j@ and invariants, makes the loop a ragged nest
--
--   > for j < N:                      (outer; dynamically scheduled)
--   >   lim = min(M, e(j))
--   >   for k < lim: i = j*M + k; body
--
--   and a conjunct @j < e@, @e@ invariant, clamps the outer trip count.
--
-- Soundness.  The guard stays in the body, so every iteration that still
-- runs does exactly what it did before; the dropped iterations are exactly
-- ones whose guard is false, whose only effect was computing pure
-- temporaries.  The bound's slice is copied out of the body (under fresh
-- names) and evaluated where the original evaluated it -- once per row, or
-- once per loop -- and only when the original would have run at least one
-- iteration, so it introduces no load or division the original did not
-- perform.  It may not read an array the loop writes (directly or through a
-- copy), since a scatter could change the bound mid-row.
module Language.Hydrangea.GuardBounds
  ( Limit (..)
  , InnerNest (..)
  , RaggedLoop (..)
  , raggedLoops
  ) where

import Control.Monad (guard)
import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S

import Language.Hydrangea.CFG
import Language.Hydrangea.CFGCore (Atom (..), BinOp (..), CType (..), RHS (..))

-- | An upper bound on an iterator: @prelude; iter < atom (+1)@.
data Limit = Limit
  { limPrelude :: [Stmt]  -- ^ Pure assignments (fresh names) computing 'limAtom'.
  , limAtom    :: Atom
  , limPlusOne :: Bool    -- ^ The guard was @<=@, so the trip count is @atom + 1@.
  }
  deriving (Eq, Show)

-- | The column loop of a ragged nest.
data InnerNest = InnerNest
  { inRowIter :: CVar         -- ^ Fresh outer (row) iterator.
  , inColIter :: CVar         -- ^ Fresh inner (column) iterator.
  , inRows    :: Atom         -- ^ N, the rectangle's row count.
  , inCols    :: Atom         -- ^ M, the rectangle's column count.
  , inLimit   :: Maybe Limit  -- ^ Per-row column bound; its prelude reads 'inRowIter'.
  }
  deriving (Eq, Show)

-- | How to run a loop over fewer iterations than its bound.
data RaggedLoop = RaggedLoop
  { rlOuterLimit :: Maybe Limit      -- ^ Loop-invariant bound on the outer iterator.
  , rlInner      :: Maybe InnerNest  -- ^ 'Nothing' for a 1-D loop.
  }
  deriving (Eq, Show)

-- | Find every loop in a procedure body that can run over fewer iterations,
-- keyed by its (unique) iterator.  Also returns the C types of the fresh
-- variables the preludes define.
raggedLoops :: Map CVar CType -> [Stmt] -> (Map CVar RaggedLoop, Map CVar CType)
raggedLoops typeEnv procStmts =
  foldl' add (M.empty, M.empty) (allLoops procStmts)
  where
    procDefs = collectDefs procStmts
    aliasOf = aliasClosure procStmts

    add acc@(loops, tys) (spec, body) =
      case analyseLoop typeEnv procDefs aliasOf spec body of
        Just (iter, rl, newTys) -> (M.insert iter rl loops, M.union newTys tys)
        Nothing -> acc

-- ---------------------------------------------------------------------------
-- Per-loop analysis
-- ---------------------------------------------------------------------------

-- | What an atom in the loop body ultimately depends on.
data Root = RIter | RRow | RCol
  deriving (Eq, Ord, Show)

analyseLoop
  :: Map CVar CType
  -> Map CVar [RHS]
  -> (CVar -> Set CVar)
  -> LoopSpec
  -> [Stmt]
  -> Maybe (CVar, RaggedLoop, Map CVar CType)
analyseLoop typeEnv procDefs aliasOf spec body = do
  [iter] <- Just (lsIters spec)
  [bound] <- Just (lsBounds spec)
  guard (all (== IConst 0) (lsOrigins spec))
  guard (lsRed spec == Nothing)
  guard (lsRole spec `notElem` [LoopFold, LoopIterate, LoopCopy, LoopReductionWrapper, LoopReduction])
  SIf cond thn [] : revPrefix <- Just (reverse body)
  let prefix = reverse revPrefix
  guard (not (any hasBreakOrReturn thn))
  -- The prefix must be pure: straight-line assignments, each defined once,
  -- and conditionals whose arms are themselves pure (a clamp such as
  -- @if e < n then e else n - 1@ lowers to one).  A variable a conditional
  -- assigns has no single definition, so no bound may depend on it.
  prefixItems <- traverse prefixItem prefix
  let prefixDefs = concat [ds | Left ds <- prefixItems]
      condDefined = S.unions [vs | Right vs <- prefixItems]
      defs = M.fromListWith (++) [(v, [r]) | (v, r) <- prefixDefs]
  guard (all ((== 1) . length) defs)
  guard (S.null (condDefined `S.intersection` M.keysSet defs))
  let def v = case M.lookup v defs of
        Just [r] -> Just r
        _ -> Nothing
      written = S.unions (map aliasOf (S.toList (writtenArrays body)))
      -- The 2-D view, if the body recovers (row, col) from the flat index.
      ndInfo = listToMaybe
        [ (nd, n, m)
        | (nd, RFlatToNd (AVar i') (AVar shp)) <- prefixDefs
        , i' == iter
        , Just [RTuple [n, m]] <- [M.lookup shp procDefs]
        , coversRectangle shp n m
        ]
      -- The loop runs over exactly the rectangle's cells.
      coversRectangle shp n m = case bound of
        IVar sz -> M.lookup sz procDefs == Just [RShapeSize (AVar shp)]
        IConst c | AInt a <- n, AInt b <- m -> a * b == c
        _ -> False
      -- Resolve a variable through copies to the iterator coordinate it is.
      coord v
        | v == iter = Just RIter
        | otherwise = case def v of
            Just (RAtom (AVar w)) -> coord w
            Just (RProj 0 (AVar nd)) | Just (nd', _, _) <- ndInfo, nd == nd' -> Just RRow
            Just (RProj 1 (AVar nd)) | Just (nd', _, _) <- ndInfo, nd == nd' -> Just RCol
            _ -> Nothing
      -- Roots of an atom, or Nothing if its slice uses anything we can't copy.
      roots :: Atom -> Maybe (Set Root)
      roots (AVar v) = rootsVar S.empty v
      roots (AVecVar _) = Nothing
      roots _ = Just S.empty
      rootsVar seen v
        | v `S.member` seen = Nothing
        | Just c <- coord v = Just (S.singleton c)
        | v `S.member` condDefined = Nothing
        | otherwise = case def v of
            Nothing -> Just S.empty              -- defined outside the loop
            Just rhs -> do
              guard (typeKnown v)
              atoms <- sliceableAtoms rhs
              S.unions <$> traverse (rootsAtom (S.insert v seen)) atoms
      rootsAtom seen (AVar w) = rootsVar seen w
      rootsAtom _ (AVecVar _) = Nothing
      rootsAtom _ _ = Just S.empty
      typeKnown v = case M.lookup v typeEnv of
        Just CTInt64 -> True
        Just CTBool -> True
        Just CTDouble -> True
        _ -> False
      -- RHS forms a bound's slice may contain.
      sliceableAtoms rhs = case rhs of
        RAtom a -> Just [a]
        RBinOp _ a b -> Just [a, b]
        RUnOp _ a -> Just [a]
        RArrayLoad (AVar arr) ix
          | Nothing <- def arr
          , S.null (aliasOf arr `S.intersection` written) -> Just [ix]
        _ -> Nothing
      -- Conjuncts of the guard, as upper bounds @x < y@ / @x <= y@.
      conjuncts a = case a of
        AVar v | Just (RBinOp CAnd x y) <- def v -> conjuncts x ++ conjuncts y
        AVar v | Just r <- def v -> [r]
        _ -> []
      upper r = case r of
        RBinOp CLt (AVar x) y -> Just (x, y, False)
        RBinOp CLe (AVar x) y -> Just (x, y, True)
        RBinOp CGt y (AVar x) -> Just (x, y, False)
        RBinOp CGe y (AVar x) -> Just (x, y, True)
        _ -> Nothing
      bounds =
        [ (c, y, le, rs)
        | (x, y, le) <- mapMaybe upper (conjuncts cond)
        , Just c <- [coord x]
        , Just rs <- [roots y]
        ]
      rowIter = iter <> "_row"
      colIter = iter <> "_col"
      mkLimit sub y le =
        let (pre, y', renamed) = sliceOut def sub ("_gb_" <> iter) y
         in Just ( Limit pre y' le
                 , M.fromList [(v', t) | (v, v') <- renamed, Just t <- [M.lookup v typeEnv]] )
      firstJust = listToMaybe . mapMaybe id
  case ndInfo of
    Nothing -> do
      -- 1-D: an invariant bound on the iterator itself.
      (lim, tys) <- firstJust
        [ mkLimit (const Nothing) y le
        | (RIter, y, le, rs) <- bounds, S.null rs ]
      Just (iter, RaggedLoop (Just lim) Nothing, tys)
    Just (nd, n, m) -> do
      let rowSub v = case def v of
            Just (RProj 0 (AVar nd')) | nd' == nd -> Just (AVar rowIter)
            _ -> Nothing
          outer = firstJust
            [ mkLimit (const Nothing) y le
            | (RRow, y, le, rs) <- bounds, S.null rs ]
          inner = firstJust
            [ mkLimit rowSub y le
            | (RCol, y, le, rs) <- bounds, rs `S.isSubsetOf` S.singleton RRow ]
      guard (maybe False (const True) outer || maybe False (const True) inner)
      let nest = InnerNest rowIter colIter n m (fst <$> inner)
          tys = M.union (maybe M.empty snd outer) (maybe M.empty snd inner)
      Just (iter, RaggedLoop (fst <$> outer) (Just nest), tys)

-- | Copy the backward slice of an atom out of the body, renaming every
-- variable it defines with @suffix@.  @sub@ replaces a variable by an atom
-- outright (the row coordinate becomes the row iterator).  Only called on
-- atoms whose slice 'roots' accepted.
sliceOut
  :: (CVar -> Maybe RHS) -> (CVar -> Maybe Atom) -> CVar -> Atom
  -> ([Stmt], Atom, [(CVar, CVar)])
sliceOut def sub suffix a0 =
  let (ren, acc) = goAtom (M.empty, []) a0
   in (reverse acc, renameAtom ren a0, [(v, v') | (v, AVar v') <- M.toList ren])
  where
    goAtom st (AVar v) = goVar st v
    goAtom st _ = st
    goVar st@(ren, _) v
      | v `M.member` ren = st
      | Just _ <- sub v = st
      | otherwise = case def v of
          Nothing -> st
          Just rhs ->
            let (ren', acc') = foldl' goAtom st (renamedAtoms rhs)
                v' = v <> suffix
             in (M.insert v (AVar v') ren', SAssign v' (renameRHS ren' rhs) : acc')
    renameAtom ren (AVar v) = case sub v of
      Just a -> a
      Nothing -> M.findWithDefault (AVar v) v ren
    renameAtom _ a = a
    renameRHS ren rhs = case rhs of
      RAtom a -> RAtom (renameAtom ren a)
      RBinOp op a b -> RBinOp op (renameAtom ren a) (renameAtom ren b)
      RUnOp op a -> RUnOp op (renameAtom ren a)
      RArrayLoad arr ix -> RArrayLoad arr (renameAtom ren ix)
      other -> other

-- | The atoms 'sliceOut' renames: the scalar operands of a slice's RHS.  An
-- array load's array is a buffer, not a value to copy out.
renamedAtoms :: RHS -> [Atom]
renamedAtoms rhs = case rhs of
  RAtom a -> [a]
  RBinOp _ a b -> [a, b]
  RUnOp _ a -> [a]
  RArrayLoad _ ix -> [ix]
  _ -> []

-- ---------------------------------------------------------------------------
-- Procedure-level helpers
-- ---------------------------------------------------------------------------

assignOf :: Stmt -> Maybe (CVar, RHS)
assignOf (SAssign v rhs) | pureRHS rhs = Just (v, rhs)
assignOf _ = Nothing

-- | One statement of the prefix before the guard: a pure assignment, or a
-- conditional of pure statements (returning the variables it assigns).
prefixItem :: Stmt -> Maybe (Either [(CVar, RHS)] (Set CVar))
prefixItem stmt = case stmt of
  SAssign {} -> Left . pure <$> assignOf stmt
  SIf _ t e -> Right <$> pureArms (t ++ e)
  _ -> Nothing
  where
    pureArms = fmap S.unions . traverse pureStmt
    pureStmt st = case st of
      SAssign v _ -> S.singleton v <$ assignOf st
      SIf _ t e -> pureArms (t ++ e)
      _ -> Nothing

-- | RHS forms that may be skipped in a dropped iteration.  Everything but
-- allocation, stores and frees; calls are to pure procedures.
pureRHS :: RHS -> Bool
pureRHS rhs = case rhs of
  RArrayAlloc _ -> False
  RVecStore {} -> False
  RArrayFree _ -> False
  _ -> True

hasBreakOrReturn :: Stmt -> Bool
hasBreakOrReturn stmt = case stmt of
  SBreak -> True
  SReturn _ -> True
  SIf _ t e -> any hasBreakOrReturn t || any hasBreakOrReturn e
  SParallelRegion b -> any hasBreakOrReturn b
  _ -> False  -- a break inside a nested loop exits that loop

-- | Every loop in a statement list, outermost first.
allLoops :: [Stmt] -> [(LoopSpec, [Stmt])]
allLoops = concatMap go
  where
    go stmt = case stmt of
      SLoop spec body -> (spec, body) : allLoops body
      SIf _ t e -> allLoops t ++ allLoops e
      SParallelRegion b -> allLoops b
      _ -> []

-- | Every assignment in a statement list, including nested ones.
collectDefs :: [Stmt] -> Map CVar [RHS]
collectDefs = M.fromListWith (++) . concatMap go
  where
    go stmt = case stmt of
      SAssign v rhs -> [(v, [rhs])]
      SLoop _ body -> concatMap go body
      SIf _ t e -> concatMap go t ++ concatMap go e
      SParallelRegion b -> concatMap go b
      _ -> []

-- | Arrays a statement list writes, including nested statements.
writtenArrays :: [Stmt] -> Set CVar
writtenArrays = S.unions . map go
  where
    go stmt = case stmt of
      SArrayWrite (AVar a) _ _ -> S.singleton a
      SAssign _ (RVecStore (AVar a) _ _) -> S.singleton a
      SLoop _ body -> writtenArrays body
      SIf _ t e -> writtenArrays t `S.union` writtenArrays e
      SParallelRegion b -> writtenArrays b
      _ -> S.empty

-- | The set of variables an array variable may share a buffer with, via
-- copies in either direction (reflexive).
aliasClosure :: [Stmt] -> CVar -> Set CVar
aliasClosure stmts = \v -> reach (S.singleton v) [v]
  where
    edges = M.fromListWith S.union $
      concat [ [(x, S.singleton y), (y, S.singleton x)]
             | (x, rhss) <- M.toList (collectDefs stmts)
             , RAtom (AVar y) <- rhss ]
    reach seen [] = seen
    reach seen (x : xs) =
      let next = S.toList (M.findWithDefault S.empty x edges `S.difference` seen)
       in reach (foldr S.insert seen next) (next ++ xs)
