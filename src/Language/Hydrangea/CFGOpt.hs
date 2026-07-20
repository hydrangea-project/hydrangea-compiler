{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.CFGOpt
--
-- Scalar CFG cleanup and optimization passes.
--
-- The main fixpoint pipeline combines:
--
-- 1. /Copy propagation/ ('copyProp') — forward substitution of direct
--    @x = a@ assignments.
-- 2. /Common subexpression elimination/ ('cseStmts') — replace repeated
--    pure computations with a reference to the first variable that computed
--    the same canonical expression.
-- 3. /Scalarization of 0-D array roundtrips/ ('scalarizeZeroDimArrayRoundtrips')
--    — collapse lowered scalar materialization patterns back to scalar values.
-- 4. /Dead assignment elimination/ ('deadAssignElim') — backwards liveness
--    analysis that removes pure assignments whose result is never used.
-- 5. /Loop-invariant code motion/ ('loopInvariantCodeMotion') — hoist pure
--    loop-invariant computations to the loop pre-header.
--
-- The module also provides targeted restructuring passes such as iterate-loop
-- unswitching and allocation hoisting.
module Language.Hydrangea.CFGOpt
  ( -- * Entry points
    optimizeStmts
    -- * Individual passes (exported for testing)
  , copyProp
  , deadAssignElim
  , loopInvariantCodeMotion
  , unswitchLoopInvariantIf
  , sinkUnusedIntoGuard
  , foldShapeOfAlloc
  , hoistIterateAllocs
  , scalarizeIteratePairs
    -- * Atom/statement substitution utilities
  , substAtom
  , substRHS
  , substStmts
  ) where

import Control.Monad (guard)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List (partition)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Language.Hydrangea.CFGCore (Atom(..), BinOp(..), RHS(..), mapRHSAtoms)
import Language.Hydrangea.Util (unsnoc)
import Language.Hydrangea.CFG
import Language.Hydrangea.CFGAnalysis
  ( definedVarsStmts
  , usedVarsAtom
  , usedVarsRHS
  , usedVarsStmt
  , usedVarsStmts
  , usedVarsIndexExpr
  )

------------------------------------------------------------------------
-- Purity predicate
------------------------------------------------------------------------

-- | Returns 'True' when an 'RHS' is /pure/: it produces a value with no
-- observable side effects and may therefore be removed if the defined
-- variable is dead. Vector stores, procedure calls, and explicit frees are
-- conservatively treated as impure.
rhsIsPure :: RHS -> Bool
rhsIsPure rhs = case rhs of
  RAtom{}       -> True
  RBinOp{}      -> True
  RUnOp{}       -> True
  RTuple{}      -> True
  RProj{}       -> True
  RRecord{}     -> True
  RRecordProj{} -> True
  RNdToFlat{}   -> True
  RFlatToNd{}   -> True
  R2DToFlat{}   -> True
  RArrayShape{} -> True
  RShapeSize{}  -> True
  RShapeInit{}  -> True
  RShapeLast{}  -> True
  RVecLoad{}    -> True
  RVecBinOp{}   -> True
  RVecUnOp{}    -> True
  RVecSplat{}   -> True
  RVecReduce{}  -> True
  RArrayLoad{}  -> True
  RPairMake{}   -> True
  RPairFst{}    -> True
  RPairSnd{}    -> True
  RArrayFree{}  -> False     -- side effect: frees heap memory
  _             -> False

-- | Recognize cached zero-argument value procedures that are safe to treat
-- like pure expressions for hoisting/unswitching purposes.
isZeroArgCall :: RHS -> Bool
isZeroArgCall (RCall _ []) = True
isZeroArgCall _ = False

-- | Count scalar assignments in a statement tree so hoisting/unswitching can
-- reject duplicated definitions.
countAssignedVars :: [Stmt] -> Map ByteString Int
countAssignedVars = foldr go M.empty
  where
    go stmt acc = case stmt of
      SAssign x _ -> M.insertWith (+) x 1 acc
      SLoop _ body -> foldr go acc body
      SIf _ thn els -> foldr go (foldr go acc thn) els
      _ -> acc

------------------------------------------------------------------------
-- Atom / RHS substitution
------------------------------------------------------------------------

-- | Substitute a variable in an 'Atom' according to a map.
-- For 'AVar', any binding in the map is applied.  For 'AVecVar', only
-- bindings that resolve to another 'AVecVar' are applied, preserving the
-- vector-variable invariant.
substAtom :: Map ByteString Atom -> Atom -> Atom
substAtom env a = case a of
  AVar    v -> M.findWithDefault a v env
  AVecVar v -> case M.lookup v env of
                 Just (AVecVar v') -> AVecVar v'
                 _                 -> a
  _         -> a

-- | Substitute all 'Atom' occurrences inside an 'RHS'.  Structural
-- information (operators, field names, type tags) is preserved; only
-- leaf variable references are replaced.
substRHS :: Map ByteString Atom -> RHS -> RHS
substRHS env = mapRHSAtoms (substAtom env)

-- | Apply a variable-to-atom substitution map throughout a list of
-- statements, replacing every 'AVar' leaf that appears in the map.
substStmts :: Map ByteString Atom -> [Stmt] -> [Stmt]
substStmts env = map go
  where
    go (SAssign v rhs)              = SAssign v (substRHS env rhs)
    go (SArrayWrite arr idx val)    =
      SArrayWrite (substAtom env arr) (substAtom env idx) (substAtom env val)
    go (SLoop spec body)            = SLoop spec (substStmts env body)
    go (SParallelRegion body)       = SParallelRegion (substStmts env body)
    go (SIf cond thn els)           =
      SIf (substAtom env cond) (substStmts env thn) (substStmts env els)
    go (SReturn a)                  = SReturn (substAtom env a)
    go s                            = s

-- | Substitute variables appearing in an 'IndexExpr' using the copy-prop
-- environment.  Integer atom bindings are folded to constants so loop bounds
-- can become static trip counts.
substIndexExpr :: Map ByteString Atom -> IndexExpr -> IndexExpr
substIndexExpr env ie =
  let go = substIndexExpr env
  in simplifyIndexExpr $ case ie of
       IVar v -> case M.lookup v env of
         Just (AInt n) -> IConst n
         Just (AVar v') -> IVar v'
         _ -> IVar v
       IConst n -> IConst n
       IAdd a b -> IAdd (go a) (go b)
       ISub a b -> ISub (go a) (go b)
       IMul a b -> IMul (go a) (go b)
       IDiv a b -> IDiv (go a) (go b)
       ITuple es -> ITuple (map go es)
       IProj i e -> IProj i (go e)
       IFlatToNd a b -> IFlatToNd (go a) (go b)
       INdToFlat a b -> INdToFlat (go a) (go b)
       ICall f args -> ICall f (map go args)

------------------------------------------------------------------------
-- Copy propagation
------------------------------------------------------------------------

-- | Forward copy-propagation pass over a statement list.  The pass
-- maintains a substitution map from variable names to atoms, built from
-- direct @x = a@ assignments.  Later uses of @x@ are rewritten to @a@.
--
-- The environment is conservatively invalidated at loop boundaries:
-- iterator variables and all variables defined inside the loop body are
-- evicted to avoid propagating loop-carried updates into or across loop
-- scopes.  Analogously, any variable assigned inside either branch of an
-- 'SIf' is evicted from the environment for the statements that follow.
--
-- Additionally, three CSE rules eliminate redundant shape/index operations:
--
-- * /Call CSE/: zero-argument value-proc calls (@f()@) are idempotent due to
--   the @__cache_*@ mechanism; a second call to the same proc is replaced with
--   a copy of the first result.
--
-- * /Shape CSE/: repeated @arr->shape@ reads for the same array pointer are
--   collapsed to a single read.
--
-- * /Roundtrip elimination/: @nd_to_flat(flat_to_nd(k, s), s) == k@ — when
--   both the @RFlatToNd@ and @RNdToFlat@ use the same shape atom, the
--   round-trip is replaced by the original flat index @k@.
copyProp :: [Stmt] -> [Stmt]
copyProp = go M.empty M.empty M.empty M.empty M.empty
  where
    -- env        : copy/constant propagation (variable → atom)
    -- tupEnv     : tuple-content tracking (variable → [atoms])
    -- callEnv    : call CSE (proc name → first-result variable)
    -- shapeEnv   : shape CSE (array variable → shape variable)
    -- flatToNdEnv: roundtrip tracking (nd-index variable → (flat atom, shape atom))
    go :: Map ByteString Atom
       -> Map ByteString [Atom]
       -> Map ByteString ByteString
       -> Map ByteString ByteString
       -> Map ByteString (Atom, Atom)
       -> [Stmt] -> [Stmt]
    go _   _        _       _        _           []             = []
    go env tupEnv callEnv shapeEnv flatToNdEnv (stmt : rest) = case stmt of
      SAssign x (RAtom a) ->
        let a' = substAtom env a
            (env0, tupEnv0, callEnv0, shapeEnv0, flatToNdEnv0) =
              killAssignedVar x env tupEnv callEnv shapeEnv flatToNdEnv
            env1
              | introducesCopyCycle x a' env0 = env0
              | otherwise = M.insert x a' env0
        in  SAssign x (RAtom a') : go env1 tupEnv0 callEnv0 shapeEnv0 flatToNdEnv0 rest

      -- Track RTuple assignments for downstream shape-folding.
      SAssign x (RTuple atoms) ->
        let atoms' = map (substAtom env) atoms
            (env0, tupEnv0, callEnv0, shapeEnv0, flatToNdEnv0) =
              killAssignedVar x env tupEnv callEnv shapeEnv flatToNdEnv
        in  SAssign x (RTuple atoms')
              : go env0 (M.insert x atoms' tupEnv0) callEnv0 shapeEnv0 flatToNdEnv0 rest

      -- Fold RShapeInit on a statically-known tuple: drop the last element.
      SAssign x (RShapeInit (AVar v))
        | Just atoms <- M.lookup v tupEnv
        , not (null atoms) ->
            let atoms' = init atoms
                (env0, tupEnv0, callEnv0, shapeEnv0, flatToNdEnv0) =
                  killAssignedVar x env tupEnv callEnv shapeEnv flatToNdEnv
            in  SAssign x (RTuple atoms')
                  : go env0 (M.insert x atoms' tupEnv0) callEnv0 shapeEnv0 flatToNdEnv0 rest

      -- Fold RShapeLast on a statically-known tuple: extract the last element.
      SAssign x (RShapeLast (AVar v))
        | Just atoms <- M.lookup v tupEnv
        , not (null atoms) ->
            let a' = last atoms
                (env0, tupEnv0, callEnv0, shapeEnv0, flatToNdEnv0) =
                  killAssignedVar x env tupEnv callEnv shapeEnv flatToNdEnv
            in  SAssign x (RAtom a')
                  : go (M.insert x a' env0) tupEnv0 callEnv0 shapeEnv0 flatToNdEnv0 rest

      -- Fold RShapeSize on a statically-known tuple: compute the product.
      SAssign x (RShapeSize (AVar v))
        | Just atoms <- M.lookup v tupEnv
        , Just ns    <- mapM atomToInt atoms ->
            let a' = AInt (product ns)
                (env0, tupEnv0, callEnv0, shapeEnv0, flatToNdEnv0) =
                  killAssignedVar x env tupEnv callEnv shapeEnv flatToNdEnv
            in  SAssign x (RAtom a')
                  : go (M.insert x a' env0) tupEnv0 callEnv0 shapeEnv0 flatToNdEnv0 rest

      -- Fold RProj on a statically-known tuple: extract the i-th element.
      SAssign x (RProj i (AVar v))
        | Just atoms <- M.lookup v tupEnv
        , fromIntegral i < length atoms ->
            let a' = atoms !! fromIntegral i
                (env0, tupEnv0, callEnv0, shapeEnv0, flatToNdEnv0) =
                  killAssignedVar x env tupEnv callEnv shapeEnv flatToNdEnv
            in  SAssign x (RAtom a')
                  : go (M.insert x a' env0) tupEnv0 callEnv0 shapeEnv0 flatToNdEnv0 rest

      -- Call CSE: zero-argument value procs are idempotent (cached).
      -- Replace a second call to the same proc with a copy of the first result.
      SAssign x (RCall proc []) ->
        let (env0, tupEnv0, callEnv0, shapeEnv0, flatToNdEnv0) =
              killAssignedVar x env tupEnv callEnv shapeEnv flatToNdEnv
        in case M.lookup proc callEnv0 of
          Just prevVar ->
             let a' = AVar prevVar
             in  SAssign x (RAtom a')
                  : go (M.insert x a' env0) tupEnv0 callEnv0 shapeEnv0 flatToNdEnv0 rest
          Nothing ->
             SAssign x (RCall proc [])
              : go env0 tupEnv0 (M.insert proc x callEnv0) shapeEnv0 flatToNdEnv0 rest

      -- Shape CSE: deduplicate arr->shape reads for the same array pointer.
      SAssign x (RArrayShape a) ->
        let a' = substAtom env a
            (env0, tupEnv0, callEnv0, shapeEnv0, flatToNdEnv0) =
              killAssignedVar x env tupEnv callEnv shapeEnv flatToNdEnv
        in  case a' of
              AVar v -> case M.lookup v shapeEnv of
                Just prevShp ->
                  let atom' = AVar prevShp
                  in  SAssign x (RAtom atom')
                        : go (M.insert x atom' env0) tupEnv0 callEnv0 shapeEnv0 flatToNdEnv0 rest
                Nothing ->
                  SAssign x (RArrayShape a')
                    : go env0 tupEnv0 callEnv0 (M.insert v x shapeEnv0) flatToNdEnv0 rest
              _ -> SAssign x (RArrayShape a')
                     : go env0 tupEnv0 callEnv0 shapeEnv0 flatToNdEnv0 rest

      -- Track flat→nd conversions for roundtrip elimination below.
      SAssign x (RFlatToNd ka sa) ->
        let ka' = substAtom env ka
            sa' = substAtom env sa
            (env0, tupEnv0, callEnv0, shapeEnv0, flatToNdEnv0) =
              killAssignedVar x env tupEnv callEnv shapeEnv flatToNdEnv
        in  SAssign x (RFlatToNd ka' sa')
              : go env0 tupEnv0 callEnv0 shapeEnv0 (M.insert x (ka', sa') flatToNdEnv0) rest

      -- Roundtrip elimination: nd_to_flat(flat_to_nd(k, s), s) == k.
      SAssign x (RNdToFlat a sa) ->
        let a'  = substAtom env a
            sa' = substAtom env sa
            (env0, tupEnv0, callEnv0, shapeEnv0, flatToNdEnv0) =
              killAssignedVar x env tupEnv callEnv shapeEnv flatToNdEnv
        in  case a' of
              AVar ndVar | Just (kAtom, s1) <- M.lookup ndVar flatToNdEnv
                         , s1 == sa' ->
                -- The nd-index was produced by flat_to_nd with the same shape:
                -- skip the round-trip and use the original flat index directly.
                SAssign x (RAtom kAtom)
                  : go (M.insert x kAtom env0) tupEnv0 callEnv0 shapeEnv0 flatToNdEnv0 rest
              _ ->
                SAssign x (RNdToFlat a' sa')
                  : go env0 tupEnv0 callEnv0 shapeEnv0 flatToNdEnv0 rest

      SAssign x rhs ->
        let (env0, tupEnv0, callEnv0, shapeEnv0, flatToNdEnv0) =
              killAssignedVar x env tupEnv callEnv shapeEnv flatToNdEnv
        in  SAssign x (substRHS env rhs) : go env0 tupEnv0 callEnv0 shapeEnv0 flatToNdEnv0 rest
      SArrayWrite a1 a2 a3 ->
        SArrayWrite (substAtom env a1) (substAtom env a2) (substAtom env a3)
          : go env tupEnv callEnv shapeEnv flatToNdEnv rest

      -- Single-iteration serial loop: substitute the iterator to 0 and
      -- inline the body into the surrounding statement sequence.
      -- LoopReductionWrapper loops are excluded here: they carry pattern
      -- information needed by the polyhedral blocked-matmul recogniser.
      -- GCC at -O3 eliminates single-iteration LoopReductionWrapper loops
      -- with no overhead after compilation.
      SLoop spec body
        | [IConst 1] <- lsBounds spec
        , [iter]     <- lsIters spec
        , lsExec spec == Serial
        , Nothing    <- lsRed spec
        , lsRole spec /= LoopReductionWrapper ->
            let env' = M.insert iter (AInt 0) env
            in go env' tupEnv callEnv shapeEnv flatToNdEnv (body ++ rest)

      SLoop spec body ->
        let spec' =
              spec
                { lsBounds = map (substIndexExpr env) (lsBounds spec)
                , lsRed = fmap (\r -> r { rsInit = substIndexExpr env (rsInit r) }) (lsRed spec)
                }
            defs     = S.toList (definedVarsStmts body)
            evicted  = lsIters spec ++ defs
            env'     = foldr M.delete env evicted
            tupEnv'  = foldr M.delete tupEnv evicted
            -- callEnv and shapeEnv are globally valid (immutable cached results
            -- and immutable array shapes), so they are not evicted at loops.
            -- flatToNdEnv tracks loop-local index vars; entries referencing
            -- evicted iter/def vars are stale but harmless: they only fire when
            -- the nd-index variable appears again, which cannot happen outside
            -- the loop since it was defined inside.
            body'    = go env' tupEnv' callEnv shapeEnv flatToNdEnv body
        in  SLoop spec' body' : go env' tupEnv' callEnv shapeEnv flatToNdEnv rest
      SIf cond thn els ->
        let cond'             = substAtom env cond
            thn'              = go env tupEnv callEnv shapeEnv flatToNdEnv thn
            els'              = go env tupEnv callEnv shapeEnv flatToNdEnv els
            definedInBranches = definedVarsStmts (thn ++ els)
            evicted           = S.toList definedInBranches
            env'              = foldr M.delete env evicted
            tupEnv'           = foldr M.delete tupEnv evicted
        in  SIf cond' thn' els' : go env' tupEnv' callEnv shapeEnv flatToNdEnv rest
      SReturn a ->
        SReturn (substAtom env a) : go env tupEnv callEnv shapeEnv flatToNdEnv rest
      SBreak ->
        SBreak : go env tupEnv callEnv shapeEnv flatToNdEnv rest

    atomToInt :: Atom -> Maybe Integer
    atomToInt (AInt n) = Just n
    atomToInt _        = Nothing

    killAssignedVar
      :: ByteString
      -> Map ByteString Atom
      -> Map ByteString [Atom]
      -> Map ByteString ByteString
      -> Map ByteString ByteString
      -> Map ByteString (Atom, Atom)
      -> ( Map ByteString Atom
         , Map ByteString [Atom]
         , Map ByteString ByteString
         , Map ByteString ByteString
         , Map ByteString (Atom, Atom)
         )
    killAssignedVar x env tupEnv callEnv shapeEnv flatToNdEnv =
      ( M.filter (not . atomDependsOn env x) (M.delete x env)
      , M.delete x tupEnv
      , M.filter (/= x) callEnv
      , M.filter (/= x) shapeEnv
      , M.delete x flatToNdEnv
      )

    atomDependsOn :: Map ByteString Atom -> ByteString -> Atom -> Bool
    atomDependsOn env0 target = depends S.empty
      where
        depends seen atom@(AVar v)
          | atomUsesTarget atom = True
          | v `S.member` seen = False
          | otherwise = case M.lookup v env0 of
              Just nextAtom -> depends (S.insert v seen) nextAtom
              Nothing -> False
        depends _ atom = atomUsesTarget atom

        atomUsesTarget :: Atom -> Bool
        atomUsesTarget atom = case atom of
          AVar v -> v == target
          _ -> False

    introducesCopyCycle :: ByteString -> Atom -> Map ByteString Atom -> Bool
    introducesCopyCycle x atom env = case atom of
      AVar y -> followsTo x S.empty y
      _ -> False
      where
        followsTo target seen v
          | v == target = True
          | v `S.member` seen = False
          | otherwise = case M.lookup v env of
              Just (AVar v') -> followsTo target (S.insert v seen) v'
              _ -> False

------------------------------------------------------------------------
-- Dead assignment elimination
------------------------------------------------------------------------

-- | Backwards liveness pass over a statement list.  Returns the filtered list
-- and the set of variables live /before/ the first statement.
--
-- Pure assignments to dead variables are discarded.  Impure assignments are
-- retained for their side effects, but the defined variable is not considered
-- live (it is defined here, not used later).
--
-- @loopCarried@ names variables that must stay live across iterations of the
-- enclosing loop (the next iteration reads them), regardless of apparent local
-- liveness.  It is empty for a straight-line list and non-empty only when
-- processing a loop body via the @SLoop@ case.  @liveAfter@ seeds the live set
-- at the end of the list.
dae :: Set ByteString -> Set ByteString -> [Stmt] -> ([Stmt], Set ByteString)
dae _           liveAfter [] = ([], liveAfter)
dae loopCarried liveAfter (stmt : rest) =
  let (rest', live) = dae loopCarried liveAfter rest
  in case stmt of
       SAssign x rhs
         | x `S.member` loopCarried ->
             -- Loop-carried: the next iteration reads this variable.
             (stmt : rest', S.unions [live, usedVarsRHS rhs, S.singleton x])
         | x `S.member` live ->
             (stmt : rest', live `S.union` usedVarsRHS rhs)
         | rhsIsPure rhs ->
             -- Dead pure assignment: safe to discard.
             (rest', live)
         | otherwise ->
             -- Dead impure assignment: retain for side effects but do not
             -- add the defined variable to the live set.
             (stmt : rest', live `S.union` usedVarsRHS rhs)
       SArrayWrite arr idx val ->
         ( stmt : rest'
         , S.unions [live, usedVarsAtom arr, usedVarsAtom idx, usedVarsAtom val]
         )
       SLoop spec body ->
         let iterVars  = S.fromList (lsIters spec)
             boundVars = S.unions (map usedVarsIndexExpr (lsBounds spec))
             carried   = usedVarsStmts body `S.intersection` definedVarsStmts body
             (body', _) = dae carried S.empty body
             bodyUsed   = usedVarsStmts body'
             -- Iterator variables are loop-local and must not leak into the
             -- surrounding live set.
             liveBefore = S.unions [live, bodyUsed, boundVars] `S.difference` iterVars
         in  (SLoop spec body' : rest', liveBefore)
       SParallelRegion body ->
         let (body', bodyLive) = dae S.empty live body
             liveBefore = live `S.union` bodyLive
         in (SParallelRegion body' : rest', liveBefore)
       SIf cond thn els ->
         -- Variables live after the 'SIf' are also live at the end of each
         -- branch, since control flow merges at the join point; loop-carried
         -- variables must likewise survive to the next iteration.
         let liveAtEnd       = live `S.union` loopCarried
             (thn', thnLive) = dae S.empty liveAtEnd thn
             (els', elsLive) = dae S.empty liveAtEnd els
             liveBefore      = S.unions [live, thnLive, elsLive, usedVarsAtom cond]
         in  (SIf cond thn' els' : rest', liveBefore)
       SReturn a ->
         (stmt : rest', usedVarsAtom a)
       SBreak ->
         (stmt : rest', live)

-- | Remove dead pure assignments from a statement list.
deadAssignElim :: [Stmt] -> [Stmt]
deadAssignElim stmts = fst (dae S.empty S.empty stmts)

------------------------------------------------------------------------
-- Loop-invariant code motion (LICM)
------------------------------------------------------------------------

-- | Hoist pure loop-invariant assignments to the loop pre-header.
-- A statement qualifies for hoisting when:
--
-- * it is a pure 'SAssign',
-- * the assigned variable is not an iterator or otherwise defined in the
--   loop body (so hoisting preserves uniqueness of definition), and
-- * its RHS does not use any variable defined inside the loop.
--
-- The pass recurses into nested loops and conditionals before hoisting
-- from the outer loop, so inner invariants are exposed first.
loopInvariantCodeMotion :: [Stmt] -> [Stmt]
loopInvariantCodeMotion = concatMap goStmt
  where
    goStmt :: Stmt -> [Stmt]
    goStmt (SLoop spec body) =
      let body'    = loopInvariantCodeMotion body
          iterDefs = S.fromList (lsIters spec)
          bodyDefs = definedVarsStmts body'
          loopDeps = iterDefs <> bodyDefs
          body'' = concatMap (hoistCommonBranchPrefix loopDeps) body'
          defCounts = countAssignedVars body''
          (hoisted, kept) = partition (isHoistable iterDefs loopDeps defCounts) body''
      in  hoisted ++ [SLoop spec kept]
    goStmt (SIf cond thn els) =
      [SIf cond (loopInvariantCodeMotion thn) (loopInvariantCodeMotion els)]
    goStmt stmt = [stmt]

    isHoistable :: Set ByteString -> Set ByteString -> Map ByteString Int -> Stmt -> Bool
    isHoistable iterDefs loopDeps defCounts (SAssign x rhs) =
      not (x `S.member` iterDefs)
      && M.findWithDefault 0 x defCounts == 1
      && (rhsIsPure rhs || isZeroArgCall rhs)
      && S.null (usedVarsRHS rhs `S.intersection` loopDeps)
    isHoistable _ _ _ _ = False

    hoistCommonBranchPrefix :: Set ByteString -> Stmt -> [Stmt]
    hoistCommonBranchPrefix loopDeps stmt = case stmt of
      SIf cond thn els ->
        let (prefix, thn', els') = commonHoistablePrefix thn els
        in prefix ++ [SIf cond thn' els']
      _ -> [stmt]
      where
        commonHoistablePrefix :: [Stmt] -> [Stmt] -> ([Stmt], [Stmt], [Stmt])
        commonHoistablePrefix (a:as) (b:bs)
          | a == b && isBranchHoistable loopDeps a =
              let (rest, as', bs') = commonHoistablePrefix as bs
              in (a : rest, as', bs')
        commonHoistablePrefix as bs = ([], as, bs)

    isBranchHoistable :: Set ByteString -> Stmt -> Bool
    isBranchHoistable loopDeps (SAssign _ rhs) =
      (rhsIsPure rhs || isZeroArgCall rhs)
      && S.null (usedVarsRHS rhs `S.intersection` loopDeps)
    isBranchHoistable _ _ = False

------------------------------------------------------------------------
-- Loop-invariant conditional unswitching
------------------------------------------------------------------------

-- | Hoist top-level loop-invariant conditionals out of @LoopIterate@ bodies.
--
-- This targets temporal kernels whose loop body starts with shape-derived
-- case splits. When the setup assignments and conditional depend only on
-- loop-invariant values, the pass hoists the setup and duplicates the
-- iterate loop per branch so later passes can see branch-free loop bodies.
-- Nested case chains are handled by the surrounding optimize-to-fixpoint
-- driver, which can expose and hoist the next conditional on a later pass.
unswitchLoopInvariantIf :: [Stmt] -> [Stmt]
unswitchLoopInvariantIf = concatMap goStmt
  where
    goStmt :: Stmt -> [Stmt]
    goStmt (SLoop spec body) =
      let body' = unswitchLoopInvariantIf body
      in case unswitchIterateInvariantIf spec body' of
           Just replacement -> replacement
           Nothing -> [SLoop spec body']
    goStmt (SIf cond thn els) =
      [SIf cond (unswitchLoopInvariantIf thn) (unswitchLoopInvariantIf els)]
    goStmt stmt = [stmt]

    unswitchIterateInvariantIf :: LoopSpec -> [Stmt] -> Maybe [Stmt]
    unswitchIterateInvariantIf spec body = do
      guard (lsRole spec == LoopIterate)
      (hoisted, cond, rest, thn, els, suffix) <- splitInvariantIf spec body
      let mkBranch branch = [SLoop spec (rest ++ branch ++ suffix)]
      pure (hoisted ++ [SIf cond (mkBranch thn) (mkBranch els)])

    -- Find the first top-level @SIf@ whose condition depends only on
    -- loop-invariant pure definitions. The condition's backward slice is
    -- hoisted before the loop (it computes the same values every
    -- iteration); statements before the @SIf@ that are not in the slice —
    -- including whole loop nests — stay inside both unswitched copies.
    splitInvariantIf
      :: LoopSpec -> [Stmt] -> Maybe ([Stmt], Atom, [Stmt], [Stmt], [Stmt], [Stmt])
    splitInvariantIf spec body = do
      (before, cond, thn, els, suffix) <- firstTopLevelIf [] body
      (sliceIdx, needed) <-
        condSlice (usedVarsAtom cond) S.empty (reverse (zip [0 :: Int ..] before))
      -- Every input the slice does not itself compute must come from
      -- outside the loop body (in particular not from the tail swaps or
      -- either branch), otherwise the condition is not invariant.
      guard (S.null (needed `S.intersection` (bodyDefs `S.union` iterDefs)))
      let inSlice i = i `S.member` sliceIdx
          hoisted = [s | (i, s) <- zip [0 ..] before, inSlice i]
          rest = [s | (i, s) <- zip [0 ..] before, not (inSlice i)]
      pure (hoisted, cond, rest, thn, els, suffix)
      where
        iterDefs = S.fromList (lsIters spec)
        bodyDefs = definedVarsStmts body
        defCounts = countAssignedVars body

        firstTopLevelIf acc stmts = case stmts of
          [] -> Nothing
          SIf cond thn els : suffix -> Just (reverse acc, cond, thn, els, suffix)
          s : rest -> firstTopLevelIf (s : acc) rest

        -- Walk backwards collecting the defining statements of the needed
        -- variables. A needed variable defined by anything other than a
        -- single pure assignment kills the unswitch.
        condSlice needed sliceIdx revBefore = case revBefore of
          [] -> Just (sliceIdx, needed)
          (i, stmt) : rest
            | S.null (definedVarsStmts [stmt] `S.intersection` needed) ->
                condSlice needed sliceIdx rest
            | SAssign x rhs <- stmt
            , not (x `S.member` iterDefs)
            , M.findWithDefault 0 x defCounts == 1
            , rhsIsPure rhs || isZeroArgCall rhs ->
                condSlice
                  (S.delete x needed `S.union` usedVarsRHS rhs)
                  (S.insert i sliceIdx)
                  rest
            | otherwise ->
                Nothing

------------------------------------------------------------------------
-- No-op cleanup
------------------------------------------------------------------------


------------------------------------------------------------------------
-- Iterate-loop allocation hoisting
------------------------------------------------------------------------

-- | Hoist @RArrayAlloc@ out of @LoopIterate@ bodies, replacing the
-- allocate-each-iteration pattern with a pre-allocated ping-pong between
-- two buffers.  The pattern detected is:
--
-- > for iter_t:
-- >   arr_next = alloc(shp)
-- >   shp = shape(arr_cur)
-- >   ...compute...
-- >   arr_cur = arr_next
--
-- which is rewritten to:
--
-- > shp = shape(arr_cur)
-- > arr_next = alloc(shp)
-- > tmp = arr_cur
-- > for iter_t:
-- >   ...compute...
-- >   arr_cur = arr_next
-- >   arr_next = tmp
-- >   tmp = arr_cur
--
-- This avoids re-allocating every temporal iteration.

-- | Split a @LoopIterate@ whose loop-carried state is a (possibly nested)
-- pair into one carried variable per component, so downstream passes (and in
-- particular polyhedral temporal-alias detection) see a plain @cur = next@
-- swap for every component array. Coupled-field steps (e.g. FDTD-style e/h
-- updates) lower to this shape:
--
-- >   cur = <pair init>                     cur = <pair init>
-- >   for iter_t:                           tf = fst cur
-- >     tf = fst cur                        ts = snd cur
-- >     ts = snd cur               =>       for iter_t:
-- >     ...compute x, y...                    ...compute x, y...
-- >     p = pair x y                          tf = x
-- >     cur = p                               ts = y
-- >                                         cur = pair tf ts
--
-- The extraction position inside the body does not matter: @cur@ is only
-- written at the tail, so the components are loop-invariant within one
-- iteration. Fires only when @cur@ and @p@ have no other reads or writes
-- in the body.
--
-- Nested-pair state (three or more carried arrays, e.g. FDTD-2D-style
-- triples) is split recursively: a pair level is descended when its old
-- value is decomposed by exactly one @fst@ and one @snd@ extraction and its
-- new value is built by a single-assignment 'RPairMake'; any level that
-- fails this stays a single opaque carried leaf.
scalarizeIteratePairs :: [Stmt] -> [Stmt]
scalarizeIteratePairs = go
  where
    go [] = []
    go (SLoop spec body : rest)
      | lsRole spec == LoopIterate
      , Just (pre, loop', post) <- splitPairState spec (scalarizeIteratePairs body) =
          pre ++ loop' : post ++ go rest
      | otherwise = SLoop spec (scalarizeIteratePairs body) : go rest
    go (SIf c t e : rest) =
      SIf c (scalarizeIteratePairs t) (scalarizeIteratePairs e) : go rest
    go (s : rest) = s : go rest

    splitPairState spec body = do
      (SAssign cur (RAtom (AVar root)) : revMid) <- Just (reverse body)
      let mid0 = reverse revMid
          defCounts = countAssignedVars mid0
          singleDef v = M.findWithDefault (0 :: Int) v defCounts == 1
          -- Grow the carried-state tree keyed on each level's old-value var
          -- and new-value atom.  Returns (new-value atom for the parent
          -- rebuild, extraction stmts top-down, rebuild stmts bottom-up,
          -- carried leaves in order).  @seen@ breaks descent cycles on
          -- malformed input.
          grow seen evar atom
            | AVar m <- atom
            , m `S.notMember` seen
            , singleDef m
            , [(tagA, tagB, a, b)] <-
                [(tA, tB, x, y) | SAssign v (RPairMake tA tB x y) <- mid0, v == m]
            , [(vf, tagF)] <- [(v, t) | SAssign v (RPairFst t (AVar c)) <- mid0, c == evar]
            , [(vs, tagS)] <- [(v, t) | SAssign v (RPairSnd t (AVar c)) <- mid0, c == evar]
            , let seen' = S.insert m seen
            , (aAtom, aExt, aReb, aLeaves) <- grow seen' vf a
            , (bAtom, bExt, bReb, bLeaves) <- grow seen' vs b =
                ( AVar m
                , SAssign vf (RPairFst tagF (AVar evar))
                    : SAssign vs (RPairSnd tagS (AVar evar))
                    : aExt ++ bExt
                , aReb ++ bReb ++ [SAssign m (RPairMake tagA tagB aAtom bAtom)]
                , aLeaves ++ bLeaves
                )
            | otherwise = (AVar evar, [], [], [(evar, atom)])
          (_, extracts, rebuilds, leaves) = grow S.empty cur (AVar root)
      guard (not (null extracts))
      let extractVars = [v | SAssign v _ <- extracts]
          makeVars = [v | SAssign v _ <- rebuilds]
          leafVars = map fst leaves
          interiorVars = filter (`notElem` leafVars) extractVars
          deadVars = cur : makeVars ++ interiorVars
          names = cur : extractVars ++ makeVars
      guard (length names == S.size (S.fromList names))
      let isRemoved s = case s of
            SAssign v RPairMake {} -> v `elem` makeVars
            SAssign _ RPairFst {} -> s `elem` extracts
            SAssign _ RPairSnd {} -> s `elem` extracts
            _ -> False
          mid = filter (not . isRemoved) mid0
          midUsed = usedVarsStmts mid
          midDefined = definedVarsStmts mid
      guard (all (`S.notMember` midUsed) deadVars)
      guard (all (`S.notMember` midDefined) (deadVars ++ leafVars))
      -- The tail assigns the leaves sequentially, so a leaf's new value may
      -- not read another leaf's carried var (a rotation would see the new
      -- epoch instead of the old), nor anything that dies with the pair
      -- structure.
      let badLeafAtom (lv, a) =
            any
              (\v -> v `elem` deadVars || (v /= lv && v `elem` leafVars))
              (S.toList (usedVarsAtom a))
      guard (not (any badLeafAtom leaves))
      -- Shapes of the carried arrays are iterate-invariant (an iterate step
      -- preserves its state shape — the same assumption the single-array
      -- ping-pong hoist makes), so their reads can move before the loop,
      -- where invariant-branch unswitching can reach the boundary guards
      -- computed from them.
      let isCarriedShape s = case s of
            SAssign sv (RArrayShape (AVar v)) ->
              v `elem` leafVars && singleDef sv
            _ -> False
          (shapeStmts, midRest) = partition isCarriedShape mid
          -- The root level rebuilds straight into @cur@; @root@ dies.
          post = case reverse rebuilds of
            SAssign _ rootMake : restReb -> reverse restReb ++ [SAssign cur rootMake]
            [] -> []
      pure
        ( extracts ++ shapeStmts
        , SLoop spec (midRest ++ [SAssign lv (RAtom a) | (lv, a) <- leaves])
        , post
        )

hoistIterateAllocs :: [Stmt] -> [Stmt]
hoistIterateAllocs = go
  where
    go :: [Stmt] -> [Stmt]
    go [] = []
    go (SLoop spec body : rest)
      | lsRole spec == LoopIterate =
          case restructureIterateBody spec body of
            Just replacement -> replacement ++ go rest
            Nothing -> SLoop spec (hoistIterateAllocs body) : go rest
      | otherwise = SLoop spec (hoistIterateAllocs body) : go rest
    go (SIf c t e : rest) =
      SIf c (hoistIterateAllocs t) (hoistIterateAllocs e) : go rest
    go (s : rest) = s : go rest

-- | Given a @LoopIterate@ body, try to hoist the initial @alloc+shape@
-- out and replace the epilogue swap with a ping-pong three-way swap over
-- two private copies of the init buffer. The init buffer itself is never
-- written or freed — it may be shared (cached proc results, other
-- consumers). Post-loop, the private buffer(s) the result did not land in
-- are freed. Returns the full replacement statement list.
--
-- The expected body ends with a swap:
--
-- > ...compute...
-- > SAssign cur (RAtom (AVar next))
--
-- And contains at most one @RArrayAlloc@ and one @RArrayShape@ whose
-- variables match the swap.  Those two stmts are hoisted before the loop.
restructureIterateBody :: LoopSpec -> [Stmt] -> Maybe [Stmt]
restructureIterateBody spec body = do
  guard (length body >= 2)
  -- The last stmt must be the swap: SAssign cur (RAtom (AVar next))
  let swapStmt = last body
  (curVar, nextVar) <- case swapStmt of
    SAssign cur (RAtom (AVar next)) -> Just (cur, next)
    _ -> Nothing
  -- A scalarized multi-array iterate (see 'scalarizeIteratePairs') ends with
  -- one swap per component; the single-buffer ping-pong below must not adopt
  -- just the last one.
  guard $ case drop 1 (reverse body) of
    SAssign _ (RAtom (AVar _)) : _ -> False
    _ -> True

  -- Collect the stmts that precede the swap
  let prefix = init body

  -- Find the shape stmt in the prefix
  let shapeCandidates = [(i, sv, av) | (i, SAssign sv (RArrayShape (AVar av))) <- zip [0..] prefix]
      allocCandidates = [(i, av) | (i, SAssign av (RArrayAlloc _)) <- zip [0..] prefix]

  -- Shared helper: given a set of stmt indices to skip (alloc + optionally
  -- shape) and a list of stmts to hoist before the loop, build the
  -- restructured program.
  let mkResult skip hoistedExtra =
        let computeBody = [s | (i, s) <- zip [0..] prefix, i `S.notMember` skip]
            tmpVar = nextVar <> "__iter_tmp"
            initTrackerVar = curVar <> "__iter_init_track"
            nextNotCurVar = nextVar <> "__iter_cleanup_not_cur"
            nextNotInitVar = nextVar <> "__iter_cleanup_not_init"
            nextFreeVar = nextVar <> "__iter_cleanup_cond"
            tmpNotCurVar = tmpVar <> "__iter_cleanup_not_cur"
            tmpNotInitVar = tmpVar <> "__iter_cleanup_not_init"
            tmpFreeVar = tmpVar <> "__iter_cleanup_cond"
            -- The tmp buffer is a private copy, NOT an alias of the init
            -- buffer: the init array may be shared (cached proc result, other
            -- consumers), so the ping-pong must never write into or free it.
            initSave = SAssign tmpVar (RArrayCopy (AVar curVar))
            -- Track the init pointer so downstream cleanup (here and in the
            -- wavefront transform, which rotates the init pointer through the
            -- ring variables) can avoid freeing it: the proc-level cleanup
            -- frees it when it owns it.
            initTracker = SAssign initTrackerVar (RAtom (AVar curVar))
            pingSwap =
              [ SAssign curVar (RAtom (AVar nextVar))
              , SAssign nextVar (RAtom (AVar tmpVar))
              , SAssign tmpVar (RAtom (AVar curVar))
              ]
            -- Free whichever private buffers the result did not land in.
            -- The init buffer is never freed here: ownership is unknown, so
            -- it leaks (once per call site) when it was a genuinely fresh
            -- temp. ponytail: leak; free-if-afFreshAlloc if it ever matters.
            condFree =
              [ SAssign nextNotCurVar (RBinOp CNeq (AVar nextVar) (AVar curVar))
              , SAssign nextNotInitVar (RBinOp CNeq (AVar nextVar) (AVar initTrackerVar))
              , SAssign nextFreeVar (RBinOp CAnd (AVar nextNotCurVar) (AVar nextNotInitVar))
              , SIf (AVar nextFreeVar) [SAssign "__hyd_discard" (RArrayFree (AVar nextVar))] []
              , SAssign tmpNotCurVar (RBinOp CNeq (AVar tmpVar) (AVar curVar))
              , SAssign tmpNotInitVar (RBinOp CNeq (AVar tmpVar) (AVar initTrackerVar))
              , SAssign tmpFreeVar (RBinOp CAnd (AVar tmpNotCurVar) (AVar tmpNotInitVar))
              , SIf (AVar tmpFreeVar) [SAssign "__hyd_discard" (RArrayFree (AVar tmpVar))] []
              ]
            newBody = computeBody ++ pingSwap
            newLoop = SLoop spec newBody
        in Just (hoistedExtra ++ [initSave, initTracker, newLoop] ++ condFree)

  -- Need at least one of shape or alloc to match the swap variables
  case (listToMaybe shapeCandidates, listToMaybe allocCandidates) of
    (Just (shapeIdx, shpVar, shapeArrVar), Just (allocIdx, allocVar)) -> do
      guard (shapeArrVar == curVar)   -- shape reads from the cur variable
      guard (allocVar == nextVar)     -- alloc writes to the next variable
      let shapeStmt = SAssign shpVar (RArrayShape (AVar curVar))
          allocStmt = SAssign nextVar (RArrayCopy (AVar curVar))
          skip = S.fromList [shapeIdx, allocIdx]
      mkResult skip [shapeStmt, allocStmt]

    -- Fallback: only the alloc matches (shape is pre-computed outside loop).
    -- Hoist just a copy; the original alloc stmt is replaced.
    (_, Just (allocIdx, allocVar))
      | allocVar == nextVar ->
          mkResult (S.singleton allocIdx) [SAssign nextVar (RArrayCopy (AVar curVar))]

    _ -> Nothing

------------------------------------------------------------------------
-- Common subexpression elimination (CSE)
------------------------------------------------------------------------

-- | An entry in the CSE table: the variable that first computed a given
-- canonical expression, together with its free variables (used to
-- invalidate the entry when any dependency is redefined).
data CSEEntry = CSEEntry
  { cseVar  :: ByteString
  , cseDeps :: Set ByteString
  }

type CSEEnv = Map ByteString CSEEntry

-- | Returns 'True' for commutative 'BinOp's.  Commutativity enables
-- canonical argument ordering so that @a + b@ and @b + a@ are
-- recognized as the same expression by the CSE key function.
commutativeBinOp :: BinOp -> Bool
commutativeBinOp op = case op of
  CAdd  -> True
  CMul  -> True
  CEq   -> True
  CNeq  -> True
  CAnd  -> True
  COr   -> True
  CAddF -> True
  CMulF -> True
  CEqF  -> True
  CNeqF -> True
  _     -> False

-- | Sort a pair of atoms into a deterministic canonical order using their
-- 'show' representations.  Used to normalize commutative binary operations
-- so that @f(a,b)@ and @f(b,a)@ produce the same CSE key.
canonicalAtomPair :: Atom -> Atom -> (Atom, Atom)
canonicalAtomPair a b
  | show a <= show b = (a, b)
  | otherwise        = (b, a)

-- | Reorder arguments of commutative binary operations into a canonical
-- form.  All other RHS forms are returned unchanged.
canonicalizeRHS :: RHS -> RHS
canonicalizeRHS rhs = case rhs of
  RBinOp op a b | commutativeBinOp op ->
    let (a', b') = canonicalAtomPair a b in RBinOp op a' b'
  RVecBinOp op a b | commutativeBinOp op ->
    let (a', b') = canonicalAtomPair a b in RVecBinOp op a' b'
  _ -> rhs

-- | Compute the CSE lookup key for a pure 'RHS' after canonicalization,
-- or 'Nothing' if the RHS is not eligible (e.g. has side effects).
rhsCSEKey :: RHS -> Maybe ByteString
rhsCSEKey rhs
  | rhsIsPure rhs = Just (BS.pack (show (canonicalizeRHS rhs)))
  | otherwise      = Nothing

-- | Remove @v@'s entry from the CSE environment, along with any entry
-- that mentions @v@ as a dependency.  Called when @v@ is redefined.
invalidateVarCSE :: ByteString -> CSEEnv -> CSEEnv
invalidateVarCSE v =
  M.filter (\e -> cseVar e /= v && not (v `S.member` cseDeps e))

-- | 'invalidateVarCSE' lifted over a list of variables.
invalidateVarsCSE :: [ByteString] -> CSEEnv -> CSEEnv
invalidateVarsCSE vars env = foldr invalidateVarCSE env vars

-- | Eliminate repeated pure computations by replacing them with a
-- reference to the first variable that computed the same canonical
-- expression.
--
-- The pass is conservative around control flow: branch-defined variables
-- are evicted from the post-branch environment.  Any memory write
-- ('SArrayWrite') flushes the entire CSE environment, since without
-- alias information we cannot determine which cached array loads remain
-- valid.
cseStmts :: [Stmt] -> [Stmt]
cseStmts = go M.empty
  where
    go :: CSEEnv -> [Stmt] -> [Stmt]
    go _   []             = []
    go env (stmt : rest) = case stmt of
      SAssign x rhs ->
        let rhs'       = canonicalizeRHS rhs
            envCleared = invalidateVarCSE x env
        in case rhsCSEKey rhs' of
             Just key
               | Just entry <- M.lookup key env ->
                   SAssign x (RAtom (AVar (cseVar entry))) : go envCleared rest
               | otherwise ->
                   let env' = M.insert key (CSEEntry x (usedVarsRHS rhs')) envCleared
                   in  SAssign x rhs' : go env' rest
             Nothing ->
               SAssign x rhs' : go envCleared rest
      SArrayWrite arr idx val ->
        -- Any array write may alias with a cached array load result, so
        -- flush the entire CSE environment conservatively.
        SArrayWrite arr idx val : go M.empty rest
      SLoop spec body ->
        let loopVars = lsIters spec ++ S.toList (definedVarsStmts body)
            envLoop  = invalidateVarsCSE loopVars env
            body'    = go envLoop body
        in  SLoop spec body' : go envLoop rest
      SParallelRegion body ->
        let body' = go env body
            env' = invalidateVarsCSE (S.toList (definedVarsStmts body)) env
        in SParallelRegion body' : go env' rest
      SIf cond thn els ->
        let thn'              = go env thn
            els'              = go env els
            definedInBranches = S.toList (definedVarsStmts (thn ++ els))
            env'              = invalidateVarsCSE definedInBranches env
        in  SIf cond thn' els' : go env' rest
      SReturn a ->
        SReturn a : go env rest
      SBreak ->
        SBreak : go env rest

------------------------------------------------------------------------
-- Scalar 0-D array roundtrip elimination
------------------------------------------------------------------------

-- | Eliminate lowered scalar 0-D array materialization roundtrips of the form:
--
-- @
-- out_shp = ()
-- out_arr = alloc(out_shp)
-- ... compute writeVal ...
-- out_arr[0] = writeVal
-- shp = shape(out_arr)
-- off = 0               -- or nd_to_flat((), shp)
-- val = out_arr[off]
-- @
--
-- and rewrite to:
--
-- @
-- ... compute writeVal ...
-- val = writeVal
-- @
--
-- This pass is conservative: it only rewrites when @out_arr@ is not otherwise
-- referenced between alloc and load except by the matched write/shape/load
-- chain.
scalarizeZeroDimArrayRoundtrips :: [Stmt] -> [Stmt]
scalarizeZeroDimArrayRoundtrips = rewriteBlock
  where
    rewriteBlock :: [Stmt] -> [Stmt]
    rewriteBlock = go M.empty
      where
        go _ [] = []
        go tupEnv (stmt : rest) =
          case stmt of
            SAssign v (RTuple atoms) ->
              SAssign v (RTuple atoms) : go (M.insert v atoms tupEnv) rest
            SLoop spec body ->
              let body' = rewriteBlock body
               in SLoop spec body' : go tupEnv rest
            SParallelRegion body ->
              let body' = rewriteBlock body
               in SParallelRegion body' : go tupEnv rest
            SIf cond thn els ->
              let thn' = rewriteBlock thn
                  els' = rewriteBlock els
               in SIf cond thn' els' : go tupEnv rest
            SAssign arr (RArrayAlloc (AVar shpVar))
              | Just [] <- M.lookup shpVar tupEnv
              , Just (prefix, tail') <- rewriteZeroDimUse arr rest ->
                  prefix ++ go tupEnv tail'
            _ ->
              stmt : go tupEnv rest

    rewriteZeroDimUse :: ByteString -> [Stmt] -> Maybe ([Stmt], [Stmt])
    rewriteZeroDimUse arr stmts =
      findPattern [] stmts
      where
        findPattern _ [] = Nothing
        findPattern accPrefix (s1 : s2 : s3 : s4 : suffix)
          | SArrayWrite (AVar arrW) writeIdx writeVal <- s1
          , arrW == arr
          , isZeroIndex writeIdx
          , SAssign shpVar (RArrayShape (AVar arrShape)) <- s2
          , arrShape == arr
          , SAssign offVar offRhs <- s3
          , isZeroOffset offRhs shpVar
          , SAssign valVar (RArrayLoad (AVar arrLoad) loadIdx) <- s4
          , arrLoad == arr
          , isMatchingLoadIdx loadIdx offVar
          , arr `S.notMember` usedVarsStmts accPrefix
          , not (arr `S.member` definedVarsStmts accPrefix)
          = Just (accPrefix ++ [SAssign valVar (RAtom writeVal)], suffix)
        findPattern accPrefix (x : xs) =
          if arr `S.member` usedVarsStmts [x] || arr `S.member` definedVarsStmts [x]
            then Nothing
            else findPattern (accPrefix ++ [x]) xs

    isZeroIndex :: Atom -> Bool
    isZeroIndex a = case a of
      AInt 0 -> True
      AUnit -> True
      _ -> False

    isZeroOffset :: RHS -> ByteString -> Bool
    isZeroOffset rhs shpVar = case rhs of
      RAtom (AInt 0) -> True
      RNdToFlat idx (AVar shp') -> shp' == shpVar && isZeroIndex idx
      _ -> False

    isMatchingLoadIdx :: Atom -> ByteString -> Bool
    isMatchingLoadIdx idx offVar = case idx of
      AVar v -> v == offVar
      AInt 0 -> True
      _ -> False

------------------------------------------------------------------------
-- Sink pure assignments inside a trailing guarded SIf
------------------------------------------------------------------------

-- | When a loop body ends with @SIf cond thn []@ (a guarded section
-- with empty else branch — the structure produced by
-- @scatter_guarded@ and similar fused guarded scatters), sink pure
-- 'SAssign' statements that do not contribute to evaluating @cond@
-- (and are not needed by any other prefix stmt) into the @thn@
-- branch.  Stmts that the guard does not depend on become wasted work
-- whenever @cond@ is false; sinking them past the guard skips that
-- work on the @!cond@ path.
--
-- Soundness: backward dataflow over the prefix maintains a
-- @keptUses@ set seeded with the SIf cond's used vars.  A pure
-- 'SAssign' whose assigned var is not in @keptUses@ is sinkable;
-- anything else (impure RHS, side-effecting stmts, nested
-- conditionals, or pure assigns whose var is referenced by something
-- already kept) stays in the outer prefix and contributes its uses
-- to @keptUses@.  The SIf must be the last statement of the loop
-- body so sunk vars cannot be referenced after the guard.
sinkUnusedIntoGuard :: [Stmt] -> [Stmt]
sinkUnusedIntoGuard = concatMap goStmt
  where
    goStmt :: Stmt -> [Stmt]
    goStmt (SLoop spec body) =
      let body' = sinkUnusedIntoGuard body
      in [SLoop spec (sinkInBody body')]
    goStmt (SIf cond thn els) =
      [SIf cond (sinkUnusedIntoGuard thn) (sinkUnusedIntoGuard els)]
    goStmt (SParallelRegion body) =
      [SParallelRegion (sinkUnusedIntoGuard body)]
    goStmt s = [s]

    sinkInBody :: [Stmt] -> [Stmt]
    sinkInBody body = case unsnoc body of
      Just (prefix, SIf cond thn []) ->
        let (toSink, toKeep) = backwardSinkPass cond prefix
        in if null toSink
             then body
             else toKeep ++ [SIf cond (toSink ++ thn) []]
      _ -> body

    -- Backward dataflow over the prefix (right-to-left).  See haddock
    -- on 'sinkUnusedIntoGuard' above for the soundness argument.
    backwardSinkPass :: Atom -> [Stmt] -> ([Stmt], [Stmt])
    backwardSinkPass cond prefix =
      let (sink, keep, _) = foldr stepFwd ([], [], usedVarsAtom cond) prefix
      in (sink, keep)
      where
        stepFwd stmt (sinkAcc, keepAcc, keptUses) = case stmt of
          SAssign v rhs
            | rhsIsPure rhs, not (v `S.member` keptUses) ->
                (stmt : sinkAcc, keepAcc, keptUses)
            | otherwise ->
                (sinkAcc, stmt : keepAcc, keptUses `S.union` usedVarsRHS rhs)
          _ ->
            -- usedVarsStmt counts variables referenced in a loop's bounds, so a
            -- statement whose bound reads a var keeps that var live.
            (sinkAcc, stmt : keepAcc, keptUses `S.union` usedVarsStmt stmt)

------------------------------------------------------------------------
-- Combined optimization pipeline
------------------------------------------------------------------------

-- | One iteration of the scalar optimization pipeline:
-- copy propagation → CSE → copy propagation → dead assignment
-- elimination → loop-invariant code motion.
--
-- Running copy propagation both before and after CSE helps CSE see
-- uniform variable names and allows copy-prop to clean up the
-- CSE-introduced @x = y@ assignments.
optimizeOnce :: [Stmt] -> [Stmt]
optimizeOnce =
    loopInvariantCodeMotion
  . sinkUnusedIntoGuard
  . deadAssignElim
  . scalarizeZeroDimArrayRoundtrips
  . copyProp
  . cseStmts
  . copyProp
  . foldShapeOfAlloc

-- | Fold @shape(a)@ back to the shape an array was allocated with:
-- @a = alloc(shp); ...; s = shape(a)@ becomes @s = shp@ when neither @a@
-- nor the shape variable is reassigned in between. Boundary-guard
-- conditions on freshly allocated buffers become expressible in terms of
-- the original shape, which downstream invariant-branch unswitching needs.
--
-- Applied only inside @LoopIterate@ bodies: some general-path passes and
-- the backward type recovery pattern-match on @RArrayShape@ syntactically,
-- so folding it program-wide regresses them, and the iterate bodies are
-- where the unswitching needs it.
foldShapeOfAlloc :: [Stmt] -> [Stmt]
foldShapeOfAlloc = map top
  where
    top stmt = case stmt of
      SLoop spec body
        | lsRole spec == LoopIterate -> SLoop spec (fst (go M.empty body))
        | otherwise -> SLoop spec (map top body)
      SIf c t e -> SIf c (map top t) (map top e)
      _ -> stmt
    go :: Map ByteString Atom -> [Stmt] -> ([Stmt], Map ByteString Atom)
    go env [] = ([], env)
    go env (stmt : rest) = case stmt of
      SAssign v (RArrayShape (AVar a))
        | Just shp <- M.lookup a env ->
            let (rest', envOut) = go (invalidate v env) rest
            in (SAssign v (RAtom shp) : rest', envOut)
      SAssign v rhs ->
        let envInval = invalidate v env
            env' = case rhs of
              RArrayAlloc shp -> M.insert v shp envInval
              _ -> envInval
            (rest', envOut) = go env' rest
        in (stmt : rest', envOut)
      SLoop spec body ->
        -- The body may reassign tracked vars on later iterations, so it is
        -- processed with an empty environment; afterwards its assignments
        -- are invalidated in the outer environment. Element writes are fine:
        -- they never change an array's shape.
        let (body', _) = go M.empty body
            envAfter = foldr invalidate env (M.keys (countAssignedVars body))
            (rest', envOut) = go envAfter rest
        in (SLoop spec body' : rest', envOut)
      SIf c t e ->
        let (t', _) = go env t
            (e', _) = go env e
            assigns = M.keys (countAssignedVars (t ++ e))
            envAfter = foldr invalidate env assigns
            (rest', envOut) = go envAfter rest
        in (SIf c t' e' : rest', envOut)
      _ ->
        let (rest', envOut) = go env rest
        in (stmt : rest', envOut)

    invalidate v =
      M.filterWithKey (\k shp -> k /= v && not (v `S.member` usedVarsAtom shp))

-- | Run 'optimizeOnce' to a fixpoint (or until the iteration limit of
-- 100 is reached).  In practice the pipeline usually converges in only
-- a few iterations.
optimizeFixpoint :: [Stmt] -> [Stmt]
optimizeFixpoint = go 100
  where
    go :: Int -> [Stmt] -> [Stmt]
    go 0 stmts = stmts
    go n stmts =
      let stmts' = optimizeOnce stmts
      in  if stmts' == stmts then stmts else go (n - 1) stmts'

-- | Optimize a statement list to a fixpoint.
optimizeStmts :: [Stmt] -> [Stmt]
optimizeStmts = optimizeFixpoint

