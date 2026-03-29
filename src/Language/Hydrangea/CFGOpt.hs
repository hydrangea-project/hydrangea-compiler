{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.CFGOpt
--
-- Scalar optimizations over the CFG IR.  The pipeline comprises:
--
-- 1. /Copy propagation/ ('copyProp2') — forward substitution of direct
--    @x = a@ assignments.
-- 2. /Common subexpression elimination/ ('cseStmts2') — replace repeated
--    pure computations with a reference to the first variable that computed
--    the same canonical expression.
-- 3. /Dead assignment elimination/ ('deadAssignElim2') — backwards liveness
--    analysis that removes pure assignments whose result is never used.
-- 4. /Loop-invariant code motion/ ('loopInvariantCodeMotion2') — hoist pure
--    loop-invariant computations to the loop pre-header.
-- 5. /Inlining/ ('inlineProgram') — inline small, non-recursive procedures
--    to expose further optimization opportunities.
--
-- Passes are composed and iterated to a fixpoint by 'optimizeProgram2'.
module Language.Hydrangea.CFGOpt
  ( -- * Entry points
    optimizeStmts2
  , optimizeProgram2
  , inlineProgram
    -- * Individual passes (exported for testing)
  , copyProp2
  , deadAssignElim2
  , loopInvariantCodeMotion2
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List (partition)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Language.Hydrangea.CFGCore (Atom(..), BinOp(..), RHS(..))
import Language.Hydrangea.CFG
import Language.Hydrangea.CFGAnalysis
  ( definedVarsStmts2
  , usedVarsAtom2
  , usedVarsRHS2
  , usedVarsStmts2
  , usedVarsIndexExpr
  )

------------------------------------------------------------------------
-- Purity predicate
------------------------------------------------------------------------

-- | Returns 'True' when an 'RHS' is /pure/: it produces a value with no
-- observable side effects and may therefore be removed if the defined
-- variable is dead.  Array and vector stores ('RArrayStore', 'RVecStore')
-- and procedure calls ('RCall') are conservatively treated as impure.
rhsIsPure2 :: RHS -> Bool
rhsIsPure2 rhs = case rhs of
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
  _             -> False

------------------------------------------------------------------------
-- Atom / RHS substitution
------------------------------------------------------------------------

-- | Substitute a variable in an 'Atom' according to a map.
-- For 'AVar', any binding in the map is applied.  For 'AVecVar', only
-- bindings that resolve to another 'AVecVar' are applied, preserving the
-- vector-variable invariant.
substAtom2 :: Map ByteString Atom -> Atom -> Atom
substAtom2 env a = case a of
  AVar    v -> M.findWithDefault a v env
  AVecVar v -> case M.lookup v env of
                 Just (AVecVar v') -> AVecVar v'
                 _                 -> a
  _         -> a

-- | Substitute all 'Atom' occurrences inside an 'RHS'.  Structural
-- information (operators, field names, type tags) is preserved; only
-- leaf variable references are replaced.
substRHS2 :: Map ByteString Atom -> RHS -> RHS
substRHS2 env rhs =
  let sub = substAtom2 env
  in case rhs of
    RAtom a                 -> RAtom (sub a)
    RBinOp op a1 a2         -> RBinOp op (sub a1) (sub a2)
    RUnOp op a              -> RUnOp op (sub a)
    RTuple as               -> RTuple (map sub as)
    RProj i a               -> RProj i (sub a)
    RRecord fields          -> RRecord [(f, sub a) | (f, a) <- fields]
    RRecordProj field a     -> RRecordProj field (sub a)
    RArrayLoad a1 a2        -> RArrayLoad (sub a1) (sub a2)
    RArrayAlloc a           -> RArrayAlloc (sub a)
    RNdToFlat a shp         -> RNdToFlat (sub a) (sub shp)
    RFlatToNd a shp         -> RFlatToNd (sub a) (sub shp)
    R2DToFlat a w           -> R2DToFlat (sub a) (sub w)
    RVecLoad a1 a2          -> RVecLoad (sub a1) (sub a2)
    RVecStore a1 a2 a3      -> RVecStore (sub a1) (sub a2) (sub a3)
    RVecBinOp op a1 a2      -> RVecBinOp op (sub a1) (sub a2)
    RVecUnOp op a           -> RVecUnOp op (sub a)
    RVecSplat a             -> RVecSplat (sub a)
    RVecReduce op a         -> RVecReduce op (sub a)
    RCall f args            -> RCall f (map sub args)
    RPairMake ct1 ct2 a1 a2 -> RPairMake ct1 ct2 (sub a1) (sub a2)
    RPairFst ct a           -> RPairFst ct (sub a)
    RPairSnd ct a           -> RPairSnd ct (sub a)
    _                       -> rhs

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
copyProp2 :: [Stmt] -> [Stmt]
copyProp2 = go M.empty
  where
    go :: Map ByteString Atom -> [Stmt] -> [Stmt]
    go _   []             = []
    go env (stmt : rest) = case stmt of
      SAssign x (RAtom a) ->
        let a' = substAtom2 env a
        in  SAssign x (RAtom a') : go (M.insert x a' env) rest
      SAssign x rhs ->
        SAssign x (substRHS2 env rhs) : go env rest
      SArrayWrite a1 a2 a3 ->
        SArrayWrite (substAtom2 env a1) (substAtom2 env a2) (substAtom2 env a3)
          : go env rest
      SLoop spec body ->
        let defs  = S.toList (definedVarsStmts2 body)
            env'  = foldr M.delete env (lsIters spec ++ defs)
            body' = go env' body
        in  SLoop spec body' : go env' rest
      SIf cond thn els ->
        let cond'             = substAtom2 env cond
            thn'              = go env thn
            els'              = go env els
            definedInBranches = definedVarsStmts2 (thn ++ els)
            env'              = foldr M.delete env (S.toList definedInBranches)
        in  SIf cond' thn' els' : go env' rest
      SReturn a ->
        SReturn (substAtom2 env a) : go env rest
      SBreak ->
        SBreak : go env rest

------------------------------------------------------------------------
-- Dead assignment elimination
------------------------------------------------------------------------

-- | Backwards liveness pass over a statement list.  Returns the filtered
-- list and the set of variables live /before/ the first statement.
--
-- Pure assignments to dead variables are discarded.  Impure assignments
-- are retained for their side effects, but the defined variable is not
-- considered live (it is defined here, not used later).
daeBackwards :: Set ByteString -> [Stmt] -> ([Stmt], Set ByteString)
daeBackwards liveAfter [] = ([], liveAfter)
daeBackwards liveAfter (stmt : rest) =
  let (rest', live) = daeBackwards liveAfter rest
  in case stmt of
       SAssign x rhs
         | x `S.member` live ->
             (stmt : rest', live `S.union` usedVarsRHS2 rhs)
         | rhsIsPure2 rhs ->
             -- Dead pure assignment: safe to discard.
             (rest', live)
         | otherwise ->
             -- Dead impure assignment: retain for side effects but do not
             -- add the defined variable to the live set.
             (stmt : rest', live `S.union` usedVarsRHS2 rhs)
       SArrayWrite arr idx val ->
         ( stmt : rest'
         , S.unions [live, usedVarsAtom2 arr, usedVarsAtom2 idx, usedVarsAtom2 val]
         )
       SLoop spec body ->
         let iterVars  = S.fromList (lsIters spec)
             boundVars = S.unions (map usedVarsIndexExpr (lsBounds spec))
             carried   = usedVarsStmts2 body `S.intersection` definedVarsStmts2 body
             (body', _) = daeLoopBody carried body
             bodyUsed   = usedVarsStmts2 body'
             -- Iterator variables are loop-local and must not leak into the
             -- surrounding live set.
             liveBefore = S.unions [live, bodyUsed, boundVars] `S.difference` iterVars
         in  (SLoop spec body' : rest', liveBefore)
       SIf cond thn els ->
         -- Variables live after the 'SIf' are also live at the end of each
         -- branch, since control flow merges at the join point.
         let (thn', thnLive) = daeBackwards live thn
             (els', elsLive) = daeBackwards live els
             liveBefore      = S.unions [live, thnLive, elsLive, usedVarsAtom2 cond]
         in  (SIf cond thn' els' : rest', liveBefore)
       SReturn a ->
         (stmt : rest', usedVarsAtom2 a)
       SBreak ->
         (stmt : rest', live)

-- | Liveness pass for loop bodies, where variables in @loopCarried@ must
-- be preserved across iterations regardless of apparent local liveness.
daeLoopBody :: Set ByteString -> [Stmt] -> ([Stmt], Set ByteString)
daeLoopBody _           []            = ([], S.empty)
daeLoopBody loopCarried (stmt : rest) =
  let (rest', live) = daeLoopBody loopCarried rest
  in case stmt of
       SAssign x rhs
         | x `S.member` loopCarried ->
             -- Loop-carried: the next iteration reads this variable.
             (stmt : rest', S.unions [live, usedVarsRHS2 rhs, S.singleton x])
         | x `S.member` live ->
             (stmt : rest', live `S.union` usedVarsRHS2 rhs)
         | rhsIsPure2 rhs ->
             (rest', live)
         | otherwise ->
             (stmt : rest', live `S.union` usedVarsRHS2 rhs)
       SArrayWrite arr idx val ->
         ( stmt : rest'
         , S.unions [live, usedVarsAtom2 arr, usedVarsAtom2 idx, usedVarsAtom2 val]
         )
       SLoop spec body ->
         let iterVars  = S.fromList (lsIters spec)
             boundVars = S.unions (map usedVarsIndexExpr (lsBounds spec))
             carried   = usedVarsStmts2 body `S.intersection` definedVarsStmts2 body
             (body', _) = daeLoopBody carried body
             bodyUsed   = usedVarsStmts2 body'
             liveBefore = S.unions [live, bodyUsed, boundVars] `S.difference` iterVars
         in  (SLoop spec body' : rest', liveBefore)
       SIf cond thn els ->
         -- Loop-carried variables must survive to the next iteration, so
         -- include them in the live set passed to each branch.
         let liveAtEnd       = live `S.union` loopCarried
             (thn', thnLive) = daeBackwards liveAtEnd thn
             (els', elsLive) = daeBackwards liveAtEnd els
             liveBefore      = S.unions [live, thnLive, elsLive, usedVarsAtom2 cond]
         in  (SIf cond thn' els' : rest', liveBefore)
       SReturn a ->
         (stmt : rest', usedVarsAtom2 a)
       SBreak ->
         (stmt : rest', live)

-- | Remove dead pure assignments from a statement list.
deadAssignElim2 :: [Stmt] -> [Stmt]
deadAssignElim2 stmts = fst (daeBackwards S.empty stmts)

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
loopInvariantCodeMotion2 :: [Stmt] -> [Stmt]
loopInvariantCodeMotion2 = concatMap goStmt
  where
    goStmt :: Stmt -> [Stmt]
    goStmt (SLoop spec body) =
      let body'    = loopInvariantCodeMotion2 body
          loopDefs = S.fromList (lsIters spec) <> definedVarsStmts2 body'
          (hoisted, kept) = partition (isHoistable loopDefs) body'
      in  hoisted ++ [SLoop spec kept]
    goStmt (SIf cond thn els) =
      [SIf cond (loopInvariantCodeMotion2 thn) (loopInvariantCodeMotion2 els)]
    goStmt stmt = [stmt]

    isHoistable :: Set ByteString -> Stmt -> Bool
    isHoistable loopDefs (SAssign x rhs) =
      not (x `S.member` loopDefs)
      && rhsIsPure2 rhs
      && S.null (usedVarsRHS2 rhs `S.intersection` loopDefs)
    isHoistable _ _ = False

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
canonicalizeRHS2 :: RHS -> RHS
canonicalizeRHS2 rhs = case rhs of
  RBinOp op a b | commutativeBinOp op ->
    let (a', b') = canonicalAtomPair a b in RBinOp op a' b'
  RVecBinOp op a b | commutativeBinOp op ->
    let (a', b') = canonicalAtomPair a b in RVecBinOp op a' b'
  _ -> rhs

-- | Compute the CSE lookup key for a pure 'RHS' after canonicalization,
-- or 'Nothing' if the RHS is not eligible (e.g. has side effects).
rhsCSEKey2 :: RHS -> Maybe ByteString
rhsCSEKey2 rhs
  | rhsIsPure2 rhs = Just (BS.pack (show (canonicalizeRHS2 rhs)))
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
cseStmts2 :: [Stmt] -> [Stmt]
cseStmts2 = go M.empty
  where
    go :: CSEEnv -> [Stmt] -> [Stmt]
    go _   []             = []
    go env (stmt : rest) = case stmt of
      SAssign x rhs ->
        let rhs'       = canonicalizeRHS2 rhs
            envCleared = invalidateVarCSE x env
        in case rhsCSEKey2 rhs' of
             Just key
               | Just entry <- M.lookup key env ->
                   SAssign x (RAtom (AVar (cseVar entry))) : go envCleared rest
               | otherwise ->
                   let env' = M.insert key (CSEEntry x (usedVarsRHS2 rhs')) envCleared
                   in  SAssign x rhs' : go env' rest
             Nothing ->
               SAssign x rhs' : go envCleared rest
      SArrayWrite arr idx val ->
        -- Any array write may alias with a cached array load result, so
        -- flush the entire CSE environment conservatively.
        SArrayWrite arr idx val : go M.empty rest
      SLoop spec body ->
        let loopVars = lsIters spec ++ S.toList (definedVarsStmts2 body)
            envLoop  = invalidateVarsCSE loopVars env
            body'    = go envLoop body
        in  SLoop spec body' : go envLoop rest
      SIf cond thn els ->
        let thn'              = go env thn
            els'              = go env els
            definedInBranches = S.toList (definedVarsStmts2 (thn ++ els))
            env'              = invalidateVarsCSE definedInBranches env
        in  SIf cond thn' els' : go env' rest
      SReturn a ->
        SReturn a : go env rest
      SBreak ->
        SBreak : go env rest

------------------------------------------------------------------------
-- Function inlining
------------------------------------------------------------------------

-- | Structural statement count for a procedure body (recursing into
-- nested loops and conditionals).
procSize :: [Stmt] -> Int
procSize = sum . map stmtSize
  where
    stmtSize (SLoop _ body)  = 1 + procSize body
    stmtSize (SIf _ thn els) = 1 + procSize thn + procSize els
    stmtSize _               = 1

-- | A procedure is inlineable when its body is small (≤ 5 by 'procSize')
-- and it does not call itself recursively.
--
-- The @_allProcs@ argument is reserved for future cross-procedure
-- analysis such as mutual-recursion detection.
isInlineable :: Proc -> Map ByteString Proc -> Bool
isInlineable Proc { procName = name, procBody = body } _allProcs =
  procSize body <= 5 && not (callsItself name body)
  where
    callsItself :: ByteString -> [Stmt] -> Bool
    callsItself target = any (stmtCallsTarget target)

    stmtCallsTarget t (SAssign _ (RCall fn _)) = fn == t
    stmtCallsTarget t (SLoop _ body')          = callsItself t body'
    stmtCallsTarget t (SIf _ thn els)          = callsItself t thn || callsItself t els
    stmtCallsTarget _ _                        = False

-- | Inline a single call to @proc@ by substituting each parameter with
-- the corresponding argument atom throughout the procedure body.
inlineCall :: Proc -> [Atom] -> [Stmt]
inlineCall Proc { procParams = params, procBody = body } args =
  let paramMap = M.fromList (zip params args)
  in  map (substStmt paramMap) body
  where
    substStmt env (SAssign v rhs)           = SAssign v (substRHS2 env rhs)
    substStmt env (SArrayWrite arr idx val)  =
      SArrayWrite (substAtom2 env arr) (substAtom2 env idx) (substAtom2 env val)
    substStmt env (SLoop spec loopBody)     = SLoop spec (map (substStmt env) loopBody)
    substStmt env (SIf cond thn els)        =
      SIf (substAtom2 env cond) (map (substStmt env) thn) (map (substStmt env) els)
    substStmt env (SReturn a)               = SReturn (substAtom2 env a)
    substStmt _   SBreak                    = SBreak

-- | Inline all calls to inlineable procedures throughout a statement list.
inlineStmts :: Map ByteString Proc -> [Stmt] -> [Stmt]
inlineStmts procs = concatMap inlineOne
  where
    inlineOne stmt = case stmt of
      SAssign v (RCall fn args)
        | Just proc <- M.lookup fn procs
        , isInlineable proc procs ->
            let inlined   = inlineCall proc args
                retAtom   = case if null inlined then Nothing else Just (last inlined) of
                              Just (SReturn a) -> a
                              _                -> AInt 0
                prefix    = if null inlined then [] else init inlined
            in  prefix ++ [SAssign v (RAtom retAtom)]
      SLoop spec body  -> [SLoop spec (inlineStmts procs body)]
      SIf cond thn els -> [SIf cond (inlineStmts procs thn) (inlineStmts procs els)]
      _                -> [stmt]

-- | Perform one pass of inlining, substituting calls to all currently
-- inlineable procedures.  Dead procedures are not removed; that is left
-- to a separate pass if required.
inlineProgramOnce :: [Proc] -> [Proc]
inlineProgramOnce [] = []
inlineProgramOnce procs =
  let procMap       = M.fromList [(procName p, p) | p <- procs]
      inlineableMap = M.filter (`isInlineable` procMap) procMap
  in  map (\p -> p { procBody = inlineStmts inlineableMap (procBody p) }) procs

-- | Inline small procedures to a fixpoint (or until the iteration limit
-- of 50 is reached).  Dead procedures are not removed.
inlineProgram :: Program -> Program
inlineProgram (Program procs) = go 50 procs
  where
    go :: Int -> [Proc] -> Program
    go 0 ps = Program ps
    go n ps =
      let ps' = inlineProgramOnce ps
      in  if ps' == ps then Program ps else go (n - 1) ps'

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
    loopInvariantCodeMotion2
  . deadAssignElim2
  . copyProp2
  . cseStmts2
  . copyProp2

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
optimizeStmts2 :: [Stmt] -> [Stmt]
optimizeStmts2 = optimizeFixpoint

-- | Optimize a full program: inline small procedures, then run the
-- scalar optimization pipeline over every procedure body.
optimizeProgram2 :: Program -> Program
optimizeProgram2 prog =
  let Program procs = inlineProgram prog
  in  Program [p { procBody = optimizeStmts2 (procBody p) } | p <- procs]
