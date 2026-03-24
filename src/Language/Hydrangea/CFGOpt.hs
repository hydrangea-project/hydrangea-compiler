{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.CFGOpt
--
-- Basic loop nest optimizations for the canonical CFG IR:
-- copy propagation, dead assignment elimination, and
-- loop-invariant code motion.
module Language.Hydrangea.CFGOpt
  ( -- * Transformations
    copyProp2
  , deadAssignElim2
  , loopInvariantCodeMotion2
  , optimizeStmts2
  , optimizeProgram2
  , inlineProgram
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS
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
  , usedVarsStmt2
  , usedVarsStmts2
  , usedVarsIndexExpr
  )

-- * Analysis helpers

-- | Predicate indicating whether an RHS is "pure" (no observable
-- side-effects). Pure RHSes may be removed when their defined variable
-- is dead (see dead assignment elimination). This conservatively treats
-- array stores and calls as potentially impure unless listed above.
rhsIsPure2 :: RHS -> Bool
rhsIsPure2 rhs = case rhs of
  RAtom{} -> True
  RBinOp{} -> True
  RUnOp{} -> True
  RTuple{} -> True
  RProj{} -> True
  RRecord{} -> True
  RRecordProj{} -> True
  RNdToFlat{} -> True
  RFlatToNd{} -> True
  R2DToFlat{} -> True
  RArrayShape{} -> True
  RShapeSize{} -> True
  RShapeInit{} -> True
  RShapeLast{} -> True
  RVecLoad{} -> True
  RVecBinOp{} -> True
  RVecUnOp{} -> True
  RVecSplat{} -> True
  RVecReduce{} -> True
  RArrayLoad{} -> True
  RPairMake{} -> True
  RPairFst{} -> True
  RPairSnd{} -> True
  _ -> False

-- * Copy Propagation

-- | Substitute variables in an atom according to an environment map.
-- This is a simple local substitution used by copy propagation.
substAtom2 :: Map ByteString Atom -> Atom -> Atom
substAtom2 env a = case a of
  AVar v -> case M.lookup v env of
    Just a' -> a'
    Nothing -> a
  AVecVar v -> case M.lookup v env of
    Just (AVecVar v') -> AVecVar v'
    _ -> a
  _ -> a

-- | Substitute variables inside an RHS using the provided atom map.
-- Only variable occurrences (`AVar`) are replaced; literals and
-- compound constructors are preserved. This is intentionally
-- conservative and does not perform deep expression simplification.
substRHS2 :: Map ByteString Atom -> RHS -> RHS
substRHS2 env rhs = case rhs of
  RAtom a -> RAtom (substAtom2 env a)
  RBinOp op a1 a2 -> RBinOp op (substAtom2 env a1) (substAtom2 env a2)
  RUnOp op a -> RUnOp op (substAtom2 env a)
  RTuple as -> RTuple (map (substAtom2 env) as)
  RProj i a -> RProj i (substAtom2 env a)
  RRecord fields -> RRecord [(field, substAtom2 env atom) | (field, atom) <- fields]
  RRecordProj field a -> RRecordProj field (substAtom2 env a)
  RArrayLoad a1 a2 -> RArrayLoad (substAtom2 env a1) (substAtom2 env a2)
  RArrayAlloc a -> RArrayAlloc (substAtom2 env a)
  RNdToFlat a shp -> RNdToFlat (substAtom2 env a) (substAtom2 env shp)
  RFlatToNd a shp -> RFlatToNd (substAtom2 env a) (substAtom2 env shp)
  R2DToFlat a w -> R2DToFlat (substAtom2 env a) (substAtom2 env w)
  RVecLoad a1 a2 -> RVecLoad (substAtom2 env a1) (substAtom2 env a2)
  RVecStore a1 a2 a3 -> RVecStore (substAtom2 env a1) (substAtom2 env a2) (substAtom2 env a3)
  RVecBinOp op a1 a2 -> RVecBinOp op (substAtom2 env a1) (substAtom2 env a2)
  RVecUnOp op a -> RVecUnOp op (substAtom2 env a)
  RVecSplat a -> RVecSplat (substAtom2 env a)
  RVecReduce op a -> RVecReduce op (substAtom2 env a)
  RCall f args -> RCall f (map (substAtom2 env) args)
  RPairMake ct1 ct2 a1 a2 -> RPairMake ct1 ct2 (substAtom2 env a1) (substAtom2 env a2)
  RPairFst ct a -> RPairFst ct (substAtom2 env a)
  RPairSnd ct a -> RPairSnd ct (substAtom2 env a)
  _ -> rhs

-- | A simple forward copy-propagation pass over a statement list.
-- It tracks direct assignments of the form `x = a` and replaces
-- later uses of `x` with `a` when safe. Substitutions are cleared
-- at loop boundaries for iterator variables and any variable defined
-- inside the loop to avoid unsound propagation across loop scopes.
copyProp2 :: [Stmt] -> [Stmt]
copyProp2 = go M.empty
  where
    go :: Map ByteString Atom -> [Stmt] -> [Stmt]
    go _ [] = []
    go env (stmt : rest) = case stmt of
      SAssign x (RAtom a) ->
        let a' = substAtom2 env a
        in  SAssign x (RAtom a') : go (M.insert x a' env) rest
      SAssign x rhs ->
        let rhs' = substRHS2 env rhs
        in  SAssign x rhs' : go env rest
      SArrayWrite a1 a2 a3 ->
        SArrayWrite (substAtom2 env a1) (substAtom2 env a2) (substAtom2 env a3)
          : go env rest
      SLoop spec body ->
        -- Clear propagated substitutions for iterator and any variables
        -- defined inside the loop body to avoid unsound propagation into
        -- loop bodies (which can lead to dead-assignment elimination
        -- removing necessary loop-carried updates).
        -- Important: also ensure substitutions for variables defined inside
        -- the loop are removed for subsequent statements after the loop.
        let defs = S.toList (definedVarsStmts2 body)
            env' = foldr M.delete env (lsIters spec ++ defs)
            body' = go env' body
        in  SLoop spec body' : go env' rest
      SIf cond thn els ->
        let cond' = substAtom2 env cond
            thn' = go env thn
            els' = go env els
            -- Kill any variable that may have been modified in either branch.
            -- After the SIf, we cannot assume those variables still hold their
            -- pre-SIf values, so remove them from the copy propagation environment.
            definedInBranches = definedVarsStmts2 (thn ++ els)
            env' = foldr M.delete env (S.toList definedInBranches)
        in  SIf cond' thn' els' : go env' rest
      SReturn a ->
        SReturn (substAtom2 env a) : go env rest

-- * Dead Assignment Elimination

-- | Process statements backwards, eliminating dead assignments.
-- Returns the filtered statements and the set of variables that are live before
-- the first statement.
daeBackwards :: Set ByteString -> [Stmt] -> ([Stmt], Set ByteString)
daeBackwards liveAfter [] = ([], liveAfter)
daeBackwards liveAfter (stmt : rest) =
  let (rest', liveAfterRest) = daeBackwards liveAfter rest
  in  case stmt of
        SAssign x rhs
          | x `S.member` liveAfterRest ->
              let liveBefore = liveAfterRest `S.union` usedVarsRHS2 rhs
              in  (stmt : rest', liveBefore)
          | rhsIsPure2 rhs ->
              -- Dead pure assignment - remove it
              (rest', liveAfterRest)
          | otherwise ->
              -- Dead impure assignment - keep for side effects, but don't add x to live
              let liveBefore = liveAfterRest `S.union` usedVarsRHS2 rhs
              in  (stmt : rest', liveBefore)
        SArrayWrite arr idx val ->
          let liveBefore = liveAfterRest `S.union` usedVarsAtom2 arr `S.union`
                           usedVarsAtom2 idx `S.union` usedVarsAtom2 val
          in  (stmt : rest', liveBefore)
        SLoop spec body ->
          let iterVars = S.fromList (lsIters spec)
              -- Variables used by later statements after the loop
              liveAfterLoop = liveAfterRest `S.difference` iterVars
              -- Variables used in loop bounds (should be considered live)
              boundVars = S.unions (map usedVarsIndexExpr (lsBounds spec))
              -- Body processing: loop-carried vars must be kept
              loopCarried = usedVarsStmts2 body `S.intersection` definedVarsStmts2 body
              (body', _) = daeLoopBody loopCarried body
              -- What's live before the loop: what the body uses (excluding iters)
              -- plus what's live after and any vars used in loop bounds
              bodyUsed = usedVarsStmts2 body'
              liveBefore = (liveAfterLoop `S.union` bodyUsed `S.union` boundVars) `S.difference` iterVars
          in  (SLoop spec body' : rest', liveBefore)
        SIf cond thn els ->
          -- Variables live after the SIf are live at the END of each branch too,
          -- since control flow merges after the if/else. Passing S.empty here was
          -- a bug that caused assignments to branch-result variables to be
          -- incorrectly eliminated as dead code.
          let (thn', thnLive) = daeBackwards liveAfterRest thn
              (els', elsLive) = daeBackwards liveAfterRest els
              branchLive = thnLive `S.union` elsLive
              liveBefore = liveAfterRest `S.union` branchLive `S.union` usedVarsAtom2 cond
          in  (SIf cond thn' els' : rest', liveBefore)
        SReturn a ->
          let liveBefore = usedVarsAtom2 a
          in  (stmt : rest', liveBefore)

-- | Special handling for loop bodies where some variables are
-- loop-carried and therefore cannot be removed even if a local
-- liveness analysis would otherwise mark them dead. The
-- The @loopCarried@ set contains variables that must be preserved.
daeLoopBody :: Set ByteString -> [Stmt] -> ([Stmt], Set ByteString)
daeLoopBody _ [] = ([], S.empty)
daeLoopBody loopCarried (stmt : rest) =
  let (rest', liveAfterRest) = daeLoopBody loopCarried rest
  in  case stmt of
        SAssign x rhs
          | x `S.member` loopCarried ->
              -- Loop-carried variable must be kept
              let liveBefore = liveAfterRest `S.union` usedVarsRHS2 rhs `S.union` S.singleton x
              in  (stmt : rest', liveBefore)
          | x `S.member` liveAfterRest ->
              let liveBefore = liveAfterRest `S.union` usedVarsRHS2 rhs
              in  (stmt : rest', liveBefore)
          | rhsIsPure2 rhs ->
              (rest', liveAfterRest)
          | otherwise ->
              let liveBefore = liveAfterRest `S.union` usedVarsRHS2 rhs
              in  (stmt : rest', liveBefore)
        SArrayWrite arr idx val ->
          let liveBefore = liveAfterRest `S.union` usedVarsAtom2 arr `S.union`
                           usedVarsAtom2 idx `S.union` usedVarsAtom2 val
          in  (stmt : rest', liveBefore)
        SLoop spec body ->
          let iterVars = S.fromList (lsIters spec)
              liveAfterLoop = liveAfterRest `S.difference` iterVars
              innerCarried = usedVarsStmts2 body `S.intersection` definedVarsStmts2 body
              (body', _) = daeLoopBody innerCarried body
              bodyUsed = usedVarsStmts2 body'
              boundVars = S.unions (map usedVarsIndexExpr (lsBounds spec))
              liveBefore = (liveAfterLoop `S.union` bodyUsed `S.union` boundVars) `S.difference` iterVars
          in  (SLoop spec body' : rest', liveBefore)
        SIf cond thn els ->
          -- Variables live after the SIf in the loop body are also live at the
          -- end of each branch. We also include loopCarried vars since they
          -- must survive across loop iterations.
          let liveAtBranchEnd = liveAfterRest `S.union` loopCarried
              (thn', thnLive) = daeBackwards liveAtBranchEnd thn
              (els', elsLive) = daeBackwards liveAtBranchEnd els
              branchLive = thnLive `S.union` elsLive
              liveBefore = liveAfterRest `S.union` branchLive `S.union` usedVarsAtom2 cond
          in  (SIf cond thn' els' : rest', liveBefore)
        SReturn a ->
          let liveBefore = usedVarsAtom2 a
          in  (stmt : rest', liveBefore)

-- | Remove assignments whose assigned variable is not live and the RHS
-- is pure; impure assignments are preserved for their side-effects.
deadAssignElim2 :: [Stmt] -> [Stmt]
deadAssignElim2 stmts = fst (daeBackwards S.empty stmts)

-- * Loop-Invariant Code Motion

-- | Partition statements from a loop body into hoistable and kept
-- statements. A statement is hoistable if it defines a variable that is
-- not an iterator nor defined inside the loop, its RHS is pure, and it
-- does not use any loop-defined variable.
partitionHoistable :: Set ByteString -> [Stmt] -> ([Stmt], [Stmt])
partitionHoistable loopDefs stmts =
  let (hoisted, kept) = go ([], []) stmts
  in  (reverse hoisted, reverse kept)
  where
    go (hoisted, kept) [] = (hoisted, kept)
    go (hoisted, kept) (stmt:rest) = case stmt of
      SAssign x rhs
        | not (x `S.member` loopDefs) && rhsIsPure2 rhs &&
          S.null (usedVarsRHS2 rhs `S.intersection` loopDefs) ->
            go (stmt : hoisted, kept) rest
        | otherwise -> go (hoisted, stmt : kept) rest
      _ -> go (hoisted, stmt : kept) rest

-- | Perform a single-pass loop-invariant code motion over statements.
-- For each loop, hoist pure assignments that do not depend on loop
-- definitions to the loop's pre-header. This is conservative and
-- intentionally simple (no value numbering or strength reduction).
loopInvariantCodeMotion2 :: [Stmt] -> [Stmt]
loopInvariantCodeMotion2 = concatMap goStmt
  where
    goStmt :: Stmt -> [Stmt]
    goStmt (SLoop spec body) =
      let body' = loopInvariantCodeMotion2 body
          loopDefs = S.fromList (lsIters spec) <> definedVarsStmts2 body'
          (hoisted, kept) = partitionHoistable loopDefs body'
      in  hoisted ++ [SLoop spec kept]
    goStmt (SIf cond thn els) =
      let thn' = loopInvariantCodeMotion2 thn
          els' = loopInvariantCodeMotion2 els
      in  [SIf cond thn' els']
    goStmt stmt = [stmt]

-- * Common subexpression elimination

data CSEEntry = CSEEntry
  { cseVar :: ByteString
  , cseDeps :: Set ByteString
  }

type CSEEnv = Map ByteString CSEEntry

commutativeBinOp :: BinOp -> Bool
commutativeBinOp op = case op of
  CAdd -> True
  CMul -> True
  CEq -> True
  CNeq -> True
  CAnd -> True
  COr -> True
  CAddF -> True
  CMulF -> True
  CEqF -> True
  CNeqF -> True
  _ -> False

canonicalAtomPair :: Atom -> Atom -> (Atom, Atom)
canonicalAtomPair a b
  | show a <= show b = (a, b)
  | otherwise = (b, a)

canonicalizeRHS2 :: RHS -> RHS
canonicalizeRHS2 rhs = case rhs of
  RBinOp op a b
    | commutativeBinOp op ->
        let (a', b') = canonicalAtomPair a b
        in RBinOp op a' b'
  RVecBinOp op a b
    | commutativeBinOp op ->
        let (a', b') = canonicalAtomPair a b
        in RVecBinOp op a' b'
  _ -> rhs

cseEligibleRHS2 :: RHS -> Bool
cseEligibleRHS2 rhs = case rhs of
  RAtom{} -> True
  RBinOp{} -> True
  RUnOp{} -> True
  RTuple{} -> True
  RProj{} -> True
  RRecord{} -> True
  RRecordProj{} -> True
  RNdToFlat{} -> True
  RFlatToNd{} -> True
  R2DToFlat{} -> True
  RArrayShape{} -> True
  RShapeSize{} -> True
  RShapeInit{} -> True
  RShapeLast{} -> True
  RVecLoad{} -> True
  RVecBinOp{} -> True
  RVecUnOp{} -> True
  RVecSplat{} -> True
  RVecReduce{} -> True
  RPairMake{} -> True
  RPairFst{} -> True
  RPairSnd{} -> True
  _ -> False

rhsCSEKey2 :: RHS -> Maybe ByteString
rhsCSEKey2 rhs
  | cseEligibleRHS2 rhs = Just (BS.pack (show (canonicalizeRHS2 rhs)))
  | otherwise = Nothing

invalidateVarCSE :: ByteString -> CSEEnv -> CSEEnv
invalidateVarCSE v =
  M.filter (\entry -> cseVar entry /= v && not (v `S.member` cseDeps entry))

invalidateVarsCSE :: [ByteString] -> CSEEnv -> CSEEnv
invalidateVarsCSE vars env = foldr invalidateVarCSE env vars

-- | Eliminate repeated pure RHS computations by reusing the first variable
-- that computed the same canonical expression. This is conservative around
-- control flow and memory writes but effective for duplicated scalar/index
-- arithmetic that survives lowering.
cseStmts2 :: [Stmt] -> [Stmt]
cseStmts2 = go M.empty
  where
    go :: CSEEnv -> [Stmt] -> [Stmt]
    go _ [] = []
    go env (stmt : rest) = case stmt of
      SAssign x rhs ->
        let rhs' = canonicalizeRHS2 rhs
            envCleared = invalidateVarCSE x env
        in case rhsCSEKey2 rhs' of
             Just key
               | Just entry <- M.lookup key env ->
                   SAssign x (RAtom (AVar (cseVar entry))) : go envCleared rest
               | otherwise ->
                   let env' = M.insert key (CSEEntry x (usedVarsRHS2 rhs')) envCleared
                   in SAssign x rhs' : go env' rest
             Nothing ->
               SAssign x rhs' : go envCleared rest
      SArrayWrite arr idx val ->
        let deps = S.toList (usedVarsAtom2 arr `S.union` usedVarsAtom2 idx `S.union` usedVarsAtom2 val)
            env' = invalidateVarsCSE deps M.empty
        in SArrayWrite arr idx val : go env' rest
      SLoop spec body ->
        let defs = S.toList (definedVarsStmts2 body)
            envBody = invalidateVarsCSE (lsIters spec ++ defs) env
            body' = go envBody body
            env' = invalidateVarsCSE (lsIters spec ++ defs) env
        in SLoop spec body' : go env' rest
      SIf cond thn els ->
        let thn' = go env thn
            els' = go env els
            definedInBranches = S.toList (definedVarsStmts2 (thn ++ els))
            env' = invalidateVarsCSE definedInBranches env
        in SIf cond thn' els' : go env' rest
      SReturn a ->
        SReturn a : go env rest

-- * Function Inlining

-- | Count statements in a procedure body (recursively).
procSize :: [Stmt] -> Int
procSize = sum . map stmtSize
  where
    stmtSize (SLoop _ body) = 1 + procSize body
    stmtSize (SIf _ thn els) = 1 + procSize thn + procSize els
    stmtSize _ = 1

-- | Check if a procedure is a good candidate for inlining.
-- Criteria: small body (<= 5 statements), no recursive calls.
isInlineable :: Proc -> Map ByteString Proc -> Bool
isInlineable (Proc { procName = name, procBody = body }) _allProcs =
  procSize body <= 5 && not (hasRecursiveCall name body)
  where
    hasRecursiveCall :: ByteString -> [Stmt] -> Bool
    hasRecursiveCall target = any (stmtHasCall target)
    
    stmtHasCall target (SAssign _ (RCall fn _)) = fn == target
    stmtHasCall target (SLoop _ body') = hasRecursiveCall target body'
    stmtHasCall target (SIf _ thn els) = hasRecursiveCall target thn || hasRecursiveCall target els
    stmtHasCall _ _ = False

-- | Substitute atoms in an RHS.
substRHS :: Map ByteString Atom -> RHS -> RHS
substRHS env rhs = case rhs of
  RAtom a -> RAtom (substAtom env a)
  RBinOp op a1 a2 -> RBinOp op (substAtom env a1) (substAtom env a2)
  RUnOp op a -> RUnOp op (substAtom env a)
  RTuple as -> RTuple (map (substAtom env) as)
  RProj i a -> RProj i (substAtom env a)
  RRecord fields -> RRecord [(field, substAtom env atom) | (field, atom) <- fields]
  RRecordProj field a -> RRecordProj field (substAtom env a)
  RArrayLoad a1 a2 -> RArrayLoad (substAtom env a1) (substAtom env a2)
  RArrayAlloc a -> RArrayAlloc (substAtom env a)
  RCall fn args -> RCall fn (map (substAtom env) args)
  _ -> rhs
  where
    substAtom env' (AVar v) = case M.lookup v env' of
      Just a' -> a'
      Nothing -> AVar v
    substAtom _ a = a

-- | Inline a procedure call by substituting parameters with arguments.
inlineCall :: Proc -> [Atom] -> [Stmt]
inlineCall (Proc { procParams = params, procBody = body }) args =
  let paramMap = M.fromList (zip params args)
  in map (substStmt paramMap) body
  where
    substStmt env (SAssign v rhs) = SAssign v (substRHS env rhs)
    substStmt env (SArrayWrite arr idx val) = 
      SArrayWrite (substAtom env arr) (substAtom env idx) (substAtom env val)
    substStmt env (SLoop spec loopBody) = 
      SLoop spec (map (substStmt env) loopBody)
    substStmt env (SIf cond thn els) = 
      SIf (substAtom env cond) (map (substStmt env) thn) (map (substStmt env) els)
    substStmt env (SReturn a) = SReturn (substAtom env a)
    
    substAtom env (AVar v) = case M.lookup v env of
      Just a' -> a'
      Nothing -> AVar v
    substAtom _ a = a

-- | Inline all calls to inlineable procedures in a statement list.
inlineStmts :: Map ByteString Proc -> [Stmt] -> [Stmt]
inlineStmts procs = concatMap (inlineStmt procs)
  where
    inlineStmt :: Map ByteString Proc -> Stmt -> [Stmt]
    inlineStmt ps stmt = case stmt of
      SAssign v (RCall fn args) -> 
        case M.lookup fn ps of
          Just proc | isInlineable proc ps ->
            let inlined = inlineCall proc args
                -- Replace the last statement (SReturn) with assignment to v
                (prefix, lastStmt) = splitLast inlined
            in case lastStmt of
              SReturn a -> prefix ++ [SAssign v (RAtom a)]
              _ -> prefix ++ [SAssign v (RAtom (AInt 0))]  -- Fallback
          _ -> [stmt]
      SLoop spec body -> [SLoop spec (concatMap (inlineStmt ps) body)]
      SIf cond thn els -> [SIf cond (concatMap (inlineStmt ps) thn) (concatMap (inlineStmt ps) els)]
      _ -> [stmt]
    
    splitLast [] = ([], SReturn (AInt 0))
    splitLast [x] = ([], x)
    splitLast (x:xs) = let (prefix, last') = splitLast xs in (x:prefix, last')

-- | Single pass of inlining: inline calls to inlineable procedures.
-- Does NOT remove procedures even if they become unused - removal can be done
-- by a separate dead procedure elimination pass if needed.
inlineProgramOnce :: [Proc] -> [Proc]
inlineProgramOnce [] = []
inlineProgramOnce procs =
  let procMap = M.fromList [(procName p, p) | p <- procs]
      inlineableProcs = M.filterWithKey (\_k v -> isInlineable v procMap) procMap
      -- Inline all calls to inlineable procedures in all procedure bodies
      optimizedProcs = map (inlineProc inlineableProcs) procs
  in optimizedProcs
  where
    inlineProc :: Map ByteString Proc -> Proc -> Proc
    inlineProc ps p = p { procBody = inlineStmts ps (procBody p) }

-- | Optimize a program by inlining small procedures to fixpoint.
-- Runs multiple passes until no more inlining can be done.
-- Note: Does not remove dead procedures - they remain in the program.
inlineProgram :: Program -> Program
inlineProgram (Program procs) = go 50 procs
  where
    go :: Int -> [Proc] -> Program
    go 0 acc = Program acc  -- Hit iteration limit
    go n acc =
      let acc' = inlineProgramOnce acc
      in if acc' == acc
           then Program acc  -- Converged
           else go (n - 1) acc'

-- * Combined optimization

-- | Single iteration of the optimization pipeline.
-- Applies: Copy Propagation -> CSE -> Copy Propagation -> Dead Assignment Elimination -> LICM
optimizeOnce :: [Stmt] -> [Stmt]
optimizeOnce stmts =
  let stmts' = copyProp2 stmts
      stmts'' = cseStmts2 stmts'
      stmts''' = copyProp2 stmts''
      stmts'''' = deadAssignElim2 stmts'''
      stmts''''' = loopInvariantCodeMotion2 stmts''''
  in  stmts'''''

-- | Bounded fixpoint iteration for optimizations.
-- Runs optimizeOnce until convergence or max iterations (100).
optimizeFixpoint :: [Stmt] -> [Stmt]
optimizeFixpoint stmts = go 100 stmts
  where
    go :: Int -> [Stmt] -> [Stmt]
    go 0 acc = acc  -- Hit iteration limit, return current
    go n acc =
      let acc' = optimizeOnce acc
      in if acc' == acc
           then acc  -- Converged
           else go (n - 1) acc'

-- | Main optimization entry point for statement lists.
-- Runs optimizations to fixpoint with a bounded iteration limit.
optimizeStmts2 :: [Stmt] -> [Stmt]
optimizeStmts2 = optimizeFixpoint

-- | Optimize a full program including inlining.
optimizeProgram2 :: Program -> Program
optimizeProgram2 prog = 
  let inlined = inlineProgram prog
  in case inlined of
    Program procs -> Program [p { procBody = optimizeStmts2 (procBody p) } | p <- procs]
