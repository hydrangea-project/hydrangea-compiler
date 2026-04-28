{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.Parallelize
--
-- Parallelization pass for the CFG IR.  The pass annotates eligible serial
-- 'SLoop' nodes with a 'Parallel' execution policy and a strategy tag that
-- downstream code generation uses to select the appropriate OpenMP pragma
-- and atomic/privatization idiom.
--
-- Two legality paths are tried in order:
--
-- 1. /Facts-guided fast path/ — for 'LoopMap' kernels that carry
--    lowering-provided 'ArrayFact' annotations, the pass checks directly
--    whether each accessed array is a fresh write-once output or a
--    read-only input, avoiding dependence analysis entirely.
--
-- 2. /Conservative dependence fallback/ — all other loops are tested
--    via 'findDependences2' on the set of array accesses visible in the
--    loop body.  The loop is parallelized only when no loop-carried
--    non-forward dependence is found and no array is both read and written.
--
-- The procedure-level entry points exploit the richer per-procedure
-- 'procArrayFacts'; the statement-level helper 'parallelizeStmts2' wraps
-- a synthetic procedure to remain conservative.
module Language.Hydrangea.Parallelize
  ( parallelizeProc2
  , parallelizeProgram2
  , parallelizeStmts2
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Language.Hydrangea.CFGCore (Atom(..), BinOp(..), CType(..), RHS(..))
import Language.Hydrangea.CFG
import Language.Hydrangea.CFGTyping (recoverProcTypeEnv2)
import Language.Hydrangea.CFGAnalysis
  ( isParallelTripCount2
  , loopTripCount2
  , tripCountValue2
  )
import Language.Hydrangea.Dependence
  ( findDependences2
  , ArrayAccess2(..)
  , AccessType2(..)
  , Dependence2(..)
  , depDirection2
  , depIsLoopCarried2
  , DependenceDirection2(..)
  )

type ArrayFacts = Map CVar ArrayFact

------------------------------------------------------------------------
-- Internal detection types
------------------------------------------------------------------------

-- | Result of identifying an injective scatter kernel: the single
-- destination array whose write index maps bijectively to the loop iterator.
data ScatterKernelInfo = ScatterKernelInfo
  { skiDestArray :: CVar
  }

-- | Result of identifying an atomic-add scatter kernel: the destination
-- array and the element type (determines which atomic intrinsic to emit).
data ScatterAtomicAddInfo = ScatterAtomicAddInfo
  { saDestArray :: CVar
  , saValueType :: CType
  }

------------------------------------------------------------------------
-- Array-access extraction
------------------------------------------------------------------------

-- | Lift an 'Atom' to an 'IndexExpr' for dependence analysis.
-- Opaque atoms (non-variable, non-constant) are represented as an
-- uninterpreted call so the dependence analyser treats them conservatively.
atomToIndexExpr :: Atom -> IndexExpr
atomToIndexExpr (AVar v) = IVar v
atomToIndexExpr (AInt n) = IConst n
atomToIndexExpr _        = ICall "opaque" []

-- | Extract all array accesses from a statement list in left-to-right,
-- depth-first order, assigning each a unique sequential index.  The index
-- is used by 'findDependences2' to determine the relative order of a source
-- and target access within the same loop body.
extractAccesses2 :: [Stmt] -> [ArrayAccess2]
extractAccesses2 stmts =
  zipWith (\i acc -> acc { aa2StmtIndex = i }) [0..] (collect stmts)
  where
    collect :: [Stmt] -> [ArrayAccess2]
    collect = concatMap collectStmt

    collectStmt (SArrayWrite (AVar arr) idx _) =
      [ArrayAccess2 arr (atomToIndexExpr idx) Write2 0]
    collectStmt (SAssign _ (RArrayLoad (AVar arr) idx)) =
      [ArrayAccess2 arr (atomToIndexExpr idx) Read2 0]
    collectStmt (SLoop _ body)  = collect body
    collectStmt (SIf _ thn els) = collect thn ++ collect els
    collectStmt _               = []

------------------------------------------------------------------------
-- Array usage summary
------------------------------------------------------------------------

-- | Per-array read/write summary used by legality checks.
data LoopArrayUsage = LoopArrayUsage
  { lauReads  :: Bool
  , lauWrites :: Bool
  }

emptyLoopArrayUsage :: LoopArrayUsage
emptyLoopArrayUsage = LoopArrayUsage False False

-- | Incorporate one access type into an existing usage summary.
accumulateArrayUsage :: LoopArrayUsage -> AccessType2 -> LoopArrayUsage
accumulateArrayUsage u accessType = case accessType of
  Read2      -> u { lauReads  = True }
  Write2     -> u { lauWrites = True }
  ReadWrite2 -> u { lauReads  = True, lauWrites = True }

-- | Build a per-array read/write summary for all accesses in a loop body.
collectLoopArrayUsage :: [Stmt] -> Map CVar LoopArrayUsage
collectLoopArrayUsage = foldl' record M.empty . extractAccesses2
  where
    record m acc =
      M.insertWith mergeUsage (aa2ArrayVar acc)
        (accumulateArrayUsage emptyLoopArrayUsage (aa2AccessType acc)) m

    mergeUsage a b = LoopArrayUsage
      { lauReads  = lauReads  a || lauReads  b
      , lauWrites = lauWrites a || lauWrites b
      }

------------------------------------------------------------------------
-- Legality checks
------------------------------------------------------------------------

-- | Conservative dependence-based legality check.  Returns 'True' when:
--
-- * the loop has a parallelizable trip count,
-- * no loop-carried non-forward dependence is found among the body's
--   array accesses (read-read pairs are excluded — they are never blocking),
-- * no array is both read and written inside the body, and
-- * no scalar variable exhibits a loop-carried mutation pattern — unless the
--   loop is a 'LoopReduction', whose accumulator is intentionally loop-carried
--   and handled by the OpenMP reduction clause.
passesConservativeDependenceCheck :: LoopSpec -> [Stmt] -> Bool
passesConservativeDependenceCheck spec body =
  isParallelTripCount2 (loopTripCount2 spec)
  && not hasBlockingDependence
  && not hasReadWriteHazard
  && (lsRole spec == LoopReduction || not (hasScalarCarriedDep spec body))
  where
    accesses = extractAccesses2 body
    deps     = findDependences2 accesses
    usage    = collectLoopArrayUsage body

    -- Read-read dependences never block parallelism; only consider pairs
    -- where at least one access is a write.
    isWriteAccess at = at == Write2 || at == ReadWrite2
    hasWriteEnd d    = isWriteAccess (aa2AccessType (depSource2 d))
                    || isWriteAccess (aa2AccessType (depTarget2 d))

    hasBlockingDependence =
      any (\d -> depIsLoopCarried2 d && depDirection2 d /= DDForward && hasWriteEnd d) deps
    hasReadWriteHazard =
      any (\u -> lauReads u && lauWrites u) (M.elems usage)

-- | Returns 'True' when the top-level body of a loop contains a scalar
-- variable that is both (a) read before it is first assigned at that level
-- AND (b) also assigned somewhere in the body — the hallmark of a
-- loop-carried accumulator (e.g. a prefix-sum @acc@).
--
-- Variables that are only read from the outer scope (loop-invariant) satisfy
-- (a) but not (b), so they are correctly excluded.  Loop iterator variables
-- (from 'lsIters') are treated as pre-initialised and never trigger the check.
-- Inner loop and conditional bodies are not recursed into.
hasScalarCarriedDep :: LoopSpec -> [Stmt] -> Bool
hasScalarCarriedDep spec body = not (S.null (rbw `S.intersection` writtenInBody))
  where
    initWritten :: Set CVar
    initWritten = S.fromList (lsIters spec)

    -- rbw: read before write at the top level; writtenInBody: all SAssign targets
    (rbw, _, writtenInBody) = foldl stepStmt (S.empty, initWritten, S.empty) body

    stepStmt :: (Set CVar, Set CVar, Set CVar) -> Stmt -> (Set CVar, Set CVar, Set CVar)
    stepStmt (rbw0, seenWritten, wib) stmt = case stmt of
      SAssign v rhs ->
        let used = atomsInRhs rhs `S.difference` seenWritten
        in  (rbw0 `S.union` used, S.insert v seenWritten, S.insert v wib)
      SArrayWrite _ idx val ->
        let used = S.fromList (atomVars idx ++ atomVars val) `S.difference` seenWritten
        in  (rbw0 `S.union` used, seenWritten, wib)
      -- Inner loops and conditionals are not recursed into; their scalar
      -- uses are not loop-carried at the outer level.
      _             -> (rbw0, seenWritten, wib)

    atomsInRhs :: RHS -> Set CVar
    atomsInRhs (RBinOp _ a b) = S.fromList (atomVars a ++ atomVars b)
    atomsInRhs (RUnOp  _ a)   = S.fromList (atomVars a)
    atomsInRhs (RAtom    a)   = S.fromList (atomVars a)
    atomsInRhs _              = S.empty

    atomVars :: Atom -> [CVar]
    atomVars (AVar v) = [v]
    atomVars _        = []

-- | Returns 'True' when lowering-provided array facts are sufficient to
-- prove that the loop body is embarrassingly parallel.  Every array
-- accessed in the body must be either a fresh write-once output (reads
-- are then rejected) or a read-only input.  Loops that read and write
-- the same array are conservatively rejected.
factsPermitParallelLoop :: ArrayFacts -> [Stmt] -> Bool
factsPermitParallelLoop arrayFacts body =
  not (M.null usage) && all arraySafe (M.toList usage)
  where
    usage = collectLoopArrayUsage body

    arraySafe (arr, u) = case M.lookup arr arrayFacts of
      -- Arrays with known facts follow the strict policy.
      Just fact
        | lauWrites u -> afFreshAlloc fact && afWriteOnce fact && not (lauReads u)
        | lauReads u  -> afReadOnly fact
        | otherwise   -> True
      -- Unknown arrays that are only *read* inside the loop body are always
      -- safe to access in parallel — reads never conflict with each other.
      -- Unknown arrays that are *written* (with or without reads) are unsafe
      -- without explicit facts proving they are fresh and write-once.
      Nothing
        | lauWrites u -> False
        | otherwise   -> True

------------------------------------------------------------------------
-- Index environment / scatter-kernel detection helpers
------------------------------------------------------------------------

-- | Resolve an 'Atom' to an 'IndexExpr' given a scalar variable
-- environment, or return 'Nothing' for atoms that cannot be expressed.
resolveAtomIndex :: Map CVar IndexExpr -> Atom -> Maybe IndexExpr
resolveAtomIndex env atom = case atom of
  AVar v -> Just (M.findWithDefault (IVar v) v env)
  AInt n -> Just (IConst n)
  _      -> Nothing

-- | Resolve an 'RHS' to an 'IndexExpr' if it denotes a simple arithmetic
-- expression.  Non-arithmetic forms (array loads, calls, etc.) return
-- 'Nothing' so callers can fall back to conservative treatment.
resolveRHSIndex :: Map CVar IndexExpr -> RHS -> Maybe IndexExpr
resolveRHSIndex env rhs = case rhs of
  RAtom a       -> resolveAtomIndex env a
  RProj i a     -> IProj (fromIntegral i) <$> resolveAtomIndex env a
  RFlatToNd a b -> IFlatToNd <$> resolveAtomIndex env a <*> resolveAtomIndex env b
  RNdToFlat a b -> INdToFlat <$> resolveAtomIndex env a <*> resolveAtomIndex env b
  RBinOp op a b -> case op of
    CAdd -> IAdd <$> resolveAtomIndex env a <*> resolveAtomIndex env b
    CSub -> ISub <$> resolveAtomIndex env a <*> resolveAtomIndex env b
    CMul -> IMul <$> resolveAtomIndex env a <*> resolveAtomIndex env b
    CDiv -> IDiv <$> resolveAtomIndex env a <*> resolveAtomIndex env b
    _    -> Nothing
  _ -> Nothing

-- | Build an index expression environment by scanning a flat statement
-- list for pure scalar assignments.  Used to inline index arithmetic
-- when examining array write indices.
buildIndexEnv :: [Stmt] -> Map CVar IndexExpr
buildIndexEnv = foldl' step M.empty
  where
    step env (SAssign v rhs) = case resolveRHSIndex env rhs of
      Just expr -> M.insert v expr env
      Nothing   -> env
    step env _ = env

-- | Returns 'True' when @idxExpr@ maps the single loop iterator to itself
-- (possibly with a constant offset), making the write index injective over
-- the iteration space.
isInjectiveScatterIndex :: LoopSpec -> IndexExpr -> Bool
isInjectiveScatterIndex spec idxExpr =
  case (lsIters spec, simplifyIndexExpr idxExpr) of
    ([i], IVar j)                                         -> i == j
    ([i], IAdd (IVar j) (IConst _))                       -> i == j
    ([i], IProj 0 (IFlatToNd (IVar j) _))                 -> i == j
    ([i], IAdd (IProj 0 (IFlatToNd (IVar j) _)) (IConst _)) -> i == j
    _ -> False

-- | Identify an injective scatter kernel: a loop body containing exactly
-- one array write whose index is injective in the loop iterator, together
-- with a matching read at the same index.  Returns 'Nothing' when the
-- body does not match this pattern.
detectInjectiveScatterKernel :: LoopSpec -> [Stmt] -> Maybe ScatterKernelInfo
detectInjectiveScatterKernel spec body = do
  let env = buildIndexEnv body
  (destArr, writeIdx) <- case
    [ (arr, expr)
    | SArrayWrite (AVar arr) idxAtom _ <- body
    , Just expr <- [resolveAtomIndex env idxAtom]
    ] of
    [single] -> Just single
    _        -> Nothing
  let hasMatchingRead stmt = case stmt of
        SAssign _ (RArrayLoad (AVar arr) idxAtom) ->
          arr == destArr
          && fmap simplifyIndexExpr (resolveAtomIndex env idxAtom)
               == Just (simplifyIndexExpr writeIdx)
        _ -> False
  if any hasMatchingRead body && isInjectiveScatterIndex spec writeIdx
    then Just (ScatterKernelInfo destArr)
    else Nothing

-- | Returns 'True' when facts prove that an injective scatter loop over a
-- fresh write-once array is safe to parallelize directly (without atomics
-- or privatization).  Requires the destination array to be fresh and
-- write-once, and all other accessed arrays to be read-only.
factsPermitDirectScatterLoop :: ArrayFacts -> LoopSpec -> [Stmt] -> Bool
factsPermitDirectScatterLoop arrayFacts spec body =
  isParallelTripCount2 (loopTripCount2 spec) &&
  case detectInjectiveScatterKernel spec body of
    Nothing   -> False
    Just info ->
      let dest  = skiDestArray info
          usage = collectLoopArrayUsage body
          destSafe = case (M.lookup dest usage, M.lookup dest arrayFacts) of
            (Just u, Just fact) ->
              lauReads u && lauWrites u && afFreshAlloc fact && afWriteOnce fact
            _ -> False
      in  destSafe && all (safeNonDest dest) (M.toList usage)
  where
    safeNonDest dest (arr, u)
      | arr == dest = True   -- destination is checked by destSafe above
      | lauWrites u = False  -- non-destination writes are unsafe
      | otherwise   = True

-- | Strip a conditional filter wrapper from a potential atomic-add loop
-- body.  If the last statement is @if cond then body else {}@, returns
-- @(Just cond, prefix ++ body)@ to let the detector inspect the inner
-- accesses directly.  Returns @(Nothing, body)@ when no such wrapper
-- is present.
scatterAtomicAddBody :: [Stmt] -> (Maybe Atom, [Stmt])
scatterAtomicAddBody body = case reverse body of
  SIf cond thn [] : revPrefix -> (Just cond, reverse revPrefix ++ thn)
  _                            -> (Nothing, body)

-- | Identify an atomic-add scatter kernel: a loop body that reads a
-- value from an array at some index, adds a scalar contribution, and
-- writes the result back to the same index.  Returns the destination
-- array and the value element type on success.
detectScatterAtomicAddKernel :: LoopSpec -> [Stmt] -> Maybe ScatterAtomicAddInfo
detectScatterAtomicAddKernel spec body = do
  let (_, atomicBody) = scatterAtomicAddBody body
      env             = buildIndexEnv body
  (destArr, writeIdx, newVar) <- case
    [ (arr, expr, nv)
    | SArrayWrite (AVar arr) idxAtom (AVar nv) <- atomicBody
    , Just expr <- [resolveAtomIndex env idxAtom]
    ] of
    [single] -> Just single
    _        -> Nothing
  oldVar <- case
    [ ov
    | SAssign ov (RArrayLoad (AVar arr) idxAtom) <- atomicBody
    , arr == destArr
    , fmap simplifyIndexExpr (resolveAtomIndex env idxAtom)
        == Just (simplifyIndexExpr writeIdx)
    ] of
    [single] -> Just single
    _        -> Nothing
  valueTy <- case
    [ ty
    | SAssign nv (RBinOp op a b) <- atomicBody
    , nv == newVar
    , usesOldVar oldVar a b
    , Just ty <- [addResultType op]
    ] of
    [single] -> Just single
    _        -> Nothing
  if isParallelTripCount2 (loopTripCount2 spec)
    then Just (ScatterAtomicAddInfo destArr valueTy)
    else Nothing
  where
    -- Returns True when one of the two atoms is a reference to @ov@,
    -- confirming that the new value is computed from the old element.
    usesOldVar ov a b = case (a, b) of
      (AVar v, _) | v == ov -> True
      (_, AVar v) | v == ov -> True
      _                     -> False

    addResultType op = case op of
      CAdd  -> Just CTInt64
      CAddF -> Just CTDouble
      _     -> Nothing

-- | Returns 'True' when facts prove that an atomic-add scatter loop over
-- a fresh destination array is safe to parallelize using hardware atomic
-- operations.
factsPermitAtomicScatterLoop :: ArrayFacts -> Map CVar CType -> LoopSpec -> [Stmt] -> Bool
factsPermitAtomicScatterLoop arrayFacts typeEnv spec body =
  case detectScatterAtomicAddKernel spec body of
    Nothing   -> False
    Just info ->
      let dest   = saDestArray info
          elemTy = saValueType info
          usage  = collectLoopArrayUsage body
          destSafe = case (M.lookup dest usage, M.lookup dest arrayFacts, M.lookup dest typeEnv) of
            (Just u, Just fact, Just (CTArray ty)) | ty == elemTy ->
              lauReads u && lauWrites u && afFreshAlloc fact && afWriteOnce fact
            _ -> False
      in  destSafe && all (safeNonDest dest) (M.toList usage)
  where
    safeNonDest dest (arr, u)
      | arr == dest = True
      | lauWrites u = False
      | otherwise   = True

-- | Collect the total element counts of statically-sized array
-- allocations.  Used by the privatization profitability heuristic to
-- decide whether a scatter destination is small enough to privatize per
-- thread.
collectArrayAllocSizes :: [Stmt] -> Map CVar Integer
collectArrayAllocSizes = snd . go M.empty M.empty
  where
    go tuples allocs [] = (tuples, allocs)
    go tuples allocs (stmt : rest) = case stmt of
      SAssign v (RTuple atoms) ->
        let tuples' = case constantProduct atoms of
              Just sz -> M.insert v sz tuples
              Nothing -> tuples
        in  go tuples' allocs rest
      SAssign v (RArrayAlloc (AVar shpVar)) ->
        let allocs' = case M.lookup shpVar tuples of
              Just sz -> M.insert v sz allocs
              Nothing -> allocs
        in  go tuples allocs' rest
      SLoop _ body ->
        let (_, bodyAllocs) = go tuples M.empty body
        in  go tuples (M.union allocs bodyAllocs) rest
      SIf _ thn els ->
        let (_, thnAllocs) = go tuples M.empty thn
            (_, elsAllocs) = go tuples M.empty els
        in  go tuples (M.unions [allocs, thnAllocs, elsAllocs]) rest
      _ -> go tuples allocs rest

    constantProduct atoms = product <$> traverse atomInt atoms
    atomInt (AInt n) = Just n
    atomInt _        = Nothing

-- | Returns 'True' when privatized reduction is both legal and
-- profitable: the destination array is a fresh integer array small
-- enough (≤ 64 elements) to fit comfortably in per-thread private
-- storage, and the loop trip count is large enough to amortize the
-- initialization and reduction costs.
factsPermitPrivatizedIntScatterLoop
  :: ArrayFacts -> Map CVar CType -> Map CVar Integer -> LoopSpec -> [Stmt] -> Bool
factsPermitPrivatizedIntScatterLoop arrayFacts typeEnv allocSizes spec body =
  case detectScatterAtomicAddKernel spec body of
    Nothing   -> False
    Just info ->
      let dest  = saDestArray info
          usage = collectLoopArrayUsage body
      in  case (M.lookup dest usage, M.lookup dest arrayFacts, M.lookup dest typeEnv) of
            (Just u, Just fact, Just (CTArray CTInt64)) ->
              lauReads u
              && lauWrites u
              && afFreshAlloc fact
              && afWriteOnce fact
              && profitable dest
              && all (safeNonDest dest) (M.toList usage)
            _ -> False
  where
    profitable dest =
      case (M.lookup dest allocSizes, tripCountValue2 (loopTripCount2 spec)) of
        (Just sz, Just tc) -> sz > 0 && sz <= 64 && tc >= max 32 (sz * 4)
        _                  -> not (hasExternalReads dest body)

    safeNonDest dest (arr, u)
      | arr == dest = True
      | lauWrites u = False
      | otherwise   = True

    hasExternalReads dest = any (stmtHasExternalRead dest)

    stmtHasExternalRead dest stmt = case stmt of
      SAssign _ (RArrayLoad (AVar arr) _) -> arr /= dest
      SLoop _ inner                       -> hasExternalReads dest inner
      SIf _ thn els                       ->
        hasExternalReads dest thn || hasExternalReads dest els
      _                                   -> False

------------------------------------------------------------------------
-- Loop parallelizability
------------------------------------------------------------------------

-- | Returns 'True' when a loop at the given nesting depth should be
-- considered for parallelization.  Reduction-wrapper loops are never
-- parallelized; all other loop roles are eligible only at the outermost
-- level (@insideLoop = False@).
shouldParallelizeLoop :: Bool -> LoopSpec -> Bool
shouldParallelizeLoop insideLoop spec = case lsRole spec of
  LoopReductionWrapper -> False
  LoopFold             -> False  -- foldl/scan loops carry an accumulator; never parallelizable
  _                    -> not insideLoop

-- | Returns 'True' when any statement in the list (or in nested conditionals,
-- but not nested loops) contains a 'SBreak'.  Loops containing breaks cannot
-- be parallelized because OpenMP does not allow @break@ inside parallel regions.
bodyContainsBreak :: [Stmt] -> Bool
bodyContainsBreak = any go
  where
    go SBreak           = True
    go (SIf _ thn els)  = any go thn || any go els
    go _                = False

-- | Attempt to annotate a single loop with a parallel execution policy.
-- Returns 'Nothing' if the loop is already non-serial, is nested inside
-- another parallel candidate, or no legality check succeeds.
-- On success, returns the updated 'LoopSpec' (with the 'Parallel' policy
-- and chosen strategy) together with the (unchanged) body.
parallelizeLoop
  :: ArrayFacts
  -> Map CVar CType
  -> Map CVar Integer
  -> Bool
  -> LoopSpec
  -> [Stmt]
  -> Maybe (LoopSpec, [Stmt])
parallelizeLoop arrayFacts typeEnv allocSizes insideLoop spec body
  | lsExec spec /= Serial                    = Nothing
  | not (shouldParallelizeLoop insideLoop spec) = Nothing
  | bodyContainsBreak body                   = Nothing
  | not eligible                             = Nothing
  | otherwise = Just (spec { lsExec = Parallel (ParallelSpec strategy Nothing) }, body)
  where
    eligible =
         canParallelizeWithFacts
      || canParallelizeDirectScatter
      || canParallelizePrivatizedScatter
      || canParallelizeAtomicScatter
      || passesConservativeDependenceCheck spec body

    strategy
      | canParallelizeDirectScatter     = ParallelScatterDirect
      | canParallelizePrivatizedScatter = ParallelScatterPrivatizedIntAdd
      | canParallelizeAtomicScatter     = case detectScatterAtomicAddKernel spec body of
          Just info | saValueType info == CTDouble -> ParallelScatterAtomicAddFloat
          _                                        -> ParallelScatterAtomicAddInt
      | otherwise                       = ParallelGeneric

    canParallelizeWithFacts =
      lsRole spec == LoopMap
      && isParallelTripCount2 (loopTripCount2 spec)
      && factsPermitParallelLoop arrayFacts body
    canParallelizeDirectScatter     = factsPermitDirectScatterLoop     arrayFacts           spec body
    canParallelizePrivatizedScatter = factsPermitPrivatizedIntScatterLoop arrayFacts typeEnv allocSizes spec body
    canParallelizeAtomicScatter     = factsPermitAtomicScatterLoop     arrayFacts typeEnv   spec body

------------------------------------------------------------------------
-- Statement and procedure traversal
------------------------------------------------------------------------

parallelizeProcWithFacts
  :: ArrayFacts -> Map CVar CType -> Map CVar Integer -> [Stmt] -> [Stmt]
parallelizeProcWithFacts arrayFacts typeEnv allocSizes stmts = go False stmts
  where
    -- insideParallel: True only when we are inside a loop that will receive
    -- #pragma omp parallel for.  Being inside a *serial* loop (e.g. the foldl
    -- outer loop) does not block inner LoopMap loops from being parallelized.
    go insideParallel = concatMap (goStmt insideParallel)

    goStmt insideParallel stmt = case stmt of
      SLoop spec body ->
        -- Tentatively check whether this loop will be parallelized (using the
        -- original body) so we can set the correct context for the inner loops.
        let outerWillParallel = case parallelizeLoop arrayFacts typeEnv allocSizes
                                       insideParallel spec body of
              Just _  -> True
              Nothing -> False
            -- Block inner parallelism when:
            --   (a) the outer loop will be parallel (prevents nested omp regions), or
            --   (b) the outer loop is a LoopReductionWrapper (inner LoopReduction must
            --       use OpenMP reduction semantics, not generic parallel).
            outerBlocksInner = outerWillParallel || lsRole spec == LoopReductionWrapper
            -- Recurse into body with updated context.
            body' = go (insideParallel || outerBlocksInner) body
        -- Now apply parallelization to the outer loop using the traversed body.
        in case parallelizeLoop arrayFacts typeEnv allocSizes insideParallel spec body' of
             Just (spec', _) -> [SLoop spec' body']
             Nothing         -> [SLoop spec  body']
      _ -> [stmt]

-- | Rewrite eligible serial loops in one procedure using the array facts
-- and type environment attached to that procedure by the lowering pass.
parallelizeProc2 :: Proc -> Proc
parallelizeProc2 proc =
  let typeEnv    = recoverProcTypeEnv2 M.empty M.empty proc
      allocSizes = collectArrayAllocSizes (procBody proc)
  in  proc { procBody = parallelizeProcWithFacts
                          (procArrayFacts proc) typeEnv allocSizes (procBody proc) }

-- | Parallelize every procedure in a program.
parallelizeProgram2 :: Program -> Program
parallelizeProgram2 (Program procs) = Program (map parallelizeProc2 procs)

-- | Rewrite eligible serial loops in a statement list using the
-- conservative dependence-based legality check (no array facts available).
parallelizeStmts2 :: [Stmt] -> [Stmt]
parallelizeStmts2 = procBody . parallelizeProc2 . mkProc "parallelize_stmts2" []
