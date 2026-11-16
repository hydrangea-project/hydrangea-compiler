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
  ( parallelizeProc
  , parallelizeProgram
  , parallelizeStmts
  ) where

import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as S
import Language.Hydrangea.CFGCore (Atom(..), BinOp(..), CType(..), RHS(..), rhsAtoms)
import Language.Hydrangea.CFG
import Language.Hydrangea.CFGTyping (recoverProcTypeEnv)
import Language.Hydrangea.CFGAnalysis
  ( definedVarsStmts
  , isParallelTripCount
  , loopTripCount
  , tripCountValue
  , usedVarsAtom
  , usedVarsStmts
  )
import Language.Hydrangea.Dependence
  ( findDependences
  , ArrayAccess(..)
  , AccessType(..)
  , Dependence(..)
  , depDirection
  , depIsLoopCarried
  , DependenceDirection(..)
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

-- | Result of identifying an overwrite scatter kernel: the destination array
-- and the element type.
data ScatterAtomicWriteInfo = ScatterAtomicWriteInfo
  { swDestArray :: CVar
  , swValueType :: CType
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
extractAccesses :: [Stmt] -> [ArrayAccess]
extractAccesses stmts =
  zipWith (\i acc -> acc { aaStmtIndex = i }) [0..] (collect stmts)
  where
    collect :: [Stmt] -> [ArrayAccess]
    collect = concatMap collectStmt

    collectStmt (SArrayWrite (AVar arr) idx _) =
      [ArrayAccess arr (atomToIndexExpr idx) Write 0]
    collectStmt (SAssign _ (RArrayLoad (AVar arr) idx)) =
      [ArrayAccess arr (atomToIndexExpr idx) Read 0]
    -- Do not recurse into nested loops: the outer loop's legality is
    -- determined only by accesses at this level; nested-loop deps are
    -- handled by those loops' own legality checks.
    collectStmt (SLoop _ _)             = []
    collectStmt (SParallelRegion body)  = collect body
    collectStmt (SIf _ thn els)         = collect thn ++ collect els
    collectStmt _                       = []

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
accumulateArrayUsage :: LoopArrayUsage -> AccessType -> LoopArrayUsage
accumulateArrayUsage u accessType = case accessType of
  Read      -> u { lauReads  = True }
  Write     -> u { lauWrites = True }

-- | Build a per-array read/write summary for all accesses in a loop body.
collectLoopArrayUsage :: [Stmt] -> Map CVar LoopArrayUsage
collectLoopArrayUsage = foldl' record M.empty . extractAccesses
  where
    record m acc =
      M.insertWith mergeUsage (aaArrayVar acc)
        (accumulateArrayUsage emptyLoopArrayUsage (aaAccessType acc)) m

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
  isParallelTripCount (loopTripCount spec)
  && not hasBlockingDependence
  && not hasReadWriteHazard
  && (lsRole spec == LoopReduction || not (hasScalarCarriedDep spec body))
  where
    accesses = extractAccesses body
    deps     = findDependences accesses
    usage    = collectLoopArrayUsage body

    -- Read-read dependences never block parallelism; only consider pairs
    -- where at least one access is a write.
    isWriteAccess at = at == Write
    hasWriteEnd d    = isWriteAccess (aaAccessType (depSource d))
                    || isWriteAccess (aaAccessType (depTarget d))

    hasBlockingDependence =
      any (\d -> depIsLoopCarried d && depDirection d /= DDForward && hasWriteEnd d) deps
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
  isParallelTripCount (loopTripCount spec) &&
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
  if isParallelTripCount (loopTripCount spec)
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
      SParallelRegion body ->
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

-- | Returns the destination element type when privatized reduction is
-- both legal and profitable.  Legal: the loop is an atomic-add scatter
-- (detected by 'detectScatterAtomicAddKernel'); the destination is a
-- fresh, write-once int64 or double array; no other array in the loop
-- is written.  Profitable: the destination's static size fits within
-- the per-thread privatization budget, and the loop trip count is
-- large enough to amortize the per-thread private buffer allocation
-- and the merge phase.  When destinations are sized via env vars
-- (every benchmark in the suite), the shape is not a literal tuple
-- and the symbolic-size branch fires.
factsPermitPrivatizedScatterLoop
  :: ArrayFacts -> Map CVar CType -> Map CVar Integer -> LoopSpec -> [Stmt]
  -> Maybe CType
factsPermitPrivatizedScatterLoop arrayFacts typeEnv allocSizes spec body =
  case detectScatterAtomicAddKernel spec body of
    Nothing   -> Nothing
    Just info ->
      let dest   = saDestArray info
          elemTy = saValueType info
          usage  = collectLoopArrayUsage body
      in  case (M.lookup dest usage, M.lookup dest arrayFacts, M.lookup dest typeEnv) of
            (Just u, Just fact, Just (CTArray ty))
              | ty == elemTy
              , ty == CTInt64 || ty == CTDouble
              , lauReads u
              , lauWrites u
              , afFreshAlloc fact
              , afWriteOnce fact
              , profitable dest
              , all (safeNonDest dest) (M.toList usage) -> Just ty
            _ -> Nothing
  where
    -- Per-thread privatization budget when the size is statically
    -- known.  262144 = 64^3 lets the voxel benchmarks (64^3 grids)
    -- privatize statically; for typical 8-16 threads this is 16-32
    -- MB total scratch.
    privatizationCap = 262144

    profitable dest =
      case (M.lookup dest allocSizes, tripCountValue (loopTripCount spec)) of
        (Just sz, Just tc) -> sz > 0 && sz <= privatizationCap && tc >= max 16 sz
        (Just sz, Nothing) -> sz > 0 && sz <= privatizationCap
        _                  -> True

    safeNonDest dest (arr, u)
      | arr == dest = True
      | lauWrites u = False
      | otherwise   = True

------------------------------------------------------------------------
-- Overwrite and read-modify-write scatters
------------------------------------------------------------------------

-- | Every definition of each variable anywhere in a procedure body.
type DefEnv = Map CVar [RHS]

-- | The 'DefEnv' of a procedure body.  The buffer swap that ends a
-- double-buffered @iterate@ body is left out: it makes the two buffers copies
-- of each other, but within one iteration they are distinct, and every check
-- here concerns races inside a loop that runs within one iteration.
collectDefEnv :: [Stmt] -> DefEnv
collectDefEnv = go M.empty
  where
    go = foldl' goStmt
    goStmt acc stmt = case stmt of
      SAssign v rhs         -> M.insertWith (flip (++)) v [rhs] acc
      SLoop spec body
        | lsRole spec == LoopIterate -> go acc (dropBufferSwap body)
      SLoop _ body          -> go acc body
      SParallelRegion body  -> go acc body
      SIf _ thn els         -> go (go acc thn) els
      _                     -> acc

-- | Strip the trailing @cur = next; next = tmp; tmp = cur@ swap that
-- 'Language.Hydrangea.CFGOpt.hoistIterateAllocs' appends to a
-- double-buffered iterate body.
dropBufferSwap :: [Stmt] -> [Stmt]
dropBufferSwap body = case splitAt (length body - 3) body of
  ( prefix
    , [ SAssign c (RAtom (AVar n))
      , SAssign n' (RAtom (AVar t))
      , SAssign t' (RAtom (AVar c'))
      ] )
    | n == n', t == t', c == c', "__iter_tmp" `BS.isSuffixOf` t -> prefix
  _ -> body

-- | Does the value of this atom depend, through the definitions in @defs@, on
-- the contents of array @arr@?  A variable with no definition (a parameter)
-- is independent unless it is @arr@ itself.
atomDependsOnArray :: DefEnv -> CVar -> Set CVar -> Atom -> Bool
atomDependsOnArray defs arr seen atom = case atom of
  AVar v
    | v == arr          -> True
    | v `S.member` seen -> False
    | otherwise         -> maybe False (any (rhsDependsOnArray defs arr (S.insert v seen))) (M.lookup v defs)
  _ -> False

-- | 'atomDependsOnArray' for a right-hand side.  An allocation or a shape
-- query does not read an array's contents, so a buffer allocated with the
-- destination's shape is not a dependence on it.
rhsDependsOnArray :: DefEnv -> CVar -> Set CVar -> RHS -> Bool
rhsDependsOnArray defs arr seen rhs = case rhs of
  RArrayAlloc _ -> False
  RArrayShape _ -> False
  _             -> any (atomDependsOnArray defs arr seen) (rhsAtoms rhs)

-- | Identify an overwrite scatter: a loop that stores an already-computed
-- value into a destination array, where neither the value nor the index
-- depends on the destination's contents.
detectScatterAtomicWriteKernel
  :: DefEnv -> Map CVar CType -> LoopSpec -> [Stmt] -> Maybe ScatterAtomicWriteInfo
detectScatterAtomicWriteKernel defEnv typeEnv spec body = do
  let (_, coreBody) = scatterAtomicAddBody body
  (destArr, idxAtom, valVar) <- case
    [ (arr, idxAtom, v)
    | SArrayWrite (AVar arr) idxAtom (AVar v) <- coreBody
    ] of
      [single] -> Just single
      _        -> Nothing
  let independent =
        not (atomDependsOnArray defEnv destArr S.empty (AVar valVar))
          && not (atomDependsOnArray defEnv destArr S.empty idxAtom)
  if not independent
    then Nothing
    else case M.lookup destArr typeEnv of
      Just (CTArray ty)
        | ty == CTInt64 || ty == CTDouble
        , isParallelTripCount (loopTripCount spec) -> Just (ScatterAtomicWriteInfo destArr ty)
      _ -> Nothing

-- | Names that denote the same buffer as @root@, through copy assignments in
-- either direction (such as the renames around an iterate's loop state).
copyAliases :: DefEnv -> CVar -> Set CVar
copyAliases defs root = go (S.singleton root) [root]
  where
    reverseCopies = [(u, k) | (k, rhss) <- M.toList defs, RAtom (AVar u) <- rhss]
    go seen [] = seen
    go seen (v : vs) =
      let fwd = [u | Just rhss <- [M.lookup v defs], RAtom (AVar u) <- rhss]
          bwd = [k | (u, k) <- reverseCopies, u == v]
          new = filter (`S.notMember` seen) (fwd ++ bwd)
      in  go (foldr S.insert seen new) (new ++ vs)

-- | True when, from this loop onwards, @body@ holds every write in the
-- procedure to @dest@ or one of its aliases: nothing else can race with the
-- loop's writes.  Writes before the loop fill the buffer and are done.
destWrittenOnlyIn :: DefEnv -> CVar -> [Stmt] -> [Stmt] -> Bool
destWrittenOnlyIn defEnv dest procStmts body =
  countAliasWrites fromLoop == countAliasWrites body
  where
    fromLoop = case break containsBody procStmts of
      (_, rest@(_ : _)) -> rest
      _                 -> procStmts
    containsBody st = any isThisLoop (allStmtsRec [st])
    isThisLoop (SLoop _ b) = b == body
    isThisLoop _           = False
    aliasSet = copyAliases defEnv dest
    countAliasWrites = length . filter isAliasWrite . allStmtsRec
    isAliasWrite (SArrayWrite (AVar w) _ _) = w `S.member` aliasSet
    isAliasWrite _                          = False

-- | Every statement, including those nested in loops, branches and regions.
allStmtsRec :: [Stmt] -> [Stmt]
allStmtsRec = concatMap go
  where
    go stmt = stmt : case stmt of
      SLoop _ b           -> allStmtsRec b
      SParallelRegion b   -> allStmtsRec b
      SIf _ thn els       -> allStmtsRec thn ++ allStmtsRec els
      _                   -> []

-- | True when an overwrite scatter loop may run in parallel with an atomic
-- store: the destination is exclusive to the loop (a fresh write-once buffer,
-- or one no other statement writes) and no other array is written.
factsPermitAtomicWriteScatterLoop
  :: ArrayFacts -> DefEnv -> Map CVar CType -> [Stmt] -> LoopSpec -> [Stmt] -> Bool
factsPermitAtomicWriteScatterLoop arrayFacts defEnv typeEnv procStmts spec body =
  case detectScatterAtomicWriteKernel defEnv typeEnv spec body of
    Nothing   -> False
    Just info ->
      let dest   = swDestArray info
          elemTy = swValueType info
          usage  = collectLoopArrayUsage body
          destWrites = maybe False lauWrites (M.lookup dest usage)
          destTypeOk = case M.lookup dest typeEnv of
            Just (CTArray ty) -> ty == elemTy
            _                 -> False
          destExclusive = case M.lookup dest arrayFacts of
            Just fact | afFreshAlloc fact && afWriteOnce fact -> True
            _ -> destWrittenOnlyIn defEnv dest procStmts body
      in     destWrites && destTypeOk && destExclusive
          && and [ not (lauWrites u) | (arr, u) <- M.toList usage, arr /= dest ]

-- | Where a buffer comes from: a local allocation, the result of a
-- zero-argument procedure (which may be cached and so shared), or unknown.
data BufferOrigin = OLocal CVar | OCall CVar | OUnknown
  deriving (Eq, Ord, Show)

isCallOrigin :: BufferOrigin -> Bool
isCallOrigin (OCall _) = True
isCallOrigin _         = False

bufferOrigins :: DefEnv -> CVar -> Set BufferOrigin
bufferOrigins defEnv v =
  let found = S.fromList
        [ o
        | a <- S.toList (copyAliases defEnv v)
        , Just rhss <- [M.lookup a defEnv]
        , rhs <- rhss
        , Just o <- [originOf a rhs] ]
  in if S.null found then S.singleton OUnknown else found
  where
    originOf a rhs = case rhs of
      RArrayAlloc _ -> Just (OLocal a)
      RCall f []    -> Just (OCall f)
      RCall _ _     -> Just OUnknown
      RAtom _       -> Nothing  -- a copy, followed by 'copyAliases'
      _             -> Just OUnknown

-- | The element type of a read-modify-write scatter loop that may run in
-- parallel as a CAS loop.  The streams (index, guard, value and combine
-- inputs) must not depend on the destination, the destination must be
-- exclusive to the loop and not alias any array it reads, and no other array
-- may be written.
factsPermitRMWScatterLoop
  :: ArrayFacts -> DefEnv -> Map CVar CType -> [Stmt] -> LoopSpec -> [Stmt] -> Maybe CType
factsPermitRMWScatterLoop arrayFacts defEnv typeEnv procStmts spec body = do
  rmw <- decomposeRMWScatter body
  dest <- case rmwArray rmw of
    AVar d -> Just d
    _      -> Nothing
  elemTy <- case M.lookup dest typeEnv of
    Just (CTArray ty) | ty == CTInt64 || ty == CTDouble -> Just ty
    _                                                   -> Nothing
  let old = rmwOld rmw
      streamStmts = rmwOuterPrefix rmw ++ rmwBranchPrefix rmw
      inputs = S.unions
        [ usedVarsStmts streamStmts
        , maybe S.empty usedVarsAtom (rmwGuard rmw)
        , usedVarsAtom (rmwIndex rmw)
        , usedVarsStmts (rmwCombine rmw) `S.difference` definedVarsStmts (rmwCombine rmw)
        ]
        `S.difference` S.singleton old
      independent = not (any (atomDependsOnArray defEnv dest S.empty . AVar) (S.toList inputs))
      usage = collectLoopArrayUsage body
      destOrigins = bufferOrigins defEnv dest
      readsCannotAlias = and
        [ S.null (S.intersection ro destOrigins)
            && not (OUnknown `S.member` ro && any isCallOrigin (S.toList destOrigins))
        | (arr, u) <- M.toList usage, arr /= dest, lauReads u
        , let ro = bufferOrigins defEnv arr ]
      destExclusive = case M.lookup dest arrayFacts of
        Just fact | afFreshAlloc fact && afWriteOnce fact -> True
        _ -> not (OUnknown `S.member` destOrigins) && readsCannotAlias
               && destWrittenOnlyIn defEnv dest procStmts body
      otherArraysReadOnly = and [ not (lauWrites u) | (arr, u) <- M.toList usage, arr /= dest ]
  if independent && destExclusive && otherArraysReadOnly
       && isParallelTripCount (loopTripCount spec)
    then Just elemTy
    else Nothing

------------------------------------------------------------------------
-- Loop parallelizability
------------------------------------------------------------------------

-- | Returns 'True' when a loop at the given nesting depth should be
-- considered for parallelization. Plain reduction-wrapper loops remain
-- serial, but tiled reduction wrappers that carry a 'ReductionSpec' are
-- eligible at the outermost level so the existing OpenMP reduction path can
-- combine tile-private partial reductions.
shouldParallelizeLoop :: Bool -> LoopSpec -> Bool
shouldParallelizeLoop insideLoop spec = case lsRole spec of
  LoopReductionWrapper -> not insideLoop && lsRed spec /= Nothing
  LoopFold             -> False  -- foldl/scan loops carry an accumulator; never parallelizable
  LoopIterate          -> False  -- iterate temporal loop carries array state; never parallelizable
  LoopCopy             -> False  -- privatization snapshot; lowered as a serial memcpy
  LoopSegRedOuter      -> not insideLoop  -- segmented reduce outer; always eligible
  _                    -> not insideLoop

-- | Returns 'True' when any statement in the list (or in nested conditionals,
-- but not nested loops) contains a 'SBreak'.  Loops containing breaks cannot
-- be parallelized because OpenMP does not allow @break@ inside parallel regions.
bodyContainsBreak :: [Stmt] -> Bool
bodyContainsBreak = any go
  where
    go SBreak           = True
    go (SParallelRegion body) = any go body
    go (SIf _ thn els)  = any go thn || any go els
    go _                = False

-- | Attempt to annotate a single loop with a parallel execution policy.
-- Returns 'Nothing' if the loop is already non-serial, is nested inside
-- another parallel candidate, or no legality check succeeds.
-- On success, returns the updated 'LoopSpec' (with the 'Parallel' policy
-- and chosen strategy) together with the (unchanged) body.
parallelizeLoop
  :: ArrayFacts
  -> DefEnv         -- ^ definitions in the whole procedure
  -> Map CVar CType
  -> Map CVar Integer
  -> [Stmt]         -- ^ the whole procedure body
  -> Bool
  -> LoopSpec
  -> [Stmt]
  -> Maybe (LoopSpec, [Stmt])
parallelizeLoop arrayFacts defEnv typeEnv allocSizes procStmts insideLoop spec body
  | execIsUnsupported                          = Nothing
  | not (shouldParallelizeLoop insideLoop spec) = Nothing
  | bodyContainsBreak body                     = Nothing
  | not eligible                               = Nothing
  | otherwise = Just (spec { lsExec = Parallel (ParallelSpec strategy Nothing simdLen) }, body)
  where
    -- Already-parallel loops are never re-parallelized; Serial and Vector
    -- loops are both eligible (Vector loops become combined parallel+simd).
    execIsUnsupported = case lsExec spec of
      Serial   -> False
      Vector _ -> False
      _        -> True

    -- Preserve the SIMD width when upgrading a Vector loop to Parallel so
    -- that CodegenC can emit '#pragma omp parallel for simd simdlen(N)'.
    simdLen = case lsExec spec of
      Vector v -> Just (vsWidth v)
      _        -> Nothing

    eligible =
         canParallelizeWithFacts
      || canParallelizeDirectScatter
      || isJust privatizedScatterTy
      || canParallelizeAtomicScatter
      || canParallelizeAtomicWriteScatter
      || isJust rmwScatterTy
      || passesConservativeDependenceCheck spec body

    strategy
      | canParallelizeDirectScatter          = ParallelScatterDirect
      | Just CTInt64  <- privatizedScatterTy = ParallelScatterPrivatizedIntAdd
      | Just CTDouble <- privatizedScatterTy = ParallelScatterPrivatizedFloatAdd
      | canParallelizeAtomicScatter          = case detectScatterAtomicAddKernel spec body of
          Just info | saValueType info == CTDouble -> ParallelScatterAtomicAddFloat
          _                                        -> ParallelScatterAtomicAddInt
      | canParallelizeAtomicWriteScatter     = case detectScatterAtomicWriteKernel defEnv typeEnv spec body of
          Just info | swValueType info == CTDouble -> ParallelScatterAtomicWriteFloat
          _                                        -> ParallelScatterAtomicWriteInt
      | Just CTDouble <- rmwScatterTy        = ParallelScatterRMWFloat
      | Just _        <- rmwScatterTy        = ParallelScatterRMWInt
      | otherwise                            = ParallelGeneric

    canParallelizeWithFacts =
      lsRole spec `elem` [LoopMap, LoopMapReduction]
      && isParallelTripCount (loopTripCount spec)
      && factsPermitParallelLoop arrayFacts body
    canParallelizeDirectScatter = factsPermitDirectScatterLoop arrayFacts spec body
    privatizedScatterTy         = factsPermitPrivatizedScatterLoop arrayFacts typeEnv allocSizes spec body
    canParallelizeAtomicScatter = factsPermitAtomicScatterLoop arrayFacts typeEnv spec body
    -- The cheaper strategies come first: an ordinary map loop also has the
    -- shape of an overwrite scatter, and must not pay for atomics.
    cheaperStrategy =
      canParallelizeWithFacts || canParallelizeDirectScatter
        || isJust privatizedScatterTy || canParallelizeAtomicScatter
    canParallelizeAtomicWriteScatter =
      not cheaperStrategy
        && factsPermitAtomicWriteScatterLoop arrayFacts defEnv typeEnv procStmts spec body
    rmwScatterTy
      | cheaperStrategy || canParallelizeAtomicWriteScatter = Nothing
      | otherwise = factsPermitRMWScatterLoop arrayFacts defEnv typeEnv procStmts spec body

------------------------------------------------------------------------
-- Statement and procedure traversal
------------------------------------------------------------------------

parallelizeProcWithFacts
  :: ArrayFacts -> Map CVar CType -> Map CVar Integer -> [Stmt] -> [Stmt]
parallelizeProcWithFacts arrayFacts typeEnv allocSizes stmts = go False stmts
  where
    defEnv = collectDefEnv stmts

    -- insideParallel: True only when we are inside a loop that will receive
    -- #pragma omp parallel for.  Being inside a *serial* loop (e.g. the foldl
    -- outer loop) does not block inner LoopMap loops from being parallelized.
    go insideParallel = concatMap (goStmt insideParallel)

    goStmt insideParallel stmt = case stmt of
      SLoop spec body ->
        -- Tentatively check whether this loop will be parallelized (using the
        -- original body) so we can set the correct context for the inner loops.
        let outerWillParallel = case parallelizeLoop arrayFacts defEnv typeEnv allocSizes stmts
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
        in case parallelizeLoop arrayFacts defEnv typeEnv allocSizes stmts insideParallel spec body' of
             Just (spec', _) -> [SLoop spec' body']
             Nothing         -> [SLoop spec  body']
      SParallelRegion body ->
        [SParallelRegion (go True body)]
      _ -> [stmt]

-- | Rewrite eligible serial loops in one procedure using the array facts
-- and type environment attached to that procedure by the lowering pass.
parallelizeProc :: Proc -> Proc
parallelizeProc proc =
  let typeEnv    = recoverProcTypeEnv M.empty M.empty proc
      allocSizes = collectArrayAllocSizes (procBody proc)
  in  proc { procBody = parallelizeProcWithFacts
                          (procArrayFacts proc) typeEnv allocSizes (procBody proc) }

-- | Parallelize every procedure in a program.
parallelizeProgram :: Program -> Program
parallelizeProgram (Program procs) = Program (map parallelizeProc procs)

-- | Rewrite eligible serial loops in a statement list using the
-- conservative dependence-based legality check (no array facts available).
parallelizeStmts :: [Stmt] -> [Stmt]
parallelizeStmts = procBody . parallelizeProc . mkProc "parallelize_stmts2" []
