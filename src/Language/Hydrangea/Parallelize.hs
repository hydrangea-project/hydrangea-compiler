{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.Parallelize
--
-- Parallelization transformation for canonical CFG loops.
--
-- The pass uses two legality checks:
--
-- * a conservative fallback based on CFG array accesses and
--   'Language.Hydrangea.Dependence.findDependences2', and
-- * a lowering-guided fast path for top-level 'LoopMap' kernels whose
--   procedure-level 'procArrayFacts' prove that written arrays are fresh,
--   write-once outputs and that read arrays are read-only inputs.
--
-- Lowering can record semantic facts that are harder to reconstruct from the
-- final imperative loop body, so the procedure-level entry points take advantage
-- of that information when it is available. The statement-list helper,
-- 'parallelizeStmts2', remains intentionally conservative because it runs
-- without procedure-level facts.
module Language.Hydrangea.Parallelize
  ( parallelizeProc2
  , parallelizeProgram2
  , parallelizeStmts2
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Language.Hydrangea.CFGCore (Atom(..), BinOp(..), CType(..), RHS(..))
import Language.Hydrangea.CFG
import Language.Hydrangea.CFGTyping (recoverProcTypeEnv2)
import Language.Hydrangea.CFGAnalysis
  ( isParallelTripCount2
  , loopTripCount2
  , tripCountValue2
  )
import Language.Hydrangea.Dependence (findDependences2, ArrayAccess2(..), AccessType2(..), depDirection2, depIsLoopCarried2, DependenceDirection2(..))

type ArrayFacts = Map CVar ArrayFact

data ScatterKernelInfo = ScatterKernelInfo
  { skiDestArray :: CVar
  }

data ScatterAtomicAddInfo = ScatterAtomicAddInfo
  { saDestArray :: CVar
  , saValueType :: CType
  }

scatterAtomicAddBody :: [Stmt] -> (Maybe Atom, [Stmt])
scatterAtomicAddBody body =
  case reverse body of
    (SIf cond thn [] : revPrefix) -> (Just cond, reverse revPrefix ++ thn)
    _ -> (Nothing, body)

collectArrayAllocSizes :: [Stmt] -> Map CVar Integer
collectArrayAllocSizes = snd . go Map.empty Map.empty
  where
    go tupleSizes allocSizes [] = (tupleSizes, allocSizes)
    go tupleSizes allocSizes (stmt:rest) =
      case stmt of
        SAssign v (RTuple atoms) ->
          let tupleSizes' =
                case constantTupleSize atoms of
                  Just sz -> Map.insert v sz tupleSizes
                  Nothing -> tupleSizes
          in go tupleSizes' allocSizes rest
        SAssign v (RArrayAlloc (AVar shpVar)) ->
          let allocSizes' =
                case Map.lookup shpVar tupleSizes of
                  Just sz -> Map.insert v sz allocSizes
                  Nothing -> allocSizes
          in go tupleSizes allocSizes' rest
        SLoop _ body ->
          let (_, bodyAllocs) = go tupleSizes Map.empty body
          in go tupleSizes (Map.union allocSizes bodyAllocs) rest
        SIf _ thn els ->
          let (_, thnAllocs) = go tupleSizes Map.empty thn
              (_, elsAllocs) = go tupleSizes Map.empty els
          in go tupleSizes (Map.unions [allocSizes, thnAllocs, elsAllocs]) rest
        _ ->
          go tupleSizes allocSizes rest

    constantTupleSize atoms = product <$> traverse atomInt atoms
    atomInt (AInt n) = Just n
    atomInt _ = Nothing

-- | Collect the array accesses visible in a loop body and assign them a stable
-- statement order.
--
-- The fallback dependence check uses the statement index to distinguish the
-- relative order of loads and stores within the same loop body.
extractAccesses2 :: [Stmt] -> [ArrayAccess2]
extractAccesses2 stmts = fst (goBlock 0 stmts)
  where
    goBlock next [] = ([], next)
    goBlock next (stmt:rest) =
      let (stmtAccesses, next') = goStmt next stmt
          (restAccesses, next'') = goBlock next' rest
      in (stmtAccesses ++ restAccesses, next'')

    goStmt next stmt = case stmt of
      SArrayWrite (AVar arr) idxAtom _ ->
        let idx = atomToIndexExpr idxAtom
        in  ([ArrayAccess2 arr idx Write2 next], next + 1)
      SAssign _ (RArrayLoad (AVar arr) idxAtom) ->
        let idx = atomToIndexExpr idxAtom
        in  ([ArrayAccess2 arr idx Read2 next], next + 1)
      SLoop _ body -> goBlock next body
      SIf _ thn els ->
        let (thnAccesses, next') = goBlock next thn
            (elsAccesses, next'') = goBlock next' els
        in (thnAccesses ++ elsAccesses, next'')
      _ -> ([], next)

    atomToIndexExpr (AVar v) = IVar v
    atomToIndexExpr (AInt n) = IConst n
    atomToIndexExpr _ = ICall "opaque" []

passesConservativeDependenceCheck :: LoopSpec -> [Stmt] -> Bool
passesConservativeDependenceCheck spec body =
  isParallelTripCount2 (loopTripCount2 spec)
    && not hasBlockingDependence
    && not hasReadWriteHazard
  where
    accesses = extractAccesses2 body
    deps = findDependences2 accesses
    usage = collectLoopArrayUsage body
    hasBlockingDependence =
      any (\d -> depIsLoopCarried2 d && depDirection2 d /= DDForward) deps
    hasReadWriteHazard =
      any (\usageInfo -> lauReads usageInfo && lauWrites usageInfo) (Map.elems usage)

data LoopArrayUsage = LoopArrayUsage
  { lauReads :: Bool
  , lauWrites :: Bool
  }

emptyLoopArrayUsage :: LoopArrayUsage
emptyLoopArrayUsage = LoopArrayUsage False False

accumulateArrayUsage :: LoopArrayUsage -> AccessType2 -> LoopArrayUsage
accumulateArrayUsage usage accessType = case accessType of
  Read2 -> usage { lauReads = True }
  Write2 -> usage { lauWrites = True }
  ReadWrite2 -> usage { lauReads = True, lauWrites = True }

collectLoopArrayUsage :: [Stmt] -> Map CVar LoopArrayUsage
collectLoopArrayUsage =
  foldl' go Map.empty . extractAccesses2
  where
    go usageMap access =
      Map.insertWith
        combineUsage
        (aa2ArrayVar access)
        (accumulateArrayUsage emptyLoopArrayUsage (aa2AccessType access))
        usageMap

    combineUsage newer older = LoopArrayUsage
      { lauReads = lauReads newer || lauReads older
      , lauWrites = lauWrites newer || lauWrites older
      }

-- | Check whether procedure-level array facts are sufficient to prove that a
-- top-level @LoopMap@ is embarrassingly parallel.
--
-- This path deliberately refuses any loop that both reads and writes the same
-- array variable. The current facts are about freshness/read-only/write-once,
-- not about in-place updates or aliasing.
factsPermitParallelLoop :: ArrayFacts -> [Stmt] -> Bool
factsPermitParallelLoop arrayFacts body =
  not (Map.null usage) && all arrayIsSafe (Map.toList usage)
  where
    usage = collectLoopArrayUsage body

    arrayIsSafe (arr, usageInfo) =
      case Map.lookup arr arrayFacts of
        Nothing -> False
        Just fact
          | lauWrites usageInfo -> afFreshAlloc fact && afWriteOnce fact && not (lauReads usageInfo)
          | lauReads usageInfo -> afReadOnly fact
          | otherwise -> True

resolveAtomIndex :: Map CVar IndexExpr -> Atom -> Maybe IndexExpr
resolveAtomIndex env atom = case atom of
  AVar v -> Just (Map.findWithDefault (IVar v) v env)
  AInt n -> Just (IConst n)
  _ -> Nothing

resolveRHSIndex :: Map CVar IndexExpr -> RHS -> Maybe IndexExpr
resolveRHSIndex env rhs = case rhs of
  RAtom atom -> resolveAtomIndex env atom
  RProj i atom -> IProj (fromIntegral i) <$> resolveAtomIndex env atom
  RFlatToNd a b -> IFlatToNd <$> resolveAtomIndex env a <*> resolveAtomIndex env b
  RNdToFlat a b -> INdToFlat <$> resolveAtomIndex env a <*> resolveAtomIndex env b
  RBinOp op a b ->
    case op of
      CAdd -> IAdd <$> resolveAtomIndex env a <*> resolveAtomIndex env b
      CSub -> ISub <$> resolveAtomIndex env a <*> resolveAtomIndex env b
      CMul -> IMul <$> resolveAtomIndex env a <*> resolveAtomIndex env b
      CDiv -> IDiv <$> resolveAtomIndex env a <*> resolveAtomIndex env b
      _ -> Nothing
  _ -> Nothing

buildIndexEnv :: [Stmt] -> Map CVar IndexExpr
buildIndexEnv = foldl' go Map.empty
  where
    go env stmt = case stmt of
      SAssign v rhs ->
        case resolveRHSIndex env rhs of
          Just expr -> Map.insert v expr env
          Nothing -> env
      _ -> env

isInjectiveScatterIndex :: LoopSpec -> IndexExpr -> Bool
isInjectiveScatterIndex spec idxExpr =
  case (lsIters spec, simplifyIndexExpr idxExpr) of
    ([i], IVar j) -> i == j
    ([i], IAdd (IVar j) (IConst _)) -> i == j
    ([i], IProj 0 (IFlatToNd (IVar j) _)) -> i == j
    ([i], IAdd (IProj 0 (IFlatToNd (IVar j) _)) (IConst _)) -> i == j
    _ -> False

detectInjectiveScatterKernel :: LoopSpec -> [Stmt] -> Maybe ScatterKernelInfo
detectInjectiveScatterKernel spec body = do
  let env = buildIndexEnv body
      writes =
        [ (arr, idxExpr)
        | SArrayWrite (AVar arr) idxAtom _ <- body
        , Just idxExpr <- [resolveAtomIndex env idxAtom]
        ]
  (destArr, writeIdx) <-
    case writes of
      [oneWrite] -> Just oneWrite
      _ -> Nothing
  let hasMatchingRead =
        any
          (\stmt -> case stmt of
              SAssign _ (RArrayLoad (AVar arr) idxAtom) ->
                arr == destArr && fmap simplifyIndexExpr (resolveAtomIndex env idxAtom) == Just (simplifyIndexExpr writeIdx)
              _ -> False
          )
          body
  if hasMatchingRead && isInjectiveScatterIndex spec writeIdx
    then Just (ScatterKernelInfo destArr)
    else Nothing

factsPermitDirectScatterLoop :: ArrayFacts -> LoopSpec -> [Stmt] -> Bool
factsPermitDirectScatterLoop arrayFacts spec body =
  isParallelTripCount2 (loopTripCount2 spec)
    && case detectInjectiveScatterKernel spec body of
         Nothing -> False
         Just info ->
           case Map.lookup (skiDestArray info) usage of
             Nothing -> False
             Just destUsage ->
               destIsSafe destUsage
                 && all otherArrayIsSafe (Map.toList usage)
  where
    usage = collectLoopArrayUsage body

    destIsSafe usageInfo =
      case Map.lookup destArr arrayFacts of
        Nothing -> False
        Just fact ->
          lauReads usageInfo && lauWrites usageInfo && afFreshAlloc fact && afWriteOnce fact

    destArr =
      case detectInjectiveScatterKernel spec body of
        Just info -> skiDestArray info
        Nothing -> "__no_scatter_dest"

    otherArrayIsSafe (arr, usageInfo)
      | arr == destArr = True
      | lauWrites usageInfo = False
      | lauReads usageInfo = True
      | otherwise = True

detectScatterAtomicAddKernel :: LoopSpec -> [Stmt] -> Maybe ScatterAtomicAddInfo
detectScatterAtomicAddKernel spec body = do
  let (_, atomicBody) = scatterAtomicAddBody body
      env = buildIndexEnv body
  (destArr, writeIdx, newVar) <-
    case [ (arr, idxExpr, newVar)
         | SArrayWrite (AVar arr) idxAtom (AVar newVar) <- atomicBody
         , Just idxExpr <- [resolveAtomIndex env idxAtom]
         ] of
      [oneWrite] -> Just oneWrite
      _ -> Nothing
  oldVar <-
    case [ oldV
         | SAssign oldV (RArrayLoad (AVar arr) idxAtom) <- atomicBody
         , arr == destArr
         , fmap simplifyIndexExpr (resolveAtomIndex env idxAtom) == Just (simplifyIndexExpr writeIdx)
         ] of
      [oneOld] -> Just oneOld
      _ -> Nothing
  valueTy <-
    case [ ty
         | SAssign newV (RBinOp op a b) <- atomicBody
         , newV == newVar
         , Just _ <- [nonOldOperand oldVar a b]
         , Just ty <- [atomicAddValueType op]
         ] of
      [oneTy] -> Just oneTy
      _ -> Nothing
  if isParallelTripCount2 (loopTripCount2 spec)
    then Just (ScatterAtomicAddInfo destArr valueTy)
    else Nothing
  where
    nonOldOperand oldVar a b = case (a, b) of
      (AVar oldV, other) | oldV == oldVar -> Just other
      (other, AVar oldV) | oldV == oldVar -> Just other
      _ -> Nothing
    atomicAddValueType op = case op of
      CAdd -> Just CTInt64
      CAddF -> Just CTDouble
      _ -> Nothing

factsPermitAtomicScatterLoop :: ArrayFacts -> Map CVar CType -> LoopSpec -> [Stmt] -> Bool
factsPermitAtomicScatterLoop arrayFacts typeEnv spec body =
  case detectScatterAtomicAddKernel spec body of
    Nothing -> False
    Just info ->
      case Map.lookup (saDestArray info) usage of
        Nothing -> False
        Just destUsage ->
          destIsSafe destUsage
            && all otherArrayIsSafe (Map.toList usage)
  where
    usage = collectLoopArrayUsage body

    destIsSafe usageInfo =
      case (Map.lookup destArr arrayFacts, Map.lookup destArr typeEnv) of
        (Just fact, Just (CTArray elemTy)) | elemTy == destElemTy ->
          lauReads usageInfo && lauWrites usageInfo && afFreshAlloc fact && afWriteOnce fact
        _ -> False

    destArr =
      case detectScatterAtomicAddKernel spec body of
        Just info -> saDestArray info
        Nothing -> "__no_atomic_scatter_dest"

    destElemTy =
      case detectScatterAtomicAddKernel spec body of
        Just info -> saValueType info
        Nothing -> CTInt64

    otherArrayIsSafe (arr, usageInfo)
      | arr == destArr = True
      | lauWrites usageInfo = False
      | lauReads usageInfo = True
      | otherwise = True

factsPermitPrivatizedIntScatterLoop :: ArrayFacts -> Map CVar CType -> Map CVar Integer -> LoopSpec -> [Stmt] -> Bool
factsPermitPrivatizedIntScatterLoop arrayFacts typeEnv allocSizes spec body =
  case detectScatterAtomicAddKernel spec body of
    Nothing -> False
    Just info ->
      case ( Map.lookup (saDestArray info) usage
           , Map.lookup (saDestArray info) arrayFacts
           , Map.lookup (saDestArray info) typeEnv
           ) of
        (Just destUsage, Just fact, Just (CTArray CTInt64)) ->
          lauReads destUsage
            && lauWrites destUsage
            && afFreshAlloc fact
            && afWriteOnce fact
            && profitable
            && all otherArrayIsSafe (Map.toList usage)
        _ -> False
  where
    usage = collectLoopArrayUsage body
    destArr =
      case detectScatterAtomicAddKernel spec body of
        Just info -> saDestArray info
        Nothing -> "__no_privatized_scatter_dest"

    shouldPrivatize allocSize tripCount =
      allocSize > 0
        && allocSize <= 64
        && tripCount >= max 32 (allocSize * 4)

    profitable =
      case (Map.lookup destArr allocSizes, tripCountValue2 (loopTripCount2 spec)) of
        (Just allocSize, Just tripCount) -> shouldPrivatize allocSize tripCount
        _ -> not (hasExternalArrayReads destArr body)

    otherArrayIsSafe (arr, usageInfo)
      | arr == destArr = True
      | lauWrites usageInfo = False
      | lauReads usageInfo = True
      | otherwise = True

    hasExternalArrayReads dest bodyStmts = any stmtHasExternalRead bodyStmts
      where
        stmtHasExternalRead stmt = case stmt of
          SAssign _ (RArrayLoad (AVar arr) _) -> arr /= dest
          SLoop _ inner -> hasExternalArrayReads dest inner
          SIf _ thn els -> hasExternalArrayReads dest thn || hasExternalArrayReads dest els
          _ -> False

shouldParallelizeLoop :: Bool -> LoopSpec -> Bool
shouldParallelizeLoop insideLoop spec = case lsRole spec of
  LoopReductionWrapper -> False
  LoopReduction -> not insideLoop
  LoopPlain -> not insideLoop
  LoopMap -> not insideLoop
  LoopMapReduction -> not insideLoop

parallelizeLoop :: ArrayFacts -> Map CVar CType -> Map CVar Integer -> Bool -> LoopSpec -> [Stmt] -> Maybe (LoopSpec, [Stmt])
parallelizeLoop arrayFacts typeEnv allocSizes insideLoop spec body
  | lsExec spec /= Serial = Nothing
  | not (shouldParallelizeLoop insideLoop spec) = Nothing
  | not (canParallelizeWithFacts || canParallelizeDirectScatter || canParallelizePrivatizedScatter || canParallelizeAtomicScatter || passesConservativeDependenceCheck spec body) = Nothing
  | otherwise =
      let strategy
            | canParallelizeDirectScatter = ParallelScatterDirect
            | canParallelizePrivatizedScatter = ParallelScatterPrivatizedIntAdd
            | canParallelizeAtomicScatter =
                case detectScatterAtomicAddKernel spec body of
                  Just info | saValueType info == CTDouble -> ParallelScatterAtomicAddFloat
                  _ -> ParallelScatterAtomicAddInt
            | otherwise = ParallelGeneric
          newSpec = spec { lsExec = Parallel (ParallelSpec strategy Nothing) }
      in  Just (newSpec, body)
  where
    canParallelizeWithFacts =
      lsRole spec == LoopMap
        && isParallelTripCount2 (loopTripCount2 spec)
        && factsPermitParallelLoop arrayFacts body
    canParallelizeDirectScatter =
      factsPermitDirectScatterLoop arrayFacts spec body
    canParallelizePrivatizedScatter =
      factsPermitPrivatizedIntScatterLoop arrayFacts typeEnv allocSizes spec body
    canParallelizeAtomicScatter =
      factsPermitAtomicScatterLoop arrayFacts typeEnv spec body

parallelizeProcWithFacts :: ArrayFacts -> Map CVar CType -> Map CVar Integer -> [Stmt] -> [Stmt]
parallelizeProcWithFacts arrayFacts typeEnv allocSizes =
  rewriteStmts2With False (const True) rewriteStmt
  where
    rewriteStmt insideLoop stmt = case stmt of
      SLoop spec body ->
         case parallelizeLoop arrayFacts typeEnv allocSizes insideLoop spec body of
               Just (newSpec, newBody) -> [SLoop newSpec newBody]
               Nothing -> [SLoop spec body]
      stmt' -> [stmt']

-- | Rewrite eligible serial loops in one procedure using any lowering-provided
-- array facts attached to that procedure.
parallelizeProc2 :: Proc -> Proc
parallelizeProc2 proc =
  let recoveredTypeEnv = recoverProcTypeEnv2 Map.empty proc
      allocSizes = collectArrayAllocSizes (procBody proc)
  in proc { procBody = parallelizeProcWithFacts (procArrayFacts proc) recoveredTypeEnv allocSizes (procBody proc) }

-- | Parallelize an entire CFG program procedure-by-procedure.
parallelizeProgram2 :: Program -> Program
parallelizeProgram2 (Program procs) = Program (map parallelizeProc2 procs)

-- | Rewrite eligible serial loops to use CFG parallel execution hints.
parallelizeStmts2 :: [Stmt] -> [Stmt]
parallelizeStmts2 = procBody . parallelizeProc2 . mkProc "parallelize_stmts2" []
