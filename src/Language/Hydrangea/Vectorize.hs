{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.Vectorize
--
-- Conservative SIMD-targeting pass for canonical CFG loops.
--
-- This pass separates three outcomes for each loop:
--
-- * leave the loop scalar,
-- * emit a SIMD hint via `ExecPolicy = Vector`, or
-- * lower all the way to explicit vector RHS nodes.
--
-- Explicit vector lowering is intentionally stricter than hint-only SIMD.
--
-- Requirements for explicit lowering:
--
-- * the loop must already be a legal SIMD candidate,
-- * every array load/store that would become `RVecLoad`/`RVecStore` must be
--   proven to operate on `CTArray CTDouble`,
-- * the body must use a contiguous lane-wise access pattern of the form
--   `base + lane` or a lowering-proved dense-linear alias of that position,
-- * the explicit body subset is deliberately small: direct loads/stores,
--   floating-point binary ops, copies, scalar splats, and reductions, and
-- * shape/index manipulations are allowed only when they remain scalar and do
--   not feed vector values.
--
-- Assumptions behind CFG-local type recovery:
--
-- * `procTypeEnv` is authoritative when present,
-- * additional local facts may be recovered from writes, loads, simple RHS
--   result types, and direct-call return types,
-- * array element types are /not/ inferred backwards from floating-point uses
--   alone, because scalar codegen may legally rely on integer-to-float
--   promotion while explicit vector loads may not, and
-- * the shared recovery logic lives in 'Language.Hydrangea.CFGTyping' so
--   CFG-level passes can reuse one conservative typing story.
module Language.Hydrangea.Vectorize
  ( vectorizeProc2
  , vectorizeProgram2
  , vectorizeProgram2WithWidth
  , vectorizeStmts2
  , defaultVectorWidth
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Language.Hydrangea.CFGCore (Atom(..), BinOp(..), CType(..))
import Language.Hydrangea.CFGCore qualified as C
import Language.Hydrangea.CFG
import Language.Hydrangea.CFGAnalysis (LoopBound2(..), analyzeLoop2, isVectorizableLoop2)
import Language.Hydrangea.CFGTyping (TypeEnv, inferProgramReturnTypes2, lookupArrayElemType2, recoverProcTypeEnv2)
import Language.Hydrangea.Dependence
  ( AccessType2(..)
  , ArrayAccess2(..)
  , DependenceDirection2(..)
  , depDirection2
  , depIsLoopCarried2
  , findDependences2
  )

data VectorContext = VectorContext
  { vcInsideLoop :: Bool
  }

data VectorPlan
  = VectorHintOnly
  | VectorExplicit [Stmt]

type VecEnv = Map CVar CVar
type AccessFacts = Map CVar VectorAccessFact

defaultVectorWidth :: Int
defaultVectorWidth = 4

extractAccesses2 :: [Stmt] -> [ArrayAccess2]
extractAccesses2 = goSeq 0
  where
    goSeq _ [] = []
    goSeq n (stmt:rest) = goStmt n stmt ++ goSeq (n + 1) rest

    goStmt n stmt = case stmt of
      SArrayWrite (AVar arr) idxAtom _ ->
        [ArrayAccess2 arr (atomToIndexExpr idxAtom) Write2 n]
      SAssign _ (C.RArrayLoad (AVar arr) idxAtom) ->
        [ArrayAccess2 arr (atomToIndexExpr idxAtom) Read2 n]
      SIf _ thn els ->
        goSeq n thn ++ goSeq (n + length thn + 1) els
      SLoop {} ->
        []
      _ ->
        []

    atomToIndexExpr (AVar v) = IVar v
    atomToIndexExpr (AInt n) = IConst n
    atomToIndexExpr _ = ICall "opaque" []

containsNestedLoop :: [Stmt] -> Bool
containsNestedLoop = any go
  where
    go stmt = case stmt of
      SLoop {} -> True
      SIf _ thn els -> containsNestedLoop thn || containsNestedLoop els
      _ -> False

hasSupportedBounds :: LoopSpec -> Bool
hasSupportedBounds spec = all (supportedBound . classifyBound) (lsBounds spec)
  where
    classifyBound (IConst n) = LB2Constant n
    classifyBound (IVar v) = LB2Var v
    classifyBound ie = LB2IndexExpr ie

    supportedBound LB2Constant {} = True
    supportedBound LB2Var {} = True
    supportedBound (LB2IndexExpr ie) = isSimpleIndexExpr ie

    isSimpleIndexExpr expr = case simplifyIndexExpr expr of
      IVar {} -> True
      IConst {} -> True
      IAdd a b -> isSimpleIndexExpr a && isSimpleIndexExpr b
      ISub a b -> isSimpleIndexExpr a && isSimpleIndexExpr b
      IMul a b -> isSimpleIndexExpr a && isSimpleIndexExpr b
      IDiv a b -> isSimpleIndexExpr a && isSimpleIndexExpr b
      _ -> False

isLegalVectorize :: [Stmt] -> Bool
isLegalVectorize body = not hasBlocking
  where
    accesses = extractAccesses2 body
    deps = findDependences2 accesses
    hasBlocking = any (\d -> depIsLoopCarried2 d && depDirection2 d /= DDForward) deps

chooseVectorWidthWith :: Int -> LoopSpec -> Int
chooseVectorWidthWith w spec = case analyzeLoop2 (SLoop spec []) of
  Just info -> if isVectorizableLoop2 info then w else 1
  Nothing   -> 1

vectorCandidate :: Int -> VectorContext -> LoopSpec -> [Stmt] -> Maybe LoopSpec
vectorCandidate w ctx spec body
  | lsExec spec /= Serial = Nothing
  | length (lsIters spec) /= 1 = Nothing
  | containsNestedLoop body = Nothing
  | lsRole spec == LoopReductionWrapper = Nothing
  | not (hasSupportedBounds spec) = Nothing
  | not (isLegalVectorize body) = Nothing
  | width <= 1 = Nothing
  | not preferSimdHere = Nothing
  | otherwise =
      let tailPolicy = case lsBounds spec of
            [IConst n]
              | n `mod` fromIntegral width == 0 -> TailNone
            _ -> TailRemainder
      in Just spec { lsExec = Vector (VectorSpec width tailPolicy) }
  where
    width = chooseVectorWidthWith w spec
    preferSimdHere =
      case lsRole spec of
        LoopReduction -> True
        LoopPlain -> vcInsideLoop ctx || lsRed spec /= Nothing
        LoopFold -> False
        LoopMap -> True
        LoopMapReduction -> False
        LoopReductionWrapper -> False

hasExplicitVectorOps :: [Stmt] -> Bool
hasExplicitVectorOps = any go
  where
    go stmt = case stmt of
      SAssign _ rhs -> case rhs of
        C.RVecLoad {} -> True
        C.RVecStore {} -> True
        C.RVecBinOp {} -> True
        C.RVecUnOp {} -> True
        C.RVecSplat {} -> True
        C.RVecReduce {} -> True
        _ -> False
      SLoop _ body -> hasExplicitVectorOps body
      SIf _ thn els -> hasExplicitVectorOps thn || hasExplicitVectorOps els
      _ -> False

boundAtom :: IndexExpr -> Maybe Atom
boundAtom (IConst n) = Just (AInt n)
boundAtom (IVar v) = Just (AVar v)
boundAtom _ = Nothing

initAtom :: IndexExpr -> Maybe Atom
initAtom = boundAtom

vecVarName :: CVar -> CVar
vecVarName v = v <> "__vec"

vecTarget :: VecEnv -> CVar -> CVar
vecTarget env v = M.findWithDefault (vecVarName v) v env

isDoubleCompatibleAtom :: TypeEnv -> Atom -> Bool
isDoubleCompatibleAtom typeEnv atom = case atom of
  AFloat {} -> True
  AInt {} -> True
  AVar v -> M.lookup v typeEnv == Just CTDouble
  _ -> False

bodyUsesOnlyDoubleArrays :: TypeEnv -> [Stmt] -> Bool
bodyUsesOnlyDoubleArrays typeEnv = all (go typeEnv)
  where
    go env stmt = case stmt of
      SAssign _ (C.RArrayLoad arr _) -> lookupArrayElemType2 env arr == Just CTDouble
      SArrayWrite arr _ _ -> lookupArrayElemType2 env arr == Just CTDouble
      SIf _ thn els -> bodyUsesOnlyDoubleArrays env thn && bodyUsesOnlyDoubleArrays env els
      SLoop {} -> True
      _ -> True

substScalarAtom :: CVar -> CVar -> Atom -> Atom
substScalarAtom iter base atom = case atom of
  AVar v | v == iter -> AVar base
  _ -> atom

atomRefsVector :: VecEnv -> Atom -> Bool
atomRefsVector env atom = case atom of
  AVar v -> M.member v env
  AVecVar {} -> True
  _ -> False

substScalarRHS :: CVar -> CVar -> C.RHS -> C.RHS
substScalarRHS iter base rhs = case rhs of
  C.RAtom a -> C.RAtom (substScalarAtom iter base a)
  C.RBinOp op a b -> C.RBinOp op (substScalarAtom iter base a) (substScalarAtom iter base b)
  C.RUnOp op a -> C.RUnOp op (substScalarAtom iter base a)
  C.RTuple as -> C.RTuple (map (substScalarAtom iter base) as)
  C.RProj i a -> C.RProj i (substScalarAtom iter base a)
  C.RRecord fields -> C.RRecord [(field, substScalarAtom iter base atom) | (field, atom) <- fields]
  C.RRecordProj field a -> C.RRecordProj field (substScalarAtom iter base a)
  C.RPairMake ct1 ct2 a b -> C.RPairMake ct1 ct2 (substScalarAtom iter base a) (substScalarAtom iter base b)
  C.RPairFst ct a -> C.RPairFst ct (substScalarAtom iter base a)
  C.RPairSnd ct a -> C.RPairSnd ct (substScalarAtom iter base a)
  C.RArrayAlloc a -> C.RArrayAlloc (substScalarAtom iter base a)
  C.RArrayLoad a b -> C.RArrayLoad (substScalarAtom iter base a) (substScalarAtom iter base b)
  C.RArrayShape a -> C.RArrayShape (substScalarAtom iter base a)
  C.RShapeSize a -> C.RShapeSize (substScalarAtom iter base a)
  C.RShapeInit a -> C.RShapeInit (substScalarAtom iter base a)
  C.RShapeLast a -> C.RShapeLast (substScalarAtom iter base a)
  C.RFlatToNd a b -> C.RFlatToNd (substScalarAtom iter base a) (substScalarAtom iter base b)
  C.RNdToFlat a b -> C.RNdToFlat (substScalarAtom iter base a) (substScalarAtom iter base b)
  C.R2DToFlat a b -> C.R2DToFlat (substScalarAtom iter base a) (substScalarAtom iter base b)
  C.RCall f args -> C.RCall f (map (substScalarAtom iter base) args)
  C.RVecUnOp op a -> C.RVecUnOp op (substScalarAtom iter base a)
  other -> other

rhsRefsVector :: VecEnv -> C.RHS -> Bool
rhsRefsVector env rhs = case rhs of
  C.RAtom a -> atomRefsVector env a
  C.RBinOp _ a b -> atomRefsVector env a || atomRefsVector env b
  C.RUnOp _ a -> atomRefsVector env a
  C.RTuple as -> any (atomRefsVector env) as
  C.RProj _ a -> atomRefsVector env a
  C.RRecord fields -> any (atomRefsVector env . snd) fields
  C.RRecordProj _ a -> atomRefsVector env a
  C.RPairMake _ _ a b -> atomRefsVector env a || atomRefsVector env b
  C.RPairFst _ a -> atomRefsVector env a
  C.RPairSnd _ a -> atomRefsVector env a
  C.RArrayAlloc a -> atomRefsVector env a
  C.RArrayLoad a b -> atomRefsVector env a || atomRefsVector env b
  C.RArrayShape a -> atomRefsVector env a
  C.RShapeSize a -> atomRefsVector env a
  C.RShapeInit a -> atomRefsVector env a
  C.RShapeLast a -> atomRefsVector env a
  C.RFlatToNd a b -> atomRefsVector env a || atomRefsVector env b
  C.RNdToFlat a b -> atomRefsVector env a || atomRefsVector env b
  C.R2DToFlat a b -> atomRefsVector env a || atomRefsVector env b
  C.RCall _ args -> any (atomRefsVector env) args
  C.RVecLoad a b -> atomRefsVector env a || atomRefsVector env b
  C.RVecStore a b c -> any (atomRefsVector env) [a, b, c]
  C.RVecBinOp _ a b -> atomRefsVector env a || atomRefsVector env b
  C.RVecUnOp _ a -> atomRefsVector env a
  C.RVecSplat a -> atomRefsVector env a
  C.RVecReduce _ a -> atomRefsVector env a

supportedVecBinOp :: BinOp -> Bool
supportedVecBinOp op = op `elem` [CAddF, CSubF, CMulF, CDivF]

supportedVecUnOp :: C.UnOp -> Bool
supportedVecUnOp op = op `elem` [C.CSqrt, C.CExpF, C.CLog, C.CErf]

toVectorOperand :: TypeEnv -> VecEnv -> CVar -> CVar -> CVar -> Atom -> Maybe ([Stmt], Atom)
toVectorOperand typeEnv env iter base owner atom =
  case substScalarAtom iter base atom of
    AVar v | Just vv <- M.lookup v env ->
      Just ([], AVecVar vv)
    scalar
      | isDoubleCompatibleAtom typeEnv scalar ->
          let splatVar = owner <> "__splat"
          in Just ([SAssign splatVar (C.RVecSplat scalar)], AVecVar splatVar)
      | otherwise ->
          Nothing

contiguousVectorIndex :: AccessFacts -> CVar -> CVar -> Atom -> Maybe Atom
contiguousVectorIndex accessFacts iter base idx =
  case substScalarAtom iter base idx of
    AVar v | v == base -> Just (AVar v)
    AVar v
      | Just fact <- M.lookup v accessFacts
      , vxfDenseLinearIndexOf fact == Just iter -> Just (AVar base)
    _ -> Nothing

arrayAllowsExplicitLoad :: TypeEnv -> AccessFacts -> Atom -> Bool
arrayAllowsExplicitLoad typeEnv accessFacts arr =
  lookupArrayElemType2 typeEnv arr == Just CTDouble
    && maybe True (not . vxfIndirectRead) (atomAccessFact accessFacts arr)

arrayAllowsExplicitWrite :: TypeEnv -> AccessFacts -> Atom -> Bool
arrayAllowsExplicitWrite typeEnv accessFacts arr =
  lookupArrayElemType2 typeEnv arr == Just CTDouble
    && maybe True vxfContiguousWrite (atomAccessFact accessFacts arr)

atomAccessFact :: AccessFacts -> Atom -> Maybe VectorAccessFact
atomAccessFact accessFacts (AVar v) = M.lookup v accessFacts
atomAccessFact _ _ = Nothing

transformVectorStmt :: TypeEnv -> AccessFacts -> CVar -> CVar -> VecEnv -> Stmt -> Maybe (VecEnv, [Stmt])
transformVectorStmt typeEnv accessFacts iter base env stmt = case stmt of
  SAssign v (C.RArrayLoad arr@(AVar _) idx)
    | arrayAllowsExplicitLoad typeEnv accessFacts arr -> do
        idx' <- contiguousVectorIndex accessFacts iter base idx
        let vv = vecTarget env v
        pure (M.insert v vv env, [SAssign vv (C.RVecLoad arr idx')])

  SAssign v (C.RBinOp op a b)
    | supportedVecBinOp op ->
        let a' = substScalarAtom iter base a
            b' = substScalarAtom iter base b
            wantsVector = atomRefsVector env a' || atomRefsVector env b'
        in if wantsVector
             then do
               let vv = vecTarget env v
               (preA, va) <- toVectorOperand typeEnv env iter base (vv <> "__lhs") a
               (preB, vb) <- toVectorOperand typeEnv env iter base (vv <> "__rhs") b
               pure (M.insert v vv env, preA ++ preB ++ [SAssign vv (C.RVecBinOp op va vb)])
             else pure (env, [SAssign v (C.RBinOp op a' b')])

  SAssign v (C.RUnOp op a)
    | supportedVecUnOp op ->
        let a' = substScalarAtom iter base a
            wantsVector = atomRefsVector env a'
        in if wantsVector
             then do
               let vv = vecTarget env v
               (preA, va) <- toVectorOperand typeEnv env iter base (vv <> "__arg") a
               pure (M.insert v vv env, preA ++ [SAssign vv (C.RVecUnOp op va)])
             else pure (env, [SAssign v (C.RUnOp op a')])

  SAssign v (C.RAtom a) ->
    let a' = substScalarAtom iter base a
    in case a' of
         AVar src | Just vvSrc <- M.lookup src env ->
           let vv = vecTarget env v
           in pure (M.insert v vv env, [SAssign vv (C.RAtom (AVecVar vvSrc))])
         _ ->
           if atomRefsVector env a'
             then Nothing
             else pure (env, [SAssign v (C.RAtom a')])

  SAssign v rhs ->
    let rhs' = substScalarRHS iter base rhs
    in if rhsRefsVector env rhs'
         then Nothing
         else pure (env, [SAssign v rhs'])

  SArrayWrite arr@(AVar _) idx val
    | arrayAllowsExplicitWrite typeEnv accessFacts arr ->
        let val' = substScalarAtom iter base val
        in case val' of
              AVar src | Just vv <- M.lookup src env
                      , Just idx' <- contiguousVectorIndex accessFacts iter base idx ->
                pure (env, [SAssign "__vec_store_discard" (C.RVecStore arr idx' (AVecVar vv))])
              _ ->
                let idx' = substScalarAtom iter base idx
               in if atomRefsVector env val'
                  then Nothing
                  else pure (env, [SArrayWrite arr idx' val'])

  SArrayWrite arr _ _ ->
    if atomRefsVector env arr then Nothing else pure (env, [stmt])

  SIf {} -> Nothing
  SLoop {} -> Nothing
  SBreak -> Nothing

  SReturn a ->
    let a' = substScalarAtom iter base a
    in if atomRefsVector env a'
         then Nothing
         else pure (env, [SReturn a'])

transformVectorBody :: TypeEnv -> AccessFacts -> CVar -> CVar -> VecEnv -> [Stmt] -> Maybe [Stmt]
transformVectorBody typeEnv accessFacts iter base env0 = go env0
  where
    go _ [] = Just []
    go env (stmt:rest) = do
      (env', stmt') <- transformVectorStmt typeEnv accessFacts iter base env stmt
      rest' <- go env' rest
      pure (stmt' ++ rest')

lowerExplicitVectorLoop :: TypeEnv -> AccessFacts -> LoopSpec -> [Stmt] -> Maybe [Stmt]
lowerExplicitVectorLoop typeEnv accessFacts spec body = do
  Vector vecSpec <- pure (lsExec spec)
  [iter] <- pure (lsIters spec)
  [boundExpr] <- pure (lsBounds spec)
  boundVal <- boundAtom boundExpr
  let widthI = fromIntegral (vsWidth vecSpec)
      vecTrips = iter <> "__vec_trips"
      vecIter = iter <> "__vec_i"
      vecBase = iter <> "__vec_base"
      tailStart = iter <> "__tail_start"
      tailLen = iter <> "__tail_len"
      tailIter = iter <> "__tail_i"
      vecTripsStmt = SAssign vecTrips (C.RBinOp C.CDiv boundVal (AInt widthI))
      vecBaseStmt = SAssign vecBase (C.RBinOp C.CMul (AVar vecIter) (AInt widthI))
      vecLoopSpec = spec { lsIters = [vecIter], lsBounds = [IVar vecTrips], lsExec = Serial, lsRed = Nothing }
      tailPrep =
        [ SAssign tailStart (C.RBinOp C.CMul (AVar vecTrips) (AInt widthI))
        , SAssign tailLen (C.RBinOp C.CSub boundVal (AVar tailStart))
        ]
      tailBody = SAssign iter (C.RBinOp C.CAdd (AVar tailStart) (AVar tailIter)) : body
      tailLoopSpec = spec { lsIters = [tailIter], lsBounds = [IVar tailLen], lsExec = Serial }
  case lsRed spec of
    Just red -> do
      let acc = rsAccVar red
      accTy <- M.lookup acc typeEnv
      if accTy /= CTDouble then Nothing else do
        initVal <- initAtom (rsInit red)
        if not (isDoubleCompatibleAtom typeEnv initVal) then Nothing else do
          redOp <- case rsRedop red of
            C.RAdd -> Just CAddF
            C.RMul -> Just CMulF
          let accVec = vecVarName acc
              vecInit = SAssign accVec (C.RVecSplat initVal)
              vecEnv0 = M.singleton acc accVec
          vecBody <- transformVectorBody typeEnv accessFacts iter vecBase vecEnv0 body
          if not (hasExplicitVectorOps vecBody)
            then Nothing
            else do
              let vecLoop = SLoop vecLoopSpec (vecBaseStmt : vecBody)
                  reduceBack = SAssign acc (C.RVecReduce redOp (AVecVar accVec))
                  tailStmts = case vsTail vecSpec of
                    TailNone -> []
                    _ -> tailPrep ++ [SLoop tailLoopSpec tailBody]
              pure (vecTripsStmt : vecInit : vecLoop : reduceBack : tailStmts)
    Nothing -> do
      vecBody <- transformVectorBody typeEnv accessFacts iter vecBase M.empty body
      if not (hasExplicitVectorOps vecBody)
        then Nothing
        else do
          let vecLoop = SLoop vecLoopSpec (vecBaseStmt : vecBody)
              tailStmts = case vsTail vecSpec of
                TailNone -> []
                _ -> tailPrep ++ [SLoop tailLoopSpec tailBody]
          pure (vecTripsStmt : vecLoop : tailStmts)

planVectorLoop :: TypeEnv -> AccessFacts -> LoopSpec -> [Stmt] -> VectorPlan
planVectorLoop typeEnv accessFacts spec body
  | not typedArrays =
      VectorHintOnly
  | otherwise =
      case lowerExplicitVectorLoop typeEnv accessFacts spec body of
        Just lowered -> VectorExplicit lowered
        Nothing -> VectorHintOnly
  where
    typedArrays = bodyUsesOnlyDoubleArrays typeEnv body

vectorizeProcWithCalls :: Int -> Map CVar CType -> Proc -> Proc
vectorizeProcWithCalls w callTypes proc =
  proc { procBody = rewriteStmts2With defaultContext enterLoop rewriteStmt (procBody proc) }
  where
    defaultContext = VectorContext False
    enterLoop _ = VectorContext True
    typeEnv = recoverProcTypeEnv2 callTypes M.empty proc
    accessFacts = procVectorAccessFacts proc

    rewriteStmt :: VectorContext -> Stmt -> [Stmt]
    rewriteStmt ctx stmt = case stmt of
      SLoop spec body ->
        case vectorCandidate w ctx spec body of
          Just vectorSpec ->
            case planVectorLoop typeEnv accessFacts vectorSpec body of
              VectorExplicit lowered -> lowered
              VectorHintOnly -> [SLoop vectorSpec body]
          Nothing ->
            [SLoop spec body]
      stmt' ->
        [stmt']

-- | Vectorize a single procedure using only its own type environment.
--
-- This is useful for unit tests and for callers that do not have whole-program
-- call information available.
vectorizeProc2 :: Proc -> Proc
vectorizeProc2 = vectorizeProcWithCalls defaultVectorWidth M.empty

-- | Vectorize a whole program using the given SIMD lane width.
vectorizeProgram2WithWidth :: Int -> Program -> Program
vectorizeProgram2WithWidth w (Program procs) =
  let callTypes = inferProgramReturnTypes2 (Program procs)
  in Program (map (vectorizeProcWithCalls w callTypes) procs)

-- | Vectorize a whole program.
--
-- This is the preferred entrypoint for the real compiler pipeline: it first
-- computes return types for direct calls between procedures, then uses those
-- facts to enrich per-procedure legality checks for explicit vector lowering.
vectorizeProgram2 :: Program -> Program
vectorizeProgram2 = vectorizeProgram2WithWidth defaultVectorWidth

-- | Backwards-compatible statement-list helper used in tests.
--
-- Without procedure-level or program-level type information, this helper may be
-- more conservative than 'vectorizeProgram2'.
vectorizeStmts2 :: [Stmt] -> [Stmt]
vectorizeStmts2 = procBody . vectorizeProc2 . mkProc "vectorize_stmts2" []
