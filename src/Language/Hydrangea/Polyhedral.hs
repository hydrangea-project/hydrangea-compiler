{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.Polyhedral
--
-- CFG-adjacent polyhedral scaffolding. This first slice extracts affine
-- static-control regions (SCoPs) from serial CFG loop nests and preserves
-- their schedule structure separately from the executable CFG.
module Language.Hydrangea.Polyhedral
  ( StmtId
  , AffineExpr(..)
  , AffineConstraintOp(..)
  , AffineConstraint(..)
  , PolyhedralAccessType(..)
  , PolyhedralAccess(..)
  , PolyhedralStmt(..)
  , PolyhedralDependenceKind(..)
  , PolyhedralDependenceDirection(..)
  , PolyhedralDependence(..)
  , LoopBand(..)
  , ScheduleTree(..)
  , Scop(..)
  , ScheduledScop(..)
  , ScopRejectReason(..)
  , ScopDiagnostic(..)
  , affineExprFromIndexExpr2
  , buildIdentitySchedule2
  , collectProcScopDiagnostics2
  , collectProgramScopDiagnostics2
  , collectScopDependences2
  , extractProcScops2
  , extractProgramScops2
  , polyhedralProgram2
  , reifyScheduledScop2
  ) where

import Control.Applicative ((<|>))
import Data.List (nub, sort, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Language.Hydrangea.CFG
import Language.Hydrangea.CFGCore (Atom(..), BinOp(..), RHS(..))

type StmtId = [Int]

data AffineExpr = AffineExpr
  { aeTerms :: Map CVar Integer
  , aeConstant :: Integer
  }
  deriving (Eq, Show)

data AffineConstraintOp = AffineEq | AffineLe | AffineGe
  deriving (Eq, Show)

data AffineConstraint = AffineConstraint
  { acOp :: AffineConstraintOp
  , acExpr :: AffineExpr
  }
  deriving (Eq, Show)

data PolyhedralAccessType = PolyRead | PolyWrite
  deriving (Eq, Show)

data PolyhedralAccess = PolyhedralAccess
  { paType :: PolyhedralAccessType
  , paArray :: CVar
  , paIndex :: [AffineExpr]
  }
  deriving (Eq, Show)

data PolyhedralStmt = PolyhedralStmt
  { psPath :: StmtId
  , psDomain :: [AffineConstraint]
  , psReads :: [PolyhedralAccess]
  , psWrites :: [PolyhedralAccess]
  , psStmt :: Stmt
  }
  deriving (Eq, Show)

data PolyhedralDependenceKind = PolyDepRAW | PolyDepWAR | PolyDepWAW
  deriving (Eq, Show)

data PolyhedralDependenceDirection = PolyDepForward | PolyDepBackward | PolyDepUnknown
  deriving (Eq, Show)

data PolyhedralDependence = PolyhedralDependence
  { pdKind :: PolyhedralDependenceKind
  , pdArray :: CVar
  , pdSourceStmt :: StmtId
  , pdTargetStmt :: StmtId
  , pdDirection :: PolyhedralDependenceDirection
  , pdIsLoopCarried :: Bool
  , pdDistance :: Maybe [Integer]
  }
  deriving (Eq, Show)

data LoopBand = LoopBand
  { lbIters :: [CVar]
  , lbBounds :: [AffineExpr]
  , lbExec :: ExecPolicy
  , lbReduction :: Maybe ReductionSpec
  , lbRole :: LoopRole
  , lbBody :: ScheduleTree
  }
  deriving (Eq, Show)

data ScheduleTree
  = ScheduleSequence [ScheduleTree]
  | ScheduleLoopBand LoopBand
  | ScheduleStmtRef StmtId
  deriving (Eq, Show)

data Scop = Scop
  { scProcName :: CVar
  , scRootPath :: StmtId
  , scSchedule :: ScheduleTree
  , scStatements :: [PolyhedralStmt]
  , scIterators :: [CVar]
  , scParameters :: [CVar]
  , scArrays :: [CVar]
  }
  deriving (Eq, Show)

data ScheduledScop = ScheduledScop
  { ssOriginal :: Scop
  , ssSchedule :: ScheduleTree
  }
  deriving (Eq, Show)

data ScopRejectReason
  = RejectNonSerialExec ExecPolicy
  | RejectUnsupportedBound IndexExpr
  | RejectUnsupportedRHS RHS
  | RejectUnsupportedStmt Stmt
  | RejectUnsupportedArrayRef Stmt
  | RejectNoMemoryAccess
  deriving (Eq, Show)

data ScopDiagnostic
  = ScopExtracted Scop
  | ScopRejected
      { sdProcName :: CVar
      , sdRootPath :: StmtId
      , sdReason :: ScopRejectReason
      }
  deriving (Eq, Show)

data AffineValue
  = AffineScalar AffineExpr
  | AffineTuple [AffineExpr]

type AffineEnv = Map CVar AffineValue

polyhedralProgram2 :: Program -> Program
polyhedralProgram2 (Program procs) = Program (map rewriteProc procs)
  where
    rewriteProc proc =
      foldl applyScheduled proc scheduledScops
      where
        scheduledScops =
          reverse . sortOn (scRootPath . ssOriginal) $
            map buildIdentitySchedule2 (extractProcScops2 proc)

        applyScheduled acc scheduled =
          fromMaybe acc (applyScheduledScopToProc2 scheduled acc)

buildIdentitySchedule2 :: Scop -> ScheduledScop
buildIdentitySchedule2 scop =
  ScheduledScop
    { ssOriginal = scop
    , ssSchedule = scSchedule scop
    }

extractProgramScops2 :: Program -> [Scop]
extractProgramScops2 = foldMap extractProcScops2 . programProcs
  where
    programProcs (Program procs) = procs

extractProcScops2 :: Proc -> [Scop]
extractProcScops2 = foldMap onlyScop . collectProcScopDiagnostics2
  where
    onlyScop diag = case diag of
      ScopExtracted scop -> [scop]
      ScopRejected {} -> []

collectProgramScopDiagnostics2 :: Program -> [ScopDiagnostic]
collectProgramScopDiagnostics2 = foldMap collectProcScopDiagnostics2 . programProcs
  where
    programProcs (Program procs) = procs

collectProcScopDiagnostics2 :: Proc -> [ScopDiagnostic]
collectProcScopDiagnostics2 proc = go [] (procBody proc)
  where
    go basePath stmts =
      concatMap (\(i, stmt) -> goStmt (basePath ++ [i]) stmt) (zip [0 ..] stmts)

    goStmt path stmt = case stmt of
      SLoop spec body ->
        case buildScop (procName proc) path spec body of
          Right scop -> [ScopExtracted scop]
          Left reason -> ScopRejected (procName proc) path reason : go path body
      SIf _ thn els ->
        go (path ++ [0]) thn ++ go (path ++ [1]) els
      _ ->
        []

collectScopDependences2 :: Scop -> [PolyhedralDependence]
collectScopDependences2 scop =
  [ PolyhedralDependence
      { pdKind = depKind
      , pdArray = paArray srcAccess
      , pdSourceStmt = psPath srcStmt
      , pdTargetStmt = psPath tgtStmt
      , pdDirection = depDirection
      , pdIsLoopCarried = maybe True (any (/= 0)) depDistance
      , pdDistance = depDistance
      }
  | (srcPos, srcStmt) <- orderedStmts
  , (tgtPos, tgtStmt) <- orderedStmts
  , srcPos < tgtPos
  , srcAccess <- psReads srcStmt ++ psWrites srcStmt
  , tgtAccess <- psReads tgtStmt ++ psWrites tgtStmt
  , paArray srcAccess == paArray tgtAccess
  , Just depKind <- [dependenceKindFromAccesses srcAccess tgtAccess]
  , let (depDirection, depDistance) =
          analyzeAccessDistance (scIterators scop) (paIndex srcAccess) (paIndex tgtAccess)
  ]
  where
    stmtMap = M.fromList [(psPath stmt, stmt) | stmt <- scStatements scop]
    orderedStmts =
      [ (pos, stmt)
      | (pos, stmtId) <- zip [0 :: Int ..] (scheduleStmtOrder (scSchedule scop))
      , Just stmt <- [M.lookup stmtId stmtMap]
      ]

buildScop :: CVar -> [Int] -> LoopSpec -> [Stmt] -> Either ScopRejectReason Scop
buildScop procName rootPath spec body = do
  (stmts, schedule) <- extractLoop rootPath [] M.empty spec body
  if not (any stmtHasMemoryAccess stmts)
    then Left RejectNoMemoryAccess
    else
      let iterators = collectIterators schedule
          arrays = collectArrays stmts
          params = collectParameters iterators arrays schedule stmts
      in Right Scop
          { scProcName = procName
          , scRootPath = rootPath
          , scSchedule = schedule
          , scStatements = stmts
          , scIterators = iterators
          , scParameters = params
          , scArrays = arrays
          }

stmtHasMemoryAccess :: PolyhedralStmt -> Bool
stmtHasMemoryAccess stmt = not (null (psReads stmt) && null (psWrites stmt))

collectIterators :: ScheduleTree -> [CVar]
collectIterators sched = nub (go sched)
  where
    go tree = case tree of
      ScheduleSequence xs -> foldMap go xs
      ScheduleStmtRef {} -> []
      ScheduleLoopBand band -> lbIters band ++ go (lbBody band)

collectArrays :: [PolyhedralStmt] -> [CVar]
collectArrays stmts =
  sort . nub $
    [paArray access | stmt <- stmts, access <- psReads stmt ++ psWrites stmt]

collectParameters :: [CVar] -> [CVar] -> ScheduleTree -> [PolyhedralStmt] -> [CVar]
collectParameters iterators arrays schedule stmts =
  sort . nub $
    [ var
    | var <- scheduleVars ++ stmtVars
    , var `notElem` iterators
    , var `notElem` arrays
    ]
  where
    scheduleVars = scheduleTreeVars schedule
    stmtVars =
      [ var
      | stmt <- stmts
      , constraint <- psDomain stmt
      , var <- affineExprVars (acExpr constraint)
      ]
        ++ [ var
           | stmt <- stmts
           , access <- psReads stmt ++ psWrites stmt
           , expr <- paIndex access
           , var <- affineExprVars expr
           ]

scheduleTreeVars :: ScheduleTree -> [CVar]
scheduleTreeVars sched = case sched of
  ScheduleSequence xs -> foldMap scheduleTreeVars xs
  ScheduleStmtRef {} -> []
  ScheduleLoopBand band ->
    foldMap affineExprVars (lbBounds band) ++ scheduleTreeVars (lbBody band)

scheduleStmtOrder :: ScheduleTree -> [StmtId]
scheduleStmtOrder sched = case sched of
  ScheduleSequence xs -> foldMap scheduleStmtOrder xs
  ScheduleLoopBand band -> scheduleStmtOrder (lbBody band)
  ScheduleStmtRef stmtId -> [stmtId]

extractLoop
  :: [Int]
  -> [AffineConstraint]
  -> AffineEnv
  -> LoopSpec
  -> [Stmt]
  -> Either ScopRejectReason ([PolyhedralStmt], ScheduleTree)
extractLoop loopPath domain env spec body
  | lsExec spec /= Serial = Left (RejectNonSerialExec (lsExec spec))
  | otherwise = do
      bounds <- case mapM affineExprFromIndexExpr2 (lsBounds spec) of
        Just ok -> Right ok
        Nothing -> case firstUnsupportedBound (lsBounds spec) of
          Just bad -> Left (RejectUnsupportedBound bad)
          Nothing -> Left (RejectUnsupportedStmt (SLoop spec body))
      let domain' = domain ++ loopDomainConstraints spec bounds
      (stmts, bodySchedule, _envOut) <- extractStmtList loopPath domain' env body
      pure
        ( stmts
        , ScheduleLoopBand
            LoopBand
              { lbIters = lsIters spec
              , lbBounds = bounds
              , lbExec = lsExec spec
              , lbReduction = lsRed spec
              , lbRole = lsRole spec
              , lbBody = bodySchedule
              }
        )
extractStmtList
  :: [Int]
  -> [AffineConstraint]
  -> AffineEnv
  -> [Stmt]
  -> Either ScopRejectReason ([PolyhedralStmt], ScheduleTree, AffineEnv)
extractStmtList basePath domain env stmts = go env [] [] (zip [0 ..] stmts)
  where
    go envNow accStmts accSched [] =
      Right (reverse accStmts, ScheduleSequence (reverse accSched), envNow)
    go envNow accStmts accSched ((i, stmt) : rest) =
      case stmt of
        SAssign v rhs -> do
          readAccesses <- rhsReadAccesses envNow rhs
          if not (rhsSupportedInScop rhs)
            then Left (RejectUnsupportedRHS rhs)
            else
              let stmtPath = basePath ++ [i]
                  polyStmt = PolyhedralStmt
                    { psPath = stmtPath
                    , psDomain = domain
                    , psReads = readAccesses
                    , psWrites = []
                    , psStmt = stmt
                    }
                  envNext = case affineValueFromRHS envNow rhs of
                    Just value -> M.insert v value envNow
                    Nothing -> M.delete v envNow
              in go envNext (polyStmt : accStmts) (ScheduleStmtRef stmtPath : accSched) rest
        SArrayWrite (AVar arr) idx val ->
          case affineIndexFromAtom envNow idx of
            Nothing -> Left (RejectUnsupportedArrayRef stmt)
            Just affineIdx ->
              let stmtPath = basePath ++ [i]
                  polyStmt = PolyhedralStmt
                    { psPath = stmtPath
                    , psDomain = domain
                    , psReads = []
                    , psWrites = [PolyhedralAccess PolyWrite arr affineIdx]
                    , psStmt = SArrayWrite (AVar arr) idx val
                    }
              in go envNow (polyStmt : accStmts) (ScheduleStmtRef stmtPath : accSched) rest
        SArrayWrite {} ->
          Left (RejectUnsupportedArrayRef stmt)
        SLoop spec body -> do
          let stmtPath = basePath ++ [i]
          (nestedStmts, nestedSchedule) <- extractLoop stmtPath domain envNow spec body
          go envNow (reverse nestedStmts ++ accStmts) (nestedSchedule : accSched) rest
        SIf {} ->
          Left (RejectUnsupportedStmt stmt)
        SReturn {} ->
          Left (RejectUnsupportedStmt stmt)
        SBreak ->
          Left (RejectUnsupportedStmt stmt)

firstUnsupportedBound :: [IndexExpr] -> Maybe IndexExpr
firstUnsupportedBound = foldr pick Nothing
  where
    pick bound acc = case affineExprFromIndexExpr2 bound of
      Just _ -> acc
      Nothing -> Just bound

loopDomainConstraints :: LoopSpec -> [AffineExpr] -> [AffineConstraint]
loopDomainConstraints spec bounds =
  concatMap oneDim (zip (lsIters spec) bounds)
  where
    oneDim (iter, bound) =
      [ AffineConstraint AffineGe (affineVar iter)
      , AffineConstraint AffineLe (addAffineExpr (subAffineExpr (affineVar iter) bound) (affineConst 1))
      ]

rhsSupportedInScop :: RHS -> Bool
rhsSupportedInScop rhs = case rhs of
  RAtom {} -> True
  RBinOp {} -> True
  RUnOp {} -> True
  RTuple {} -> True
  RProj {} -> True
  RRecord {} -> True
  RRecordProj {} -> True
  RPairMake {} -> True
  RPairFst {} -> True
  RPairSnd {} -> True
  RArrayLoad {} -> True
  RCall {} -> False
  RArrayAlloc {} -> False
  RArrayShape {} -> False
  RShapeSize {} -> False
  RShapeInit {} -> False
  RShapeLast {} -> False
  RFlatToNd {} -> False
  RNdToFlat {} -> False
  R2DToFlat {} -> False
  RVecLoad {} -> False
  RVecStore {} -> False
  RVecBinOp {} -> False
  RVecUnOp {} -> False
  RVecSplat {} -> False
  RVecReduce {} -> False

rhsReadAccesses :: AffineEnv -> RHS -> Either ScopRejectReason [PolyhedralAccess]
rhsReadAccesses env rhs = case rhs of
  RArrayLoad (AVar arr) idx ->
    case affineIndexFromAtom env idx of
      Just affineIdx -> Right [PolyhedralAccess PolyRead arr affineIdx]
      Nothing -> Left (RejectUnsupportedArrayRef (SAssign "scop_tmp" rhs))
  RArrayLoad {} ->
    Left (RejectUnsupportedArrayRef (SAssign "scop_tmp" rhs))
  _ ->
    Right []

affineValueFromRHS :: AffineEnv -> RHS -> Maybe AffineValue
affineValueFromRHS env rhs = case rhs of
  RAtom atom ->
    affineValueFromAtom env atom
  RBinOp op a b -> do
    lhs <- affineScalarFromAtom env a
    rhs' <- affineScalarFromAtom env b
    case op of
      CAdd -> Just (AffineScalar (addAffineExpr lhs rhs'))
      CSub -> Just (AffineScalar (subAffineExpr lhs rhs'))
      CMul ->
        AffineScalar <$> affineMul lhs rhs'
      CDiv ->
        AffineScalar <$> affineDiv lhs rhs'
      _ -> Nothing
  RTuple atoms ->
    AffineTuple <$> mapM (affineScalarFromAtom env) atoms
  RProj i atom -> do
    AffineTuple exprs <- affineValueFromAtom env atom
    exprs `atMay` fromIntegral i >>= (Just . AffineScalar)
  _ ->
    Nothing

affineValueFromAtom :: AffineEnv -> Atom -> Maybe AffineValue
affineValueFromAtom env atom = case atom of
  AVar v -> Just (M.findWithDefault (AffineScalar (affineVar v)) v env)
  AInt n -> Just (AffineScalar (affineConst n))
  _ -> Nothing

affineScalarFromAtom :: AffineEnv -> Atom -> Maybe AffineExpr
affineScalarFromAtom env atom = do
  value <- affineValueFromAtom env atom
  case value of
    AffineScalar expr -> Just expr
    AffineTuple {} -> Nothing

affineIndexFromAtom :: AffineEnv -> Atom -> Maybe [AffineExpr]
affineIndexFromAtom env atom = do
  value <- affineValueFromAtom env atom
  case value of
    AffineScalar expr -> Just [expr]
    AffineTuple exprs -> Just exprs

affineExprFromIndexExpr2 :: IndexExpr -> Maybe AffineExpr
affineExprFromIndexExpr2 = go . simplifyIndexExpr
  where
    go expr = case expr of
      IVar v -> Just (affineVar v)
      IConst n -> Just (affineConst n)
      IAdd a b -> addAffineExpr <$> go a <*> go b
      ISub a b -> subAffineExpr <$> go a <*> go b
      IMul a b -> do
        (k, rest) <- affineConstTimes a b <|> affineConstTimes b a
        scaleAffineExpr k <$> go rest
      IDiv a b -> case (go a, go b) of
        (Just numerator, Just denominator) -> affineDiv numerator denominator
        _ -> Nothing
      _ -> Nothing

affineConstTimes :: IndexExpr -> IndexExpr -> Maybe (Integer, IndexExpr)
affineConstTimes coeff expr = case simplifyIndexExpr coeff of
  IConst k -> Just (k, expr)
  _ -> Nothing

affineDiv :: AffineExpr -> AffineExpr -> Maybe AffineExpr
affineDiv numerator denominator =
  case (aeTerms denominator, aeConstant denominator) of
    (terms, 1) | M.null terms -> Just numerator
    (terms, k) | M.null terms, k /= 0, all (\n -> n `mod` k == 0) (aeConstant numerator : M.elems (aeTerms numerator)) ->
      Just (normalizeAffineExpr (AffineExpr (M.map (`div` k) (aeTerms numerator)) (aeConstant numerator `div` k)))
    _ -> Nothing

affineMul :: AffineExpr -> AffineExpr -> Maybe AffineExpr
affineMul lhs rhs =
  case (isConstAffine lhs, isConstAffine rhs) of
    (Just k, _) -> Just (scaleAffineExpr k rhs)
    (_, Just k) -> Just (scaleAffineExpr k lhs)
    _ -> Nothing

isConstAffine :: AffineExpr -> Maybe Integer
isConstAffine expr
  | M.null (aeTerms expr) = Just (aeConstant expr)
  | otherwise = Nothing

affineVar :: CVar -> AffineExpr
affineVar v = AffineExpr (M.singleton v 1) 0

affineConst :: Integer -> AffineExpr
affineConst n = AffineExpr M.empty n

addAffineExpr :: AffineExpr -> AffineExpr -> AffineExpr
addAffineExpr (AffineExpr xs xc) (AffineExpr ys yc) =
  normalizeAffineExpr (AffineExpr (M.unionWith (+) xs ys) (xc + yc))

subAffineExpr :: AffineExpr -> AffineExpr -> AffineExpr
subAffineExpr lhs rhs = addAffineExpr lhs (scaleAffineExpr (-1) rhs)

scaleAffineExpr :: Integer -> AffineExpr -> AffineExpr
scaleAffineExpr k (AffineExpr terms constant) =
  normalizeAffineExpr (AffineExpr (M.map (* k) terms) (k * constant))

normalizeAffineExpr :: AffineExpr -> AffineExpr
normalizeAffineExpr (AffineExpr terms constant) =
  AffineExpr (M.filter (/= 0) terms) constant

affineExprVars :: AffineExpr -> [CVar]
affineExprVars = M.keys . aeTerms

atMay :: [a] -> Int -> Maybe a
atMay xs n
  | n < 0 = Nothing
  | otherwise = go xs n
  where
    go [] _ = Nothing
    go (y : _) 0 = Just y
    go (_ : ys) k = go ys (k - 1)

dependenceKindFromAccesses :: PolyhedralAccess -> PolyhedralAccess -> Maybe PolyhedralDependenceKind
dependenceKindFromAccesses src tgt = case (paType src, paType tgt) of
  (PolyWrite, PolyRead) -> Just PolyDepRAW
  (PolyRead, PolyWrite) -> Just PolyDepWAR
  (PolyWrite, PolyWrite) -> Just PolyDepWAW
  _ -> Nothing

analyzeAccessDistance
  :: [CVar]
  -> [AffineExpr]
  -> [AffineExpr]
  -> (PolyhedralDependenceDirection, Maybe [Integer])
analyzeAccessDistance iterators srcIdx tgtIdx
  | length srcIdx /= length tgtIdx = (PolyDepUnknown, Nothing)
  | otherwise =
      case foldl step (Just (M.fromList [(iter, 0) | iter <- iterators])) (zip srcIdx tgtIdx) of
        Nothing -> (PolyDepUnknown, Nothing)
        Just distancesByIter ->
          let distances = [M.findWithDefault 0 iter distancesByIter | iter <- iterators]
          in (classifyDistance distances, Just distances)
  where
    step Nothing _ = Nothing
    step (Just distances) (srcExpr, tgtExpr) =
      case analyzeAffineDimension iterators srcExpr tgtExpr of
        DimExact -> Just distances
        DimUnknown -> Nothing
        DimShift iter distance ->
          let existing = M.findWithDefault 0 iter distances
          in if existing == 0
               then Just (M.insert iter distance distances)
               else Nothing

data DimDistance
  = DimExact
  | DimShift CVar Integer
  | DimUnknown

analyzeAffineDimension :: [CVar] -> AffineExpr -> AffineExpr -> DimDistance
analyzeAffineDimension iterators src tgt
  | src == tgt = DimExact
  | srcOtherTerms /= tgtOtherTerms = DimUnknown
  | srcIterTerms /= tgtIterTerms = DimUnknown
  | otherwise =
      case M.toList srcIterTerms of
        [(iter, coeff)]
          | coeff /= 0
          , let distance = aeConstant tgt - aeConstant src
          , distance `mod` coeff == 0 ->
              DimShift iter (distance `div` coeff)
        _ ->
          DimUnknown
  where
    iteratorSet = M.fromList [(iter, ()) | iter <- iterators]
    (srcIterTerms, srcOtherTerms) = partitionAffineTerms iteratorSet src
    (tgtIterTerms, tgtOtherTerms) = partitionAffineTerms iteratorSet tgt

partitionAffineTerms :: Map CVar () -> AffineExpr -> (Map CVar Integer, Map CVar Integer)
partitionAffineTerms iteratorSet expr =
  M.partitionWithKey (\var _ -> M.member var iteratorSet) (aeTerms expr)

classifyDistance :: [Integer] -> PolyhedralDependenceDirection
classifyDistance distances
  | all (>= 0) distances = PolyDepForward
  | all (<= 0) distances = PolyDepBackward
  | otherwise = PolyDepUnknown

reifyScheduledScop2 :: ScheduledScop -> Maybe [Stmt]
reifyScheduledScop2 scheduled =
  reifyScheduleTree stmtMap (ssSchedule scheduled)
  where
    stmtMap =
      M.fromList
        [ (psPath stmt, psStmt stmt)
        | stmt <- scStatements (ssOriginal scheduled)
        ]

reifyScheduleTree :: Map StmtId Stmt -> ScheduleTree -> Maybe [Stmt]
reifyScheduleTree stmtMap sched = case sched of
  ScheduleSequence xs ->
    fmap concat (mapM (reifyScheduleTree stmtMap) xs)
  ScheduleLoopBand band -> do
    bounds <- pure (map affineExprToIndexExpr (lbBounds band))
    body <- reifyScheduleTree stmtMap (lbBody band)
    pure
      [ SLoop
          LoopSpec
            { lsIters = lbIters band
            , lsBounds = bounds
            , lsExec = lbExec band
            , lsRed = lbReduction band
            , lsRole = lbRole band
            }
          body
      ]
  ScheduleStmtRef stmtId ->
    pure . pure =<< M.lookup stmtId stmtMap

affineExprToIndexExpr :: AffineExpr -> IndexExpr
affineExprToIndexExpr expr =
  simplifyIndexExpr $
    case terms ++ constantTerm of
      [] -> IConst 0
      [single] -> single
      xs -> foldr1 IAdd xs
  where
    terms =
      [ affineTermToIndexExpr coeff var
      | (var, coeff) <- M.toAscList (aeTerms expr)
      , coeff /= 0
      ]
    constantTerm
      | aeConstant expr == 0 = []
      | otherwise = [IConst (aeConstant expr)]

affineTermToIndexExpr :: Integer -> CVar -> IndexExpr
affineTermToIndexExpr coeff var
  | coeff == 1 = IVar var
  | otherwise = IMul (IConst coeff) (IVar var)

applyScheduledScopToProc2 :: ScheduledScop -> Proc -> Maybe Proc
applyScheduledScopToProc2 scheduled proc
  | procName proc /= scProcName (ssOriginal scheduled) = Nothing
  | otherwise = do
      replacement <- reifyScheduledScop2 scheduled
      body' <- replaceStmtRangeAtPath (scRootPath (ssOriginal scheduled)) replacement (procBody proc)
      pure proc { procBody = body' }

replaceStmtRangeAtPath :: StmtId -> [Stmt] -> [Stmt] -> Maybe [Stmt]
replaceStmtRangeAtPath [] _ _ = Nothing
replaceStmtRangeAtPath (ix : rest) replacement stmts = do
  (prefix, target, suffix) <- splitStmtListAt ix stmts
  case rest of
    [] ->
      pure (prefix ++ replacement ++ suffix)
    _ -> do
      target' <- descendIntoStmt rest replacement target
      pure (prefix ++ (target' : suffix))

splitStmtListAt :: Int -> [Stmt] -> Maybe ([Stmt], Stmt, [Stmt])
splitStmtListAt ix stmts
  | ix < 0 = Nothing
  | otherwise =
      case splitAt ix stmts of
        (prefix, target : suffix) -> Just (prefix, target, suffix)
        _ -> Nothing

descendIntoStmt :: StmtId -> [Stmt] -> Stmt -> Maybe Stmt
descendIntoStmt path replacement stmt = case (path, stmt) of
  (ix : rest, SLoop spec body) ->
    SLoop spec <$> replaceStmtRangeAtPath (ix : rest) replacement body
  (branch : ix : rest, SIf cond thn els)
    | branch == 0 ->
        (\thn' -> SIf cond thn' els) <$> replaceStmtRangeAtPath (ix : rest) replacement thn
    | branch == 1 ->
        (\els' -> SIf cond thn els') <$> replaceStmtRangeAtPath (ix : rest) replacement els
  _ ->
    Nothing
