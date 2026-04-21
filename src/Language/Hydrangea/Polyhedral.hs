{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.Polyhedral
--
-- Polyhedral analysis and scheduling for the CFG loop representation.
-- This module extracts affine static-control regions (SCoPs), records their
-- domains, accesses, and schedules, derives dependence information, and
-- reifies scheduled regions back into executable CFG loops. Loop bounds are
-- accepted in a slightly broader class than access expressions so the
-- scheduled form can preserve common tiled-loop bounds such as ceil-div tile
-- counts.
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
  , PolyhedralCarryStatus(..)
  , PolyhedralCarryInfo(..)
  , PolyhedralBandCarryStatus(..)
  , PolyhedralBandCarry(..)
  , PolyhedralDependenceClass(..)
  , PolyhedralDependence(..)
  , PolyhedralDependenceRelation(..)
  , LoopBand(..)
  , ScheduleTree(..)
  , ScheduleDim(..)
  , AffineLoopBand(..)
  , AffineScheduleTree(..)
  , AffineSchedule(..)
  , Scop(..)
  , ScheduledScop(..)
  , ScopRejectReason(..)
  , ScopDiagnostic(..)
  , affineExprFromIndexExpr2
  , buildIdentitySchedule2
  , collectProcScopDiagnostics2
  , collectProgramScopDiagnostics2
  , blockingScopDependences2
  , collectScopDependences2
  , collectScopDependenceRelations2
  , extractProcScops2
  , extractProgramScops2
  , polyhedralProgram2
  , polyhedralTileProgram2
  , reifyScheduledScop2
  , synthesizeScopSchedule2
  , tileScop2
  ) where

import Control.Applicative ((<|>))
import Control.Monad.State.Strict (State, evalState, state)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List (find, nub, permutations, sort, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Language.Hydrangea.CFG
import Language.Hydrangea.CFGAnalysis (definedVarsStmts2, usedVarsIndexExpr, usedVarsStmts2)
import Language.Hydrangea.CFGCore (Atom(..), BinOp(..), RHS(..), Redop(..))

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
  , acExpr :: IndexExpr
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

data PolyhedralCarryStatus
  = PolyCarryIndependent
  | PolyCarryDistance Integer
  | PolyCarryUnknown
  deriving (Eq, Show)

data PolyhedralCarryInfo = PolyhedralCarryInfo
  { pciIter :: CVar
  , pciStatus :: PolyhedralCarryStatus
  }
  deriving (Eq, Show)

data PolyhedralBandCarryStatus
  = PolyBandIndependent
  | PolyBandForward
  | PolyBandBackward
  | PolyBandUnknown
  deriving (Eq, Show)

data PolyhedralBandCarry = PolyhedralBandCarry
  { pbcIters :: [CVar]
  , pbcRole :: LoopRole
  , pbcStatus :: PolyhedralBandCarryStatus
  , pbcCarryInfo :: [PolyhedralCarryInfo]
  }
  deriving (Eq, Show)

data PolyhedralDependenceClass = PolyDepClassRegular | PolyDepClassReductionLike
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

data PolyhedralDependenceRelation = PolyhedralDependenceRelation
  { pdrKind :: PolyhedralDependenceKind
  , pdrArray :: CVar
  , pdrSourceStmt :: StmtId
  , pdrTargetStmt :: StmtId
  , pdrDirection :: PolyhedralDependenceDirection
  , pdrIsLoopCarried :: Bool
  , pdrDistance :: Maybe [Integer]
  , pdrCarryInfo :: [PolyhedralCarryInfo]
  , pdrBandCarry :: [PolyhedralBandCarry]
  , pdrClassification :: PolyhedralDependenceClass
  , pdrIsBlocking :: Bool
  }
  deriving (Eq, Show)

data LoopBand = LoopBand
  { lbIters :: [CVar]
  , lbBounds :: [IndexExpr]
  , lbExec :: ExecPolicy
  , lbReduction :: Maybe ReductionSpec
  , lbRole :: LoopRole
  , lbBody :: ScheduleTree
  }
  deriving (Eq, Show)

data TileConfig = TileConfig
  { tcDefaultTile :: Integer
  , tcReductionTile :: Integer
  }

defaultTileConfig :: TileConfig
defaultTileConfig = TileConfig
  { tcDefaultTile = 32
  , tcReductionTile = 32
  }

data StripMinePlan
  = StripKeep CVar IndexExpr
  | StripTile StripMinedDim
  deriving (Eq, Show)

data StripMinedDim = StripMinedDim
  { smdOrigIter :: CVar
  , smdOrigBound :: IndexExpr
  , smdTileSize :: Integer
  , smdTileIter :: CVar
  , smdTileStart :: CVar
  , smdTileRemain :: CVar
  , smdTileLen :: CVar
  , smdTileShort :: CVar
  , smdLocalIter :: CVar
  , smdBoundPrelude :: [Stmt]
  , smdBoundAtom :: Atom
  }
  deriving (Eq, Show)

data ScheduleTree
  = ScheduleSequence [ScheduleTree]
  | ScheduleLoopBand LoopBand
  | ScheduleStripMine LoopBand [StripMinePlan]
  | ScheduleStmtRef StmtId
  deriving (Eq, Show)

data ScheduleDim = ScheduleDim
  { sdIter :: CVar
  , sdBound :: IndexExpr
  }
  deriving (Eq, Show)

data AffineLoopBand = AffineLoopBand
  { albDims :: [ScheduleDim]
  , albExec :: ExecPolicy
  , albReduction :: Maybe ReductionSpec
  , albRole :: LoopRole
  , albBody :: AffineScheduleTree
  }
  deriving (Eq, Show)

data AffineScheduleTree
  = AffineScheduleSequence [AffineScheduleTree]
  | AffineScheduleLoopBand AffineLoopBand
  | AffineScheduleStmtRef StmtId
  deriving (Eq, Show)

data AffineSchedule = AffineSchedule
  { asRoot :: AffineScheduleTree
  , asStmtOrder :: [StmtId]
  }
  deriving (Eq, Show)

data Scop = Scop
  { scProcName :: CVar
  , scRootPath :: StmtId
  , scSchedule :: ScheduleTree
  , scAffineSchedule :: AffineSchedule
  , scStatements :: [PolyhedralStmt]
  , scIterators :: [CVar]
  , scParameters :: [CVar]
  , scArrays :: [CVar]
  }
  deriving (Eq, Show)

data ScheduledScop = ScheduledScop
  { ssOriginal :: Scop
  , ssAffineSchedule :: AffineSchedule
  , ssSchedule :: ScheduleTree
  , ssReplacement :: Maybe [Stmt]
  , ssArrayFactOverrides :: Map CVar ArrayFact
  , ssVectorAccessFactOverrides :: Map CVar VectorAccessFact
  }
  deriving (Eq, Show)

data MatmulKernel = MatmulKernel
  { mkOuterIterI :: CVar
  , mkOuterIterJ :: CVar
  , mkOuterIBound :: IndexExpr
  , mkOuterJBound :: IndexExpr
  , mkReductionIter :: CVar
  , mkReductionBound :: IndexExpr
  , mkReductionSetup :: Stmt
  , mkInitAtom :: Atom
  , mkOutputArray :: CVar
  , mkMatASetup :: Stmt
  , mkMatBSetup :: Stmt
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
type TileM = State (Set CVar, Int)

polyhedralProgram2 :: Program -> Program
polyhedralProgram2 = scheduleProgram2 synthesizeScopSchedule2

polyhedralTileProgram2 :: Program -> Program
polyhedralTileProgram2 = scheduleProgram2 tileScop2

scheduleProgram2 :: (Scop -> ScheduledScop) -> Program -> Program
scheduleProgram2 mkSchedule (Program procs) = Program (map rewriteProc procs)
  where
    rewriteProc proc =
      foldl applyScheduled proc scheduledScops
      where
        scheduledScops =
          reverse . sortOn (scRootPath . ssOriginal) $
            map mkSchedule (extractProcScops2 proc)

        applyScheduled acc scheduled =
          fromMaybe acc (applyScheduledScopToProc2 scheduled acc)

buildIdentitySchedule2 :: Scop -> ScheduledScop
buildIdentitySchedule2 scop =
  buildScheduledScop2 scop (scAffineSchedule scop) (scSchedule scop)

synthesizeScopSchedule2 :: Scop -> ScheduledScop
synthesizeScopSchedule2 scop =
  let affineSchedule = synthesizeAffineSchedule2 scop
  in  buildScheduledScop2 scop affineSchedule (scheduleTreeFromAffineSchedule2 affineSchedule)

tileScop2 :: Scop -> ScheduledScop
tileScop2 =
  tileScheduledScop2 . synthesizeScopSchedule2

tileScheduledScop2 :: ScheduledScop -> ScheduledScop
tileScheduledScop2 scheduled =
  fromMaybe generic (buildMatmulBlockedScop2 scheduled)
  where
    scop = ssOriginal scheduled
    generic =
      scheduled
        { ssSchedule =
            evalState (tileScheduleTree2 0 (ssSchedule scheduled)) (collectScopNames scop, 0)
        }

buildScheduledScop2 :: Scop -> AffineSchedule -> ScheduleTree -> ScheduledScop
buildScheduledScop2 scop affineSchedule scheduleTree =
  ScheduledScop
    { ssOriginal = scop
    , ssAffineSchedule = affineSchedule
    , ssSchedule = scheduleTree
    , ssReplacement = Nothing
    , ssArrayFactOverrides = M.empty
    , ssVectorAccessFactOverrides = M.empty
    }

affineScheduleFromScheduleTree2 :: ScheduleTree -> AffineSchedule
affineScheduleFromScheduleTree2 scheduleTree =
  AffineSchedule
    { asRoot = go scheduleTree
    , asStmtOrder = scheduleStmtOrder scheduleTree
    }
  where
    go sched = case sched of
      ScheduleSequence xs ->
        AffineScheduleSequence (map go xs)
      ScheduleLoopBand band ->
        AffineScheduleLoopBand
          AffineLoopBand
            { albDims = zipWith ScheduleDim (lbIters band) (lbBounds band)
            , albExec = lbExec band
            , albReduction = lbReduction band
            , albRole = lbRole band
            , albBody = go (lbBody band)
            }
      ScheduleStripMine band _ ->
        go (ScheduleLoopBand band)
      ScheduleStmtRef stmtId ->
        AffineScheduleStmtRef stmtId

scheduleTreeFromAffineSchedule2 :: AffineSchedule -> ScheduleTree
scheduleTreeFromAffineSchedule2 = go . asRoot
  where
    go sched = case sched of
      AffineScheduleSequence xs ->
        ScheduleSequence (map go xs)
      AffineScheduleLoopBand band ->
        ScheduleLoopBand
          LoopBand
            { lbIters = map sdIter (albDims band)
            , lbBounds = map sdBound (albDims band)
            , lbExec = albExec band
            , lbReduction = albReduction band
            , lbRole = albRole band
            , lbBody = go (albBody band)
            }
      AffineScheduleStmtRef stmtId ->
        ScheduleStmtRef stmtId

synthesizeAffineSchedule2 :: Scop -> AffineSchedule
synthesizeAffineSchedule2 scop =
  let relations = collectScopDependenceRelations2 scop
      root' = synthesizeAffineScheduleTree2 0 relations (asRoot (scAffineSchedule scop))
  in  AffineSchedule
        { asRoot = root'
        , asStmtOrder = affineScheduleStmtOrder2 root'
        }

synthesizeAffineScheduleTree2
  :: Int
  -> [PolyhedralDependenceRelation]
  -> AffineScheduleTree
  -> AffineScheduleTree
synthesizeAffineScheduleTree2 bandDepth relations sched = case sched of
  AffineScheduleSequence xs ->
    AffineScheduleSequence (map (synthesizeAffineScheduleTree2 bandDepth relations) xs)
  AffineScheduleStmtRef stmtId ->
    AffineScheduleStmtRef stmtId
  AffineScheduleLoopBand band ->
    let dims' = applyPermutation (chooseBandPermutation2 bandDepth (albDims band) relations) (albDims band)
    in  AffineScheduleLoopBand
          band
            { albDims = dims'
            , albBody = synthesizeAffineScheduleTree2 (bandDepth + 1) relations (albBody band)
            }

chooseBandPermutation2
  :: Int
  -> [ScheduleDim]
  -> [PolyhedralDependenceRelation]
  -> [Int]
chooseBandPermutation2 _ dims _
  | length dims <= 1 = [0 .. length dims - 1]
chooseBandPermutation2 bandDepth dims relations =
  fromMaybe identityPermutation (listToMaybe orderedPermutations)
  where
    identityPermutation = [0 .. length dims - 1]
    bandIters = map sdIter dims
    allPermutations = permutations identityPermutation
    legalPermutations =
      filter
        (\perm -> perm == identityPermutation || all (relationAllowsPermutation2 bandDepth bandIters perm) relations)
        allPermutations
    orderedPermutations = sortOn (permutationScore2 bandDepth dims relations) legalPermutations

relationAllowsPermutation2 :: Int -> [CVar] -> [Int] -> PolyhedralDependenceRelation -> Bool
relationAllowsPermutation2 bandDepth bandIters permutation relation =
  case prefixBandStatus2 bandDepth relation of
    PrefixNonZero -> True
    PrefixUnknown -> False
    PrefixZero ->
      case currentBandCarryForDepth2 bandDepth relation of
        Nothing -> True
        Just bandCarry ->
          case classifyBandCarry2 (permuteBandCarryInfo2 permutedBandIters bandCarry) of
            PolyBandBackward -> False
            PolyBandUnknown -> False
            PolyBandForward -> True
            PolyBandIndependent -> True
  where
    permutedBandIters = applyPermutation permutation bandIters

data PrefixDistanceStatus = PrefixZero | PrefixNonZero | PrefixUnknown
  deriving (Eq, Show)

prefixBandStatus2 :: Int -> PolyhedralDependenceRelation -> PrefixDistanceStatus
prefixBandStatus2 bandDepth relation =
  foldl step PrefixZero (take bandDepth (pdrBandCarry relation))
  where
    step PrefixNonZero _ = PrefixNonZero
    step PrefixUnknown _ = PrefixUnknown
    step PrefixZero bandCarry = case pbcStatus bandCarry of
      PolyBandIndependent -> PrefixZero
      PolyBandForward -> PrefixNonZero
      PolyBandBackward -> PrefixNonZero
      PolyBandUnknown -> PrefixUnknown

permutationScore2
  :: Int
  -> [ScheduleDim]
  -> [PolyhedralDependenceRelation]
  -> [Int]
  -> [(Int, Int, Int, Int, Int)]
permutationScore2 bandDepth dims relations permutation =
  map scoreDim (zip [0 :: Int ..] permutedDims)
  where
    permutedDims = applyPermutation permutation dims
    scoringRelations =
      [ relation
      | relation <- relations
      , prefixBandStatus2 bandDepth relation /= PrefixUnknown
      , hasCurrentBandCarry2 bandDepth relation
      ]
    scoreDim (position, dim) =
      ( negativeCount dim
      , carriedCount dim
      , unknownCount dim
      , negate (independentCount dim)
      , position
      )
    negativeCount dim =
      length
        [ ()
        | relation <- scoringRelations
        , Just distance <- [currentBandDistanceForIter2 bandDepth relation (sdIter dim)]
        , distance < 0
        ]
    carriedCount dim =
      length
        [ ()
        | relation <- scoringRelations
        , Just distance <- [currentBandDistanceForIter2 bandDepth relation (sdIter dim)]
        , distance /= 0
        ]
    unknownCount dim =
      length
        [ ()
        | relation <- scoringRelations
        , Nothing <- [currentBandDistanceForIter2 bandDepth relation (sdIter dim)]
        ]
    independentCount dim =
      length
        [ ()
        | relation <- scoringRelations
        , Just 0 <- [currentBandDistanceForIter2 bandDepth relation (sdIter dim)]
        ]

applyPermutation :: [Int] -> [a] -> [a]
applyPermutation permutation xs =
  [ x
  | ix <- permutation
  , Just x <- [atMay xs ix]
  ]

affineScheduleStmtOrder2 :: AffineScheduleTree -> [StmtId]
affineScheduleStmtOrder2 sched = case sched of
  AffineScheduleSequence xs -> foldMap affineScheduleStmtOrder2 xs
  AffineScheduleLoopBand band -> affineScheduleStmtOrder2 (albBody band)
  AffineScheduleStmtRef stmtId -> [stmtId]

extractProgramScops2 :: Program -> [Scop]
extractProgramScops2 (Program procs) = foldMap extractProcScops2 procs

extractProcScops2 :: Proc -> [Scop]
extractProcScops2 = foldMap onlyScop . collectProcScopDiagnostics2
  where
    onlyScop diag = case diag of
      ScopExtracted scop -> [scop]
      ScopRejected {} -> []

collectProgramScopDiagnostics2 :: Program -> [ScopDiagnostic]
collectProgramScopDiagnostics2 (Program procs) =
  foldMap collectProcScopDiagnostics2 procs

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
collectScopDependences2 =
  map relationToLegacyDependence2 . collectScopDependenceRelations2

collectScopDependenceRelations2 :: Scop -> [PolyhedralDependenceRelation]
collectScopDependenceRelations2 scop =
  [ mkRelation srcStmt tgtStmt srcAccess depKind accessAnalysis
  | (srcPos, srcStmt) <- orderedStmts
  , (tgtPos, tgtStmt) <- orderedStmts
  , srcPos < tgtPos
  , srcAccess <- psReads srcStmt ++ psWrites srcStmt
  , tgtAccess <- psReads tgtStmt ++ psWrites tgtStmt
  , paArray srcAccess == paArray tgtAccess
  , Just depKind <- [dependenceKindFromAccesses srcAccess tgtAccess]
  , let accessAnalysis =
          analyzeAccessDistance2 (scIterators scop) (paIndex srcAccess) (paIndex tgtAccess)
  ]
  where
    stmtMap = M.fromList [(psPath stmt, stmt) | stmt <- scStatements scop]
    orderedStmts =
      [ (pos, stmt)
       | (pos, stmtId) <- zip [0 :: Int ..] (scheduleStmtOrder (scSchedule scop))
       , Just stmt <- [M.lookup stmtId stmtMap]
       ]
    stmtContexts = stmtLoopContexts2 (scSchedule scop)
    mkRelation srcStmt tgtStmt srcAccess depKind accessAnalysis =
      let srcCtx = M.findWithDefault [] (psPath srcStmt) stmtContexts
          tgtCtx = M.findWithDefault [] (psPath tgtStmt) stmtContexts
          bandCarry = dependenceBandCarry2 srcCtx tgtCtx accessAnalysis
          depClass = dependenceClassFor2 bandCarry
          isLoopCarried = accessIsLoopCarried2 accessAnalysis
          isBlocking = dependenceIsBlocking2 bandCarry accessAnalysis depClass
      in  PolyhedralDependenceRelation
            { pdrKind = depKind
            , pdrArray = paArray srcAccess
            , pdrSourceStmt = psPath srcStmt
            , pdrTargetStmt = psPath tgtStmt
            , pdrDirection = adaDirection accessAnalysis
            , pdrIsLoopCarried = isLoopCarried
            , pdrDistance = accessDistanceVector2 accessAnalysis
            , pdrCarryInfo = adaCarryInfo accessAnalysis
            , pdrBandCarry = bandCarry
            , pdrClassification = depClass
            , pdrIsBlocking = isBlocking
            }

relationToLegacyDependence2 :: PolyhedralDependenceRelation -> PolyhedralDependence
relationToLegacyDependence2 relation =
  PolyhedralDependence
    { pdKind = pdrKind relation
    , pdArray = pdrArray relation
    , pdSourceStmt = pdrSourceStmt relation
    , pdTargetStmt = pdrTargetStmt relation
    , pdDirection = pdrDirection relation
    , pdIsLoopCarried = pdrIsLoopCarried relation
    , pdDistance = pdrDistance relation
    }

blockingScopDependences2 :: Scop -> [PolyhedralDependence]
blockingScopDependences2 =
  map relationToLegacyDependence2 . filter pdrIsBlocking . collectScopDependenceRelations2

data LoopContext = LoopContext
  { lcIters :: [CVar]
  , lcRole :: LoopRole
  }

stmtLoopContexts2 :: ScheduleTree -> Map StmtId [LoopContext]
stmtLoopContexts2 = go []
  where
    go stack sched = case sched of
      ScheduleSequence xs ->
        M.unions (map (go stack) xs)
      ScheduleStmtRef stmtId ->
        M.singleton stmtId (reverse stack)
      ScheduleLoopBand band ->
        enterBand stack band
      ScheduleStripMine band _ ->
        enterBand stack band

    enterBand stack band =
      go (LoopContext (lbIters band) (lbRole band) : stack) (lbBody band)

dependenceBandCarry2 :: [LoopContext] -> [LoopContext] -> AccessDistanceAnalysis -> [PolyhedralBandCarry]
dependenceBandCarry2 srcCtx tgtCtx accessAnalysis =
  map mkBandCarry (commonLoopPrefix2 srcCtx tgtCtx)
  where
    carryMap = M.fromList [(pciIter info, info) | info <- adaCarryInfo accessAnalysis]
    mkBandCarry loopCtx =
      let carryInfo =
            [ M.findWithDefault (PolyhedralCarryInfo iter PolyCarryUnknown) iter carryMap
            | iter <- lcIters loopCtx
            ]
      in  PolyhedralBandCarry
            { pbcIters = lcIters loopCtx
            , pbcRole = lcRole loopCtx
            , pbcStatus = classifyBandCarry2 carryInfo
            , pbcCarryInfo = carryInfo
            }

classifyBandCarry2 :: [PolyhedralCarryInfo] -> PolyhedralBandCarryStatus
classifyBandCarry2 carryInfo =
  case traverse carryDistance2 carryInfo of
    Just distances
      | all (== 0) distances ->
          PolyBandIndependent
      | otherwise ->
          case classifyDistance distances of
            PolyDepForward -> PolyBandForward
            PolyDepBackward -> PolyBandBackward
            PolyDepUnknown -> PolyBandUnknown
    Nothing
      | all ((== PolyCarryIndependent) . pciStatus) carryInfo ->
          PolyBandIndependent
      | otherwise ->
          PolyBandUnknown

dependenceClassFor2 :: [PolyhedralBandCarry] -> PolyhedralDependenceClass
dependenceClassFor2 bandCarry
  | any isReductionBand bandCarry && not (any isBlockingRegularBand bandCarry) =
      PolyDepClassReductionLike
  | otherwise =
      PolyDepClassRegular
  where
    isReductionBand band =
      isReductionRole2 (pbcRole band) && pbcStatus band /= PolyBandIndependent
    isBlockingRegularBand band =
      not (isReductionRole2 (pbcRole band)) && bandStatusBlocks2 (pbcStatus band)

dependenceIsBlocking2
  :: [PolyhedralBandCarry]
  -> AccessDistanceAnalysis
  -> PolyhedralDependenceClass
  -> Bool
dependenceIsBlocking2 bandCarry accessAnalysis depClass
  | any (\band -> not (isReductionRole2 (pbcRole band)) && bandStatusBlocks2 (pbcStatus band)) bandCarry =
      True
  | depClass == PolyDepClassReductionLike =
      False
  | otherwise =
      accessIsLoopCarried2 accessAnalysis && adaDirection accessAnalysis /= PolyDepForward

isReductionRole2 :: LoopRole -> Bool
isReductionRole2 role =
  role == LoopReduction || role == LoopReductionWrapper || role == LoopMapReduction

bandStatusBlocks2 :: PolyhedralBandCarryStatus -> Bool
bandStatusBlocks2 status = case status of
  PolyBandBackward -> True
  PolyBandUnknown -> True
  _ -> False

commonLoopPrefix2 :: [LoopContext] -> [LoopContext] -> [LoopContext]
commonLoopPrefix2 (x : xs) (y : ys)
  | lcIters x == lcIters y && lcRole x == lcRole y =
      x : commonLoopPrefix2 xs ys
commonLoopPrefix2 _ _ = []

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
           , scAffineSchedule = affineScheduleFromScheduleTree2 schedule
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
      ScheduleStripMine band plans ->
        lbIters band
          ++ [smdTileIter td | StripTile td <- plans]
          ++ [smdLocalIter td | StripTile td <- plans]
          ++ go (lbBody band)

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
      , var <- S.toList (usedVarsIndexExpr (acExpr constraint))
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
    foldMap (S.toList . usedVarsIndexExpr) (lbBounds band) ++ scheduleTreeVars (lbBody band)
  ScheduleStripMine band plans ->
    foldMap (S.toList . usedVarsIndexExpr) (lbBounds band)
      ++ concatMap stripPlanVars plans
      ++ scheduleTreeVars (lbBody band)
    where
      stripPlanVars plan = case plan of
        StripKeep {} -> []
        StripTile td ->
          [ smdTileIter td
          , smdTileStart td
          , smdTileRemain td
          , smdTileLen td
          , smdTileShort td
          , smdLocalIter td
          ]

scheduleStmtOrder :: ScheduleTree -> [StmtId]
scheduleStmtOrder sched = case sched of
  ScheduleSequence xs -> foldMap scheduleStmtOrder xs
  ScheduleLoopBand band -> scheduleStmtOrder (lbBody band)
  ScheduleStripMine band _ -> scheduleStmtOrder (lbBody band)
  ScheduleStmtRef stmtId -> [stmtId]

tileScheduleTree2 :: Int -> ScheduleTree -> TileM ScheduleTree
tileScheduleTree2 depth sched = case sched of
  ScheduleSequence xs ->
    ScheduleSequence <$> mapM (tileScheduleTree2 depth) xs
  ScheduleStmtRef stmtId ->
    pure (ScheduleStmtRef stmtId)
  ScheduleStripMine band plans -> do
    body' <- tileScheduleTree2 (depth + 1) (lbBody band)
    pure (ScheduleStripMine band { lbBody = body' } plans)
  ScheduleLoopBand band -> do
    body' <- tileScheduleTree2 (depth + 1) (lbBody band)
    let band' = band { lbBody = body' }
    if not (shouldTileBand depth band')
      then pure (ScheduleLoopBand band')
      else do
        plans <- buildStripMinePlans defaultTileConfig band'
        if any isStripTiled plans
          then pure (ScheduleStripMine band' plans)
          else pure (ScheduleLoopBand band')

shouldTileBand :: Int -> LoopBand -> Bool
shouldTileBand depth band =
  lbExec band == Serial
    && lbRole band /= LoopReductionWrapper
    && lbRole band /= LoopFold
    && lbRole band /= LoopMap
    && boundsAreIteratorIndependent band
    && all (supportsAtomBound . simplifyIndexExpr) (lbBounds band)
    && (depth > 0 || hasNestedBand (lbBody band) || length (lbIters band) > 1)

boundsAreIteratorIndependent :: LoopBand -> Bool
boundsAreIteratorIndependent band =
  let iterVars = S.fromList (lbIters band)
  in all (S.null . (`S.intersection` iterVars) . usedVarsIndexExpr) (lbBounds band)

hasNestedBand :: ScheduleTree -> Bool
hasNestedBand sched = case sched of
  ScheduleSequence xs -> any hasNestedBand xs
  ScheduleStmtRef {} -> False
  ScheduleLoopBand {} -> True
  ScheduleStripMine {} -> True

isStripTiled :: StripMinePlan -> Bool
isStripTiled StripKeep {} = False
isStripTiled StripTile {} = True

buildStripMinePlans :: TileConfig -> LoopBand -> TileM [StripMinePlan]
buildStripMinePlans cfg band = mapM buildOne (zip (lbIters band) (lbBounds band))
  where
    buildOne (iter, bound) =
      case chooseTileSize cfg band bound of
        Nothing -> pure (StripKeep iter bound)
        Just tileSize -> do
          (prelude, boundAtom) <- indexExprToAtom bound
          tileIter <- freshLike iter "_tile"
          tileStart <- freshLike iter "_tile_start"
          tileRemain <- freshLike iter "_tile_remain"
          tileLen <- freshLike iter "_tile_len"
          tileShort <- freshLike iter "_tile_short"
          localIter <- freshLike iter "_tile_idx"
          pure (StripTile StripMinedDim
            { smdOrigIter = iter
            , smdOrigBound = bound
            , smdTileSize = tileSize
            , smdTileIter = tileIter
            , smdTileStart = tileStart
            , smdTileRemain = tileRemain
            , smdTileLen = tileLen
            , smdTileShort = tileShort
            , smdLocalIter = localIter
            , smdBoundPrelude = prelude
            , smdBoundAtom = boundAtom
            })

chooseTileSize :: TileConfig -> LoopBand -> IndexExpr -> Maybe Integer
chooseTileSize cfg band bound
  | usefulBound tileSize (simplifyIndexExpr bound) = Just tileSize
  | otherwise = Nothing
  where
    tileSize = case lbRole band of
      LoopReduction -> tcReductionTile cfg
      _ -> tcDefaultTile cfg

    usefulBound n expr = case expr of
      IConst k -> k > n
      _ -> True

supportsAtomBound :: IndexExpr -> Bool
supportsAtomBound expr = case expr of
  IConst {} -> True
  IVar {} -> True
  IAdd a b -> supportsAtomBound a && supportsAtomBound b
  ISub a b -> supportsAtomBound a && supportsAtomBound b
  IMul a b -> supportsAtomBound a && supportsAtomBound b
  IDiv a b -> supportsAtomBound a && supportsAtomBound b
  _ -> False

tileCountExpr :: IndexExpr -> Integer -> IndexExpr
tileCountExpr bound tileSize =
  case simplifyIndexExpr bound of
    IConst n -> IConst ((n + tileSize - 1) `div` tileSize)
    simpleBound -> simplifyIndexExpr (IDiv (IAdd simpleBound (IConst (tileSize - 1))) (IConst tileSize))

indexExprToAtom :: IndexExpr -> TileM ([Stmt], Atom)
indexExprToAtom expr = case simplifyIndexExpr expr of
  IConst n -> pure ([], AInt n)
  IVar v -> pure ([], AVar v)
  IAdd a b -> lowerBinary CAdd a b
  ISub a b -> lowerBinary CSub a b
  IMul a b -> lowerBinary CMul a b
  IDiv a b -> lowerBinary CDiv a b
  _ -> error "unsupported index expression in polyhedral strip-mining"
  where
    lowerBinary op a b = do
      (sa, aa) <- indexExprToAtom a
      (sb, ab) <- indexExprToAtom b
      tmp <- freshLike "tile_bound" ""
      pure (sa ++ sb ++ [SAssign tmp (RBinOp op aa ab)], AVar tmp)

freshLike :: CVar -> ByteString -> TileM CVar
freshLike base suffix =
  state $ \(seen, nextId) ->
    let (fresh, nextId') = pickFresh seen nextId
    in (fresh, (S.insert fresh seen, nextId'))
  where
    pickFresh seen n =
      let candidate = base <> suffix <> "_" <> BS.pack (show n)
      in if candidate `S.member` seen
           then pickFresh seen (n + 1)
           else (candidate, n + 1)

collectScopNames :: Scop -> Set CVar
collectScopNames scop =
  usedVarsStmts2 stmts
    `S.union` definedVarsStmts2 stmts
    `S.union` collectScheduleNames (scSchedule scop)
  where
    stmts = map psStmt (scStatements scop)

collectScheduleNames :: ScheduleTree -> Set CVar
collectScheduleNames sched = case sched of
  ScheduleSequence xs -> S.unions (map collectScheduleNames xs)
  ScheduleStmtRef {} -> S.empty
  ScheduleLoopBand band ->
    S.fromList (lbIters band)
      `S.union` S.unions (map usedVarsIndexExpr (lbBounds band))
      `S.union` collectScheduleNames (lbBody band)
  ScheduleStripMine band plans ->
    collectScheduleNames (ScheduleLoopBand band)
      `S.union` S.fromList
        [ v
        | StripTile td <- plans
        , v <-
            [ smdTileIter td
            , smdTileStart td
            , smdTileRemain td
            , smdTileLen td
            , smdTileShort td
            , smdLocalIter td
            ]
        ]

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
      bounds <- case mapM supportedBoundExprFromIndexExpr2 (lsBounds spec) of
        Just ok -> Right ok
        Nothing -> case firstUnsupportedBound (lsBounds spec) of
          Just bad -> Left (RejectUnsupportedBound bad)
          Nothing -> Left (RejectUnsupportedStmt (SLoop spec body))
      let domain' = domain ++ loopDomainConstraints spec bounds
      (stmts, bodySchedule, _) <- extractStmtList loopPath domain' env body
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
firstUnsupportedBound =
  find (\bound -> supportedBoundExprFromIndexExpr2 bound == Nothing)

loopDomainConstraints :: LoopSpec -> [IndexExpr] -> [AffineConstraint]
loopDomainConstraints spec bounds =
  concatMap oneDim (zip (lsIters spec) bounds)
  where
    oneDim (iter, bound) =
      [ AffineConstraint AffineGe (IVar iter)
      , AffineConstraint AffineLe (simplifyIndexExpr (IAdd (ISub (IVar iter) bound) (IConst 1)))
      ]

rhsSupportedInScop :: RHS -> Bool
rhsSupportedInScop rhs =
  rhsIsAffineScopCore rhs || rhsIsSupportedScopPrelude rhs

rhsIsAffineScopCore :: RHS -> Bool
rhsIsAffineScopCore rhs = case rhs of
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
  _ -> False

-- | Scalar setup forms that preserve affine memory behavior.
-- These are accepted inside a SCoP so extraction can keep shape normalization
-- and index conversion around an affine kernel without admitting general
-- effectful or non-affine operations.
rhsIsSupportedScopPrelude :: RHS -> Bool
rhsIsSupportedScopPrelude rhs = case rhs of
  RCall _ [] -> True
  RArrayShape {} -> True
  RFlatToNd {} -> True
  RNdToFlat {} -> True
  _ -> False

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
affineValueFromRHS env rhs =
  affineCoreValueFromRHS env rhs <|> affinePreludeValueFromRHS env rhs

affineCoreValueFromRHS :: AffineEnv -> RHS -> Maybe AffineValue
affineCoreValueFromRHS env rhs = case rhs of
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

affinePreludeValueFromRHS :: AffineEnv -> RHS -> Maybe AffineValue
affinePreludeValueFromRHS env rhs = case rhs of
  RArrayShape atom ->
    AffineTuple <$> arrayShapeTupleFromAtom env atom 2
  RFlatToNd flat shape -> do
    flatExpr <- affineScalarFromAtom env flat
    dims <- arrayShapeTupleFromAtom env shape 1
    case dims of
      [_] -> Just (AffineTuple [flatExpr])
      _ -> Nothing
  RNdToFlat idx shape -> do
    AffineTuple idxExprs <- affineValueFromAtom env idx
    shapeExprs <- arrayShapeTupleFromAtom env shape (length idxExprs)
    AffineScalar <$> affineNdToFlat idxExprs shapeExprs
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

supportedBoundExprFromIndexExpr2 :: IndexExpr -> Maybe IndexExpr
supportedBoundExprFromIndexExpr2 expr
  | boundExprSupported simple = Just simple
  | otherwise = Nothing
  where
    simple = simplifyIndexExpr expr

boundExprSupported :: IndexExpr -> Bool
boundExprSupported expr = case expr of
  IVar {} -> True
  IConst {} -> True
  IAdd a b -> boundExprSupported a && boundExprSupported b
  ISub a b -> boundExprSupported a && boundExprSupported b
  IMul a b -> (isConstExpr a && boundExprSupported b) || (boundExprSupported a && isConstExpr b)
  IDiv a b -> case b of
    IConst k -> k /= 0 && boundExprSupported a
    _ -> False
  _ -> False
  where
    isConstExpr IConst {} = True
    isConstExpr _ = False

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

arrayShapeTupleFromAtom :: AffineEnv -> Atom -> Int -> Maybe [AffineExpr]
arrayShapeTupleFromAtom env atom fallbackRank = case atom of
  AVar v -> case M.lookup v env of
    Just (AffineTuple exprs) -> Just exprs
    Just (AffineScalar _) -> Nothing
    Nothing ->
      Just [affineVar (v <> "_dim" <> BS.pack (show i)) | i <- [0 .. fallbackRank - 1]]
  _ -> do
    AffineTuple exprs <- affineValueFromAtom env atom
    Just exprs

affineNdToFlat :: [AffineExpr] -> [AffineExpr] -> Maybe AffineExpr
affineNdToFlat idxExprs shapeExprs =
  case (idxExprs, shapeExprs) of
    ([i], [_]) ->
      Just i
    ([i, j], [_, width]) -> do
      rowOffset <- affineMul i width
      Just (addAffineExpr rowOffset j)
    _ ->
      Nothing

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

data AccessDistanceAnalysis = AccessDistanceAnalysis
  { adaDirection :: PolyhedralDependenceDirection
  , adaCarryInfo :: [PolyhedralCarryInfo]
  }

analyzeAccessDistance2
  :: [CVar]
  -> [AffineExpr]
  -> [AffineExpr]
  -> AccessDistanceAnalysis
analyzeAccessDistance2 iterators srcIdx tgtIdx
  | length srcIdx /= length tgtIdx =
      AccessDistanceAnalysis
        { adaDirection = PolyDepUnknown
        , adaCarryInfo = [PolyhedralCarryInfo iter PolyCarryUnknown | iter <- iterators]
        }
  | otherwise =
      let carryInfo = foldl step initialCarryInfo (zip srcIdx tgtIdx)
          direction =
            case traverse carryDistance2 carryInfo of
              Just distances -> classifyDistance distances
              Nothing -> PolyDepUnknown
      in  AccessDistanceAnalysis
            { adaDirection = direction
            , adaCarryInfo = carryInfo
            }
  where
    initialCarryInfo = [PolyhedralCarryInfo iter PolyCarryIndependent | iter <- iterators]
    step carryInfo (srcExpr, tgtExpr) =
      case analyzeAffineDimension iterators srcExpr tgtExpr of
        DimExact ->
          carryInfo
        DimUnknown ->
          map (\info -> info { pciStatus = PolyCarryUnknown }) carryInfo
        DimShift iter distance ->
          map (mergeCarryInfo2 iter distance) carryInfo

mergeCarryInfo2 :: CVar -> Integer -> PolyhedralCarryInfo -> PolyhedralCarryInfo
mergeCarryInfo2 iter distance info
  | pciIter info /= iter = info
  | otherwise =
      info
        { pciStatus = case pciStatus info of
            PolyCarryIndependent -> PolyCarryDistance distance
            PolyCarryDistance existing
              | existing == distance ->
                  PolyCarryDistance distance
            _ ->
              PolyCarryUnknown
        }

accessDistanceVector2 :: AccessDistanceAnalysis -> Maybe [Integer]
accessDistanceVector2 = traverse carryDistance2 . adaCarryInfo

accessIsLoopCarried2 :: AccessDistanceAnalysis -> Bool
accessIsLoopCarried2 analysis =
  case accessDistanceVector2 analysis of
    Just distances -> any (/= 0) distances
    Nothing ->
      any
        (\info -> case pciStatus info of
          PolyCarryDistance distance -> distance /= 0
          PolyCarryUnknown -> True
          PolyCarryIndependent -> False
        )
        (adaCarryInfo analysis)

carryDistance2 :: PolyhedralCarryInfo -> Maybe Integer
carryDistance2 info = case pciStatus info of
  PolyCarryIndependent -> Just 0
  PolyCarryDistance distance -> Just distance
  PolyCarryUnknown -> Nothing

currentBandCarryForDepth2 :: Int -> PolyhedralDependenceRelation -> Maybe PolyhedralBandCarry
currentBandCarryForDepth2 bandDepth relation =
  atMay (pdrBandCarry relation) bandDepth

hasCurrentBandCarry2 :: Int -> PolyhedralDependenceRelation -> Bool
hasCurrentBandCarry2 bandDepth =
  maybe False (const True) . currentBandCarryForDepth2 bandDepth

permuteBandCarryInfo2 :: [CVar] -> PolyhedralBandCarry -> [PolyhedralCarryInfo]
permuteBandCarryInfo2 bandIters bandCarry =
  [ M.findWithDefault (PolyhedralCarryInfo iter PolyCarryUnknown) iter carryInfoByIter
  | iter <- bandIters
  ]
  where
    carryInfoByIter = M.fromList [(pciIter info, info) | info <- pbcCarryInfo bandCarry]

currentBandDistanceForIter2 :: Int -> PolyhedralDependenceRelation -> CVar -> Maybe Integer
currentBandDistanceForIter2 bandDepth relation iter = do
  bandCarry <- currentBandCarryForDepth2 bandDepth relation
  carryInfo <- find ((== iter) . pciIter) (pbcCarryInfo bandCarry)
  carryDistance2 carryInfo

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
classifyDistance distances =
  case dropWhile (== 0) distances of
    [] -> PolyDepForward
    d : _
      | d > 0 -> PolyDepForward
      | d < 0 -> PolyDepBackward
      | otherwise -> PolyDepUnknown

reifyScheduledScop2 :: ScheduledScop -> Maybe [Stmt]
reifyScheduledScop2 scheduled =
  case ssReplacement scheduled of
    Just replacement -> Just replacement
    Nothing -> reifyScheduleTree stmtMap (ssSchedule scheduled)
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
  ScheduleStripMine band plans -> do
    body <- reifyScheduleTree stmtMap (lbBody band)
    let tiledDims = [td | StripTile td <- plans]
        tilePrelude = concatMap smdBoundPrelude tiledDims
        outerTileSpec = LoopSpec
          { lsIters = map smdTileIter tiledDims
          , lsBounds = map (\td -> tileCountExpr (smdOrigBound td) (smdTileSize td)) tiledDims
          , lsExec = Serial
          , lsRed = Nothing
          , lsRole = tiledLoopRole (lbRole band)
          }
        innerLocalSpec = LoopSpec
          { lsIters = map localIter plans
          , lsBounds = map localBound plans
          , lsExec = lbExec band
          , lsRed = lbReduction band
          , lsRole = lbRole band
          }
        outerSetup = concatMap setupStripDim tiledDims
        innerSetup = map assignOrigIter tiledDims
        innerLocalLoop = SLoop innerLocalSpec (innerSetup ++ body)
    pure (tilePrelude ++ [SLoop outerTileSpec (outerSetup ++ [innerLocalLoop])])
    where
      localIter plan = case plan of
        StripKeep iter _ -> iter
        StripTile td -> smdLocalIter td

      localBound plan = case plan of
        StripKeep _ bound -> bound
        StripTile td -> IVar (smdTileLen td)
  ScheduleLoopBand band -> do
    bounds <- pure (lbBounds band)
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

setupStripDim :: StripMinedDim -> [Stmt]
setupStripDim td =
  [ SAssign (smdTileStart td) (RBinOp CMul (AVar (smdTileIter td)) (AInt (smdTileSize td))) ]
    ++ minLenSetup td

assignOrigIter :: StripMinedDim -> Stmt
assignOrigIter td =
  SAssign (smdOrigIter td) (RBinOp CAdd (AVar (smdTileStart td)) (AVar (smdLocalIter td)))

tiledLoopRole :: LoopRole -> LoopRole
tiledLoopRole role = case role of
  LoopPlain -> LoopPlain
  LoopFold -> LoopFold
  LoopMap -> LoopMap
  LoopReductionWrapper -> LoopMap
  LoopReduction -> LoopMap
  LoopMapReduction -> LoopMap

minLenSetup :: StripMinedDim -> [Stmt]
minLenSetup td =
  [ SAssign (smdTileRemain td) (RBinOp CSub (smdBoundAtom td) (AVar (smdTileStart td)))
  , SAssign (smdTileShort td) (RBinOp CLt (AVar (smdTileRemain td)) (AInt (smdTileSize td)))
  , SIf
      (AVar (smdTileShort td))
      [SAssign (smdTileLen td) (RAtom (AVar (smdTileRemain td)))]
      [SAssign (smdTileLen td) (RAtom (AInt (smdTileSize td)))]
  ]

applyScheduledScopToProc2 :: ScheduledScop -> Proc -> Maybe Proc
applyScheduledScopToProc2 scheduled proc
  | procName proc /= scProcName (ssOriginal scheduled) = Nothing
  | otherwise = do
      replacement <- reifyScheduledScop2 scheduled
      body' <- replaceStmtRangeAtPath (scRootPath (ssOriginal scheduled)) replacement (procBody proc)
      pure
        proc
          { procBody = body'
          , procArrayFacts =
              M.union (ssArrayFactOverrides scheduled) (procArrayFacts proc)
          , procVectorAccessFacts =
              M.union
                (ssVectorAccessFactOverrides scheduled)
                (procVectorAccessFacts proc)
          }

buildMatmulBlockedScop2 :: ScheduledScop -> Maybe ScheduledScop
buildMatmulBlockedScop2 scheduled = do
  kernel <- matchMatmulKernel2 scop
  let (replacement, vectorFacts) =
        evalState (buildMatmulBlockedBody2 kernel) (collectScopNames scop, 0)
      scopedArrayOverrides =
        M.singleton
          (mkOutputArray kernel)
          ArrayFact
            { afFreshAlloc = True
            , afWriteOnce = False
            , afReadOnly = False
            }
  pure
    scheduled
      { ssReplacement = Just replacement
      , ssArrayFactOverrides =
          M.union scopedArrayOverrides (ssArrayFactOverrides scheduled)
      , ssVectorAccessFactOverrides =
          M.union vectorFacts (ssVectorAccessFactOverrides scheduled)
      }
  where
    scop = ssOriginal scheduled

matchMatmulKernel2 :: Scop -> Maybe MatmulKernel
matchMatmulKernel2 scop = do
  [SLoop outerSpec outerBody] <- reifyScheduledScop2 (buildIdentitySchedule2 scop)
  [outerI, outerJ] <- pure (lsIters outerSpec)
  [outerIBound, outerJBound] <- pure (lsBounds outerSpec)
  if lsRole outerSpec /= LoopMap
    then Nothing
    else do
      wrapperBody <- do
        SLoop wrapperSpec wrapperBody <-
          find
            (\stmt -> case stmt of
              SLoop spec _ -> lsRole spec == LoopReductionWrapper
              _ -> False)
            outerBody
        if lsBounds wrapperSpec == [IConst 1]
          then Just wrapperBody
          else Nothing
      reductionLoop@(redSpec, redBody) <- extractReductionLoop2 wrapperBody
      ReductionSpec accVar _ RAdd <- lsRed redSpec
      [reductionIter] <- pure (lsIters redSpec)
      [reductionBound] <- pure (lsBounds redSpec)
      reductionSetup <- case reductionBound of
        IVar redBoundVar ->
          find
            (\stmt -> case stmt of
              SAssign v (RCall _ []) -> v == redBoundVar
              _ -> False)
            outerBody
        _ ->
          Nothing
      SAssign accInitVar (RAtom initAtom) <-
        find
          (\stmt -> case stmt of
            SAssign v (RAtom _) -> v == accVar
            _ -> False)
          outerBody
      if accInitVar /= accVar
        then Nothing
        else do
          SArrayWrite (AVar outArr) _ (AVar outAcc) <- listToMaybe (reverse outerBody)
          if outAcc /= accVar
            then Nothing
            else do
              (matASetup, matBSetup) <- extractMatmulInputSetups2 redBody
              if reductionLooksLikeMatmul2 accVar reductionLoop
                then
                  Just
                    MatmulKernel
                      { mkOuterIterI = outerI
                      , mkOuterIterJ = outerJ
                      , mkOuterIBound = outerIBound
                      , mkOuterJBound = outerJBound
                      , mkReductionIter = reductionIter
                      , mkReductionBound = reductionBound
                      , mkReductionSetup = reductionSetup
                      , mkInitAtom = initAtom
                      , mkOutputArray = outArr
                      , mkMatASetup = matASetup
                      , mkMatBSetup = matBSetup
                      }
                else Nothing

extractReductionLoop2 :: [Stmt] -> Maybe (LoopSpec, [Stmt])
extractReductionLoop2 body = do
  SLoop spec redBody <-
    find
      (\stmt -> case stmt of
        SLoop loopSpec _ -> lsRole loopSpec == LoopReduction
        _ -> False)
      body
  pure (spec, redBody)

extractMatmulInputSetups2 :: [Stmt] -> Maybe (Stmt, Stmt)
extractMatmulInputSetups2 body = do
  (firstSetup : secondSetup : _) <-
    pure
      [ stmt
      | stmt@(SAssign _ (RCall _ [])) <- body
      ]
  pure (firstSetup, secondSetup)

reductionLooksLikeMatmul2 :: CVar -> (LoopSpec, [Stmt]) -> Bool
reductionLooksLikeMatmul2 accVar (_redSpec, redBody) =
  hasFloatMul && hasAccumulate && readCount >= 2
  where
    hasFloatMul =
      any
        (\stmt -> case stmt of
          SAssign _ (RBinOp CMulF _ _) -> True
          _ -> False)
        redBody
    hasAccumulate =
      any
        (\stmt -> case stmt of
          SAssign v (RBinOp CAddF (AVar acc) _) -> v == accVar && acc == accVar
          _ -> False)
        redBody
    readCount =
      length
        [ ()
        | SAssign _ (RArrayLoad _ _) <- redBody
        ]

buildMatmulBlockedBody2 :: MatmulKernel -> TileM ([Stmt], Map CVar VectorAccessFact)
buildMatmulBlockedBody2 kernel = do
  iTile <- buildForcedStripDim2 (mkOuterIterI kernel) (mkOuterIBound kernel) (tcDefaultTile defaultTileConfig)
  jTile <- buildForcedStripDim2 (mkOuterIterJ kernel) (mkOuterJBound kernel) (tcDefaultTile defaultTileConfig)
  kTile <- buildForcedStripDim2 (mkReductionIter kernel) (mkReductionBound kernel) (tcReductionTile defaultTileConfig)
  cInitRowBase <- freshLike "c_row_base_init" ""
  cInitFlat <- freshLike "c_flat_init" ""
  aRowBase <- freshLike "a_row_base" ""
  bRowBase <- freshLike "b_row_base" ""
  cRowBase <- freshLike "c_row_base" ""
  aFlat <- freshLike "a_flat" ""
  bFlat <- freshLike "b_flat" ""
  cFlat <- freshLike "c_flat" ""
  aVal <- freshLike "a_val" ""
  bVal <- freshLike "b_val" ""
  cVal <- freshLike "c_val" ""
  prod <- freshLike "prod" ""
  cNext <- freshLike "c_next" ""
  let outputArr = AVar (mkOutputArray kernel)
      outWidthAtom = smdBoundAtom jTile
      redWidthAtom = smdBoundAtom kTile
      denseAliasFact iter = VectorAccessFact
        { vxfDenseLinearIndexOf = Just iter
        , vxfDenseRead = False
        , vxfIndirectRead = False
        , vxfContiguousWrite = False
        }
      vectorFacts =
        M.fromList
          [ (cInitFlat, denseAliasFact (smdLocalIter jTile))
          , (bFlat, denseAliasFact (smdLocalIter jTile))
          , (cFlat, denseAliasFact (smdLocalIter jTile))
          ]
      initJLoop =
        localLoop2
          jTile
          LoopMap
          [ SAssign cInitFlat (RBinOp CAdd (AVar cInitRowBase) (AVar (mkOuterIterJ kernel)))
          , SArrayWrite outputArr (AVar cInitFlat) (mkInitAtom kernel)
          ]
      initILoop =
        localLoop2
          iTile
          LoopPlain
          [ SAssign cInitRowBase (RBinOp CMul outWidthAtom (AVar (mkOuterIterI kernel)))
          , initJLoop
          ]
      computeJLoop =
        localLoop2
          jTile
          LoopMap
          [ SAssign bFlat (RBinOp CAdd (AVar bRowBase) (AVar (mkOuterIterJ kernel)))
          , SAssign cFlat (RBinOp CAdd (AVar cRowBase) (AVar (mkOuterIterJ kernel)))
          , SAssign cVal (RArrayLoad outputArr (AVar cFlat))
          , SAssign bVal (RArrayLoad (stmtAssignedVarAtom2 (mkMatBSetup kernel)) (AVar bFlat))
          , SAssign prod (RBinOp CMulF (AVar aVal) (AVar bVal))
          , SAssign cNext (RBinOp CAddF (AVar cVal) (AVar prod))
          , SArrayWrite outputArr (AVar cFlat) (AVar cNext)
          ]
      computeKLoop =
        localLoop2
          kTile
          LoopPlain
          [ SAssign aFlat (RBinOp CAdd (AVar aRowBase) (AVar (mkReductionIter kernel)))
          , SAssign aVal (RArrayLoad (stmtAssignedVarAtom2 (mkMatASetup kernel)) (AVar aFlat))
          , SAssign bRowBase (RBinOp CMul outWidthAtom (AVar (mkReductionIter kernel)))
          , computeJLoop
          ]
      computeILoop =
        localLoop2
          iTile
          LoopPlain
          [ SAssign aRowBase (RBinOp CMul redWidthAtom (AVar (mkOuterIterI kernel)))
          , SAssign cRowBase (RBinOp CMul outWidthAtom (AVar (mkOuterIterI kernel)))
          , computeKLoop
          ]
      kkLoop = tileLoop2 kTile LoopPlain [computeILoop]
      jjLoop = tileLoop2 jTile LoopPlain [initILoop, kkLoop]
      iiLoop = tileLoop2 iTile LoopPlain [jjLoop]
      tilePrelude =
        smdBoundPrelude iTile
          ++ smdBoundPrelude jTile
          ++ smdBoundPrelude kTile
      replacement =
        [ mkReductionSetup kernel
        , mkMatASetup kernel
        , mkMatBSetup kernel
        ]
          ++ tilePrelude
          ++ [iiLoop]
  pure (replacement, vectorFacts)

buildForcedStripDim2 :: CVar -> IndexExpr -> Integer -> TileM StripMinedDim
buildForcedStripDim2 iter bound tileSize = do
  (prelude, boundAtom) <- indexExprToAtom bound
  tileIter <- freshLike iter "_tile"
  tileStart <- freshLike iter "_tile_start"
  tileRemain <- freshLike iter "_tile_remain"
  tileLen <- freshLike iter "_tile_len"
  tileShort <- freshLike iter "_tile_short"
  localIter <- freshLike iter "_tile_idx"
  pure
    StripMinedDim
      { smdOrigIter = iter
      , smdOrigBound = bound
      , smdTileSize = tileSize
      , smdTileIter = tileIter
      , smdTileStart = tileStart
      , smdTileRemain = tileRemain
      , smdTileLen = tileLen
      , smdTileShort = tileShort
      , smdLocalIter = localIter
      , smdBoundPrelude = prelude
      , smdBoundAtom = boundAtom
      }

tileLoop2 :: StripMinedDim -> LoopRole -> [Stmt] -> Stmt
tileLoop2 td role body =
  SLoop
    LoopSpec
      { lsIters = [smdTileIter td]
      , lsBounds = [tileCountExpr (smdOrigBound td) (smdTileSize td)]
      , lsExec = Serial
      , lsRed = Nothing
      , lsRole = role
      }
    (setupStripDim td ++ body)

localLoop2 :: StripMinedDim -> LoopRole -> [Stmt] -> Stmt
localLoop2 td role body =
  SLoop
    LoopSpec
      { lsIters = [smdLocalIter td]
      , lsBounds = [IVar (smdTileLen td)]
      , lsExec = Serial
      , lsRed = Nothing
      , lsRole = role
      }
    (assignOrigIter td : body)

stmtAssignedVarAtom2 :: Stmt -> Atom
stmtAssignedVarAtom2 stmt = case stmt of
  SAssign v _ -> AVar v
  _ -> error "expected assignment statement when extracting matmul setup variable"

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
