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
  , SkewSpec(..)
  , IteratorProfitability(..)
  , ProfitabilityFacts
  , AffineLoopBand(..)
  , AffineScheduleTree(..)
  , AffineSchedule(..)
  , Scop(..)
  , ScheduledScop(..)
  , ScopRejectReason(..)
  , ScopDiagnostic(..)
  , affineExprFromIndexExpr2
  , bandsStructurallyCompatible2
  , buildIdentitySchedule2
  , collectProcScopDiagnostics2
  , collectProgramScopDiagnostics2
  , blockingScopDependences2
  , collectScopDependences2
  , collectScopDependenceRelations2
  , collectScopProfitabilityFacts2
  , chooseBandPermutation2
  , extractProcScops2
  , extractProgramScops2
  , fusionLegalAtDepth2
  , polyhedralProgram2
  , polyhedralIdentityTileProgram2
  , polyhedralTileProgram2
  , reifyScheduledScop2
  , suggestBandSkew2
  , suggestCrossBandSkew2
  , synthesizeScopSchedule2
  , tileIdentityScop2
  , tileScop2
  , tryFuseBands2
  , detectTemporalAlias
  , augmentWithTemporalDeps
  ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Monad.State.Strict (State, evalState, modify, runState, state)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List (find, nub, permutations, sort, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, isJust, listToMaybe, maybeToList)
import Data.Set (Set)
import Data.Set qualified as S
import Language.Hydrangea.CFG
import Language.Hydrangea.CFGAnalysis (definedVarsStmts2, usedVarsIndexExpr, usedVarsStmts2)
import Language.Hydrangea.CFGCore (Atom(..), BinOp(..), CType(..), RHS(..), Redop(..))
import Language.Hydrangea.CFGOpt (substStmts2)

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
  , pdrSrcIndex :: [AffineExpr]
  , pdrTgtIndex :: [AffineExpr]
  }
  deriving (Eq, Show)

data LoopBand = LoopBand
  { lbIters :: [CVar]
  , lbBounds :: [IndexExpr]
  , lbOrigins :: [IndexExpr]
  , lbExec :: ExecPolicy
  , lbReduction :: Maybe ReductionSpec
  , lbRole :: LoopRole
  , lbBody :: ScheduleTree
  , lbSkew :: [SkewSpec]
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

defaultWavefrontBlockWidth :: Integer
defaultWavefrontBlockWidth = 4

defaultWavefrontExtraBufferBudget :: Integer
defaultWavefrontExtraBufferBudget = 262144

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

data SkewSpec = SkewSpec
  { skewTarget :: CVar
  , skewSource :: CVar
  , skewCoeff  :: Integer
  }
  deriving (Eq, Show)

data AffineLoopBand = AffineLoopBand
  { albDims :: [ScheduleDim]
  , albOrigins :: [IndexExpr]
  , albExec :: ExecPolicy
  , albReduction :: Maybe ReductionSpec
  , albRole :: LoopRole
  , albSkew :: [SkewSpec]
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
  , ssTypeOverrides :: Map CVar CType
  , ssArrayFactOverrides :: Map CVar ArrayFact
  , ssVectorAccessFactOverrides :: Map CVar VectorAccessFact
  }
  deriving (Eq, Show)

data BlockedMapReductionKernel = BlockedMapReductionKernel
  { bmrOuterIterI :: CVar
  , bmrOuterIterJ :: CVar
  , bmrOuterIBound :: IndexExpr
  , bmrOuterJBound :: IndexExpr
  , bmrReductionIter :: CVar
  , bmrReductionBound :: IndexExpr
  , bmrAccVar :: CVar
  , bmrInitAtom :: Atom
  , bmrOutputArray :: CVar
  , bmrOuterSetup :: [Stmt]
  , bmrWrapperIter :: CVar
  , bmrWrapperSetup :: [Stmt]
  , bmrReductionSetup :: [Stmt]
  , bmrOutputIndex :: Atom
  , bmrUpdateOp :: BinOp
  , bmrUpdateLhs :: Atom
  , bmrUpdateRhs :: Atom
  }
  deriving (Eq, Show)

data WavefrontKernel = WavefrontKernel
  { wfkIterT :: CVar
  , wfkIterBound :: IndexExpr
  , wfkCurArray :: CVar
  , wfkNextArray :: CVar
  , wfkAllocShape :: Atom
  , wfkPreallocatedNext :: Bool
  , wfkInitTracker :: Maybe CVar
  , wfkHoistedPrefix :: [Stmt]
  , wfkStageSuffix :: [Stmt]
  , wfkInnerSpec :: LoopSpec
  , wfkInnerBody :: [Stmt]
  , wfkSkewCoeff :: Integer
  , wfkStageWidth :: Integer
  , wfkStageExec :: ExecPolicy
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
  | AffineFlatIndex [AffineExpr]
  | AffineRowOffset AffineExpr AffineExpr

data IteratorProfitability = IteratorProfitability
  { ipAccessDimHits :: Map Int Int
  , ipUnitStrideLastHits :: Int
  , ipReadTouchHits :: Int
  , ipInvariantReadHits :: Int
  }

type AffineEnv = Map CVar AffineValue
type TileM = State (Set CVar, Int)
type ProfitabilityFacts = Map CVar IteratorProfitability

polyhedralProgram2 :: Program -> Program
polyhedralProgram2 = scheduleProgram2 (specializeScheduledScop2 . synthesizeScopSchedule2)

polyhedralTileProgram2 :: Program -> Program
polyhedralTileProgram2 = scheduleProgram2 tileScop2

polyhedralIdentityTileProgram2 :: Program -> Program
polyhedralIdentityTileProgram2 = scheduleProgram2 tileIdentityScop2

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
  let (affineSchedule, fusionRenames) = synthesizeAffineSchedule2 scop
      renamedScop
        | M.null fusionRenames = scop
        | otherwise =
            let substEnv = M.map AVar fusionRenames
            in  scop
                  { scStatements =
                      map
                        (\ps -> ps { psStmt = case substStmts2 substEnv [psStmt ps] of { s : _ -> s; [] -> psStmt ps } })
                        (scStatements scop)
                  }
  in  buildScheduledScop2 renamedScop affineSchedule (scheduleTreeFromAffineSchedule2 affineSchedule)

tileScop2 :: Scop -> ScheduledScop
tileScop2 =
  tileScheduledScop2 . synthesizeScopSchedule2

tileIdentityScop2 :: Scop -> ScheduledScop
tileIdentityScop2 =
  tileScheduledScop2 . buildIdentitySchedule2

tileScheduledScop2 :: ScheduledScop -> ScheduledScop
tileScheduledScop2 scheduled =
  case ssReplacement specialized of
    Just {} -> specialized
    Nothing -> fromMaybe generic (buildBlockedMapReductionScop2 generic)
  where
    specialized = specializeScheduledScop2 scheduled
    scop = ssOriginal specialized
    profitability = collectScopProfitabilityFacts2 scop
    relations = collectScopDependenceRelations2 scop
    generic =
      specialized
        { ssSchedule =
            evalState
              (tileScheduleTree2 profitability relations 0 (ssSchedule specialized))
              (collectScopNames scop, 0)
        }

specializeScheduledScop2 :: ScheduledScop -> ScheduledScop
specializeScheduledScop2 scheduled =
  fromMaybe scheduled (buildWavefrontScop2 scheduled)

buildScheduledScop2 :: Scop -> AffineSchedule -> ScheduleTree -> ScheduledScop
buildScheduledScop2 scop affineSchedule scheduleTree =
  ScheduledScop
    { ssOriginal = scop
    , ssAffineSchedule = affineSchedule
    , ssSchedule = scheduleTree
    , ssReplacement = Nothing
    , ssTypeOverrides = M.empty
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
            , albOrigins = lbOrigins band
            , albExec = lbExec band
            , albReduction = lbReduction band
            , albRole = lbRole band
            , albSkew = []
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
            , lbOrigins = albOrigins band
            , lbExec = albExec band
            , lbReduction = albReduction band
            , lbRole = albRole band
            , lbBody = go (albBody band)
            , lbSkew = albSkew band
            }
      AffineScheduleStmtRef stmtId ->
        ScheduleStmtRef stmtId

synthesizeAffineSchedule2 :: Scop -> (AffineSchedule, Map CVar CVar)
synthesizeAffineSchedule2 scop =
  let relations = collectScopDependenceRelations2 scop
      profitability = collectScopProfitabilityFacts2 scop
      (root', fusionRenames) =
        runState
          (synthesizeAffineScheduleTree2 profitability 0 relations (asRoot (scAffineSchedule scop)))
          M.empty
      schedule =
        AffineSchedule
          { asRoot = root'
          , asStmtOrder = affineScheduleStmtOrder2 root'
          }
  in  (schedule, fusionRenames)

emptyIteratorProfitability :: IteratorProfitability
emptyIteratorProfitability =
  IteratorProfitability
    { ipAccessDimHits = M.empty
    , ipUnitStrideLastHits = 0
    , ipReadTouchHits = 0
    , ipInvariantReadHits = 0
    }

mergeIteratorProfitability :: IteratorProfitability -> IteratorProfitability -> IteratorProfitability
mergeIteratorProfitability lhs rhs =
  IteratorProfitability
    { ipAccessDimHits = M.unionWith (+) (ipAccessDimHits lhs) (ipAccessDimHits rhs)
    , ipUnitStrideLastHits = ipUnitStrideLastHits lhs + ipUnitStrideLastHits rhs
    , ipReadTouchHits = ipReadTouchHits lhs + ipReadTouchHits rhs
    , ipInvariantReadHits = ipInvariantReadHits lhs + ipInvariantReadHits rhs
    }

collectScopProfitabilityFacts2 :: Scop -> ProfitabilityFacts
collectScopProfitabilityFacts2 scop =
  foldl' collectStmt M.empty (scStatements scop)
  where
    collectStmt facts stmt =
      foldl' collectWriteAccess (foldl' collectReadAccess facts (psReads stmt)) (psWrites stmt)

    collectReadAccess facts access =
      collectInvariantReads access (collectReadTouches access (collectAccessTerms facts access))

    collectWriteAccess facts access =
      collectAccessTerms facts access

    collectAccessTerms facts access =
      let rank = length (paIndex access)
      in foldl' (collectIndexExpr rank) facts (zip [0 :: Int ..] (paIndex access))

    collectReadTouches access facts =
      foldl' collectReadExpr facts (paIndex access)

    collectReadExpr facts expr =
      foldl' collectReadTerm facts (M.toList (aeTerms expr))

    collectReadTerm facts (iter, _)
      | iter `elem` scIterators scop =
          M.insertWith
            mergeIteratorProfitability
            iter
            emptyIteratorProfitability { ipReadTouchHits = 1 }
            facts
      | otherwise =
          facts

    collectInvariantReads access facts =
      foldl'
        (\acc iter ->
          M.insertWith
            mergeIteratorProfitability
            iter
            emptyIteratorProfitability { ipInvariantReadHits = 1 }
            acc
        )
        facts
        invariantIters
      where
        accessIters =
          S.fromList
            [ iter
            | expr <- paIndex access
            , (iter, _) <- M.toList (aeTerms expr)
            ]
        invariantIters =
          [ iter
          | iter <- scIterators scop
          , iter `S.notMember` accessIters
          ]

    collectIndexExpr rank facts (dimPos, expr) =
      foldl' (collectTerm rank dimPos) facts (M.toList (aeTerms expr))

    collectTerm rank dimPos facts (iter, coeff)
      | iter `elem` scIterators scop =
          let contribution =
                IteratorProfitability
                  { ipAccessDimHits = M.singleton dimPos 1
                  , ipUnitStrideLastHits =
                      if dimPos == rank - 1 && abs coeff == 1 then 1 else 0
                  , ipReadTouchHits = 0
                  , ipInvariantReadHits = 0
                  }
          in M.insertWith mergeIteratorProfitability iter contribution facts
      | otherwise =
          facts

synthesizeAffineScheduleTree2
  :: ProfitabilityFacts
  -> Int
  -> [PolyhedralDependenceRelation]
  -> AffineScheduleTree
  -> FusionM AffineScheduleTree
synthesizeAffineScheduleTree2 profitability bandDepth relations sched = case sched of
  AffineScheduleSequence xs -> do
    synthesized <- mapM (synthesizeAffineScheduleTree2 profitability bandDepth relations) xs
    fused <- greedyFuse2M bandDepth relations synthesized
    pure (AffineScheduleSequence fused)
  AffineScheduleStmtRef stmtId ->
    pure (AffineScheduleStmtRef stmtId)
  AffineScheduleLoopBand band -> do
    let dims' =
          applyPermutation
            (chooseBandPermutation2 profitability bandDepth (albDims band) relations)
            (albDims band)
    body' <- synthesizeAffineScheduleTree2 profitability (bandDepth + 1) relations (albBody band)
    let body'' = case albRole band of
          LoopIterate ->
            case body' of
              AffineScheduleLoopBand inner
                | albRole inner == LoopMap ->
                    applyCrossBandSkewToInner bandDepth (map sdIter dims') inner relations
              AffineScheduleSequence xs ->
                applyCrossBandSkewToSeq bandDepth (map sdIter dims') xs relations
              _ -> body'
          _ -> body'
    pure $ AffineScheduleLoopBand band
      { albDims = dims'
      , albSkew = []
      , albBody = body''
      }

chooseBandPermutation2
  :: ProfitabilityFacts
  -> Int
  -> [ScheduleDim]
  -> [PolyhedralDependenceRelation]
  -> [Int]
chooseBandPermutation2 _ _ dims _
  | length dims <= 1 = [0 .. length dims - 1]
chooseBandPermutation2 profitability bandDepth dims relations =
  fromMaybe identityPermutation (listToMaybe orderedPermutations)
  where
    identityPermutation = [0 .. length dims - 1]
    bandIters = map sdIter dims
    allPermutations = permutations identityPermutation
    legalPermutations =
      filter
        (\perm -> perm == identityPermutation || all (relationAllowsPermutation2 bandDepth bandIters perm) relations)
        allPermutations
    orderedPermutations = sortOn (permutationScore2 profitability bandDepth dims relations) legalPermutations

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
  :: ProfitabilityFacts
  -> Int
  -> [ScheduleDim]
  -> [PolyhedralDependenceRelation]
  -> [Int]
  -> [(Int, Int, Int, Int, Int, Int, Int, Int, Int)]
permutationScore2 profitability bandDepth dims relations permutation =
  map scoreDim (zip [0 :: Int ..] permutedDims)
  where
    permutedDims = applyPermutation permutation dims
    innermostPos = length dims - 1
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
      , accessPositionPenalty position dim
      , if position == innermostPos then negate (unitStrideLastHits dim) else 0
      , reuseInnerPenalty position dim
      , parallelOuterReward position dim
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
    accessPositionPenalty position dim =
      let targetHits =
            ipAccessDimHits (M.findWithDefault emptyIteratorProfitability (sdIter dim) profitability)
      in sum
           [ hits * abs (min accessPos innermostPos - position)
           | (accessPos, hits) <- M.toList targetHits
           ]
    unitStrideLastHits dim =
      ipUnitStrideLastHits (M.findWithDefault emptyIteratorProfitability (sdIter dim) profitability)
    reuseInnerPenalty position dim =
      iteratorReuseSignal2 (iteratorProfitabilityFor2 profitability (sdIter dim)) * (innermostPos - position)
    parallelOuterReward position dim =
      negate (parallelFreedom dim * outerParallelWeight position * constantTripCountScore (sdBound dim))
    parallelFreedom dim
      | null scoringRelations = 1
      | otherwise = independentCount dim
    outerParallelWeight position = innermostPos - position + 1

iteratorProfitabilityFor2 :: ProfitabilityFacts -> CVar -> IteratorProfitability
iteratorProfitabilityFor2 profitability iter =
  M.findWithDefault emptyIteratorProfitability iter profitability

iteratorReuseSignal2 :: IteratorProfitability -> Int
iteratorReuseSignal2 info =
  ipInvariantReadHits info + max 0 (ipReadTouchHits info - 1)

constantTripCountScore :: IndexExpr -> Int
constantTripCountScore expr = case simplifyIndexExpr expr of
  IConst n ->
    fromInteger (max 1 (min 1024 n))
  _ ->
    1

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

-- ---------------------------------------------------------------------------
-- Fusion helpers
-- ---------------------------------------------------------------------------

bandsStructurallyCompatible2 :: AffineLoopBand -> AffineLoopBand -> Bool
bandsStructurallyCompatible2 b1 b2 =
  length (albDims b1) == length (albDims b2)
    && albRole b1 == albRole b2
    && map sdBound (albDims b1) == map sdBound (albDims b2)
    && albExec b1 == albExec b2
    && albReduction b1 == albReduction b2

-- | Rename variable references in an affine expression using a substitution map.
renameAffineExpr :: Map CVar CVar -> AffineExpr -> AffineExpr
renameAffineExpr renaming ae =
  ae { aeTerms = M.mapKeys (\v -> M.findWithDefault v v renaming) (aeTerms ae) }

-- | For a cross-sibling dependence whose band carry is unknown due to different
-- iterator names, apply the iter renaming (B2 iters → B1 iters) to the target
-- access index and re-analyze.  Returns True (safe) iff the effective distance
-- is ≤ 0 (reads from the past or same iteration in the fused loop).
crossSiblingDepSafe2 :: Map CVar CVar -> PolyhedralDependenceRelation -> Bool
crossSiblingDepSafe2 renaming rel =
  let renamedTgt = map (renameAffineExpr renaming) (pdrTgtIndex rel)
      allIters = M.keys (M.unions (map aeTerms (pdrSrcIndex rel) ++ map aeTerms renamedTgt))
      analysis = analyzeAccessDistance2 allIters (pdrSrcIndex rel) renamedTgt
  in not (any positiveCarry (adaCarryInfo analysis))
  where
    positiveCarry info = case pciStatus info of
      PolyCarryDistance d -> d > 0
      PolyCarryUnknown -> True
      PolyCarryIndependent -> False

-- | True when this dependence does not block fusion of the two adjacent bands
-- at the given depth.  @renaming@ maps B2 iterator names to B1 iterator names
-- and is used when the standard band-carry analysis cannot determine the carry
-- (because the two bands use different iterator variable names).
fusionLegalAtDepth2
  :: Int
  -> Set StmtId
  -> Set StmtId
  -> Map CVar CVar
  -> PolyhedralDependenceRelation
  -> Bool
fusionLegalAtDepth2 bandDepth stmtsA stmtsB renaming rel
  | pdrClassification rel == PolyDepClassReductionLike = True
  | not crossBands = True
  | otherwise = case currentBandCarryForDepth2 bandDepth rel of
      Nothing -> crossSiblingDepSafe2 renaming rel
      Just bc -> pbcStatus bc /= PolyBandBackward && pbcStatus bc /= PolyBandUnknown
  where
    srcA = pdrSourceStmt rel `S.member` stmtsA
    tgtB = pdrTargetStmt rel `S.member` stmtsB
    srcB = pdrSourceStmt rel `S.member` stmtsB
    tgtA = pdrTargetStmt rel `S.member` stmtsA
    crossBands = (srcA && tgtB) || (srcB && tgtA)

-- | Pure check: are two bands compatible for fusion?  Returns the fused tree
-- (using b1's iterator names) without accumulating any renaming.  Exported
-- for unit tests.
tryFuseBands2
  :: Int
  -> [PolyhedralDependenceRelation]
  -> AffineScheduleTree
  -> AffineScheduleTree
  -> Maybe AffineScheduleTree
tryFuseBands2 bandDepth relations t1 t2 =
  fst <$> tryFuseBands2Internal bandDepth relations t1 t2

tryFuseBands2Internal
  :: Int
  -> [PolyhedralDependenceRelation]
  -> AffineScheduleTree
  -> AffineScheduleTree
  -> Maybe (AffineScheduleTree, Map CVar CVar)
tryFuseBands2Internal bandDepth relations (AffineScheduleLoopBand b1) (AffineScheduleLoopBand b2)
  | bandsStructurallyCompatible2 b1 b2
  , all (fusionLegalAtDepth2 bandDepth stmts1 stmts2 renaming) relations =
      Just
        ( AffineScheduleLoopBand b1
            { albBody =
                AffineScheduleSequence
                  [ albBody b1
                  , albBody b2
                  ]
            }
        , renaming
        )
  where
    renaming = M.fromList (zip (map sdIter (albDims b2)) (map sdIter (albDims b1)))
    stmts1 = S.fromList (affineScheduleStmtOrder2 (AffineScheduleLoopBand b1))
    stmts2 = S.fromList (affineScheduleStmtOrder2 (AffineScheduleLoopBand b2))
tryFuseBands2Internal _ _ _ _ = Nothing

type FusionM = State (Map CVar CVar)

greedyFuse2M
  :: Int
  -> [PolyhedralDependenceRelation]
  -> [AffineScheduleTree]
  -> FusionM [AffineScheduleTree]
greedyFuse2M _ _ [] = pure []
greedyFuse2M _ _ [x] = pure [x]
greedyFuse2M bandDepth relations (x : y : rest) =
  case tryFuseBands2Internal bandDepth relations x y of
    Just (fused, renaming) -> do
      modify (M.union renaming)
      greedyFuse2M bandDepth relations (fused : rest)
    Nothing -> (x :) <$> greedyFuse2M bandDepth relations (y : rest)

-- ---------------------------------------------------------------------------
-- Skewing helpers
-- ---------------------------------------------------------------------------

suggestBandSkew2
  :: Int
  -> [ScheduleDim]
  -> [PolyhedralDependenceRelation]
  -> Maybe SkewSpec
suggestBandSkew2 bandDepth dims relations
  | length dims < 2 = Nothing
  | otherwise = listToMaybe
      [ SkewSpec
          { skewTarget = sdIter innerDim
          , skewSource = sdIter outerDim
          , skewCoeff = coeff
          }
      | (outerDim, innerDim) <- zip dims (drop 1 dims)
      , let outerIter = sdIter outerDim
            innerIter = sdIter innerDim
      , coeff <- maybeToList (skewCoeffForBand2 bandDepth outerIter innerIter relations)
      ]

skewCoeffForBand2
  :: Int
  -> CVar
  -> CVar
  -> [PolyhedralDependenceRelation]
  -> Maybe Integer
skewCoeffForBand2 bandDepth outerIter innerIter relations =
  case
    [ coeff
    | rel <- relations
    , Just innerDist <- [currentBandDistanceForIter2 bandDepth rel innerIter]
    , Just outerDist <- [currentBandDistanceForIter2 bandDepth rel outerIter]
    , innerDist < 0
    , outerDist > 0
    , let coeff = ((-innerDist) + outerDist - 1) `div` outerDist
    ]
  of
    [] -> Nothing
    coeffs -> Just (maximum coeffs)

-- | Suggest cross-band skews between an outer band's iterators and all inner
-- band iterators (e.g., wavefront skew of @i@ and @j@ by @iter_t@).  Each
-- skew target is an inner iterator; the source is an outer iterator.  This
-- differs from 'suggestBandSkew2' which operates on adjacents dims /within/
-- a single band.
suggestCrossBandSkew2
  :: Int              -- ^ outer band depth
  -> [CVar]           -- ^ outer band iterators
  -> [CVar]           -- ^ inner band iterators
  -> [PolyhedralDependenceRelation]
  -> [SkewSpec]
suggestCrossBandSkew2 outerDepth outerIters innerIters relations
  | null outerIters || null innerIters = []
  | otherwise =
      maybeToList $
        listToMaybe
          [ SkewSpec
              { skewTarget = innerIter
              , skewSource = outerIter
              , skewCoeff  = coeff
              }
          | innerIter <- innerIters
          , outerIter <- outerIters
          , coeff <- maybeToList (crossSkewCoeff2 outerDepth outerIter innerIter relations)
          ]

-- | Compute the required skew coefficient for a cross-band dependence where
-- the outer iterator has positive distance and the inner iterator has
-- negative distance.  Returns @ceil(-innerDist \/ outerDist)@.
crossSkewCoeff2
  :: Int
  -> CVar
  -> CVar
  -> [PolyhedralDependenceRelation]
  -> Maybe Integer
crossSkewCoeff2 outerDepth outerIter innerIter relations =
  case [ coeff
       | rel <- relations
       , Just innerDist <- [currentBandDistanceForIter2 (outerDepth + 1) rel innerIter]
       , Just outerDist <- [currentBandDistanceForIter2 outerDepth rel outerIter]
       , innerDist < 0
       , outerDist > 0
       , let coeff = ((-innerDist) + outerDist - 1) `div` outerDist
       ] of
    [] -> Nothing
    coeffs -> Just (maximum coeffs)

strictCrossSkewCoeff2
  :: Int
  -> CVar
  -> CVar
  -> [PolyhedralDependenceRelation]
  -> Maybe Integer
strictCrossSkewCoeff2 outerDepth outerIter innerIter relations =
  case [ coeff
       | rel <- relations
       , Just innerDist <- [currentBandDistanceForIter2 (outerDepth + 1) rel innerIter]
       , Just outerDist <- [currentBandDistanceForIter2 outerDepth rel outerIter]
       , innerDist < 0
       , outerDist > 0
       , let coeff = ((-innerDist) `div` outerDist) + 1
       ] of
    [] -> Nothing
    coeffs -> Just (maximum coeffs)

loopBandOriginsFromSkews2 :: [SkewSpec] -> [CVar] -> [IndexExpr]
loopBandOriginsFromSkews2 skews bandIters =
  let iterToOrigin = M.fromList
        [ (skewTarget s, IMul (IConst (skewCoeff s)) (IVar (skewSource s)))
        | s <- skews
        ]
      origins = [M.findWithDefault (IConst 0) iter iterToOrigin | iter <- bandIters]
  in if all (== IConst 0) origins then [] else origins

-- | Compute the per-dimension origin list for a set of cross-band skews.
-- Each skewed dimension gets @coeff * sourceIter@; unskewed dims get @IConst 0@.
crossBandOrigins :: [SkewSpec] -> AffineLoopBand -> [IndexExpr]
crossBandOrigins skews inner =
  let iterToOrigin = M.fromList
        [ (skewTarget s, IMul (IConst (skewCoeff s)) (IVar (skewSource s)))
        | s <- skews
        ]
  in [M.findWithDefault (IConst 0) dimIter iterToOrigin | ScheduleDim dimIter _ <- albDims inner]

-- | Apply cross-band skew to all eligible dimensions of an inner 'LoopMap'
-- band.  Each inner dimension with a backward spatial dep gets an origin of
-- @coeff * outerIter@ and a SkewSpec is recorded.
applyCrossBandSkewToInner
  :: Int              -- ^ outer band depth
  -> [CVar]           -- ^ outer band iterators
  -> AffineLoopBand   -- ^ the inner LoopMap band
  -> [PolyhedralDependenceRelation]
  -> AffineScheduleTree
applyCrossBandSkewToInner outerDepth outerIters inner relations =
  let skews = suggestCrossBandSkew2 outerDepth outerIters (map sdIter (albDims inner)) relations
  in case skews of
    [] -> AffineScheduleLoopBand inner
    _ ->
      let inner' = inner { albSkew = skews ++ albSkew inner, albOrigins = crossBandOrigins skews inner }
      in AffineScheduleLoopBand inner'

-- | Walk a sequence of 'AffineScheduleTree' children and apply cross-band
-- skew to the first child whose role is 'LoopMap'.
applyCrossBandSkewToSeq
  :: Int
  -> [CVar]
  -> [AffineScheduleTree]
  -> [PolyhedralDependenceRelation]
  -> AffineScheduleTree
applyCrossBandSkewToSeq outerDepth outerIters xs relations =
  case span (not . isLoopMapBand) xs of
    (before, AffineScheduleLoopBand inner : after) ->
      let skews = suggestCrossBandSkew2 outerDepth outerIters (map sdIter (albDims inner)) relations
          inner' = case skews of
            [] -> inner
            _  -> inner { albSkew = skews ++ albSkew inner, albOrigins = crossBandOrigins skews inner }
      in AffineScheduleSequence (before ++ AffineScheduleLoopBand inner' : after)
    _ -> AffineScheduleSequence xs
  where
    isLoopMapBand (AffineScheduleLoopBand b) = albRole b == LoopMap
    isLoopMapBand _ = False

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
          Left RejectNoMemoryAccess -> go path body
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
  let baseDeps =
        [ mkRelation srcStmt tgtStmt srcAccess tgtAccess depKind accessAnalysis
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
  in  case outerIterateInfo (scSchedule scop) of
        Just (iterT, _arrNext) ->
          augmentWithTemporalDeps iterT baseDeps
        Nothing ->
          baseDeps
  where
    stmtMap = M.fromList [(psPath stmt, stmt) | stmt <- scStatements scop]
    orderedStmts =
      [ (pos, stmt)
       | (pos, stmtId) <- zip [0 :: Int ..] (scheduleStmtOrder (scSchedule scop))
       , Just stmt <- [M.lookup stmtId stmtMap]
       ]
    stmtContexts = stmtLoopContexts2 (scSchedule scop)
    mkRelation srcStmt tgtStmt srcAccess tgtAccess depKind accessAnalysis =
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
            , pdrSrcIndex = paIndex srcAccess
            , pdrTgtIndex = paIndex tgtAccess
            }

-- | Detect if the outermost schedule band is a LoopIterate and return its
-- iterator variable.  Returns @Just (iterT, "")@ when found; the second
-- component is unused but kept for symmetry with 'detectTemporalAlias'.
outerIterateInfo :: ScheduleTree -> Maybe (CVar, CVar)
outerIterateInfo sched = case sched of
  ScheduleLoopBand band
    | lbRole band == LoopIterate
    , (iterT : _) <- lbIters band ->
        Just (iterT, "")
  _ -> Nothing

-- | Detect temporal alias at the end of a @LoopIterate@ body.
-- Supports both the simple tail swap @cur = next@ and the hoisted ping-pong
-- suffix @cur = next; next = tmp; tmp = cur@.
detectTemporalAlias :: [Stmt] -> Maybe (CVar, CVar)
detectTemporalAlias stmts = case reverse stmts of
  ( SAssign _tmpOut (RAtom (AVar cur2))
    : SAssign next1 (RAtom (AVar _tmpIn))
    : SAssign cur1 (RAtom (AVar next))
    : _
    )
      | cur1 == cur2, next1 == next ->
          Just (cur1, next1)
  (SAssign cur (RAtom (AVar next)) : _) -> Just (cur, next)
  _ -> Nothing

-- | Augment dependence relations from a LoopIterate scop with the temporal
-- dimension.  For every dep that is carried purely within spatial (non-iterate)
-- bands, prepend an outer LoopIterate band carry of distance +1.  This makes
-- the WAR deps from stencil reads visible to @suggestBandSkew2@.
--
-- In a @LoopIterate@ body the polyhedral analysis treats reads and writes as
-- occurring in the *same* temporal iteration (iter_t distance = 0).  But the
-- actual semantics is that the array read at the start of body[iter_t] was
-- written at body[iter_t-1].  For a WAR dep (read-before-write, same array)
-- with zero iter_t distance and at least one negative spatial distance, we
-- bump the iter_t component from 0 to +1 to expose the true cross-iteration
-- nature of the dependence so that @suggestBandSkew2@ can fire.
augmentWithTemporalDeps
  :: CVar                          -- ^ iter_t: the LoopIterate iterator
  -> [PolyhedralDependenceRelation]
  -> [PolyhedralDependenceRelation]
augmentWithTemporalDeps iterT = map augmentOne
  where
    augmentOne rel =
      if needsTemporalAugment rel
        then bumpIterTDistance rel
        else rel

    -- A dep needs augmentation when:
    --   1. It is a WAR dep (read before write in static order, same array)
    --   2. The iter_t carry distance is currently 0 (both read and write are
    --      modelled in the same temporal iteration)
    --   3. There is at least one negative spatial distance (backward spatial dep)
    needsTemporalAugment rel =
      pdrKind rel == PolyDepWAR
      && case iterTCarryDistance rel of
           Just 0 -> True
           _ -> False
      && case pdrDistance rel of
           Just ds -> any (< 0) ds
           Nothing -> False

    -- Extract the effective carry distance for iter_t from the flat carry info list.
    -- PolyCarryIndependent means distance = 0 in this context.
    iterTCarryDistance rel =
      case [pciStatus ci | ci <- pdrCarryInfo rel, pciIter ci == iterT] of
        [PolyCarryDistance d] -> Just d
        [PolyCarryIndependent] -> Just 0
        _ -> Nothing

    -- Bump iter_t carry distance from 0 to +1 throughout the relation.
    bumpIterTDistance rel =
      let newCarryInfo = map bumpCI (pdrCarryInfo rel)
          newBandCarry = map bumpBC (pdrBandCarry rel)
          -- The iter_t dimension in pdrDistance corresponds to its position
          -- in pdrCarryInfo; bump just that slot from 0 to 1.
          iters = map pciIter (pdrCarryInfo rel)
          iterTIdx = length (takeWhile (/= iterT) iters)
          newDistance = case pdrDistance rel of
            Nothing -> Nothing
            Just ds ->
              Just [if idx == iterTIdx then 1 else d | (idx, d) <- zip [0 ..] ds]
      in rel
           { pdrCarryInfo = newCarryInfo
           , pdrBandCarry = newBandCarry
           , pdrDistance = newDistance
           , pdrIsLoopCarried = True
           , pdrDirection = PolyDepForward
           , pdrIsBlocking = False
           }

    bumpCI ci
      | pciIter ci == iterT
      , pciStatus ci == PolyCarryDistance 0 || pciStatus ci == PolyCarryIndependent =
          ci { pciStatus = PolyCarryDistance 1 }
      | otherwise = ci

    bumpBC bc =
      bc
        { pbcCarryInfo = map bumpCI (pbcCarryInfo bc)
        , pbcStatus = recomputeBandStatus (map bumpCI (pbcCarryInfo bc))
        }

    recomputeBandStatus carryInfo =
      case traverse carryDistance2 carryInfo of
        Just distances
          | all (== 0) distances -> PolyBandIndependent
          | otherwise ->
              case classifyDistance distances of
                PolyDepForward -> PolyBandForward
                PolyDepBackward -> PolyBandBackward
                PolyDepUnknown -> PolyBandUnknown
        Nothing -> PolyBandUnknown

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

tileScheduleTree2 :: ProfitabilityFacts -> [PolyhedralDependenceRelation] -> Int -> ScheduleTree -> TileM ScheduleTree
tileScheduleTree2 profitability relations depth sched = case sched of
  ScheduleSequence xs ->
    ScheduleSequence <$> mapM (tileScheduleTree2 profitability relations depth) xs
  ScheduleStmtRef stmtId ->
    pure (ScheduleStmtRef stmtId)
  ScheduleStripMine band plans -> do
    body' <- tileScheduleTree2 profitability relations (depth + 1) (lbBody band)
    pure (ScheduleStripMine band { lbBody = body' } plans)
  ScheduleLoopBand band -> do
    body' <- tileScheduleTree2 profitability relations (depth + 1) (lbBody band)
    let band' = band { lbBody = body' }
    if not (shouldTileBand depth band')
      then pure (ScheduleLoopBand band')
      else do
        plans <- buildStripMinePlans defaultTileConfig profitability relations depth band'
        if any isStripTiled plans
          then pure (ScheduleStripMine band' plans)
          else pure (ScheduleLoopBand band')

shouldTileBand :: Int -> LoopBand -> Bool
shouldTileBand depth band =
  lbExec band == Serial
    && lbRole band /= LoopReductionWrapper
    && lbRole band /= LoopFold
    && lbRole band /= LoopIterate
    && loopRoleSupportsTiling2 band
    && boundsAreIteratorIndependent band
    && all (supportsAtomBound . simplifyIndexExpr) (lbBounds band)
    && (depth > 0 || hasNestedBand (lbBody band) || length (lbIters band) > 1)

loopRoleSupportsTiling2 :: LoopBand -> Bool
loopRoleSupportsTiling2 band = case lbRole band of
  LoopMap ->
    hasNestedBand (lbBody band) || length (lbIters band) > 1
  _ ->
    True

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

buildStripMinePlans
  :: TileConfig
  -> ProfitabilityFacts
  -> [PolyhedralDependenceRelation]
  -> Int
  -> LoopBand
  -> TileM [StripMinePlan]
buildStripMinePlans cfg profitability relations depth band
  | shouldSkipBandTilingForProfitability2 profitability relations depth band =
      pure [StripKeep iter bound | (iter, bound) <- zip (lbIters band) (lbBounds band)]
  | otherwise =
      mapM buildOne (zip3 [0 :: Int ..] (lbIters band) (lbBounds band))
  where
    innermostPos = length (lbIters band) - 1

    buildOne (position, iter, bound)
      | shouldKeepDimUntiledForProfitability2 profitability band innermostPos position iter =
          pure (StripKeep iter bound)
      | otherwise =
          buildTiledPlan iter bound

    buildTiledPlan iter bound =
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

shouldSkipBandTilingForProfitability2
  :: ProfitabilityFacts
  -> [PolyhedralDependenceRelation]
  -> Int
  -> LoopBand
  -> Bool
shouldSkipBandTilingForProfitability2 profitability relations depth band =
  bandRoleUsesReuseHeuristics2 band
    && bandFullyIndependent2 depth relations
    && all ((== 0) . iteratorReuseSignal2 . iteratorProfitabilityFor2 profitability) (lbIters band)

shouldKeepDimUntiledForProfitability2
  :: ProfitabilityFacts
  -> LoopBand
  -> Int
  -> Int
  -> CVar
  -> Bool
shouldKeepDimUntiledForProfitability2 profitability band innermostPos position iter =
  bandRoleUsesReuseHeuristics2 band
    && position == innermostPos
    && ipUnitStrideLastHits info > 0
    && iteratorReuseSignal2 info == 0
  where
    info = iteratorProfitabilityFor2 profitability iter

bandRoleUsesReuseHeuristics2 :: LoopBand -> Bool
bandRoleUsesReuseHeuristics2 band =
  lbRole band == LoopPlain || lbRole band == LoopMap

bandFullyIndependent2 :: Int -> [PolyhedralDependenceRelation] -> Bool
bandFullyIndependent2 depth relations =
  all relationIndependent relevantRelations
  where
    relevantRelations =
      [ relation
      | relation <- relations
      , prefixBandStatus2 depth relation /= PrefixUnknown
      , hasCurrentBandCarry2 depth relation
      ]
    relationIndependent relation = case currentBandCarryForDepth2 depth relation of
      Nothing -> True
      Just bandCarry -> pbcStatus bandCarry == PolyBandIndependent

chooseTileSize :: TileConfig -> LoopBand -> IndexExpr -> Maybe Integer
chooseTileSize cfg band bound
  | usefulBound tileSize simplifiedBound
      && profitableTailShape tileSize simplifiedBound = Just tileSize
  | otherwise = Nothing
  where
    simplifiedBound = simplifyIndexExpr bound
    tileSize = case lbRole band of
      LoopReduction -> tcReductionTile cfg
      _ -> tcDefaultTile cfg

    usefulBound n expr = case expr of
      IConst k -> k > n
      _ -> True

    -- Avoid strip-mining constant map-like bands when it would only create a
    -- tiny cleanup tile. Larger tails, multiple full tiles, and reduction
    -- bands still justify the extra loop/setup overhead.
    profitableTailShape n expr = case expr of
      IConst k
        | lbRole band == LoopReduction -> True
        | otherwise ->
            let (fullTiles, remainder) = k `divMod` n
            in  fullTiles >= 2 || remainder * 2 >= n
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

-- | True for execution policies that polyhedral extraction can handle.
-- Serial and Parallel are both fine: Parallel means "no loop-carried deps,"
-- which is exactly what polyhedral dep analysis will verify.  Vector loops
-- are not supported because they operate on SIMD sub-words.
isPolyhedralCompatibleExec :: ExecPolicy -> Bool
isPolyhedralCompatibleExec Serial = True
isPolyhedralCompatibleExec (Parallel {}) = True
isPolyhedralCompatibleExec _ = False

-- | Top-level entry point for loop extraction (no inherited alias map).
extractLoop
  :: [Int]
  -> [AffineConstraint]
  -> AffineEnv
  -> LoopSpec
  -> [Stmt]
  -> Either ScopRejectReason ([PolyhedralStmt], ScheduleTree)
extractLoop = extractLoopWithAlias M.empty

-- | Core loop extraction.  The @inheritedAlias@ is the temporal alias map
-- propagated down from an enclosing @LoopIterate@.  When this loop is itself
-- a @LoopIterate@, a new alias is detected from the body and used instead.
extractLoopWithAlias
  :: Map CVar CVar
  -> [Int]
  -> [AffineConstraint]
  -> AffineEnv
  -> LoopSpec
  -> [Stmt]
  -> Either ScopRejectReason ([PolyhedralStmt], ScheduleTree)
extractLoopWithAlias inheritedAlias loopPath domain env spec body
  | not (isPolyhedralCompatibleExec (lsExec spec)) = Left (RejectNonSerialExec (lsExec spec))
  | otherwise = do
      bounds <- case mapM supportedBoundExprFromIndexExpr2 (lsBounds spec) of
        Just ok -> Right ok
        Nothing -> case firstUnsupportedBound (lsBounds spec) of
          Just bad -> Left (RejectUnsupportedBound bad)
          Nothing -> Left (RejectUnsupportedStmt (SLoop spec body))
      let domain' = domain ++ loopDomainConstraints spec bounds
          -- For LoopIterate bodies, detect a new temporal alias; otherwise
          -- inherit the alias from the enclosing scope so that nested inner
          -- loops (e.g. LoopMap inside LoopIterate) still substitute reads.
          aliasMap = case lsRole spec of
            LoopIterate ->
              case detectTemporalAlias body of
                Just (arrCur, arrNext) -> M.singleton arrCur arrNext
                Nothing -> inheritedAlias
            _ -> inheritedAlias
      (stmts, bodySchedule, _) <- extractStmtListWithAlias aliasMap loopPath domain' env body
      pure
        ( stmts
        , ScheduleLoopBand
            LoopBand
              { lbIters = lsIters spec
              , lbBounds = bounds
              , lbOrigins = lsOrigins spec
              , lbExec = lsExec spec
              , lbReduction = lsRed spec
              , lbRole = lsRole spec
              , lbBody = bodySchedule
              , lbSkew = []
              }
        )
extractStmtListWithAlias
  :: Map CVar CVar
  -> [Int]
  -> [AffineConstraint]
  -> AffineEnv
  -> [Stmt]
  -> Either ScopRejectReason ([PolyhedralStmt], ScheduleTree, AffineEnv)
extractStmtListWithAlias aliasMap basePath domain env stmts = go env [] [] (zip [0 ..] stmts)
  where
    go envNow accStmts accSched [] =
      Right (reverse accStmts, ScheduleSequence (reverse accSched), envNow)
    go envNow accStmts accSched ((i, stmt) : rest) =
      case stmt of
        SAssign v rhs -> do
          readAccesses <- rhsReadAccessesWithAlias aliasMap envNow rhs
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
                  -- Also substitute array name on writes for consistency
                  arr' = M.findWithDefault arr arr aliasMap
                  polyStmt = PolyhedralStmt
                    { psPath = stmtPath
                    , psDomain = domain
                    , psReads = []
                    , psWrites = [PolyhedralAccess PolyWrite arr' affineIdx]
                    , psStmt = SArrayWrite (AVar arr) idx val
                    }
              in go envNow (polyStmt : accStmts) (ScheduleStmtRef stmtPath : accSched) rest
        SArrayWrite {} ->
          Left (RejectUnsupportedArrayRef stmt)
        SLoop innerSpec innerBody -> do
          let stmtPath = basePath ++ [i]
          -- Propagate the alias map into nested loops.
          (nestedStmts, nestedSchedule) <-
            extractLoopWithAlias aliasMap stmtPath domain envNow innerSpec innerBody
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
-- RArrayAlloc and RShapeSize are permitted so that the preamble of a
-- LoopIterate body (buffer allocation, shape extraction) does not block
-- scop extraction.
rhsIsSupportedScopPrelude :: RHS -> Bool
rhsIsSupportedScopPrelude rhs = case rhs of
  RCall _ [] -> True
  RArrayShape {} -> True
  RFlatToNd {} -> True
  RNdToFlat {} -> True
  RArrayAlloc {} -> True
  RShapeSize {} -> True
  _ -> False

-- | Like 'rhsReadAccesses' (= 'rhsReadAccessesWithAlias' with empty alias map)
-- but substitutes array names via the alias map.
-- This is used for LoopIterate bodies where @arr_cur@ is aliased to @arr_next@
-- so that dep analysis sees both reads and writes on the same array.
rhsReadAccessesWithAlias :: Map CVar CVar -> AffineEnv -> RHS -> Either ScopRejectReason [PolyhedralAccess]
rhsReadAccessesWithAlias aliasMap env rhs = case rhs of
  RArrayLoad (AVar arr) idx ->
    case affineIndexFromAtom env idx of
      Just affineIdx ->
        let arr' = M.findWithDefault arr arr aliasMap
        in Right [PolyhedralAccess PolyRead arr' affineIdx]
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
  RBinOp op a b ->
    affineSpecialValueFromBinOp env op a b <|> do
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
    idxExprs <- affineTupleLikeFromAtom env idx
    shapeExprs <- arrayShapeTupleFromAtom env shape (length idxExprs)
    case affineNdToFlat idxExprs shapeExprs of
      Just expr -> Just (AffineScalar expr)
      Nothing -> Just (AffineFlatIndex idxExprs)
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
    AffineFlatIndex {} -> Nothing
    AffineRowOffset {} -> Nothing

affineIndexFromAtom :: AffineEnv -> Atom -> Maybe [AffineExpr]
affineIndexFromAtom env atom = do
  value <- affineValueFromAtom env atom
  case value of
    AffineScalar expr -> Just [expr]
    AffineTuple exprs -> Just exprs
    AffineFlatIndex exprs -> Just exprs
    AffineRowOffset {} -> Nothing

affineTupleLikeFromAtom :: AffineEnv -> Atom -> Maybe [AffineExpr]
affineTupleLikeFromAtom env atom = do
  value <- affineValueFromAtom env atom
  case value of
    AffineTuple exprs -> Just exprs
    AffineFlatIndex exprs -> Just exprs
    _ -> Nothing

affineSpecialValueFromBinOp :: AffineEnv -> BinOp -> Atom -> Atom -> Maybe AffineValue
affineSpecialValueFromBinOp env op a b = case op of
  CMul ->
    affineRowOffsetFromMul env a b <|> affineRowOffsetFromMul env b a
  CAdd ->
    affineFlatIndexFromAdd env a b <|> affineFlatIndexFromAdd env b a
  _ ->
    Nothing

affineRowOffsetFromMul :: AffineEnv -> Atom -> Atom -> Maybe AffineValue
affineRowOffsetFromMul env rowAtom widthAtom = do
  rowExpr <- affineScalarFromAtom env rowAtom
  widthExpr <- affineScalarFromAtom env widthAtom
  if isJust (isConstAffine widthExpr)
    then Nothing
    else Just (AffineRowOffset rowExpr widthExpr)

affineFlatIndexFromAdd :: AffineEnv -> Atom -> Atom -> Maybe AffineValue
affineFlatIndexFromAdd env rowOffsetAtom colAtom = do
  rowOffset <- affineValueFromAtom env rowOffsetAtom
  colExpr <- affineScalarFromAtom env colAtom
  case rowOffset of
    AffineRowOffset rowExpr _ ->
      Just (AffineFlatIndex [rowExpr, colExpr])
    _ ->
      Nothing

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
    Just (AffineFlatIndex _) -> Nothing
    Just (AffineRowOffset _ _) -> Nothing
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

-- | For a loop band with skew specs, rename the target iterators to skewed
-- names and produce prelude assignments that recover the original iterator
-- value: @orig = skewed - coeff * source@.
applyBandSkew2 :: [CVar] -> [SkewSpec] -> ([CVar], [Stmt])
applyBandSkew2 iters skews =
  let skewMap = M.fromList [(skewTarget s, s) | s <- skews]
      renameIter iter = case M.lookup iter skewMap of
        Just _ -> iter <> "__s"
        Nothing -> iter
      iters' = map renameIter iters
      preludes = concatMap mkPrelude skews
      mkPrelude s =
        let mulVar = skewTarget s <> "__s__mul"
        in  [ SAssign mulVar (RBinOp CMul (AInt (skewCoeff s)) (AVar (skewSource s)))
             , SAssign (skewTarget s) (RBinOp CSub (AVar (skewTarget s <> "__s")) (AVar mulVar))
             ]
  in  (iters', preludes)

-- | Preserve skewed iterator semantics when a band is reified through
-- strip-mining. Kept dimensions inherit their original origins directly on the
-- local loop, while tiled skewed dimensions recover the logical iterator from
-- a skewed position inside the local-loop setup.
applyBandSkewToStripMine
  :: LoopBand
  -> [StripMinePlan]
  -> ([CVar], [IndexExpr], [Stmt], [Stmt])
applyBandSkewToStripMine band plans =
  let skews = lbSkew band
      skewMap = M.fromList [(skewTarget s, s) | s <- skews]
      originMap = M.fromList (zip (lbIters band) (lbOrigins band ++ repeat (IConst 0)))
      skewOriginVar s = skewTarget s <> "__skew_origin"
      skewPrelude =
        [ SAssign (skewOriginVar s) (RBinOp CMul (AInt (skewCoeff s)) (AVar (skewSource s)))
        | s <- skews
        ]
      localIterFor plan = case plan of
        StripKeep iter _ ->
          case M.lookup iter skewMap of
            Just _ -> iter <> "__s"
            Nothing -> iter
        StripTile td ->
          smdLocalIter td
      localOriginFor plan = case plan of
        StripKeep iter _ ->
          M.findWithDefault (IConst 0) iter originMap
        StripTile {} ->
          IConst 0
      localOrigins =
        let origins = map localOriginFor plans
        in if all (== IConst 0) origins then [] else origins
      innerSetup = concatMap setupPlan plans
      setupPlan plan = case plan of
        StripKeep iter _ ->
          case M.lookup iter skewMap of
            Just s ->
              [ SAssign iter (RBinOp CSub (AVar (iter <> "__s")) (AVar (skewOriginVar s)))
              ]
            Nothing ->
              []
        StripTile td ->
          case M.lookup (smdOrigIter td) skewMap of
            Just s ->
              let orig = smdOrigIter td
                  skewed = orig <> "__s"
              in
                [ assignOrigIter td
                , SAssign skewed (RBinOp CAdd (AVar orig) (AVar (skewOriginVar s)))
                , SAssign orig (RBinOp CSub (AVar skewed) (AVar (skewOriginVar s)))
                ]
            Nothing ->
              [assignOrigIter td]
  in  (map localIterFor plans, localOrigins, skewPrelude, innerSetup)

reifyScheduleTree :: Map StmtId Stmt -> ScheduleTree -> Maybe [Stmt]
reifyScheduleTree stmtMap sched = case sched of
  ScheduleSequence xs ->
    fmap concat (mapM (reifyScheduleTree stmtMap) xs)
  ScheduleStripMine band plans -> do
    body <- reifyScheduleTree stmtMap (lbBody band)
    let tiledDims = [td | StripTile td <- plans]
        tilePrelude = concatMap smdBoundPrelude tiledDims
        (localIters, localOrigins, skewPrelude, innerSetup) =
          applyBandSkewToStripMine band plans
        outerTileSpec = LoopSpec
          { lsIters = map smdTileIter tiledDims
          , lsBounds = map (\td -> tileCountExpr (smdOrigBound td) (smdTileSize td)) tiledDims
          , lsOrigins = []
          , lsExec = Serial
          , lsRed = Nothing
          , lsRole = tiledLoopRole (lbRole band)
          }
        innerLocalSpec = LoopSpec
          { lsIters = localIters
          , lsBounds = map localBound plans
          , lsOrigins = localOrigins
          , lsExec = lbExec band
          , lsRed = lbReduction band
          , lsRole = lbRole band
          }
        outerSetup = concatMap setupStripDim tiledDims
        innerLocalLoop = SLoop innerLocalSpec (innerSetup ++ body)
    pure (skewPrelude ++ tilePrelude ++ [SLoop outerTileSpec (outerSetup ++ [innerLocalLoop])])
    where
      localBound plan = case plan of
        StripKeep _ bound -> bound
        StripTile td -> IVar (smdTileLen td)
  ScheduleLoopBand band -> do
    body <- reifyScheduleTree stmtMap (lbBody band)
    let (iters', skewPreludes) = applyBandSkew2 (lbIters band) (lbSkew band)
    pure
      [ SLoop
          LoopSpec
            { lsIters = iters'
            , lsBounds = lbBounds band
            , lsOrigins = lbOrigins band
            , lsExec = lbExec band
            , lsRed = lbReduction band
            , lsRole = lbRole band
            }
          (skewPreludes ++ body)
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
  LoopIterate -> LoopIterate

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
      let vectorFacts =
            deriveVectorAccessFacts2
              body'
              ( scheduledVectorAccessFacts2
                  scheduled
                  (M.union (ssVectorAccessFactOverrides scheduled) (procVectorAccessFacts proc))
              )
      pure
        proc
          { procBody = body'
          , procTypeEnv =
              M.union (ssTypeOverrides scheduled) (procTypeEnv proc)
          , procArrayFacts =
              M.union (ssArrayFactOverrides scheduled) (procArrayFacts proc)
          , procVectorAccessFacts = vectorFacts
          }

buildWavefrontScop2 :: ScheduledScop -> Maybe ScheduledScop
buildWavefrontScop2 scheduled = do
  let strengthened = strengthenWavefrontScheduled2 scheduled
      scop = ssOriginal strengthened
  kernel <- matchWavefrontKernel2 strengthened
  let replacement =
        evalState (buildWavefrontBody2 kernel) (collectScopNames scop, 0)
  pure strengthened { ssReplacement = Just replacement }

strengthenWavefrontScheduled2 :: ScheduledScop -> ScheduledScop
strengthenWavefrontScheduled2 scheduled =
  scheduled { ssSchedule = strengthenWavefrontScheduleTree2 relations 0 (ssSchedule scheduled) }
  where
    relations = collectScopDependenceRelations2 (ssOriginal scheduled)

strengthenWavefrontScheduleTree2
  :: [PolyhedralDependenceRelation]
  -> Int
  -> ScheduleTree
  -> ScheduleTree
strengthenWavefrontScheduleTree2 relations depth sched = case sched of
  ScheduleSequence xs ->
    ScheduleSequence (map (strengthenWavefrontScheduleTree2 relations depth) xs)
  ScheduleStmtRef {} ->
    sched
  ScheduleStripMine band plans ->
    ScheduleStripMine
      band { lbBody = strengthenWavefrontScheduleTree2 relations (depth + 1) (lbBody band) }
      plans
  ScheduleLoopBand band ->
    let body' = strengthenWavefrontScheduleTree2 relations (depth + 1) (lbBody band)
        band' = band { lbBody = body' }
    in case lbRole band of
         LoopIterate ->
           ScheduleLoopBand band' { lbBody = strengthenWavefrontInnerSchedule2 relations depth (lbIters band') body' }
         _ ->
           ScheduleLoopBand band'

strengthenWavefrontInnerSchedule2
  :: [PolyhedralDependenceRelation]
  -> Int
  -> [CVar]
  -> ScheduleTree
  -> ScheduleTree
strengthenWavefrontInnerSchedule2 relations outerDepth outerIters sched = case sched of
  ScheduleLoopBand inner
    | lbRole inner == LoopMap ->
        let skews = strengthenWavefrontSkews2 relations outerDepth outerIters (lbSkew inner)
        in ScheduleLoopBand inner
             { lbSkew = skews
             , lbOrigins = loopBandOriginsFromSkews2 skews (lbIters inner)
             }
  ScheduleSequence xs ->
    case span (not . isWavefrontLoopMapTree2) xs of
      (before, target : after) ->
        ScheduleSequence (before ++ strengthenWavefrontInnerSchedule2 relations outerDepth outerIters target : after)
      _ ->
        sched
  _ ->
    sched

strengthenWavefrontSkews2
  :: [PolyhedralDependenceRelation]
  -> Int
  -> [CVar]
  -> [SkewSpec]
  -> [SkewSpec]
strengthenWavefrontSkews2 relations outerDepth outerIters skews =
  map strengthenOne skews
  where
    strengthenOne skew =
      let strictCoeff =
            maximum
              ( skewCoeff skew
              : [ coeff
                | outerIter <- outerIters
                , outerIter == skewSource skew
                , coeff <- maybeToList (strictCrossSkewCoeff2 outerDepth outerIter (skewTarget skew) relations)
                ]
              )
      in skew { skewCoeff = strictCoeff }

isWavefrontLoopMapTree2 :: ScheduleTree -> Bool
isWavefrontLoopMapTree2 sched = case sched of
  ScheduleLoopBand band -> lbRole band == LoopMap
  _ -> False

matchWavefrontKernel2 :: ScheduledScop -> Maybe WavefrontKernel
matchWavefrontKernel2 scheduled = do
  [SLoop outerSpec outerBody] <- reifyScheduleTree stmtMap (ssSchedule scheduled)
  guard (lsRole outerSpec == LoopIterate)
  [iterT] <- pure (lsIters outerSpec)
  [outerBound] <- pure (map simplifyIndexExpr (lsBounds outerSpec))
  (prefix, innerStmt, suffix) <- splitFirstMatching2 isWavefrontInnerLoop2 outerBody
  SLoop innerSpec innerBody <- Just innerStmt
  coeff <- wavefrontSkewCoeffForLoop2 iterT innerSpec
  (stageSuffix, curVar, nextVar, preallocatedNext) <- matchWavefrontTail2 suffix
  (allocShape, prefixWithoutAlloc) <-
    case removeWavefrontAlloc2 nextVar prefix of
      Just matched ->
        Just matched
      Nothing
        | preallocatedNext ->
            Just (AVar "__wavefront_prealloc_shape", prefix)
      Nothing ->
        Nothing
  guard (all hoistableWavefrontStmt2 prefixWithoutAlloc)
  guard
    ( preallocatedNext
        || shapePreservingWavefrontPrelude2 curVar prefixWithoutAlloc allocShape
    )
  guard (not (usesVar2 iterT prefixWithoutAlloc))
  guard (not (usesVar2 nextVar prefixWithoutAlloc))
  guard (all (\iter -> not (usesVar2 iter stageSuffix)) (lsIters innerSpec))
  stageWidth <- chooseWavefrontStageWidth2 outerBound innerSpec
  let headIter = wavefrontHeadIter2 innerSpec
      stageExec =
        if wavefrontParallelLegal2 (collectScopDependenceRelations2 (ssOriginal scheduled)) iterT headIter coeff
          then Parallel
                 ParallelSpec
                    { psStrategy = ParallelGeneric
                    , psPolicy = Just "schedule(static)"
                   , psSimdLen = Nothing
                   }
          else Serial
  pure
    WavefrontKernel
      { wfkIterT = iterT
      , wfkIterBound = outerBound
      , wfkCurArray = curVar
       , wfkNextArray = nextVar
       , wfkAllocShape = allocShape
       , wfkPreallocatedNext = preallocatedNext
       , wfkInitTracker =
           if preallocatedNext
             then Just (curVar <> "__iter_init_track")
             else Nothing
       , wfkHoistedPrefix = prefixWithoutAlloc
       , wfkStageSuffix = stageSuffix
       , wfkInnerSpec = innerSpec
       , wfkInnerBody = innerBody
      , wfkSkewCoeff = coeff
      , wfkStageWidth = stageWidth
      , wfkStageExec = stageExec
      }
  where
    stmtMap =
      M.fromList
        [ (psPath stmt, psStmt stmt)
        | stmt <- scStatements (ssOriginal scheduled)
        ]

wavefrontParallelLegal2
  :: [PolyhedralDependenceRelation]
  -> CVar
  -> CVar
  -> Integer
  -> Bool
wavefrontParallelLegal2 relations iterT headIter coeff =
  all relationSafe relevantRelations
  where
    relevantRelations =
      [ rel
      | rel <- relations
      , pdrClassification rel /= PolyDepClassReductionLike
      , Just outerDist <- [findCarryDistanceByIter2 iterT (pdrCarryInfo rel)]
      , Just _ <- [findCarryDistanceByIter2 headIter (pdrCarryInfo rel)]
      , outerDist > 0
      ]
    relationSafe rel =
      case ( findCarryDistanceByIter2 iterT (pdrCarryInfo rel)
           , findCarryDistanceByIter2 headIter (pdrCarryInfo rel)
           ) of
        (Just outerDist, Just headDist) ->
          coeff * outerDist + headDist > 0
        _ ->
          False

findCarryDistanceByIter2 :: CVar -> [PolyhedralCarryInfo] -> Maybe Integer
findCarryDistanceByIter2 iter infos =
  find ((== iter) . pciIter) infos >>= carryDistance2

chooseWavefrontStageWidth2 :: IndexExpr -> LoopSpec -> Maybe Integer
chooseWavefrontStageWidth2 iterBound innerSpec = do
  let tripCap =
        case simplifyIndexExpr iterBound of
          IConst n -> max 1 (min defaultWavefrontBlockWidth n)
          _ -> defaultWavefrontBlockWidth
      footprintCap = wavefrontFootprintWidthCap2 innerSpec
      stageWidth = min tripCap footprintCap
  guard (stageWidth >= 2)
  pure stageWidth

wavefrontFootprintWidthCap2 :: LoopSpec -> Integer
wavefrontFootprintWidthCap2 innerSpec =
  case constantWavefrontFootprint2 innerSpec of
    Nothing ->
      defaultWavefrontBlockWidth
    Just footprint
      | footprint <= 0 ->
          1
      | otherwise ->
          let extraCap = defaultWavefrontExtraBufferBudget `div` footprint
          in if extraCap <= 0
               then 1
               else min defaultWavefrontBlockWidth (1 + extraCap)

constantWavefrontFootprint2 :: LoopSpec -> Maybe Integer
constantWavefrontFootprint2 innerSpec =
  case traverse constantIndexExpr2 (map simplifyIndexExpr (lsBounds innerSpec)) of
    Just bounds -> Just (product bounds)
    Nothing -> Nothing

constantIndexExpr2 :: IndexExpr -> Maybe Integer
constantIndexExpr2 expr = case simplifyIndexExpr expr of
  IConst n -> Just n
  _ -> Nothing

buildWavefrontBody2 :: WavefrontKernel -> TileM [Stmt]
buildWavefrontBody2 kernel = do
  timeBlock <- buildForcedStripDim2 (wfkIterT kernel) (wfkIterBound kernel) (wfkStageWidth kernel)
  diagIter <- freshLike (wavefrontHeadIter2 (wfkInnerSpec kernel)) "__wavefront"
  stageIter <- freshLike (wfkIterT kernel) "__wf_stage"
  stageTimeVars <-
    mapM
      (\n -> freshLike (wfkIterT kernel) ("__wf_time" <> BS.pack (show n)))
      [0 .. wfkStageWidth kernel - 1]
  extraBuffers <-
    mapM
      (\n -> freshLike (wfkNextArray kernel) ("__wf_ring" <> BS.pack (show n)))
      [2 .. wfkStageWidth kernel]
  let stageBuffers = [wfkCurArray kernel, wfkNextArray kernel] ++ extraBuffers
  rotations <- buildWavefrontRotation2 stageBuffers (smdTileLen timeBlock)
  stageCases <- buildWavefrontStageCases2 kernel diagIter stageIter stageBuffers stageTimeVars
  extraShapeVar <- freshLike (wfkCurArray kernel) "__wf_shape"
  let (extraShapeSetup, allocTargets) =
        if wfkPreallocatedNext kernel
          then
            ( [SAssign extraShapeVar (RArrayShape (AVar (wfkCurArray kernel)))]
            , extraBuffers
            )
          else
            ( []
            , drop 1 stageBuffers
            )
      allocShape =
        if wfkPreallocatedNext kernel
          then AVar extraShapeVar
          else wfkAllocShape kernel
      allocs =
        extraShapeSetup
          ++ [ SAssign arr (RArrayAlloc allocShape)
             | arr <- allocTargets
             ]
      stageTimeSetup =
        [ SAssign tVar (RBinOp CAdd (AVar (smdTileStart timeBlock)) (AInt offset))
        | (offset, tVar) <- zip [0 ..] stageTimeVars
        ]
      stageLoop =
        SLoop
          LoopSpec
            { lsIters = [stageIter]
            , lsBounds = [IVar (smdTileLen timeBlock)]
            , lsOrigins = []
            , lsExec = wfkStageExec kernel
            , lsRed = Nothing
            , lsRole = LoopMap
            }
          stageCases
      diagLoop =
        SLoop
          LoopSpec
            { lsIters = [diagIter]
            , lsBounds = [wavefrontDiagBound2 kernel (smdTileLen timeBlock)]
            , lsOrigins = []
            , lsExec = Serial
            , lsRed = Nothing
            , lsRole = LoopPlain
            }
          [stageLoop]
      blockLoop =
        SLoop
          LoopSpec
            { lsIters = [smdTileIter timeBlock]
            , lsBounds = [tileCountExpr (wfkIterBound kernel) (wfkStageWidth kernel)]
            , lsOrigins = []
            , lsExec = Serial
            , lsRed = Nothing
            , lsRole = LoopPlain
            }
          ( setupStripDim timeBlock
              ++ stageTimeSetup
              ++ [diagLoop]
              ++ rotations
          )
      cleanupTargets =
        if wfkPreallocatedNext kernel
          then extraBuffers
          else drop 1 stageBuffers
  cleanup <- fmap concat (mapM (buildWavefrontCleanup2 kernel) cleanupTargets)
  pure (wfkHoistedPrefix kernel ++ allocs ++ [blockLoop] ++ cleanup)

buildWavefrontCleanup2 :: WavefrontKernel -> CVar -> TileM [Stmt]
buildWavefrontCleanup2 kernel arr = do
  notCurVar <- freshLike arr "__wf_free_not_cur"
  case wfkInitTracker kernel of
    Nothing ->
      pure
        [ SAssign notCurVar (RBinOp CNeq (AVar arr) (AVar (wfkCurArray kernel)))
        , SIf (AVar notCurVar) [SAssign "__hyd_discard" (RArrayFree (AVar arr))] []
        ]
    Just initTrackerVar -> do
      notInitVar <- freshLike arr "__wf_free_not_init"
      freeVar <- freshLike arr "__wf_free"
      pure
        [ SAssign notCurVar (RBinOp CNeq (AVar arr) (AVar (wfkCurArray kernel)))
        , SAssign notInitVar (RBinOp CNeq (AVar arr) (AVar initTrackerVar))
        , SAssign freeVar (RBinOp CAnd (AVar notCurVar) (AVar notInitVar))
        , SIf (AVar freeVar) [SAssign "__hyd_discard" (RArrayFree (AVar arr))] []
        ]

matchWavefrontTail2 :: [Stmt] -> Maybe ([Stmt], CVar, CVar, Bool)
matchWavefrontTail2 suffix = case reverse suffix of
  ( SAssign _tmpOut (RAtom (AVar cur2))
    : SAssign next1 (RAtom (AVar _tmpIn))
    : SAssign cur1 (RAtom (AVar next))
    : restRev
    )
      | cur1 == cur2, next1 == next ->
          Just (reverse restRev, cur1, next1, True)
  (SAssign cur (RAtom (AVar next)) : restRev) ->
    Just (reverse restRev, cur, next, False)
  _ ->
    Nothing

wavefrontDiagBound2 :: WavefrontKernel -> CVar -> IndexExpr
wavefrontDiagBound2 kernel blockLenVar =
  simplifyIndexExpr $
    IAdd
      (wavefrontHeadBound2 (wfkInnerSpec kernel))
      ( IMul
          (IConst (wfkSkewCoeff kernel))
          (ISub (IVar blockLenVar) (IConst 1))
      )

buildWavefrontStageCases2
  :: WavefrontKernel
  -> CVar
  -> CVar
  -> [CVar]
  -> [CVar]
  -> TileM [Stmt]
buildWavefrontStageCases2 kernel diagIter stageIter stageBuffers stageTimeVars =
  fmap concat (mapM buildOne (zip3 [0 :: Integer ..] stageBuffers stageTimeVars))
  where
    buildOne (slotIx, curBuf, timeVar) =
      case atMay stageBuffers (fromIntegral slotIx + 1) of
        Nothing ->
          pure []
        Just nextBuf ->
          do
            let eqVar = stageIter <> "__eq_" <> BS.pack (show slotIx)
                substEnv =
                  M.fromList
                    [ (wfkCurArray kernel, AVar curBuf)
                    , (wfkNextArray kernel, AVar nextBuf)
                    , (wfkIterT kernel, AVar timeVar)
                    ]
                innerBody =
                  collapseWavefrontInnerLoop2
                    diagIter
                    (wfkInnerSpec kernel)
                    (substStmts2 substEnv (wfkInnerBody kernel))
                suffixBody = substStmts2 substEnv (wfkStageSuffix kernel)
            guardedInner <- buildWavefrontInnerGuard2 kernel diagIter timeVar innerBody
            pure
              [ SAssign eqVar (RBinOp CEq (AVar stageIter) (AInt slotIx))
              , SIf (AVar eqVar) (guardedInner ++ suffixBody) []
              ]

buildWavefrontInnerGuard2 :: WavefrontKernel -> CVar -> CVar -> [Stmt] -> TileM [Stmt]
buildWavefrontInnerGuard2 kernel diagIter timeVar innerBody = do
  let originExpr =
        simplifyIndexExpr $
          IMul (IConst (wfkSkewCoeff kernel)) (IVar timeVar)
      upperExpr = IAdd originExpr (wavefrontHeadBound2 (wfkInnerSpec kernel))
  (lowerSetup, lowerAtom) <- indexExprToAtom originExpr
  (upperSetup, upperAtom) <- indexExprToAtom upperExpr
  geVar <- freshLike diagIter "__wf_valid_ge"
  ltVar <- freshLike diagIter "__wf_valid_lt"
  validVar <- freshLike diagIter "__wf_valid"
  pure
    ( lowerSetup
        ++ upperSetup
        ++ [ SAssign geVar (RBinOp CGe (AVar diagIter) lowerAtom)
           , SAssign ltVar (RBinOp CLt (AVar diagIter) upperAtom)
           , SAssign validVar (RBinOp CAnd (AVar geVar) (AVar ltVar))
           , SIf (AVar validVar) innerBody []
           ]
    )

buildWavefrontRotation2 :: [CVar] -> CVar -> TileM [Stmt]
buildWavefrontRotation2 ringVars blockLenVar = do
  tempVars <- mapM (`freshLike` "__wf_tmp") ringVars
  eqVars <- mapM (`freshLike` "__wf_rot_eq") [blockLenVar <> "_" <> BS.pack (show i) | i <- [1 .. length ringVars - 1]]
  let saved =
        [ SAssign tmp (RAtom (AVar ring))
        | (tmp, ring) <- zip tempVars ringVars
        ]
      ringCount = length ringVars
      oneRotation shift eqVar =
        [ SAssign eqVar (RBinOp CEq (AVar blockLenVar) (AInt (fromIntegral shift)))
        , SIf
            (AVar eqVar)
            [ SAssign ring (RAtom (AVar (tempVars !! ((ix + shift) `mod` ringCount))))
            | (ix, ring) <- zip [0 :: Int ..] ringVars
            ]
            []
        ]
  pure (saved ++ concat (zipWith oneRotation [1 .. ringCount - 1] eqVars))

collapseWavefrontInnerLoop2 :: CVar -> LoopSpec -> [Stmt] -> [Stmt]
collapseWavefrontInnerLoop2 diagIter innerSpec innerBody =
  case lsIters innerSpec of
    [] ->
      innerBody
    headIter : tailIters ->
      let tailBounds = drop 1 (lsBounds innerSpec)
          tailOriginsRaw = drop 1 (lsOrigins innerSpec ++ repeat (IConst 0))
          tailOrigins = take (length tailIters) tailOriginsRaw
          trimmedOrigins
            | all (== IConst 0) tailOrigins = []
            | otherwise = tailOrigins
          fixedLead = [SAssign headIter (RAtom (AVar diagIter))]
      in if null tailIters
           then fixedLead ++ innerBody
           else
             fixedLead
               ++ [ SLoop
                      innerSpec
                        { lsIters = tailIters
                        , lsBounds = tailBounds
                        , lsOrigins = trimmedOrigins
                        }
                      innerBody
                  ]

isWavefrontInnerLoop2 :: Stmt -> Bool
isWavefrontInnerLoop2 stmt = case stmt of
  SLoop spec _ ->
    lsRole spec == LoopMap || lsRole spec == LoopPlain
  _ ->
    False

wavefrontSkewCoeffForLoop2 :: CVar -> LoopSpec -> Maybe Integer
wavefrontSkewCoeffForLoop2 iterT spec = do
  origin <- listToMaybe (lsOrigins spec ++ [IConst 0])
  case simplifyIndexExpr origin of
    IVar v
      | v == iterT ->
          Just 1
    IMul (IConst coeff) (IVar v)
      | v == iterT, coeff > 0 ->
          Just coeff
    IMul (IVar v) (IConst coeff)
      | v == iterT, coeff > 0 ->
          Just coeff
    _ ->
      Nothing

removeWavefrontAlloc2 :: CVar -> [Stmt] -> Maybe (Atom, [Stmt])
removeWavefrontAlloc2 nextVar = go []
  where
    go _ [] = Nothing
    go prefixRev (stmt : rest) = case stmt of
      SAssign v (RArrayAlloc shape)
        | v == nextVar ->
            Just (shape, reverse prefixRev ++ rest)
      _ ->
        go (stmt : prefixRev) rest

hoistableWavefrontStmt2 :: Stmt -> Bool
hoistableWavefrontStmt2 stmt = case stmt of
  SAssign _ rhs -> wavefrontHoistableRHS2 rhs
  _ -> False

wavefrontHoistableRHS2 :: RHS -> Bool
wavefrontHoistableRHS2 rhs = case rhs of
  RArrayAlloc {} -> False
  RArrayFree {} -> False
  RCall {} -> False
  _ -> True

shapePreservingWavefrontPrelude2 :: CVar -> [Stmt] -> Atom -> Bool
shapePreservingWavefrontPrelude2 curVar prefix allocShape =
  case resolveWavefrontShape2 rhsMap allocShape of
    Just shapeVar ->
      any
        (\stmt -> case stmt of
          SAssign v (RArrayShape (AVar arr)) -> v == shapeVar && arr == curVar
          _ -> False
        )
        prefix
    Nothing ->
      False
  where
    rhsMap =
      M.fromList
        [ (v, rhs)
        | SAssign v rhs <- prefix
        ]

resolveWavefrontShape2 :: Map CVar RHS -> Atom -> Maybe CVar
resolveWavefrontShape2 rhsMap atom = case atom of
  AVar v -> resolveVar v
  _ -> Nothing
  where
    resolveVar v = case M.lookup v rhsMap of
      Just (RAtom (AVar v')) -> resolveVar v'
      Just (RArrayShape (AVar _)) -> Just v
      _ -> Just v

wavefrontHeadIter2 :: LoopSpec -> CVar
wavefrontHeadIter2 spec = case lsIters spec of
  iter : _ -> iter
  [] -> error "wavefront loop requires at least one iterator"

wavefrontHeadBound2 :: LoopSpec -> IndexExpr
wavefrontHeadBound2 spec = case lsBounds spec of
  bound : _ -> bound
  [] -> error "wavefront loop requires at least one bound"

buildBlockedMapReductionScop2 :: ScheduledScop -> Maybe ScheduledScop
buildBlockedMapReductionScop2 scheduled = do
  kernel <- matchBlockedMapReductionKernel2 scop
  let replacement =
        evalState (buildBlockedMapReductionBody2 kernel) (collectScopNames scop, 0)
      typeOverrides =
        M.singleton (bmrOutputArray kernel) (CTArray (blockedMapReductionElemType2 kernel))
      scopedArrayOverrides =
        M.singleton
          (bmrOutputArray kernel)
          ArrayFact
            { afFreshAlloc = True
            , afWriteOnce = False
            , afReadOnly = False
            }
  pure
    scheduled
      { ssReplacement = Just replacement
      , ssTypeOverrides =
          M.union typeOverrides (ssTypeOverrides scheduled)
      , ssArrayFactOverrides =
          M.union scopedArrayOverrides (ssArrayFactOverrides scheduled)
      }
  where
    scop = ssOriginal scheduled

matchBlockedMapReductionKernel2 :: Scop -> Maybe BlockedMapReductionKernel
matchBlockedMapReductionKernel2 scop = do
  [SLoop outerSpec outerBody] <- reifyScheduledScop2 (buildIdentitySchedule2 scop)
  [outerI, outerJ] <- pure (lsIters outerSpec)
  [outerIBound, outerJBound] <- pure (lsBounds outerSpec)
  if lsRole outerSpec /= LoopMap
    then Nothing
    else do
      (outerBeforeWrite, finalWrite) <- unsnoc2 outerBody
      SArrayWrite (AVar outArr) outIdx (AVar outAcc) <- Just finalWrite
      (beforeWrapper, wrapperStmt, afterWrapper) <- splitFirstMatching2 isReductionWrapperStmt2 outerBeforeWrite
      (outerSetupPrefix, accInitStmt) <- unsnoc2 beforeWrapper
      SAssign accVar (RAtom initAtom) <- Just accInitStmt
      if outAcc /= accVar
           || usesVar2 accVar (outerSetupPrefix ++ afterWrapper)
        then Nothing
        else do
          SLoop wrapperSpec wrapperBody <- Just wrapperStmt
          if lsBounds wrapperSpec /= [IConst 1]
            then Nothing
            else do
              [wrapperIter] <- pure (lsIters wrapperSpec)
              (wrapperSetup, reductionStmt) <- unsnoc2 wrapperBody
              if usesVar2 accVar wrapperSetup
                then Nothing
                else do
                  SLoop reductionSpec reductionBody <- Just reductionStmt
                  if lsRole reductionSpec /= LoopReduction
                    then Nothing
                    else do
                      ReductionSpec redAcc _ RAdd <- lsRed reductionSpec
                      [reductionIter] <- pure (lsIters reductionSpec)
                      [reductionBound] <- pure (lsBounds reductionSpec)
                      if redAcc /= accVar
                           || not
                                (S.null
                                   ( usedVarsIndexExpr reductionBound
                                       `S.intersection` S.fromList [outerI, outerJ]
                                   )
                                )
                        then Nothing
                        else do
                          (reductionSetup, accUpdateStmt) <- unsnoc2 reductionBody
                          if usesVar2 accVar reductionSetup
                            then Nothing
                            else do
                              SAssign accDst (RBinOp updateOp updateLhs updateRhs) <- Just accUpdateStmt
                              if accDst /= accVar || not (atomRefsVar2 accVar updateLhs || atomRefsVar2 accVar updateRhs)
                                then Nothing
                                else
                                  Just
                                    BlockedMapReductionKernel
                                      { bmrOuterIterI = outerI
                                      , bmrOuterIterJ = outerJ
                                      , bmrOuterIBound = outerIBound
                                      , bmrOuterJBound = outerJBound
                                      , bmrReductionIter = reductionIter
                                      , bmrReductionBound = reductionBound
                                      , bmrAccVar = accVar
                                      , bmrInitAtom = initAtom
                                      , bmrOutputArray = outArr
                                      , bmrOuterSetup = outerSetupPrefix ++ afterWrapper
                                      , bmrWrapperIter = wrapperIter
                                      , bmrWrapperSetup = wrapperSetup
                                      , bmrReductionSetup = reductionSetup
                                      , bmrOutputIndex = outIdx
                                      , bmrUpdateOp = updateOp
                                      , bmrUpdateLhs = updateLhs
                                      , bmrUpdateRhs = updateRhs
                                      }

buildBlockedMapReductionBody2 :: BlockedMapReductionKernel -> TileM [Stmt]
buildBlockedMapReductionBody2 kernel = do
  iTile <- buildForcedStripDim2 (bmrOuterIterI kernel) (bmrOuterIBound kernel) (tcDefaultTile defaultTileConfig)
  jTile <- buildForcedStripDim2 (bmrOuterIterJ kernel) (bmrOuterJBound kernel) (tcDefaultTile defaultTileConfig)
  kTile <- buildForcedStripDim2 (bmrReductionIter kernel) (bmrReductionBound kernel) (tcReductionTile defaultTileConfig)
  cVal <- freshLike (bmrAccVar kernel) "_tile_acc"
  cNext <- freshLike (bmrAccVar kernel) "_tile_next"
  -- When the reduction setup contains the pattern:
  --   flatIn = base + k;  ndIdx = flat_to_nd(flatIn, {k_size});  projVar = ndIdx.elems[0]
  -- the projection is redundant because k < k_size is guaranteed by the loop bounds
  -- (base is always a multiple of k_size from the wrapper setup).  Substitute
  -- projVar → k directly so downstream optimizers can eliminate the dead chain.
  let reductionSetup =
        case findReductionNdProjection (bmrReductionIter kernel) (bmrReductionSetup kernel) of
          Just projVar ->
            substStmts2
              (M.singleton projVar (AVar (bmrReductionIter kernel)))
              (bmrReductionSetup kernel)
          Nothing -> bmrReductionSetup kernel
  let (reductionPrelude, outerSetup) =
        extractReductionPrelude2
          (usedVarsIndexExpr (bmrReductionBound kernel))
          [bmrOuterIterI kernel, bmrOuterIterJ kernel]
          (bmrOuterSetup kernel)
      outputArr = AVar (bmrOutputArray kernel)
      loadOutput = SAssign cVal (RArrayLoad outputArr (bmrOutputIndex kernel))
      updateOutput =
        SAssign
          cNext
          ( RBinOp
              (bmrUpdateOp kernel)
              (replaceAccAtom2 (bmrAccVar kernel) cVal (bmrUpdateLhs kernel))
              (replaceAccAtom2 (bmrAccVar kernel) cVal (bmrUpdateRhs kernel))
          )
      storeOutput = SArrayWrite outputArr (bmrOutputIndex kernel) (AVar cNext)
      initJLoop =
        localLoop2
          jTile
          LoopMap
          (outerSetup ++ [SArrayWrite outputArr (bmrOutputIndex kernel) (bmrInitAtom kernel)])
      initILoop =
        localLoop2
          iTile
          LoopPlain
          [initJLoop]
      computeJLoop =
        localLoop2
          jTile
          LoopMap
          ( outerSetup
              ++ reductionSetup
              ++ [loadOutput, updateOutput, storeOutput]
          )
      computeKLoop =
        localLoop2
          kTile
          LoopPlain
          [computeJLoop]
      wrapperLoop =
        SLoop
          LoopSpec
            { lsIters = [bmrWrapperIter kernel]
            , lsBounds = [IConst 1]
            , lsOrigins = []
            , lsExec = Serial
            , lsRed = Nothing
            , lsRole = LoopReductionWrapper
            }
          (bmrWrapperSetup kernel ++ [computeKLoop])
      computeILoop =
        localLoop2
          iTile
          LoopPlain
          [wrapperLoop]
      kkLoop = tileLoop2 kTile LoopPlain [computeILoop]
      jjLoop = tileLoop2 jTile LoopPlain [initILoop, kkLoop]
      iiLoop = tileLoop2 iTile LoopPlain [jjLoop]
      tilePrelude =
        reductionPrelude
          ++ smdBoundPrelude iTile
          ++ smdBoundPrelude jTile
          ++ smdBoundPrelude kTile
  pure (tilePrelude ++ [iiLoop])

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
       , lsOrigins = []
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
       , lsOrigins = []
       , lsExec = Serial
       , lsRed = Nothing
       , lsRole = role
       }
     (assignOrigIter td : body)

unsnoc2 :: [a] -> Maybe ([a], a)
unsnoc2 xs = case reverse xs of
  [] -> Nothing
  y : ys -> Just (reverse ys, y)

splitFirstMatching2 :: (a -> Bool) -> [a] -> Maybe ([a], a, [a])
splitFirstMatching2 matches xs =
  case break matches xs of
    (prefix, target : suffix) -> Just (prefix, target, suffix)
    _ -> Nothing

isReductionWrapperStmt2 :: Stmt -> Bool
isReductionWrapperStmt2 stmt = case stmt of
  SLoop spec _ -> lsRole spec == LoopReductionWrapper
  _ -> False

usesVar2 :: CVar -> [Stmt] -> Bool
usesVar2 v = S.member v . usedVarsStmts2

atomRefsVar2 :: CVar -> Atom -> Bool
atomRefsVar2 v atom = case atom of
  AVar v' -> v == v'
  _ -> False

replaceAccAtom2 :: CVar -> CVar -> Atom -> Atom
replaceAccAtom2 old new atom = case atom of
  AVar v | v == old -> AVar new
  _ -> atom

-- | Detect the pattern in @bmrReductionSetup@ where a flat index computed as
-- @flatIn = base + reductionIter@ is immediately converted back via
-- @ndIdx = hyd_flat_to_nd(flatIn, {k_size})@ and projected as
-- @projVar = ndIdx.elems[0]@.  When @reductionIter < k_size@ (guaranteed by
-- the enclosing loop), @projVar == reductionIter@.  Returns the variable to
-- substitute away, if found.
findReductionNdProjection :: CVar -> [Stmt] -> Maybe CVar
findReductionNdProjection reductionIter stmts = do
  flatIn  <- listToMaybe [v | SAssign v (RBinOp CAdd a1 a2) <- stmts
                             , a1 == AVar reductionIter || a2 == AVar reductionIter]
  ndIdx   <- listToMaybe [v | SAssign v (RFlatToNd a _) <- stmts
                             , a == AVar flatIn]
  projVar <- listToMaybe [v | SAssign v (RProj 0 a) <- stmts
                             , a == AVar ndIdx]
  pure projVar

extractReductionPrelude2 :: Set CVar -> [CVar] -> [Stmt] -> ([Stmt], [Stmt])
extractReductionPrelude2 needed forbidden stmts =
  (selected, remaining)
  where
    forbiddenSet = S.fromList forbidden
    step stmt (neededNow, picked, kept)
      | not (S.null (defs `S.intersection` neededNow))
          && S.null (uses `S.intersection` forbiddenSet) =
          ( (neededNow `S.difference` defs) `S.union` uses
          , stmt : picked
          , kept
          )
      | otherwise =
          (neededNow, picked, stmt : kept)
      where
        defs = definedVarsStmts2 [stmt]
        uses = usedVarsStmts2 [stmt]
    (_, selected, remaining) =
      foldr step (needed, [], []) stmts

data DenseValueInfo
  = DenseScalarInfo CVar
  | DenseTupleInfo [Maybe CVar]

deriveVectorAccessFacts2 :: [Stmt] -> Map CVar VectorAccessFact -> Map CVar VectorAccessFact
deriveVectorAccessFacts2 stmts baseFacts = snd (deriveStmtFacts2 env0 baseFacts stmts)
  where
    env0 =
      M.fromList
        [ (v, DenseScalarInfo iter)
        | (v, fact) <- M.toList baseFacts
        , Just iter <- [vxfDenseLinearIndexOf fact]
        ]

deriveStmtFacts2
  :: Map CVar DenseValueInfo
  -> Map CVar VectorAccessFact
  -> [Stmt]
  -> (Map CVar DenseValueInfo, Map CVar VectorAccessFact)
deriveStmtFacts2 env facts [] = (env, facts)
deriveStmtFacts2 env facts (stmt : rest) =
  deriveStmtFacts2 env' facts' rest
  where
    (env', facts') = deriveOneStmtFacts2 env facts stmt

deriveOneStmtFacts2
  :: Map CVar DenseValueInfo
  -> Map CVar VectorAccessFact
  -> Stmt
  -> (Map CVar DenseValueInfo, Map CVar VectorAccessFact)
deriveOneStmtFacts2 env facts stmt = case stmt of
  SAssign v rhs ->
    case inferDenseValueInfo2 env rhs of
      Just info ->
        ( M.insert v info env
        , case info of
            DenseScalarInfo iter ->
              M.insert
                v
                ((M.findWithDefault emptyVectorAccessFact2 v facts) { vxfDenseLinearIndexOf = Just iter })
                facts
            DenseTupleInfo {} ->
              facts
        )
      Nothing ->
        (M.delete v env, facts)
  SLoop spec body ->
    let loopEnv =
          M.union
            (M.fromList [(iter, DenseScalarInfo iter) | iter <- lsIters spec])
            env
        (envBody, factsBody) = deriveStmtFacts2 loopEnv facts body
    in  (M.union envBody env, factsBody)
  SIf _ thn els ->
    let (envThn, factsThn) = deriveStmtFacts2 env facts thn
        (envEls, factsEls) = deriveStmtFacts2 env factsThn els
    in  (M.union envEls envThn, factsEls)
  _ ->
    (env, facts)

inferDenseValueInfo2 :: Map CVar DenseValueInfo -> RHS -> Maybe DenseValueInfo
inferDenseValueInfo2 env rhs = case rhs of
  RAtom atom ->
    DenseScalarInfo <$> atomDenseOrigin2 env atom
  RBinOp op a b ->
    DenseScalarInfo <$> inferDenseBinOpOrigin2 env op a b
  RTuple atoms ->
    let comps = map (atomDenseOrigin2 env) atoms
    in  if any isJust comps then Just (DenseTupleInfo comps) else Nothing
  RProj i (AVar src) ->
    case M.lookup src env of
      Just (DenseTupleInfo comps)
        | Just iter <- atMay comps (fromIntegral i) >>= id -> Just (DenseScalarInfo iter)
      _ -> Nothing
  RNdToFlat (AVar src) _ ->
    case M.lookup src env of
      Just (DenseTupleInfo comps)
        | Just (Just iter) <- listToMaybe (reverse comps) ->
            Just (DenseScalarInfo iter)
      _ -> Nothing
  RFlatToNd atom shape ->
    do
      iter <- atomDenseOrigin2 env atom
      rank <- atomTupleArity2 env shape
      pure (DenseTupleInfo (replicate (rank - 1) Nothing ++ [Just iter]))
  _ ->
    Nothing

atomDenseOrigin2 :: Map CVar DenseValueInfo -> Atom -> Maybe CVar
atomDenseOrigin2 env atom = case atom of
  AVar v ->
    case M.lookup v env of
      Just (DenseScalarInfo iter) -> Just iter
      _ -> Nothing
  _ ->
    Nothing

atomTupleArity2 :: Map CVar DenseValueInfo -> Atom -> Maybe Int
atomTupleArity2 env atom = case atom of
  AVar v ->
    case M.lookup v env of
      Just (DenseTupleInfo comps) -> Just (length comps)
      _ -> Nothing
  _ ->
    Nothing

inferDenseBinOpOrigin2 :: Map CVar DenseValueInfo -> BinOp -> Atom -> Atom -> Maybe CVar
inferDenseBinOpOrigin2 env op a b = case op of
  CAdd ->
    case (atomDenseOrigin2 env a, atomDenseOrigin2 env b) of
      (Just iter, Nothing) -> Just iter
      (Nothing, Just iter) -> Just iter
      _ -> Nothing
  CSub ->
    case (atomDenseOrigin2 env a, atomDenseOrigin2 env b) of
      (Just iter, Nothing) -> Just iter
      _ -> Nothing
  _ ->
    Nothing

blockedMapReductionElemType2 :: BlockedMapReductionKernel -> CType
blockedMapReductionElemType2 kernel = case bmrUpdateOp kernel of
  CAddF -> CTDouble
  CSubF -> CTDouble
  CMulF -> CTDouble
  CDivF -> CTDouble
  _ ->
    case bmrInitAtom kernel of
      AFloat {} -> CTDouble
      _ -> CTInt64

scheduledVectorAccessFacts2
  :: ScheduledScop
  -> Map CVar VectorAccessFact
  -> Map CVar VectorAccessFact
scheduledVectorAccessFacts2 scheduled facts =
  M.union generatedIterFacts adjustedFacts
  where
    scopNames = collectScopNames (ssOriginal scheduled)
    iterRebindings = collectStripMineIterRebindings2 (ssSchedule scheduled)
    adjustedFacts =
      M.mapWithKey adjustFact facts
    adjustFact v fact
      | v `S.member` scopNames =
          remapDenseLinearOrigin2 iterRebindings fact
      | otherwise =
          fact
    generatedIterFacts =
      M.fromList
        [ ( iter
          , (M.findWithDefault emptyVectorAccessFact2 iter facts)
              { vxfDenseLinearIndexOf = Just localIter
              }
          )
        | (iter, localIter) <- M.toList iterRebindings
        ]

emptyVectorAccessFact2 :: VectorAccessFact
emptyVectorAccessFact2 =
  VectorAccessFact
    { vxfDenseLinearIndexOf = Nothing
    , vxfDenseRead = False
    , vxfIndirectRead = False
    , vxfContiguousWrite = False
    }

remapDenseLinearOrigin2 :: Map CVar CVar -> VectorAccessFact -> VectorAccessFact
remapDenseLinearOrigin2 iterRebindings fact =
  fact
    { vxfDenseLinearIndexOf =
        fmap (\iter -> M.findWithDefault iter iter iterRebindings) (vxfDenseLinearIndexOf fact)
    }

collectStripMineIterRebindings2 :: ScheduleTree -> Map CVar CVar
collectStripMineIterRebindings2 sched = case sched of
  ScheduleSequence xs ->
    M.unions (map collectStripMineIterRebindings2 xs)
  ScheduleStmtRef {} ->
    M.empty
  ScheduleLoopBand band ->
    collectStripMineIterRebindings2 (lbBody band)
  ScheduleStripMine band plans ->
    M.unions
      [ M.fromList
          [ (smdOrigIter td, smdLocalIter td)
          | StripTile td <- plans
          ]
      , collectStripMineIterRebindings2 (lbBody band)
      ]

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
