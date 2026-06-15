{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.CodegenMSL
--
-- Metal Shading Language (MSL) compute backend for macOS / Apple Silicon.
--
-- Translates the same @CFG.Program@ IR consumed by the C backend into:
--
-- * A @.metal@ kernel source file (scalar compute kernel).
-- * A self-contained Objective-C harness (@.m@) that invokes CPU helper
--   procs, fills Metal buffers, dispatches the GPU kernel, and prints the
--   result.
--
-- Supported kernel forms:
--
-- * 1-D parallel \/ vector map kernels (outermost @LoopMap@ loop).
-- * Scalar and 1-D array inputs\/outputs.
-- * @CTInt64@ (mapped to @long@) and @CTDouble@ (demoted to @float@).
-- * Inner serial loops and conditionals inside the kernel body.
-- * No dynamic allocation inside the kernel.
-- * No @RFlatToNd@ \/ @RNdToFlat@ inside the kernel body.
module Language.Hydrangea.CodegenMSL
  ( MSLArtifacts (..),
    MSLOptions (..),
    defaultMSLOptions,
    codegenMSL,

    -- * Backend-neutral pieces shared with the CUDA backend
    -- $shared
    -- These are reused verbatim by "Language.Hydrangea.CodegenCUDA": the kernel
    -- analysis is target-independent, and the statement/expression emitters
    -- produce plain C (no Metal address spaces or attributes), so the CUDA
    -- backend only has to supply its own kernel signature, atomics, headers and
    -- host harness. See the @GpuDialect@ note in "CodegenCUDA".
    KernelAnalysis (..),
    analyzeOneKernel,
    findKernelProc,
    genHelperC,
    retResolvesToOutput,
    topLevelLoopSpecs,
    genMSLStmt,
    genMSLAtom,
    genMSLIndexExpr,
    collectTupleDefs,
    collectArrayAliases,
    collectAssignedVars,
    collectPairTypesFromEnv,
    mslPairStructName,
    genPreLoopLine,
    genCAtom,
    cpuArrayElemCType,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Char (toUpper)
import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf, maximumBy, nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Language.Hydrangea.CFG qualified as CFG
import Language.Hydrangea.CFGAnalysis (usedVarsStmts)
import Language.Hydrangea.CFGOpt (substStmts)
import Language.Hydrangea.CFGCore
  ( Atom (..),
    BinOp (..),
    CElemType (..),
    CType (..),
    CVar,
    RHS (..),
    UnOp (..),
    ctypeToElemType,
  )
import Language.Hydrangea.CFGTyping
  ( TypeEnv,
    buildCallParamTypes,
    inferProgramReturnTypes,
    lookupArrayElemType,
    recoverProcTypeEnv,
  )
import Language.Hydrangea.CodegenC
  ( CodegenArtifacts (..),
    CodegenOptions (..),
    ExportSpec (..),
    VarKind (..),
    cTypeName,
    celemTypeCType,
    celemTypeLetter,
    codegenProgramWithOptions,
    defaultCodegenOptions,
    detectAtomicScatterAddLoop,
    inferArrayElemTypesFromStmts,
    isFloatArithBinOp,
    isMathFloatOp,
    pairStructName,
    procReturnKinds,
    resolveExportSpec,
    sanitize,
    sanitizeExportName,
    splitFinalReturn,
  )

-- ---------------------------------------------------------------------------
-- Public types

-- | Artifacts produced by the MSL codegen.
data MSLArtifacts = MSLArtifacts
  { -- | Contents of the @.metal@ kernel file.
    mslKernelSource :: String,
    -- | Contents of the self-contained ObjC harness.
    mslHarnessSource :: String,
    -- | Header file (present in export mode).
    mslHeaderSource :: Maybe String,
    -- | Diagnostics about work that falls back to the CPU (printed to stderr
    -- by the CLI). Empty when the whole computation runs on the GPU.
    mslWarnings :: [String]
  }

-- | Options controlling which proc is selected as the GPU kernel.
data MSLOptions = MSLOptions
  { -- | If 'Just name', use that proc as the kernel. If 'Nothing', use the
    -- first proc with a top-level @LoopMap@ loop.
    mslKernelToEmit :: Maybe CVar,
    -- | When 'True' (default), automatically dispatch multiple GPU kernels
    -- when the program has more than one GPU-eligible proc.
    mslMultiKernel :: Bool,
    -- | If 'Just name', generate a reusable library with init/dispatch/cleanup
    -- lifecycle functions instead of a standalone executable.
    mslExportKernel :: Maybe CVar
  }

defaultMSLOptions :: MSLOptions
defaultMSLOptions =
  MSLOptions
    { mslKernelToEmit = Nothing,
      mslMultiKernel = True,
      mslExportKernel = Nothing
    }

-- | Per-kernel analysis record.  Captures everything needed to generate the
-- Metal kernel source and harness dispatch for one proc.
data KernelAnalysis = KernelAnalysis
  { kaProc :: CFG.Proc,
    kaName :: CVar,
    kaPreLoopStmts :: [CFG.Stmt],
    kaLoopSpec :: CFG.LoopSpec,
    kaLoopBody :: [CFG.Stmt],
    kaRetAtom :: Maybe Atom,
    kaTypeEnv :: TypeEnv,
    kaArrayElemTys :: Map CVar CType,
    kaInputArrays :: [CVar],
    kaOutputArrays :: [CVar],
    -- | Includes proc-param scalars
    kaScalarInputs :: [CVar],
    -- | preLoopStmts ++ inBodyHoisted
    kaEffectivePreLoop :: [CFG.Stmt],
    kaHoistedCallMap :: Map CVar CVar,
    kaPreLoopAliases :: [(CVar, CVar)],
    -- | Statements between kernel loop and return
    kaPostLoopStmts :: [CFG.Stmt],
    -- | All proc params (for harness defaults)
    kaProcParams :: [CVar]
  }

-- | Whether an input array originates from a CPU helper or a prior GPU kernel.
data BufferOrigin
  = CPUProduced
  | -- | producing kernel name
    GPUProduced CVar

-- ---------------------------------------------------------------------------
-- Top-level entry point

-- | Translate a @CFG.Program@ to Metal artifacts.
-- Uses multi-kernel dispatch when multiple zero-arg procs have parallelizable
-- loops; falls back to single-kernel for explicit @--metal-kernel@ selection
-- or when only one proc is GPU-eligible.
codegenMSL :: MSLOptions -> CFG.Program -> Either String MSLArtifacts
codegenMSL opts prog@(CFG.Program procs) =
  case mslExportKernel opts of
    Just _ -> codegenMSLExport opts prog -- export-kernel mode
    Nothing ->
      case mslKernelToEmit opts of
        Just _ -> codegenMSLSingle opts prog -- explicit kernel: single-kernel path
        Nothing
          | not (mslMultiKernel opts) -> codegenMSLSingle opts prog
          -- A `main` that is a thin file-write sink over a GPU proc (a stencil
          -- whose result is written to CSV rather than returned). Inline the
          -- callee so the kernel paths offload the compute and the CSV write
          -- runs as post-kernel CPU work.
          | Just sinkProg@(CFG.Program sinkProcs) <- inlineFileSinkMain prog,
            Just rp <- resultProc sinkProcs ->
              if hasTopLevelIterate rp
                then codegenMSLTemporal sinkProg rp
                else
                  if gpuLoopCountDeep rp >= 2
                    then codegenMSLMultiPhase sinkProg rp
                    else codegenMSLSingle opts sinkProg
          -- A `main` that is itself a temporal loop (`iterate`): one GPU map
          -- kernel dispatched N times over two ping-pong buffers.
          | Just rp <- resultProc procs,
            hasTopLevelIterate rp ->
              codegenMSLTemporal prog rp
          -- A result proc that is itself a chain of several GPU loops
          -- (e.g. fill -> scatter -> reduce) runs each loop as its own kernel
          -- with GPU-resident buffers between them.
          | Just rp <- resultProc procs,
            gpuLoopCountDeep rp >= 2 ->
              codegenMSLMultiPhase prog rp
          | otherwise ->
              let gpuProcs = findGPUEligibleProcs procs
               in case gpuProcs of
                    [] -> Left "MSL backend: no proc with a parallelizable loop found"
                    _ ->
                      -- The harness prints the program result: the `main` entry or
                      -- the final binding. The last GPU-eligible proc is the buffer
                      -- that gets printed; if it isn't the result proc, the result
                      -- isn't GPU-computable and we'd silently print an intermediate.
                      case resultProcName procs of
                        Nothing -> Left "MSL backend: empty program"
                        Just rname
                          | rname /= CFG.procName (last gpuProcs) ->
                              Left (resultNotLowerableMsg rname)
                          | otherwise -> case gpuProcs of
                              [_] -> codegenMSLSingle opts prog -- one GPU proc: existing path
                              _ -> codegenMSLMulti prog gpuProcs

-- | Multi-kernel code path: dispatch multiple GPU kernels in sequence.
codegenMSLMulti :: CFG.Program -> [CFG.Proc] -> Either String MSLArtifacts
codegenMSLMulti prog@(CFG.Program procs) gpuProcs = do
  -- Analyze each GPU-eligible proc; silently drop those that fail analysis
  let analyses = [ka | p <- gpuProcs, Right ka <- [analyzeOneKernel prog p]]
  case analyses of
    [] -> Left "MSL backend: no GPU-eligible proc passed kernel analysis"
    [ka] -> codegenMSLSingle defaultMSLOptions prog -- degenerate: one survived
    kas -> do
      let gpuNames = Set.fromList [kaName ka | ka <- kas]
          bufOrigins = classifyBufferOrigins kas gpuNames
          gpuBufAliases = buildGPUBufferAliases kas gpuNames
          retKinds = procReturnKinds prog
          retTypes = inferProgramReturnTypes prog
      helperC <- genHelperC procs (kaName (last kas)) retKinds
      let kernelSrc = genMultiKernelMSL kas
          harnessSrc =
            genMultiKernelHarnessSrc
              prog
              kas
              bufOrigins
              gpuBufAliases
              retKinds
              retTypes
              helperC
      Right
        MSLArtifacts
          { mslKernelSource = kernelSrc,
            mslHarnessSource = harnessSrc,
            mslHeaderSource = Nothing,
            mslWarnings = cpuFallbackWarningsMulti procs kas ++ fp32DemotionWarning kas
          }

-- | Single-kernel code path (existing behaviour).
codegenMSLSingle :: MSLOptions -> CFG.Program -> Either String MSLArtifacts
codegenMSLSingle opts prog@(CFG.Program procs) = do
  kernelProc <- findKernelProc opts procs
  ka <- analyzeOneKernel prog kernelProc
  -- The harness can finish post-kernel CPU work only if it is straight-line
  -- (projection / reshape). A loop after the kernel (e.g. a reduction) can't be
  -- emitted on the CPU side, so refuse rather than produce wrong output. This is
  -- only reachable via an explicit --metal-kernel override (normal selection
  -- rejects procs with a loop after the kernel).
  let postLoopHasLoop = not (null (topLevelLoopSpecs (kaPostLoopStmts ka)))
      retPrintable = retResolvesToOutput (kaRetAtom ka) (kaPostLoopStmts ka) (kaOutputArrays ka)
  () <-
    if postLoopHasLoop && not retPrintable
      then
        Left $
          "MSL backend: kernel `"
            ++ BS.unpack (kaName ka)
            ++ "` leaves a loop (e.g. a reduction) running after it that the Metal"
            ++ " harness cannot finish on the CPU. Select a different kernel"
            ++ " (--metal-kernel=<name>) or run on the C backend."
      else Right ()
  let retKinds = procReturnKinds prog
      retTypes = inferProgramReturnTypes prog
  helperC <- genHelperC procs (kaName ka) retKinds
  let kernelSrc =
        genMSLKernelSrc
          (kaName ka)
          (kaLoopSpec ka)
          (kaLoopBody ka)
          (kaTypeEnv ka)
          (kaArrayElemTys ka)
          (kaInputArrays ka)
          (kaOutputArrays ka)
          (kaScalarInputs ka)
          (kaHoistedCallMap ka)
          (kaPreLoopAliases ka)
      harnessSrc =
        genObjCHarnessSrc
          prog
          (kaProc ka)
          (kaName ka)
          (kaEffectivePreLoop ka)
          (kaLoopSpec ka)
          (kaRetAtom ka)
          (kaPostLoopStmts ka)
          (kaTypeEnv ka)
          (kaArrayElemTys ka)
          retKinds
          retTypes
          (kaInputArrays ka)
          (kaOutputArrays ka)
          (kaScalarInputs ka)
          (kaProcParams ka)
          helperC
  Right
    MSLArtifacts
      { mslKernelSource = kernelSrc,
        mslHarnessSource = harnessSrc,
        mslHeaderSource = Nothing,
        mslWarnings = cpuFallbackWarningsSingle ka ++ fp32DemotionWarning [ka]
      }

-- ---------------------------------------------------------------------------
-- Multi-phase code path (Layer A): one proc, several GPU loops chained with
-- GPU-resident buffers between them (e.g. fill -> scatter -> reduce).
-- ---------------------------------------------------------------------------

-- | True for a loop the GPU can run as a kernel: a map/map-reduction or any
-- parallelized loop (which includes atomic/privatized scatter-add).
isGpuLoopSpec :: CFG.LoopSpec -> Bool
isGpuLoopSpec spec =
  CFG.lsRole spec `elem` [CFG.LoopMap, CFG.LoopMapReduction]
    || (case CFG.lsExec spec of CFG.Parallel _ -> True; _ -> False)

-- | GPU loops anywhere in a statement list, descending through @SIf@ guards but
-- not into a loop's own body (that body is the kernel). Pre-order. This finds
-- the region maps of a clamped stencil, which the lowering nests inside
-- grid-size feasibility guards rather than leaving at the top level.
gpuLoopsDeep :: [CFG.Stmt] -> [CFG.Stmt]
gpuLoopsDeep = concatMap go
  where
    go s@(CFG.SLoop spec _) | isGpuLoopSpec spec = [s]
    go (CFG.SLoop _ _) = []
    go (CFG.SIf _ thn els) = gpuLoopsDeep thn ++ gpuLoopsDeep els
    go _ = []

-- | True if any loop (descending through @SIf@) is *not* a GPU loop — i.e. there
-- is serial work that the region-pipeline path can't offload.
hasNonGpuLoopDeep :: [CFG.Stmt] -> Bool
hasNonGpuLoopDeep = any go
  where
    go (CFG.SLoop spec body) = not (isGpuLoopSpec spec) || hasNonGpuLoopDeep body
    go (CFG.SIf _ thn els) = hasNonGpuLoopDeep thn || hasNonGpuLoopDeep els
    go _ = False

-- | Number of GPU loops in a proc's body, descending through @SIf@ guards.
gpuLoopCountDeep :: CFG.Proc -> Int
gpuLoopCountDeep p = length (gpuLoopsDeep (fst (splitFinalReturn (CFG.procBody p))))

-- | A proc that offloads cleanly as a (possibly multi-region) GPU pipeline:
-- it has at least one GPU loop and no serial loops. Covers single maps,
-- fill→scatter→reduce chains, and clamped-stencil region fills.
allLoopsGpu :: CFG.Proc -> Bool
allLoopsGpu p =
  let (b, _) = splitFinalReturn (CFG.procBody p)
   in not (null (gpuLoopsDeep b)) && not (hasNonGpuLoopDeep b)

-- | True when a proc's body (return split off) has a top-level @LoopIterate@
-- (an @iterate@ temporal loop) whose body contains a GPU map loop — i.e. it is
-- a candidate for the ping-pong temporal kernel path.
hasTopLevelIterate :: CFG.Proc -> Bool
hasTopLevelIterate p =
  let (bodyNoRet, _) = splitFinalReturn (CFG.procBody p)
   in any isTemporalLoopStmt bodyNoRet
  where
    isTemporalLoopStmt (CFG.SLoop spec b) =
      CFG.lsRole spec == CFG.LoopIterate && any innerGpuLoop b
    isTemporalLoopStmt _ = False
    innerGpuLoop (CFG.SLoop sp _) = isGpuLoopSpec sp
    innerGpuLoop _ = False

-- | Builtins that consume an array purely for a side effect (a CSV file write),
-- producing no GPU-relevant value.
isFileWriteBuiltin :: BS.ByteString -> Bool
isFileWriteBuiltin fn =
  fn == "hyd_write_array_csv" || fn == "hyd_write_array_csv_float"

-- | When @main@ is a thin "file-write sink" — it calls a single GPU-eligible
-- proc and then only writes that array to a CSV file (the shape produced by
-- @write_array@ / @write_array_float@) — inline the callee's body into @main@
-- so the kernel paths can offload the computation and run the CSV write as
-- post-kernel CPU work. Temporaries are globally unique across procs, so the
-- inlining needs no renaming. Returns 'Nothing' when the pattern doesn't hold,
-- leaving the program untouched for the normal routing.
inlineFileSinkMain :: CFG.Program -> Maybe CFG.Program
inlineFileSinkMain (CFG.Program procs) = do
  mp <- listToMaybe [p | p <- procs, CFG.procName p == "main"]
  let body = CFG.procBody mp
      (bodyNoRet, retAtom) = splitFinalReturn body
      -- A callee is inlinable if it offloads: a plain GPU proc, a region
      -- pipeline (all loops are GPU maps — e.g. a clamped stencil's interior +
      -- boundary fills), or a temporal (`iterate`) proc.
      gpuNames =
        Set.fromList (map CFG.procName (findGPUEligibleProcs procs))
          `Set.union` Set.fromList [CFG.procName p | p <- procs, hasTopLevelIterate p]
          `Set.union` Set.fromList [CFG.procName p | p <- procs, allLoopsGpu p]
  -- A pure side-effecting sink returns unit.
  guard (retAtom == Just AUnit || retAtom == Nothing)
  -- Exactly one top-level call to a GPU-eligible, zero-arg proc.
  (callIdx, callVar, pName) <-
    case [ (i, callVar, pName)
         | (i, CFG.SAssign callVar (RCall pName [])) <- zip [0 ..] bodyNoRet,
           pName `Set.member` gpuNames
         ] of
      [c] -> Just c
      _ -> Nothing
  -- That result must be consumed by a file-write builtin downstream.
  let afterCall = drop (callIdx + 1) bodyNoRet
      writesCallVar =
        any
          ( \s -> case s of
              CFG.SAssign _ (RCall fn (AVar a : _)) ->
                isFileWriteBuiltin fn && a == callVar
              _ -> False
          )
          afterCall
  guard writesCallVar
  callee <- listToMaybe [p | p <- procs, CFG.procName p == pName]
  let (calleeNoRet, calleeRet) = splitFinalReturn (CFG.procBody callee)
  calleeRetAtom <- calleeRet
  let before = take callIdx bodyNoRet
      inlinedBody =
        before
          ++ calleeNoRet
          ++ [CFG.SAssign callVar (RAtom calleeRetAtom)]
          ++ afterCall
          ++ [CFG.SReturn (fromMaybe AUnit retAtom)]
      mergedTypeEnv = Map.union (CFG.procTypeEnv mp) (CFG.procTypeEnv callee)
      mp' = mp {CFG.procBody = inlinedBody, CFG.procTypeEnv = mergedTypeEnv}
  Just (CFG.Program [if CFG.procName p == "main" then mp' else p | p <- procs])

-- | The proc whose result the program prints: @main@ if present, else the last.
resultProc :: [CFG.Proc] -> Maybe CFG.Proc
resultProc procs = case filter (\p -> CFG.procName p == "main") procs of
  (p : _) -> Just p
  [] -> if null procs then Nothing else Just (last procs)


-- | Lower a proc that is a straight-line sequence of GPU loops (with scalar/
-- shape glue between them) to a chain of kernel dispatches sharing GPU buffers.
codegenMSLMultiPhase :: CFG.Program -> CFG.Proc -> Either String MSLArtifacts
codegenMSLMultiPhase prog@(CFG.Program procs) rp = do
  let body = CFG.procBody rp
      (bodyNoRet, retAtom) = splitFinalReturn body
      retTypes = inferProgramReturnTypes prog
      retKinds = procReturnKinds prog
      callParamTys = buildCallParamTypes retTypes procs
      typeEnv = recoverProcTypeEnv retTypes callParamTys rp
      arrayElemTys =
        inferArrayElemTypesFromStmts
          retTypes
          body
          (Map.fromList [(v, elt) | (v, CTArray elt) <- Map.toList typeEnv])
      -- Flatten GPU loops in pre-order, descending through SIf guards. Each
      -- loop's glue is the non-loop statements lexically before it (a superset
      -- across both branches, which is fine for input/scalar classification).
      flatToks = flattenPhaseToks bodyNoRet
      withGlue =
        [ ([s | TGlue s <- take i flatToks], spec, lb)
        | (i, TLoop spec lb) <- zip [0 ..] flatToks
        ]
      mkPhase phaseNo (glueBefore, spec, lb) =
        let (ins, outs, scals, hoisted) =
              classifyKernelParams glueBefore lb spec typeEnv retKinds
            aliases = collectArrayAliases (Set.fromList ins) (glueBefore ++ hoisted)
         in KernelAnalysis
              { kaProc = rp,
                kaName = CFG.procName rp <> BS.pack ("_p" ++ show (phaseNo :: Int)),
                kaPreLoopStmts = glueBefore,
                kaLoopSpec = spec,
                kaLoopBody = lb,
                kaRetAtom = Nothing,
                kaTypeEnv = typeEnv,
                kaArrayElemTys = arrayElemTys,
                kaInputArrays = ins,
                kaOutputArrays = outs,
                kaScalarInputs = scals,
                kaEffectivePreLoop = glueBefore ++ hoisted,
                kaHoistedCallMap = buildHoistedCallMap hoisted,
                kaPreLoopAliases = aliases,
                kaPostLoopStmts = [],
                kaProcParams = CFG.procParams rp
              }
      phases = zipWith mkPhase [0 ..] withGlue
  mapM_ (validateMSLLoopBody . kaLoopBody) phases
  helperC <- genHelperC procs (CFG.procName rp) retKinds
  let kernelSrc = genMultiKernelMSL phases
      harnessSrc =
        genMultiPhaseHarness
          rp
          bodyNoRet
          phases
          retAtom
          typeEnv
          arrayElemTys
          retKinds
          helperC
  Right
    MSLArtifacts
      { mslKernelSource = kernelSrc,
        mslHarnessSource = harnessSrc,
        mslHeaderSource = Nothing,
        mslWarnings = fp32DemotionWarning phases
      }

-- | A flattened multi-phase token: either host glue or a GPU loop (a kernel
-- dispatch). Produced by pre-order traversal that descends through @SIf@ guards
-- so the region maps of a clamped stencil become a flat dispatch sequence.
data PhaseTok = TGlue CFG.Stmt | TLoop CFG.LoopSpec [CFG.Stmt]

-- | Flatten a statement list into phase tokens, descending through @SIf@. The
-- @SIf@ wrapper is dropped (its condition computations remain as glue tokens);
-- the harness re-derives the guards when it walks the tree for dispatch.
flattenPhaseToks :: [CFG.Stmt] -> [PhaseTok]
flattenPhaseToks = concatMap go
  where
    go (CFG.SLoop spec body) | isGpuLoopSpec spec = [TLoop spec body]
    go (CFG.SIf _ thn els) = flattenPhaseToks thn ++ flattenPhaseToks els
    go s = [TGlue s]

-- | All statements in a body, descending through @SIf@ branches (but not into
-- loop bodies). Used to find array allocations regardless of guard nesting.
flattenAllStmts :: [CFG.Stmt] -> [CFG.Stmt]
flattenAllStmts = concatMap go
  where
    go (CFG.SIf _ thn els) = flattenAllStmts thn ++ flattenAllStmts els
    go s = [s]

-- | All variables referenced anywhere in an 'IndexExpr' (recursively). Used to
-- find scalar inputs hidden in nested loop-bound expressions (e.g. a tiled
-- bound @(n + 29) / 32@ references @n@).
indexExprVars :: CFG.IndexExpr -> [CVar]
indexExprVars e = case e of
  CFG.IVar v -> [v]
  CFG.IConst _ -> []
  CFG.IAdd a b -> indexExprVars a ++ indexExprVars b
  CFG.ISub a b -> indexExprVars a ++ indexExprVars b
  CFG.IMul a b -> indexExprVars a ++ indexExprVars b
  CFG.IDiv a b -> indexExprVars a ++ indexExprVars b
  CFG.ITuple es -> concatMap indexExprVars es
  CFG.IProj _ a -> indexExprVars a
  CFG.IFlatToNd a b -> indexExprVars a ++ indexExprVars b
  CFG.INdToFlat a b -> indexExprVars a ++ indexExprVars b
  CFG.ICall _ es -> concatMap indexExprVars es

-- ---------------------------------------------------------------------------
-- Temporal-loop code path: an `iterate` repeats one GPU map kernel N times,
-- feeding each step's output back as the next step's input (Jacobi/PDE-style
-- relaxation). Lowered to a host loop of dispatches over two ping-pong buffers;
-- only the final buffer is read back. Sibling of Layer A: a bounded host loop
-- around a single kernel rather than a chain of distinct kernels.
-- ---------------------------------------------------------------------------

-- | Split an iterate body into (step work, trailing ping-pong swap glue). The
-- swap is the maximal suffix of array-alias assignments (@x = y@) the lowering
-- emits to rotate the current/next/temp buffers each iteration.
splitSwapGlue :: [CFG.Stmt] -> ([CFG.Stmt], [CFG.Stmt])
splitSwapGlue stmts =
  let isAlias (CFG.SAssign _ (RAtom (AVar _))) = True
      isAlias _ = False
      (revSwap, revWork) = span isAlias (reverse stmts)
   in (reverse revWork, reverse revSwap)

-- | Lower a proc whose body is a temporal @iterate@ loop around one GPU map
-- kernel to a host loop of dispatches over two alternating buffers.
codegenMSLTemporal :: CFG.Program -> CFG.Proc -> Either String MSLArtifacts
codegenMSLTemporal prog@(CFG.Program procs) rp = do
  let body = CFG.procBody rp
      (bodyNoRet, retAtom) = splitFinalReturn body
      indexed = zip [0 ..] bodyNoRet
  (iterIdx, iterSpec, iterBody) <-
    maybe (Left "MSL temporal: no iterate loop found") Right $
      listToMaybe
        [ (i, spec, b)
        | (i, CFG.SLoop spec b) <- indexed,
          CFG.lsRole spec == CFG.LoopIterate
        ]
  iterCount <-
    maybe (Left "MSL temporal: iterate loop has no bound") Right $
      listToMaybe (CFG.lsBounds iterSpec)
  let (kernelWork, swapGlue) = splitSwapGlue iterBody
      swapVars = Set.fromList [v | CFG.SAssign v (RAtom (AVar _)) <- swapGlue]
      preGlueAll = [s | (i, s) <- indexed, i < iterIdx]
      postGlueAll = [s | (i, s) <- indexed, i > iterIdx]
  nextVar <-
    case Set.toList (collectWrittenArrays kernelWork) of
      [v] -> Right v
      vs -> Left ("MSL temporal: step must write exactly one array, got " ++ show vs)
  -- The current buffer is the ping-pong array the step reads.
  curVar <-
    case [v | v <- Set.toList (collectReadArrays kernelWork), v `Set.member` swapVars] of
      (v : _) -> Right v
      [] -> Left "MSL temporal: step reads no ping-pong buffer"
  -- The initial array seeds both buffers: a stable (non-ping-pong) array fed
  -- into a swap buffer in the pre-glue (the lowering copy-propagates, so this
  -- may appear as either an alias or an array copy).
  initArrVar <-
    case [ src
         | CFG.SAssign v rhs <- preGlueAll,
           v `Set.member` swapVars,
           src <- arraySourceOf rhs,
           src `Set.notMember` swapVars
         ] of
      (s : _) -> Right s
      [] -> Left "MSL temporal: could not identify the initial array"
  -- Analyze the step as an ordinary kernel. The kernel-param classifier only
  -- recognizes an input array that is a zero-arg call or a pre-loop alloc, and
  -- an output that is a pre-loop alloc written in the loop. The ping-pong cur/
  -- next buffers are aliases of the initial array, so we re-introduce them as
  -- explicit allocs (read-only `cur`, written `next`) over the initial array's
  -- shape; the real buffers are managed by the harness, so the alloc shape only
  -- drives classification.
  let shapeVar = BS.pack "__hyd_iter_shape"
      stepPreGlue =
        filter (not . assignsTo swapVars) preGlueAll
          ++ [ CFG.SAssign shapeVar (RArrayShape (AVar initArrVar)),
               CFG.SAssign curVar (RArrayAlloc (AVar shapeVar)),
               CFG.SAssign nextVar (RArrayAlloc (AVar shapeVar))
             ]
      stepProc = rp {CFG.procBody = stepPreGlue ++ kernelWork ++ [CFG.SReturn (AVar nextVar)]}
  ka <- analyzeOneKernel prog stepProc
  -- Keep to the simple, well-understood shape: one ping-pong input that is the
  -- current buffer, one output that is the next buffer, no extra static inputs.
  () <-
    case (kaInputArrays ka, kaOutputArrays ka) of
      ([inV], [outV]) | inV == curVar && outV == nextVar -> Right ()
      (ins, outs) ->
        Left
          ( "MSL temporal: unsupported step shape (inputs="
              ++ show ins
              ++ ", outputs="
              ++ show outs
              ++ "); expected one ping-pong input and one output"
          )
  let retKinds = procReturnKinds prog
  helperC <- genHelperC procs (CFG.procName rp) retKinds
  let kernelSrc =
        genMSLKernelSrc
          (kaName ka)
          (kaLoopSpec ka)
          (kaLoopBody ka)
          (kaTypeEnv ka)
          (kaArrayElemTys ka)
          (kaInputArrays ka)
          (kaOutputArrays ka)
          (kaScalarInputs ka)
          (kaHoistedCallMap ka)
          (kaPreLoopAliases ka)
      harnessSrc =
        genTemporalHarness
          ka
          curVar
          nextVar
          initArrVar
          iterCount
          postGlueAll
          retAtom
          swapVars
          retKinds
          helperC
  Right
    MSLArtifacts
      { mslKernelSource = kernelSrc,
        mslHarnessSource = harnessSrc,
        mslHeaderSource = Nothing,
        mslWarnings = fp32DemotionWarning [ka]
      }
  where
    assignsTo vs (CFG.SAssign v _) = v `Set.member` vs
    assignsTo _ _ = False
    arraySourceOf (RAtom (AVar s)) = [s]
    arraySourceOf (RArrayCopy (AVar s)) = [s]
    arraySourceOf _ = []

-- | Array variables loaded (@RArrayLoad@) anywhere in a statement list,
-- descending into nested loops and branches.
collectReadArrays :: [CFG.Stmt] -> Set CVar
collectReadArrays = foldMap go
  where
    go (CFG.SAssign _ (RArrayLoad (AVar v) _)) = Set.singleton v
    go (CFG.SLoop _ body) = collectReadArrays body
    go (CFG.SIf _ thn els) = collectReadArrays thn `Set.union` collectReadArrays els
    go _ = Set.empty

-- | Harness for a temporal @iterate@: seed two ping-pong buffers from the
-- initial array, dispatch the step kernel @N@ times swapping cur/next, read
-- the final buffer back, then run trailing CPU work (a CSV write, if any).
-- Buffer layout matches the single-input kernel: 0=cur data, 1=cur shape,
-- 2=next data, 3.. = scalar inputs.
genTemporalHarness ::
  KernelAnalysis ->
  -- | current / next / initial-source array vars
  CVar ->
  CVar ->
  CVar ->
  -- | iteration count
  CFG.IndexExpr ->
  -- | post-glue (raw; only aliases + CSV writes are emitted)
  [CFG.Stmt] ->
  -- | return atom
  Maybe Atom ->
  -- | swap vars (cur/next/tmp)
  Set CVar ->
  Map CVar VarKind ->
  String ->
  String
genTemporalHarness ka curVar nextVar initArrVar iterCount postGlue retAtom swapVars retKinds helperC =
  unlines $
    [ "// Hydrangea Metal harness (temporal iterate) — generated by the MSL backend.",
      "#include \"hydrangea_runtime.h\"",
      "#include <stdio.h>",
      "#include <string.h>",
      "#include <stdlib.h>",
      "#import <Foundation/Foundation.h>",
      "#import <Metal/Metal.h>",
      "",
      "// ---- CPU helper procs ----",
      helperC,
      "",
      "int main(int argc, const char* argv[]) {",
      "    @autoreleasepool {",
      "    const char* _metallib = argc > 1 ? argv[1] : \"kernel.metallib\";",
      "    // --- Proc parameters (defaults) ---"
    ]
      ++ procParamLines
      ++ ["    // --- CPU pre-loop setup (scalars, shapes, initial array) ---"]
      ++ preLoopLines
      ++ [ "",
           "    long _n = " ++ gridSizeExpr ++ ";",
           "    id<MTLDevice> _dev = MTLCreateSystemDefaultDevice();",
           "    if (!_dev) { fprintf(stderr, \"hydrangea: Metal not available\\n\"); return 1; }",
           "    NSError* _err = nil;",
           "    NSURL* _libURL = [NSURL fileURLWithPath:[NSString stringWithUTF8String:_metallib]];",
           "    id<MTLLibrary> _lib = [_dev newLibraryWithURL:_libURL error:&_err];",
           "    if (!_lib) { fprintf(stderr, \"hydrangea: failed to load metallib: %s\\n\", [_err.localizedDescription UTF8String]); return 1; }",
           "    id<MTLFunction> _fn = [_lib newFunctionWithName:@\"" ++ knm ++ "\"];",
           "    if (!_fn) { fprintf(stderr, \"hydrangea: kernel " ++ knm ++ " not found\\n\"); return 1; }",
           "    id<MTLComputePipelineState> _pso = [_dev newComputePipelineStateWithFunction:_fn error:&_err];",
           "    if (!_pso) { fprintf(stderr, \"hydrangea: pipeline error: %s\\n\", [_err.localizedDescription UTF8String]); return 1; }",
           "    id<MTLCommandQueue> _queue = [_dev newCommandQueue];",
           "",
           "    // --- Two ping-pong buffers, both seeded from the initial array ---",
           "    long _elemN = hyd_shape_size(" ++ san initArrVar ++ "->shape);",
           "    size_t _bufsz = (size_t)(_elemN * sizeof(" ++ elemTy ++ "));",
           "    id<MTLBuffer> _bufA = [_dev newBufferWithLength:_bufsz options:MTLResourceStorageModeShared];",
           "    id<MTLBuffer> _bufB = [_dev newBufferWithLength:_bufsz options:MTLResourceStorageModeShared];"
         ]
      ++ seedLines "_bufA"
      ++ seedLines "_bufB"
      ++ [ "    id<MTLBuffer> _bufShape = [_dev newBufferWithBytes:&" ++ san initArrVar ++ "->shape length:sizeof(hyd_tuple_t) options:MTLResourceStorageModeShared];",
           "    id<MTLBuffer> _bufCur = _bufA;",
           "    id<MTLBuffer> _bufNext = _bufB;"
         ]
      ++ scalarDecls
      ++ [ "    // --- Temporal loop: dispatch the step kernel, swap, repeat ---",
           "    for (long _it = 0; _it < (long)(" ++ iterCountExpr ++ "); _it++) {",
           "      id<MTLCommandBuffer> _cmd = [_queue commandBuffer];",
           "      id<MTLComputeCommandEncoder> _enc = [_cmd computeCommandEncoder];",
           "      [_enc setComputePipelineState:_pso];",
           "      [_enc setBuffer:_bufCur offset:0 atIndex:0];",
           "      [_enc setBuffer:_bufShape offset:0 atIndex:1];",
           "      [_enc setBuffer:_bufNext offset:0 atIndex:2];"
         ]
      ++ scalarSet
      ++ [ "      NSUInteger _tgs = MIN(256UL, _pso.maxTotalThreadsPerThreadgroup); if (_tgs==0) _tgs=1;",
           "      [_enc dispatchThreads:MTLSizeMake((NSUInteger)_n,1,1) threadsPerThreadgroup:MTLSizeMake(_tgs,1,1)];",
           "      [_enc endEncoding]; [_cmd commit]; [_cmd waitUntilCompleted];",
           "      id<MTLBuffer> _swap = _bufCur; _bufCur = _bufNext; _bufNext = _swap;",
           "    }",
           "",
           "    // --- Read the final buffer back into the result array ---",
           "    hyd_array_t* " ++ san curVar ++ " = hyd_array_alloc(" ++ san initArrVar ++ "->shape);"
         ]
      ++ readbackLines
      ++ postLines
      ++ printReturnLines
      ++ [ "    } // @autoreleasepool",
           "    return 0;",
           "}"
         ]
  where
    knm = sanitize (kaName ka)
    typeEnv = kaTypeEnv ka
    arrayElemTys = kaArrayElemTys ka
    san = sanitize
    elemCType = case Map.lookup curVar arrayElemTys of
      Just t -> t
      Nothing -> case Map.lookup curVar typeEnv of
        Just (CTArray t) -> t
        _ -> CTDouble
    elemTy = mslTypeName elemCType
    scalars = kaScalarInputs ka
    scalarTyOf v = case Map.lookup v typeEnv of Just t -> mslTypeName t; _ -> "long"

    procParamLines = concatMap declProcParam (kaProcParams ka)
    declProcParam v =
      let ty = Map.findWithDefault CTInt64 v typeEnv
          defVal = case ty of
            CTDouble -> "0.0"
            CTBool -> "0"
            CTArray _ -> "NULL"
            CTPair _ _ -> "((" ++ cTypeName ty ++ "){0})"
            _ -> "0LL"
       in ["    " ++ cTypeName ty ++ " " ++ san v ++ " = " ++ defVal ++ ";"]

    -- Host setup: the kernel's effective pre-loop, which besides the original
    -- pre-glue also holds any loop-invariant step preamble (e.g. a shape read
    -- of the current buffer). References to the ping-pong buffers are redirected
    -- to the initial array (same shape/contents on entry); their alloc stubs are
    -- dropped since the harness owns the real Metal buffers.
    hostPreLoop =
      substStmts
        (Map.fromList [(v, AVar initArrVar) | v <- Set.toList swapVars])
        (kaEffectivePreLoop ka)
    preLoopLines =
      concatMap
        (genPreLoopLine typeEnv retKinds (Set.fromList [curVar, nextVar]) Set.empty)
        hostPreLoop

    gridSizeExpr = case CFG.lsBounds (kaLoopSpec ka) of
      [b] -> "(long)" ++ genMSLIndexExpr b
      bs -> intercalate " * " ["(long)" ++ genMSLIndexExpr b | b <- bs]
    iterCountExpr = genMSLIndexExpr iterCount

    seedLines buf
      | elemTy == "float" =
          [ "    { double* _s = (double*)" ++ san initArrVar ++ "->data; float* _d = (float*)" ++ buf ++ ".contents;",
            "      for (long _i = 0; _i < _elemN; _i++) _d[_i] = (float)_s[_i]; }"
          ]
      | otherwise =
          ["    memcpy(" ++ buf ++ ".contents, " ++ san initArrVar ++ "->data, _bufsz);"]

    scalarDecls =
      ["    " ++ scalarTyOf v ++ " _sc" ++ show i ++ " = " ++ san v ++ ";" | (v, i) <- zip scalars [0 :: Int ..]]
    scalarSet =
      [ "      [_enc setBytes:&_sc" ++ show i ++ " length:sizeof(" ++ scalarTyOf v ++ ") atIndex:" ++ show (3 + i) ++ "];"
      | (v, i) <- zip scalars [0 :: Int ..]
      ]

    readbackLines
      | elemTy == "float" =
          [ "    { float* _gs = (float*)_bufCur.contents; double* _gd = (double*)" ++ san curVar ++ "->data;",
            "      for (long _i = 0; _i < _elemN; _i++) _gd[_i] = (double)_gs[_i]; }"
          ]
      | otherwise =
          ["    memcpy(" ++ san curVar ++ "->data, _bufCur.contents, _bufsz);"]

    -- Trailing CPU work: only array-alias bindings (e.g. `result = cur`) and CSV
    -- writes. Iterate bookkeeping (frees, alias conditionals) is skipped — the
    -- GPU manages the buffers.
    postLines = concatMap emitPost postGlue
    emitPost s = case s of
      CFG.SAssign _ (RCall fn _) | isFileWriteBuiltin fn -> genPreLoopLine typeEnv retKinds Set.empty Set.empty s
      CFG.SAssign _ (RAtom (AVar _)) -> genPreLoopLine typeEnv retKinds Set.empty Set.empty s
      _ -> []

    printReturnLines = case retAtom of
      Just AUnit -> []
      Just (AVar v) -> case Map.lookup v typeEnv of
        Just (CTArray elt) -> tempArrayPrint v elt
        Just CTDouble -> ["    printf(\"%.17g\\n\", (double)" ++ san v ++ ");"]
        _ -> ["    printf(\"%ld\\n\", (long)" ++ san v ++ ");"]
      Just (AInt n) -> ["    printf(\"%ld\\n\", (long)" ++ show n ++ "LL);"]
      Just (AFloat f) -> ["    printf(\"%.17g\\n\", (double)" ++ show f ++ ");"]
      _ -> []

    -- Print a CPU result array. Float elements are stored as 8-byte doubles in
    -- the host @hyd_array_t@, so read them as @double@ (not the GPU's @float@).
    tempArrayPrint v elt =
      let isF = elt == CTDouble
          cty = if isF then "double" else "int64_t"
          fmt = if isF then "%.17g" else "%ld"
          cast = if isF then "(double)" else "(long)"
       in [ "    { " ++ cty ++ "* _rp = (" ++ cty ++ "*)" ++ san v ++ "->data; long _rn = hyd_shape_size(" ++ san v ++ "->shape);",
            "      printf(\"[\"); for (long _pi = 0; _pi < _rn; _pi++) { if (_pi>0) printf(\", \"); printf(\"" ++ fmt ++ "\", " ++ cast ++ "_rp[_pi]); } printf(\"]\\n\"); }"
          ]

-- | Generate the harness for a multi-phase proc: walk the body in order
-- (descending through @SIf@ guards, which become host-side conditionals around
-- their dispatches), running scalar/shape glue on the host, allocating a Metal
-- buffer per GPU array (shared across phases by name), uploading any external
-- CPU input arrays, dispatching each GPU loop as a kernel, then reading back the
-- result, finishing on the CPU, and printing.
genMultiPhaseHarness ::
  CFG.Proc ->
  -- | bodyNoRet (top-level statements, return split off)
  [CFG.Stmt] ->
  -- | phases (in pre-order, aligned with flattenPhaseToks)
  [KernelAnalysis] ->
  -- | return atom
  Maybe Atom ->
  TypeEnv ->
  Map CVar CType ->
  Map CVar VarKind ->
  -- | helper C source
  String ->
  String
genMultiPhaseHarness _rp bodyNoRet phases retAtom typeEnv arrayElemTys retKinds helperC =
  unlines $
    [ "// Hydrangea Metal harness (multi-phase) — generated by the MSL backend.",
      "#include \"hydrangea_runtime.h\"",
      "#include <stdio.h>",
      "#include <string.h>",
      "#include <stdlib.h>",
      "#import <Foundation/Foundation.h>",
      "#import <Metal/Metal.h>",
      "",
      "// ---- CPU helper procs ----",
      helperC,
      "",
      "// ---- Metal harness main ----",
      "int main(int argc, const char* argv[]) {",
      "    @autoreleasepool {",
      "    const char* _metallib = argc > 1 ? argv[1] : \"kernel.metallib\";",
      "    id<MTLDevice> _dev = MTLCreateSystemDefaultDevice();",
      "    if (!_dev) { fprintf(stderr, \"hydrangea: Metal not available\\n\"); return 1; }",
      "    NSError* _err = nil;",
      "    NSURL* _libURL = [NSURL fileURLWithPath:[NSString stringWithUTF8String:_metallib]];",
      "    id<MTLLibrary> _lib = [_dev newLibraryWithURL:_libURL error:&_err];",
      "    if (!_lib) { fprintf(stderr, \"hydrangea: failed to load metallib: %s\\n\", [_err.localizedDescription UTF8String]); return 1; }",
      "    id<MTLCommandQueue> _queue = [_dev newCommandQueue];",
      ""
    ]
      ++ concatMap genPSO phases
      ++ [""]
      ++ glueDecls
      ++ concatMap emitGlue setupStmts
      ++ concatMap uploadExternalInput externalInputs
      ++ [""]
      ++ fst (emitItems (0, Set.empty) (phaseStmts ++ postStmts))
      ++ printReturnLines
      ++ [ "    } // @autoreleasepool",
           "    return 0;",
           "}"
         ]
  where
    indexedTop = zip [0 :: Int ..] bodyNoRet
    containsPhase s = not (null (gpuLoopsDeep [s]))
    phaseTopIdxs = [j | (j, s) <- indexedTop, containsPhase s]
    firstPhaseTop = if null phaseTopIdxs then maxBound else minimum phaseTopIdxs
    lastPhaseTop = if null phaseTopIdxs then -1 else maximum phaseTopIdxs
    -- Host glue before the first dispatch (where external inputs are produced),
    -- the phase region (dispatches + interleaved glue + SIf guards), and the
    -- trailing CPU work after the last dispatch.
    setupStmts = [s | (j, s) <- indexedTop, j < firstPhaseTop]
    phaseStmts = [s | (j, s) <- indexedTop, j >= firstPhaseTop, j <= lastPhaseTop]
    postStmts = [s | (j, s) <- indexedTop, j > lastPhaseTop]

    gpuArrays = nub (concatMap (\p -> kaInputArrays p ++ kaOutputArrays p) phases)
    gpuArraySet = Set.fromList gpuArrays
    phaseOutputs = nub (concatMap kaOutputArrays phases)
    -- Arrays the harness allocates as GPU-resident buffers (alloc'd, not uploaded).
    gpuAllocedArrays =
      Set.fromList [v | CFG.SAssign v (RArrayAlloc _) <- allBodyStmts, v `Set.member` gpuArraySet]
    allBodyStmts = flattenAllStmts bodyNoRet
    -- External CPU input arrays: read by a kernel but neither produced by a phase
    -- nor allocated as a GPU buffer — they must be computed on the host and
    -- uploaded (e.g. an array read from CSV feeding a stencil).
    externalInputs =
      [ v
      | v <- nub (concatMap kaInputArrays phases),
        v `notElem` phaseOutputs,
        v `Set.notMember` gpuAllocedArrays
      ]
    -- Walk the phase region and trailing CPU work, threading (next phase index,
    -- arrays already read back). Emits host glue, host `if`/`else` around SIf
    -- guards, a dispatch per GPU loop (phases in pre-order), and — before any
    -- CPU statement that consumes a phase output's data (e.g. a CSV write) — a
    -- readback of that output. Outputs consumed only by later kernels stay
    -- GPU-resident (kernels read them via buffers, not glue references).
    emitItems :: (Int, Set CVar) -> [CFG.Stmt] -> ([String], (Int, Set CVar))
    emitItems st [] = ([], st)
    emitItems (n, rb) (s : rest) = case s of
      CFG.SLoop spec _ | isGpuLoopSpec spec ->
        let (rs, st') = emitItems (n + 1, rb) rest
         in (genDispatch (phases !! n) ++ rs, st')
      CFG.SIf cond thn els ->
        let (tl, st1) = emitItems (n, rb) thn
            (el, st2) = emitItems st1 els
            blk =
              ["    if (" ++ genCAtom cond ++ ") {"]
                ++ tl
                ++ (if null els then ["    }"] else ["    } else {"] ++ el ++ ["    }"])
            (rs, st3) = emitItems st2 rest
         in (blk ++ rs, st3)
      _ ->
        let used = usedVarsStmts [s]
            newRb = [v | v <- phaseOutputs, v `Set.member` used, v `Set.notMember` rb]
            rb' = rb `Set.union` Set.fromList newRb
            (rs, st') = emitItems (n, rb') rest
         in (concatMap genReadback newRb ++ emitGlue s ++ rs, st')

    -- The clamped stencil reuses scalar glue var names across boundary regions
    -- that were separate lexical scopes in the source. Flattened into one C
    -- function they collide, so pre-declare every glue var once at the top and
    -- emit plain assignments in the body (matching the C backend).
    glueStmtsAll = flattenAllStmts (setupStmts ++ phaseStmts ++ postStmts)
    -- Exclude GPU arrays (buffers/stubs and on-demand readback arrays, which are
    -- declared by genBufferAlloc / genReadback) — only scalar/tuple glue is
    -- pre-declared here.
    glueVarList =
      nub [v | CFG.SAssign v _ <- glueStmtsAll, v `Set.notMember` gpuArraySet]
    glueVarSet = Set.fromList glueVarList
    glueDecls =
      ["    " ++ preLoopCTypeVar typeEnv retKinds v glueStmtsAll ++ " " ++ sanitize v ++ ";" | v <- glueVarList]

    emitGlue s = case s of
      CFG.SAssign v (RArrayAlloc shp)
        | v `Set.member` gpuArraySet -> genBufferAlloc v shp
      _ -> genPreLoopLine typeEnv retKinds gpuArraySet glueVarSet s

    -- Upload an external CPU input array to a Metal buffer (data + shape).
    uploadExternalInput v =
      let elemTy = mslElemTy v
          nm = sanitize v
          fill
            | elemTy == "float" =
                [ "    { double* _us = (double*)" ++ nm ++ "->data; float* _ud = (float*)_buf_" ++ nm ++ ".contents;",
                  "      for (long _ui = 0; _ui < _inN_" ++ nm ++ "; _ui++) _ud[_ui] = (float)_us[_ui]; }"
                ]
            | otherwise =
                ["    memcpy(_buf_" ++ nm ++ ".contents, " ++ nm ++ "->data, _bufsz_" ++ nm ++ ");"]
       in [ "    // upload external input " ++ nm,
            "    long _inN_" ++ nm ++ " = hyd_shape_size(" ++ nm ++ "->shape);",
            "    size_t _bufsz_" ++ nm ++ " = (size_t)(_inN_" ++ nm ++ " * sizeof(" ++ elemTy ++ "));",
            "    id<MTLBuffer> _buf_" ++ nm ++ " = [_dev newBufferWithLength:_bufsz_" ++ nm ++ " options:MTLResourceStorageModeShared];"
          ]
            ++ fill
            ++ ["    id<MTLBuffer> _bufshape_" ++ nm ++ " = [_dev newBufferWithBytes:&" ++ nm ++ "->shape length:sizeof(hyd_tuple_t) options:MTLResourceStorageModeShared];"]

    mslElemTy v = case Map.lookup v arrayElemTys of
      Just elt -> mslTypeName elt
      Nothing -> case Map.lookup v typeEnv of
        Just (CTArray elt) -> mslTypeName elt
        _ -> "long"
    scalarTyOf v = case Map.lookup v typeEnv of
      Just ct -> mslTypeName ct
      _ -> "long"

    genPSO p =
      let knm = sanitize (kaName p)
       in [ "    id<MTLFunction> _fn_" ++ knm ++ " = [_lib newFunctionWithName:@\"" ++ knm ++ "\"];",
            "    if (!_fn_" ++ knm ++ ") { fprintf(stderr, \"hydrangea: kernel " ++ knm ++ " not found\\n\"); return 1; }",
            "    id<MTLComputePipelineState> _pso_" ++ knm ++ " = [_dev newComputePipelineStateWithFunction:_fn_" ++ knm ++ " error:&_err];",
            "    if (!_pso_" ++ knm ++ ") { fprintf(stderr, \"hydrangea: pipeline error: %s\\n\", [_err.localizedDescription UTF8String]); return 1; }"
          ]

    genBufferAlloc v shp =
      let elemTy = mslElemTy v
          nm = sanitize v
          szExpr = "(size_t)(" ++ nm ++ "_n * sizeof(" ++ elemTy ++ "))"
       in [ "    hyd_tuple_t " ++ nm ++ "_shape = " ++ genCAtom shp ++ ";",
            "    long " ++ nm ++ "_n = hyd_shape_size(" ++ nm ++ "_shape);",
            "    id<MTLBuffer> _buf_" ++ nm ++ " = [_dev newBufferWithLength:" ++ szExpr ++ " options:MTLResourceStorageModeShared];",
            "    memset(_buf_" ++ nm ++ ".contents, 0, " ++ szExpr ++ ");",
            "    id<MTLBuffer> _bufshape_" ++ nm ++ " = [_dev newBufferWithBytes:&" ++ nm ++ "_shape length:sizeof(hyd_tuple_t) options:MTLResourceStorageModeShared];",
            "    hyd_array_t " ++ nm ++ "_stub = { .shape = " ++ nm ++ "_shape, .data = NULL };",
            "    hyd_array_t* " ++ nm ++ " = &" ++ nm ++ "_stub;"
          ]

    genDispatch p =
      let knm = sanitize (kaName p)
          ins = kaInputArrays p
          outs = kaOutputArrays p
          scals = kaScalarInputs p
          nIn = length ins
          inB = ["      [_enc setBuffer:_buf_" ++ sanitize v ++ " offset:0 atIndex:" ++ show i ++ "];" | (v, i) <- zip ins [0 ..]]
          shB = ["      [_enc setBuffer:_bufshape_" ++ sanitize v ++ " offset:0 atIndex:" ++ show i ++ "];" | (v, i) <- zip ins [nIn ..]]
          outB = ["      [_enc setBuffer:_buf_" ++ sanitize v ++ " offset:0 atIndex:" ++ show i ++ "];" | (v, i) <- zip outs [nIn * 2 ..]]
          scB =
            [ "      { " ++ scalarTyOf v ++ " _s = " ++ sanitize v ++ "; [_enc setBytes:&_s length:sizeof(" ++ scalarTyOf v ++ ") atIndex:" ++ show i ++ "]; }"
            | (v, i) <- zip scals [nIn * 2 + length outs ..]
            ]
          gridExpr = case CFG.lsBounds (kaLoopSpec p) of
            [b] -> "(long)" ++ genMSLIndexExpr b
            bs -> intercalate " * " ["(long)" ++ genMSLIndexExpr b | b <- bs]
       in [ "    // --- phase " ++ knm ++ " ---",
            "    { id<MTLCommandBuffer> _cmd = [_queue commandBuffer];",
            "      id<MTLComputeCommandEncoder> _enc = [_cmd computeCommandEncoder];",
            "      [_enc setComputePipelineState:_pso_" ++ knm ++ "];"
          ]
            ++ inB
            ++ shB
            ++ outB
            ++ scB
            ++ [ "      long _gn = " ++ gridExpr ++ ";",
                 "      NSUInteger _tgs = MIN(256UL, _pso_" ++ knm ++ ".maxTotalThreadsPerThreadgroup); if (_tgs==0) _tgs=1;",
                 "      [_enc dispatchThreads:MTLSizeMake((NSUInteger)_gn,1,1) threadsPerThreadgroup:MTLSizeMake(_tgs,1,1)];",
                 "      [_enc endEncoding]; [_cmd commit]; [_cmd waitUntilCompleted]; }"
               ]

    genReadback v =
      let elemTy = mslElemTy v
          nm = sanitize v
       in [ "    // read back GPU output " ++ nm,
            "    " ++ nm ++ " = hyd_array_alloc(" ++ nm ++ "_shape);"
          ]
            ++ ( if elemTy == "float"
                   then
                     [ "    { float* _gs = (float*)_buf_" ++ nm ++ ".contents; double* _gd = (double*)" ++ nm ++ "->data;",
                       "      for (long _i = 0; _i < " ++ nm ++ "_n; _i++) _gd[_i] = (double)_gs[_i]; }"
                     ]
                   else
                     ["    memcpy(" ++ nm ++ "->data, _buf_" ++ nm ++ ".contents, (size_t)(" ++ nm ++ "_n * sizeof(" ++ elemTy ++ ")));"]
               )

    printReturnLines = case retAtom of
      Just AUnit -> []
      Just (AVar v) -> case Map.lookup v typeEnv of
        Just (CTArray elt) -> arrayPrint v (mslTypeName elt)
        Just CTDouble -> ["    printf(\"%.17g\\n\", (double)" ++ sanitize v ++ ");"]
        Just _ -> ["    printf(\"%ld\\n\", (long)" ++ sanitize v ++ ");"]
        Nothing -> ["    printf(\"%ld\\n\", (long)" ++ sanitize v ++ ");"]
      Just (AInt n) -> ["    printf(\"%ld\\n\", (long)" ++ show n ++ "LL);"]
      Just (AFloat f) -> ["    printf(\"%.17g\\n\", (double)" ++ show f ++ ");"]
      _ -> []

    arrayPrint v eTy =
      let fmt = if eTy == "float" then "%.17g" else "%ld"
          cast = if eTy == "float" then "(double)" else "(long)"
          nm = sanitize v
       in [ "    { " ++ eTy ++ "* _rp = (" ++ eTy ++ "*)" ++ nm ++ "->data; long _rn = hyd_shape_size(" ++ nm ++ "->shape);",
            "      printf(\"[\"); for (long _pi = 0; _pi < _rn; _pi++) { if (_pi>0) printf(\", \"); printf(\"" ++ fmt ++ "\", " ++ cast ++ "_rp[_pi]); } printf(\"]\\n\"); }"
          ]

-- ---------------------------------------------------------------------------
-- Export-kernel code path
-- ---------------------------------------------------------------------------

-- | Export mode: generate a reusable Metal library with init/dispatch/cleanup.
codegenMSLExport :: MSLOptions -> CFG.Program -> Either String MSLArtifacts
codegenMSLExport opts prog@(CFG.Program procs) = do
  exportName <- case mslExportKernel opts of
    Just n -> Right n
    Nothing -> Left "MSL export: no kernel name specified"
  -- Find the proc to export
  kp <- case filter (\p -> CFG.procName p == exportName) procs of
    (p : _) -> Right p
    [] -> Left $ "MSL export: proc not found: " ++ BS.unpack exportName
  ka <- analyzeOneKernel prog kp
  let retKinds = procReturnKinds prog
      retTypes = inferProgramReturnTypes prog
      callParamTys = buildCallParamTypes retTypes procs
  -- Resolve export spec (reuse CodegenC infrastructure)
  spec <- resolveExportSpec retKinds retTypes callParamTys procs exportName
  -- Build metal export spec with hyd_metal_ prefix
  let metalSpec = spec {exportWrapperName = "hyd_metal_" ++ sanitizeExportName exportName}
  -- Identify which input arrays are cached (zero-arg RCall returning arrays,
  -- not derived from proc params). Only arrays get cached as GPU buffers;
  -- scalar zero-arg calls are just CPU helper functions run each frame.
  let procParamSet = Set.fromList (kaProcParams ka)
      inputArraySet = Set.fromList (kaInputArrays ka)
      cachedArrayBindings =
        [ (v, fn)
        | CFG.SAssign v (RCall fn []) <- kaEffectivePreLoop ka,
          v `Set.member` inputArraySet, -- must be a kernel input array
          v `Set.notMember` procParamSet, -- not a proc param
          -- not derived from a proc param via RProj
          not (any (derivesFromParam procParamSet v) (kaEffectivePreLoop ka))
        ]
  -- Generate .metal kernel source
  helperC <- genHelperC procs (kaName ka) retKinds
  let kernelSrc =
        genMSLKernelSrc
          (kaName ka)
          (kaLoopSpec ka)
          (kaLoopBody ka)
          (kaTypeEnv ka)
          (kaArrayElemTys ka)
          (kaInputArrays ka)
          (kaOutputArrays ka)
          (kaScalarInputs ka)
          (kaHoistedCallMap ka)
          (kaPreLoopAliases ka)
      headerSrc = genMetalExportHeader metalSpec
      harnessSrc =
        genExportObjCHarness
          prog
          ka
          metalSpec
          cachedArrayBindings
          retKinds
          retTypes
          helperC
  Right
    MSLArtifacts
      { mslKernelSource = kernelSrc,
        mslHarnessSource = harnessSrc,
        mslHeaderSource = Just headerSrc,
        mslWarnings = cpuFallbackWarningsSingle ka ++ fp32DemotionWarning [ka]
      }

-- | Check if a variable derives from a proc parameter (e.g., via RProj).
derivesFromParam :: Set CVar -> CVar -> CFG.Stmt -> Bool
derivesFromParam paramSet target (CFG.SAssign v (RProj _ (AVar p))) =
  v == target && p `Set.member` paramSet
derivesFromParam paramSet target (CFG.SAssign v (RAtom (AVar p))) =
  v == target && p `Set.member` paramSet
derivesFromParam _ _ _ = False

-- | Generate the export header file (.h).
genMetalExportHeader :: ExportSpec -> String
genMetalExportHeader spec =
  let guardName = "HYDRANGEA_METAL_EXPORT_" ++ map toUpper (sanitizeExportName (exportKernelName spec)) ++ "_H"
      ps = exportParams spec
      cParams = case ps of
        [] -> "void"
        _ -> intercalate ", " [ct ++ " " ++ cn | (cn, ct) <- ps]
      -- Collect all type strings from return type and params, emit pair struct
      -- typedefs for any that match the hyd_pair_*_t pattern.
      allTypeStrs = exportReturnType spec : map snd ps
      pairDefs =
        nub
          [ genPairTypedef name
          | name <- allTypeStrs,
            "hyd_pair_" `isPrefixOf` name
          ]
   in unlines $
        [ "#ifndef " ++ guardName,
          "#define " ++ guardName,
          "",
          "#include \"hydrangea_runtime.h\"",
          "",
          "#ifdef __cplusplus",
          "extern \"C\" {",
          "#endif"
        ]
          ++ (if null pairDefs then [] else "" : pairDefs)
          ++ [ "",
               "int hyd_metal_init(const char* metallib_path);",
               exportReturnType spec ++ " " ++ exportWrapperName spec ++ "(" ++ cParams ++ ");",
               "void hyd_metal_cleanup(void);",
               "",
               "#ifdef __cplusplus",
               "}",
               "#endif",
               "",
               "#endif"
             ]

-- | Generate a typedef for a pair struct from its name (e.g., "hyd_pair_aa_t").
-- Parses the letter codes between "hyd_pair_" and "_t" to determine field types.
genPairTypedef :: String -> String
genPairTypedef name =
  let -- Extract letter codes: "hyd_pair_XY_t" → "XY"
      stripped = drop 9 name -- drop "hyd_pair_"
      codes = takeWhile (/= '_') stripped
      letterToCType 'a' = "hyd_array_t*"
      letterToCType 'i' = "int64_t"
      letterToCType 'f' = "float"
      letterToCType 'b' = "int64_t"
      letterToCType _ = "int64_t"
   in case codes of
        [c1, c2] ->
          "typedef struct { "
            ++ letterToCType c1
            ++ " fst; "
            ++ letterToCType c2
            ++ " snd; } "
            ++ name
            ++ ";"
        _ -> "/* unknown pair type: " ++ name ++ " */"

-- | Generate the export ObjC harness (.m) with init/dispatch/cleanup lifecycle.
--
-- Architecture:
-- * @hyd_metal_init@: set up Metal device/pipeline/queue, compute and upload
--   cached array bindings (zero-arg array-returning procs) to persistent GPU buffers.
-- * Dispatch function: run pre-loop to compute scalars and decompose params,
--   upload dynamic input arrays, create output/scalar buffers, dispatch, read back.
-- * @hyd_metal_cleanup@: nil out persistent Metal objects.
genExportObjCHarness ::
  CFG.Program ->
  KernelAnalysis ->
  -- | Metal export spec (hyd_metal_ prefix)
  ExportSpec ->
  -- | Cached array bindings: (var, proc_name) pairs
  [(CVar, CVar)] ->
  -- | Return kinds
  Map CVar VarKind ->
  -- | Program-level return types
  Map CVar CType ->
  -- | Helper C source
  String ->
  String
genExportObjCHarness _prog ka spec cachedArrayBindings retKinds _retTypes helperC =
  unlines $
    -- Header
    [ "// Hydrangea Metal export harness — generated by the MSL backend.",
      "#include \"hydrangea_runtime.h\"",
      "#include <stdio.h>",
      "#include <string.h>",
      "#include <stdlib.h>",
      "#import <Foundation/Foundation.h>",
      "#import <Metal/Metal.h>",
      "",
      "// ---- CPU helper procs ----",
      helperC,
      "",
      "// ---- Static Metal state ----",
      "static id<MTLDevice> _hyd_dev;",
      "static id<MTLLibrary> _hyd_lib;",
      "static id<MTLComputePipelineState> _hyd_pso;",
      "static id<MTLCommandQueue> _hyd_queue;"
    ]
      ++
      -- Static cached buffer declarations
      concatMap genCachedBufDecl cachedArrayBindings
      ++ [ "",
           "// ---- hyd_metal_init ----",
           "int hyd_metal_init(const char* metallib_path) {",
           "    @autoreleasepool {",
           "    _hyd_dev = MTLCreateSystemDefaultDevice();",
           "    if (!_hyd_dev) { fprintf(stderr, \"hydrangea: Metal not available\\n\"); return 1; }",
           "    NSError* _err = nil;",
           "    NSURL* _libURL = [NSURL fileURLWithPath:[NSString stringWithUTF8String:metallib_path]];",
           "    _hyd_lib = [_hyd_dev newLibraryWithURL:_libURL error:&_err];",
           "    if (!_hyd_lib) {",
           "        fprintf(stderr, \"hydrangea: failed to load metallib: %s\\n\",",
           "                [_err.localizedDescription UTF8String]);",
           "        return 1;",
           "    }",
           "    id<MTLFunction> _fn = [_hyd_lib newFunctionWithName:@\"" ++ sanitize (kaName ka) ++ "\"];",
           "    if (!_fn) { fprintf(stderr, \"hydrangea: kernel function not found\\n\"); return 1; }",
           "    _hyd_pso = [_hyd_dev newComputePipelineStateWithFunction:_fn error:&_err];",
           "    if (!_hyd_pso) {",
           "        fprintf(stderr, \"hydrangea: pipeline error: %s\\n\",",
           "                [_err.localizedDescription UTF8String]);",
           "        return 1;",
           "    }",
           "    _hyd_queue = [_hyd_dev newCommandQueue];"
         ]
      ++
      -- Compute and upload cached array bindings
      concatMap genCachedBufInit cachedArrayBindings
      ++ [ "    } // @autoreleasepool",
           "    return 0;",
           "}",
           "",
           "// ---- dispatch function ----"
         ]
      ++ genDispatchFn
      ++ [ "",
           "// ---- hyd_metal_cleanup ----",
           "void hyd_metal_cleanup(void) {",
           "    _hyd_pso = nil;",
           "    _hyd_queue = nil;",
           "    _hyd_lib = nil;"
         ]
      ++ concatMap genCachedBufCleanup cachedArrayBindings
      ++ [ "    _hyd_dev = nil;",
           "}"
         ]
  where
    typeEnv = kaTypeEnv ka
    arrayElemTys = kaArrayElemTys ka
    cachedSet = Set.fromList (map fst cachedArrayBindings)
    inputArrs = kaInputArrays ka
    outputArrs = kaOutputArrays ka
    scals = kaScalarInputs ka
    nIn = length inputArrs

    mslElemTy v = case Map.lookup v arrayElemTys of
      Just elt -> mslTypeName elt
      Nothing -> case Map.lookup v typeEnv of
        Just (CTArray elt) -> mslTypeName elt
        _ -> "long"

    -- Static declarations for cached GPU buffers
    genCachedBufDecl (v, _fn) =
      [ "static id<MTLBuffer> _hyd_cached_buf_" ++ sanitize v ++ ";",
        "static hyd_tuple_t _hyd_cached_shape_" ++ sanitize v ++ ";"
      ]

    -- Init: compute cached binding on CPU, upload to persistent GPU buffer
    genCachedBufInit (v, fn) =
      let elemTy = mslElemTy v
       in [ "    // Cached binding: " ++ sanitize v ++ " = " ++ sanitize fn ++ "()",
            "    hyd_array_t* _cpu_" ++ sanitize v ++ " = " ++ sanitize fn ++ "();",
            "    _hyd_cached_shape_" ++ sanitize v ++ " = _cpu_" ++ sanitize v ++ "->shape;",
            "    long _cachedN_" ++ sanitize v ++ " = hyd_shape_size(_cpu_" ++ sanitize v ++ "->shape);",
            "    size_t _cachedSz_"
              ++ sanitize v
              ++ " = (size_t)(_cachedN_"
              ++ sanitize v
              ++ " * sizeof("
              ++ elemTy
              ++ "));",
            "    _hyd_cached_buf_"
              ++ sanitize v
              ++ " = [_hyd_dev newBufferWithLength:_cachedSz_"
              ++ sanitize v
              ++ " options:MTLResourceStorageModeShared];"
          ]
            ++ uploadCachedBuf v elemTy
            ++ [ "    hyd_array_free(_cpu_" ++ sanitize v ++ ");"
               ]

    uploadCachedBuf v "float" =
      [ "    { double* _src = (double*)_cpu_" ++ sanitize v ++ "->data;",
        "      float* _dst = (float*)_hyd_cached_buf_" ++ sanitize v ++ ".contents;",
        "      for (long _i = 0; _i < _cachedN_" ++ sanitize v ++ "; _i++) _dst[_i] = (float)_src[_i]; }"
      ]
    uploadCachedBuf v _ =
      [ "    memcpy(_hyd_cached_buf_"
          ++ sanitize v
          ++ ".contents, _cpu_"
          ++ sanitize v
          ++ "->data, _cachedSz_"
          ++ sanitize v
          ++ ");"
      ]

    genCachedBufCleanup (v, _fn) =
      ["    _hyd_cached_buf_" ++ sanitize v ++ " = nil;"]

    -- The dispatch function runs the pre-loop (for scalars + pair decomposition),
    -- then creates Metal buffers, dispatches, and reads back output.
    genDispatchFn :: [String]
    genDispatchFn =
      let ps = exportParams spec
          cParams = case ps of
            [] -> "void"
            _ -> intercalate ", " [ct ++ " " ++ cn | (cn, ct) <- ps]
       in [ exportReturnType spec ++ " " ++ exportWrapperName spec ++ "(" ++ cParams ++ ") {",
            "    @autoreleasepool {"
          ]
            ++
            -- Run pre-loop: computes scalars, decomposes pairs, allocates output arrays.
            -- Cached array RCalls are replaced with shape stubs (data comes from GPU buffer).
            -- Output array allocations are skipped (replaced by Metal buffers).
            concatMap (genExportPreLoopLine (Set.fromList outputArrs) cachedSet) (kaEffectivePreLoop ka)
            ++ [ "",
                 "    // Grid size",
                 "    long _n = " ++ gridSizeExpr ++ ";",
                 "",
                 "    // --- Create Metal buffers ---"
               ]
            ++
            -- Input buffers (cached or dynamic)
            concatMap genInputBuf (zip inputArrs [0 ..])
            ++
            -- Shape buffers
            concatMap genInputShapeBuf (zip inputArrs [nIn ..])
            ++
            -- Output buffers
            concatMap genOutputBuf (zip outputArrs [nIn * 2 ..])
            ++
            -- Scalar buffers
            concatMap genScalarBuf (zip scals [nIn * 2 + length outputArrs ..])
            ++ [ "",
                 "    // --- Dispatch ---",
                 "    id<MTLCommandBuffer> _cmd = [_hyd_queue commandBuffer];",
                 "    id<MTLComputeCommandEncoder> _enc = [_cmd computeCommandEncoder];",
                 "    [_enc setComputePipelineState:_hyd_pso];"
               ]
            ++
            -- setBuffer for array data, shape, and output buffers
            [ "    [_enc setBuffer:_buf" ++ show i ++ " offset:0 atIndex:" ++ show i ++ "];"
            | i <- [0 .. nIn * 2 + length outputArrs - 1]
            ]
            ++
            -- setBytes for scalar constants (no buffer allocation needed)
            [ let sty = case Map.lookup v typeEnv of
                    Just ct -> mslTypeName ct
                    Nothing -> "long"
               in "    [_enc setBytes:&_scalar" ++ show i ++ " length:sizeof(" ++ sty ++ ") atIndex:" ++ show i ++ "];"
            | (v, i) <- zip scals [nIn * 2 + length outputArrs ..]
            ]
            ++ [ "    NSUInteger _tgs = MIN(256UL, _hyd_pso.maxTotalThreadsPerThreadgroup);",
                 "    if (_tgs == 0) _tgs = 1;",
                 "    [_enc dispatchThreads:MTLSizeMake((NSUInteger)_n, 1, 1)",
                 "      threadsPerThreadgroup:MTLSizeMake(_tgs, 1, 1)];",
                 "    [_enc endEncoding];",
                 "    [_cmd commit];",
                 "    [_cmd waitUntilCompleted];",
                 ""
               ]
            ++
            -- Read back outputs and construct return value
            genReadback
            ++
            -- Post-loop statements (dummy array allocations, reshaping, etc.)
            -- Skip RPairMake (handled by return construction) and reshape_view
            -- (handled by readback shape logic).
            concatMap genPostLoopLine (kaPostLoopStmts ka)
            ++ genReturnConstruction
            ++ [ "    } // @autoreleasepool",
                 "}"
               ]

    -- Pre-loop line for export dispatch: like genPreLoopLine but:
    -- 1. Skips output array allocations (Metal buffers replace them)
    -- 2. For cached array RCalls: emits a shape-only stub (GPU buffer has data)
    genExportPreLoopLine :: Set CVar -> Set CVar -> CFG.Stmt -> [String]
    genExportPreLoopLine skipAlloc cachedVars stmt = case stmt of
      CFG.SAssign v (RArrayAlloc _)
        | v `Set.member` skipAlloc ->
            [] -- output arrays: Metal buffer, skip CPU alloc
      CFG.SAssign v (RCall fn [])
        | v `Set.member` cachedVars ->
            -- Cached array: use shape stub so downstream shape accesses work
            [ "    // Cached array (GPU buffer): " ++ sanitize v,
              "    hyd_array_t _stub_" ++ sanitize v ++ " = { .shape = _hyd_cached_shape_" ++ sanitize v ++ ", .data = NULL };",
              "    hyd_array_t* " ++ sanitize v ++ " = &_stub_" ++ sanitize v ++ ";"
            ]
      _ -> genPreLoopLine typeEnv retKinds skipAlloc Set.empty stmt

    -- Grid size expression
    gridSizeExpr = case CFG.lsBounds (kaLoopSpec ka) of
      [b] -> "(long)" ++ genMSLIndexExpr b
      bs -> intercalate " * " ["(long)" ++ genMSLIndexExpr b | b <- bs]

    -- Input buffer: cached → reference static GPU buffer, dynamic → upload from hyd_array_t*
    genInputBuf (v, i) =
      let elemTy = mslElemTy v
       in if v `Set.member` cachedSet
            then
              [ "    // Cached input: " ++ sanitize v,
                "    id<MTLBuffer> _buf" ++ show i ++ " = _hyd_cached_buf_" ++ sanitize v ++ ";"
              ]
            else
              [ "    // Dynamic input: " ++ sanitize v,
                "    long _inN" ++ show i ++ " = hyd_shape_size(" ++ sanitize v ++ "->shape);",
                "    size_t _buf" ++ show i ++ "_sz = (size_t)(_inN" ++ show i ++ " * sizeof(" ++ elemTy ++ "));",
                "    id<MTLBuffer> _buf"
                  ++ show i
                  ++ " = [_hyd_dev newBufferWithLength:_buf"
                  ++ show i
                  ++ "_sz options:MTLResourceStorageModeShared];"
              ]
                ++ fillBuf i v elemTy

    fillBuf i v "float" =
      [ "    { double* _fsrc = (double*)" ++ sanitize v ++ "->data; float* _fdst = (float*)_buf" ++ show i ++ ".contents;",
        "      for (long _fci = 0; _fci < _inN" ++ show i ++ "; _fci++) _fdst[_fci] = (float)_fsrc[_fci]; }"
      ]
    fillBuf i v _ =
      ["    memcpy(_buf" ++ show i ++ ".contents, " ++ sanitize v ++ "->data, _buf" ++ show i ++ "_sz);"]

    -- Shape buffer: cached → stored shape, dynamic → array->shape
    genInputShapeBuf (v, i) =
      if v `Set.member` cachedSet
        then
          [ "    id<MTLBuffer> _buf"
              ++ show i
              ++ " = [_hyd_dev newBufferWithBytes:&_hyd_cached_shape_"
              ++ sanitize v
              ++ " length:sizeof(hyd_tuple_t) options:MTLResourceStorageModeShared];"
          ]
        else
          [ "    id<MTLBuffer> _buf"
              ++ show i
              ++ " = [_hyd_dev newBufferWithBytes:&"
              ++ sanitize v
              ++ "->shape length:sizeof(hyd_tuple_t) options:MTLResourceStorageModeShared];"
          ]

    -- Output buffer
    genOutputBuf (v, i) =
      let elemTy = mslElemTy v
          outputAllocShapes =
            Map.fromList
              [(ov, shpAtom) | CFG.SAssign ov (RArrayAlloc shpAtom) <- kaEffectivePreLoop ka]
          sizeExpr = case Map.lookup v outputAllocShapes of
            Just shpAtom -> "hyd_shape_size(" ++ genCAtom shpAtom ++ ")"
            Nothing -> "_n"
       in [ "    // Output: " ++ sanitize v,
            "    long _outN" ++ show i ++ " = " ++ sizeExpr ++ ";",
            "    size_t _buf" ++ show i ++ "_sz = (size_t)(_outN" ++ show i ++ " * sizeof(" ++ elemTy ++ "));",
            "    id<MTLBuffer> _buf"
              ++ show i
              ++ " = [_hyd_dev newBufferWithLength:_buf"
              ++ show i
              ++ "_sz options:MTLResourceStorageModeShared];",
            "    memset(_buf" ++ show i ++ ".contents, 0, _buf" ++ show i ++ "_sz);"
          ]

    -- Scalar: declare local variable only (passed via setBytes, no buffer alloc)
    genScalarBuf (v, i) =
      let sty = case Map.lookup v typeEnv of
            Just ct -> mslTypeName ct
            Nothing -> "long"
       in [ "    // Scalar: " ++ sanitize v,
            "    " ++ sty ++ " _scalar" ++ show i ++ " = " ++ sanitize v ++ ";"
          ]

    -- Read back GPU output arrays from Metal buffers (float→double).
    genReadback :: [String]
    genReadback =
      concat
        [ let i = nIn * 2 + idx
              elemTy = mslElemTy v
              sizeVar = "_outN" ++ show i
              outputAllocShapes =
                Map.fromList
                  [(ov, shpAtom) | CFG.SAssign ov (RArrayAlloc shpAtom) <- kaEffectivePreLoop ka]
              reshapeShape =
                listToMaybe
                  [ shpAtom
                  | CFG.SAssign _ (RCall "hyd_array_reshape_view" [arrAtom, shpAtom]) <-
                      kaPostLoopStmts ka,
                    arrAtom == AVar v
                  ]
              shapeExpr = case reshapeShape of
                Just shpAtom -> genCAtom shpAtom
                Nothing -> case Map.lookup v outputAllocShapes of
                  Just shpAtom -> genCAtom shpAtom
                  Nothing -> "hyd_tuple_make(1, (int64_t)" ++ sizeVar ++ ")"
           in [ "    // Read back: " ++ sanitize v,
                "    hyd_array_t* _result_" ++ sanitize v ++ " = hyd_array_alloc(" ++ shapeExpr ++ ");"
              ]
                ++ ( if elemTy == "float"
                       then
                         [ "    { float* _gsrc = (float*)_buf" ++ show i ++ ".contents;",
                           "      double* _gdst = (double*)_result_" ++ sanitize v ++ "->data;",
                           "      for (long _ri = 0; _ri < " ++ sizeVar ++ "; _ri++) _gdst[_ri] = (double)_gsrc[_ri]; }"
                         ]
                       else
                         [ "    memcpy(_result_"
                             ++ sanitize v
                             ++ "->data, _buf"
                             ++ show i
                             ++ ".contents, "
                             ++ sizeVar
                             ++ " * sizeof("
                             ++ elemTy
                             ++ "));"
                         ]
                   )
        | (idx, v) <- zip [0 ..] outputArrs
        ]

    -- Generate post-loop statements needed before return (e.g., dummy array allocs).
    -- Skip RPairMake and reshape_view (handled by return construction / readback).
    genPostLoopLine :: CFG.Stmt -> [String]
    genPostLoopLine (CFG.SAssign _ (RPairMake {})) = [] -- handled by genReturnConstruction
    genPostLoopLine (CFG.SAssign _ (RCall "hyd_array_reshape_view" _)) = [] -- handled by readback
    genPostLoopLine stmt = genPreLoopLine typeEnv retKinds Set.empty Set.empty stmt

    -- Construct the return value from post-loop RPairMake or output arrays.
    genReturnConstruction :: [String]
    genReturnConstruction =
      let retTy = exportReturnType spec
          outputSet = Set.fromList outputArrs
          pairMake =
            listToMaybe
              [(a1, a2) | CFG.SAssign _ (RPairMake _ _ a1 a2) <- kaPostLoopStmts ka]
          atomToRetExpr (AVar v)
            | v `Set.member` outputSet = "_result_" ++ sanitize v
            | otherwise = sanitize v -- CPU-produced array
          atomToRetExpr _ = "NULL"
       in case pairMake of
            Just (a1, a2) ->
              [ "    "
                  ++ retTy
                  ++ " _ret = { .fst = "
                  ++ atomToRetExpr a1
                  ++ ", .snd = "
                  ++ atomToRetExpr a2
                  ++ " };",
                "    return _ret;"
              ]
            Nothing -> case outputArrs of
              [v]
                | retTy == "hyd_array_t*" ->
                    ["    return _result_" ++ sanitize v ++ ";"]
                | otherwise ->
                    [ "    "
                        ++ retTy
                        ++ " _ret = { .fst = _result_"
                        ++ sanitize v
                        ++ ", .snd = _result_"
                        ++ sanitize v
                        ++ " };",
                      "    return _ret;"
                    ]
              [v1, v2] ->
                [ "    "
                    ++ retTy
                    ++ " _ret = { .fst = _result_"
                    ++ sanitize v1
                    ++ ", .snd = _result_"
                    ++ sanitize v2
                    ++ " };",
                  "    return _ret;"
                ]
              [] -> error "internal error: generateOutputReturn: no output arrays"
              _ -> ["    return _result_" ++ sanitize (head outputArrs) ++ ";"]

-- | Analyze a single proc for Metal kernel generation.
analyzeOneKernel :: CFG.Program -> CFG.Proc -> Either String KernelAnalysis
analyzeOneKernel prog@(CFG.Program procs) kernelProc = do
  let kernelName = CFG.procName kernelProc
      body = CFG.procBody kernelProc
  (preLoopStmts, mapLoopSpec, mapLoopBody, postLoopStmts, retAtom) <- analyzeKernelProc body
  () <- validateMSLLoopBody mapLoopBody
  let retTypes = inferProgramReturnTypes prog
      retKinds = procReturnKinds prog
      callParamTys = buildCallParamTypes retTypes procs
      typeEnv = recoverProcTypeEnv retTypes callParamTys kernelProc
      arrayElemTys =
        inferArrayElemTypesFromStmts
          retTypes
          body
          (Map.fromList [(v, elt) | (v, CTArray elt) <- Map.toList typeEnv])
      (inputArrays, outputArrays, scalarInputs, inBodyHoisted) =
        classifyKernelParams preLoopStmts mapLoopBody mapLoopSpec typeEnv retKinds
      effectivePreLoop = preLoopStmts ++ inBodyHoisted
      hoistedCallMap = buildHoistedCallMap inBodyHoisted
      procParams' = CFG.procParams kernelProc
      procParamScalars =
        [ v
        | v <- procParams',
          case Map.lookup v typeEnv of
            Just (CTArray _) -> False
            Just (CTPair _ _) -> False
            _ -> True
        ]
      procParamScalarsUsed =
        [ v
        | v <- procParamScalars,
          v `Set.member` usedVarsStmts (effectivePreLoop ++ mapLoopBody)
        ]
      allScalarInputs = scalarInputs ++ procParamScalarsUsed
      inputArraySet = Set.fromList inputArrays
      preLoopAliases = collectArrayAliases inputArraySet effectivePreLoop
  Right
    KernelAnalysis
      { kaProc = kernelProc,
        kaName = kernelName,
        kaPreLoopStmts = preLoopStmts,
        kaLoopSpec = mapLoopSpec,
        kaLoopBody = mapLoopBody,
        kaRetAtom = retAtom,
        kaTypeEnv = typeEnv,
        kaArrayElemTys = arrayElemTys,
        kaInputArrays = inputArrays,
        kaOutputArrays = outputArrays,
        kaScalarInputs = allScalarInputs,
        kaEffectivePreLoop = effectivePreLoop,
        kaHoistedCallMap = hoistedCallMap,
        kaPreLoopAliases = preLoopAliases,
        kaPostLoopStmts = postLoopStmts,
        kaProcParams = procParams'
      }

-- ---------------------------------------------------------------------------
-- Analysis helpers

-- | The proc whose result the harness prints: the @main@ entry if present,
-- otherwise the final proc (the program's last top-level binding).
resultProcName :: [CFG.Proc] -> Maybe BS.ByteString
resultProcName procs =
  case filter (\p -> CFG.procName p == "main") procs of
    (p : _) -> Just (CFG.procName p)
    [] -> case procs of
      [] -> Nothing
      _ -> Just (CFG.procName (last procs))

-- | Error explaining that the program's result can't be realized as a GPU
-- kernel, so the Metal backend declines rather than printing an intermediate.
resultNotLowerableMsg :: BS.ByteString -> String
resultNotLowerableMsg rname =
  unlines
    [ "MSL backend: the program result `"
        ++ BS.unpack rname
        ++ "` cannot be lowered to a GPU kernel.",
      "  Its computation uses control flow or serial loops the Metal backend can't",
      "  express as a single kernel (e.g. stencil boundary handling).",
      "  Use --export-metal-kernel=<name> to export a specific GPU kernel, or run on",
      "  the C backend."
    ]

-- | Human-readable label for a CPU loop's role, for fallback diagnostics.
cpuLoopLabel :: CFG.LoopSpec -> String
cpuLoopLabel spec = case CFG.lsRole spec of
  CFG.LoopReduction -> "reduction"
  CFG.LoopReductionWrapper -> "reduction"
  CFG.LoopMapReduction -> "reduction"
  CFG.LoopSegRedOuter -> "segmented-reduction"
  CFG.LoopFold -> "fold"
  CFG.LoopMap -> "map"
  CFG.LoopIterate -> "iterate"
  CFG.LoopPlain -> "scatter/serial"

-- | Top-level loops in a statement list (descending through @if@ but not into
-- nested loops, which are tiles/inner loops of the same logical loop).
topLevelLoopSpecs :: [CFG.Stmt] -> [CFG.LoopSpec]
topLevelLoopSpecs = concatMap go
  where
    go (CFG.SLoop spec _) = [spec]
    go (CFG.SIf _ t e) = topLevelLoopSpecs t ++ topLevelLoopSpecs e
    go _ = []

-- | Render a CPU-fallback summary line, or [] when @specs@ is empty.
cpuFallbackLine :: String -> [CFG.LoopSpec] -> [String]
cpuFallbackLine context specs
  | null specs = []
  | otherwise =
      [ "Metal: " ++ context ++ show (length specs)
          ++ " loop(s) run on the CPU, not the GPU ("
          ++ intercalate ", " (nub (map cpuLoopLabel specs))
          ++ "). The Metal backend could not offload these to a GPU kernel."
      ]

-- | CPU-fallback warnings for a single-kernel offload: the GPU runs the kernel
-- loop; any pre-loop producers or post-loop consumers run on the CPU.
cpuFallbackWarningsSingle :: KernelAnalysis -> [String]
cpuFallbackWarningsSingle ka =
  cpuFallbackLine
    ("in `" ++ BS.unpack (kaName ka) ++ "`, ")
    (topLevelLoopSpecs (kaEffectivePreLoop ka) ++ topLevelLoopSpecs (kaPostLoopStmts ka))

-- | CPU-fallback warnings for the multi-kernel path: any reachable proc that is
-- not a GPU kernel, plus the GPU kernels' own pre/post loops, run on the CPU.
cpuFallbackWarningsMulti :: [CFG.Proc] -> [KernelAnalysis] -> [String]
cpuFallbackWarningsMulti allProcs kas =
  let gpuNames = Set.fromList (map kaName kas)
      cpuProcLoops =
        [ spec
        | p <- allProcs,
          CFG.procName p `Set.notMember` gpuNames,
          spec <- topLevelLoopSpecs (CFG.procBody p)
        ]
      gpuSideLoops =
        concatMap
          (\ka -> topLevelLoopSpecs (kaEffectivePreLoop ka) ++ topLevelLoopSpecs (kaPostLoopStmts ka))
          kas
   in cpuFallbackLine "" (cpuProcLoops ++ gpuSideLoops)

-- | Does the proc's return value resolve to (a reshaped view of) one of the
-- kernel's output arrays? If so, the GPU output buffer can be printed/returned
-- directly; otherwise the harness must finish on the CPU and print the actual
-- return value.
retResolvesToOutput :: Maybe Atom -> [CFG.Stmt] -> [CVar] -> Bool
retResolvesToOutput retAtom postLoop outputs =
  let aliases =
        Map.fromList
          [ (v, out)
          | CFG.SAssign v (RCall "hyd_array_reshape_view" (AVar out : _)) <- postLoop
          ]
      resolve v = case Map.lookup v aliases of
        Just o -> resolve o
        Nothing -> v
   in case retAtom of
        Just (AVar v) -> resolve v `Set.member` Set.fromList outputs
        _ -> False

-- | Find the proc to use as the Metal kernel.
findKernelProc :: MSLOptions -> [CFG.Proc] -> Either String CFG.Proc
findKernelProc opts procs =
  case mslKernelToEmit opts of
    Just name ->
      case filter (\p -> CFG.procName p == name) procs of
        [] -> Left $ "MSL backend: kernel proc not found: " ++ BS.unpack name
        (p : _) -> Right p
    Nothing ->
      let candidates = filter (procHasMapLoop . CFG.procBody) procs
       in -- Prefer the "main" proc; otherwise the program result must itself be a
          -- kernel candidate, else we'd emit and print an unrelated intermediate.
          case filter (\p -> CFG.procName p == "main") candidates of
            (p : _) -> Right p
            [] -> case resultProcName procs of
              Just rname
                | not (any (\p -> CFG.procName p == rname) candidates) ->
                    Left (resultNotLowerableMsg rname)
              _ -> case candidates of
                [] -> Left "MSL backend: no proc with a map loop found in program"
                _ -> Right (last candidates)
  where
    procHasMapLoop = any isKernelLoopStmt
    isKernelLoopStmt (CFG.SLoop spec _) =
      CFG.lsRole spec `elem` [CFG.LoopMap, CFG.LoopMapReduction]
        || (case CFG.lsExec spec of CFG.Parallel _ -> True; _ -> False)
    isKernelLoopStmt _ = False

-- | Find all zero-arg procs with parallelizable loops (GPU-eligible).
-- Returns procs in original definition order (callees before callers).
findGPUEligibleProcs :: [CFG.Proc] -> [CFG.Proc]
findGPUEligibleProcs procs =
  [ p
  | p <- procs,
    null (CFG.procParams p), -- zero-arg only
    gpuKernelCapturesAllWork (CFG.procBody p)
  ]
  where
    gpuKernelCapturesAllWork body =
      let (bodyNoRet, _) = splitFinalReturn body
       in case findKernelLoopIndex bodyNoRet of
            Nothing -> False
            Just idx ->
              -- Reject procs where the GPU kernel would miss serial loops after it.
              -- Such procs (e.g., scatter_guarded with a fill init + serial scatter)
              -- need CPU execution for correctness.
              let afterKernel = drop (idx + 1) bodyNoRet
               in not (any hasLoopNested afterKernel)
    findKernelLoopIndex stmts =
      let indexed = zip [0 ..] stmts
          candidates =
            [ (i, stmtCount lb)
            | (i, CFG.SLoop spec lb) <- indexed,
              isKernelLoopStmt (CFG.SLoop spec lb)
            ]
          stmtCount = length . concatMap collectAllStmts
          collectAllStmts (CFG.SLoop _ b) = b ++ concatMap collectAllStmts b
          collectAllStmts (CFG.SIf _ t e) = t ++ e ++ concatMap collectAllStmts t ++ concatMap collectAllStmts e
          collectAllStmts s = [s]
       in case candidates of
            [] -> Nothing
            _ -> Just (fst (maximumBy (\a b -> compare (snd a) (snd b)) candidates))
    isKernelLoopStmt (CFG.SLoop spec _) =
      CFG.lsRole spec `elem` [CFG.LoopMap, CFG.LoopMapReduction]
        || (case CFG.lsExec spec of CFG.Parallel _ -> True; _ -> False)
    isKernelLoopStmt _ = False
    hasLoopNested (CFG.SLoop _ _) = True
    hasLoopNested (CFG.SIf _ thn els) = any hasLoopNested thn || any hasLoopNested els
    hasLoopNested _ = False

-- | Split proc body into (preLoopStmts, loopSpec, loopBody, postLoopStmts, retAtom).
-- Selects the largest kernel-eligible loop (by body size) in the proc body,
-- treating everything before and after it as pre-loop / post-loop CPU work.
analyzeKernelProc :: [CFG.Stmt] -> Either String ([CFG.Stmt], CFG.LoopSpec, [CFG.Stmt], [CFG.Stmt], Maybe Atom)
analyzeKernelProc body = do
  let (bodyNoRet, retAtom) = splitFinalReturn body
  case findBestKernelLoop bodyNoRet of
    Nothing ->
      Left "MSL backend: no parallelizable loop found in kernel proc body"
    Just (preLoop, spec, loopBody, postLoop) ->
      Right (preLoop, spec, loopBody, postLoop, retAtom)
  where
    -- Find the largest kernel-eligible loop by body statement count,
    -- returning (stmts before it, spec, body, stmts after it).
    findBestKernelLoop stmts =
      let indexed = zip [0 ..] stmts
          candidates =
            [ (i, spec, lb)
            | (i, CFG.SLoop spec lb) <- indexed,
              isKernelLoopStmt (CFG.SLoop spec lb)
            ]
          stmtCount = length . concatMap collectAllStmts
          collectAllStmts (CFG.SLoop _ b) = b ++ concatMap collectAllStmts b
          collectAllStmts (CFG.SIf _ t e) = t ++ e ++ concatMap collectAllStmts t ++ concatMap collectAllStmts e
          collectAllStmts s = [s]
       in case candidates of
            [] -> Nothing
            _ ->
              let best = maximumBy (\(_, _, b1) (_, _, b2) -> compare (stmtCount b1) (stmtCount b2)) candidates
                  (i, spec, lb) = best
               in Just (take i stmts, spec, lb, drop (i + 1) stmts)
    isKernelLoopStmt (CFG.SLoop spec _) =
      CFG.lsRole spec `elem` [CFG.LoopMap, CFG.LoopMapReduction]
        || (case CFG.lsExec spec of CFG.Parallel _ -> True; _ -> False)
    isKernelLoopStmt _ = False

-- | Validation: reject unsupported MSL constructs in the kernel loop body.
validateMSLLoopBody :: [CFG.Stmt] -> Either String ()
validateMSLLoopBody stmts = mapM_ checkStmt stmts
  where
    checkStmt stmt = case stmt of
      CFG.SAssign _ rhs -> checkRHS rhs
      CFG.SArrayWrite {} -> Right ()
      CFG.SLoop _ body -> mapM_ checkStmt body
      CFG.SIf _ thn els -> mapM_ checkStmt thn >> mapM_ checkStmt els
      CFG.SReturn {} -> Right ()
      CFG.SBreak -> Right ()
    checkRHS rhs = case rhs of
      RArrayAlloc {} ->
        Left "MSL backend: dynamic array allocation inside kernel loop not supported"
      _ -> Right ()

-- | Classify kernel parameters from pre-loop stmts and loop body.
-- Returns (inputArrays, outputArrays, scalarInputs, inBodyHoisted).
-- inBodyHoisted contains zero-arg array calls found inside the loop body
-- that must be pre-fetched on CPU and passed as Metal buffers.
classifyKernelParams ::
  -- | Pre-loop stmts
  [CFG.Stmt] ->
  -- | Loop body
  [CFG.Stmt] ->
  -- | Loop spec (for iterator names)
  CFG.LoopSpec ->
  TypeEnv ->
  Map CVar VarKind ->
  ([CVar], [CVar], [CVar], [CFG.Stmt])
classifyKernelParams preLoopStmts loopBody loopSpec typeEnv retKinds =
  let iterVars = Set.fromList (CFG.lsIters loopSpec)
      -- Include variables from loop bounds (needed for multi-dim decomposition)
      boundVars = Set.fromList (concatMap indexExprVars (CFG.lsBounds loopSpec))
      loopUsedVars = (usedVarsStmts loopBody `Set.union` boundVars) `Set.difference` iterVars
      -- Also scan for written arrays inside nested loops
      loopWrittenAll = collectWrittenArrays loopBody
      -- Input arrays: results of zero-arg RCall in pre-loop, or allocated and
      -- filled by earlier loops but only read (not written) in kernel loop.
      preLoopCallArrays =
        [ v
        | CFG.SAssign v (RCall _ []) <- preLoopStmts,
          v `Set.member` loopUsedVars,
          isArrayVar v
        ]
      -- Arrays allocated in pre-loop, used in kernel loop but not written there
      preLoopAllocReadArrays =
        [ v
        | CFG.SAssign v (RArrayAlloc _) <- preLoopStmts,
          v `Set.member` loopUsedVars,
          v `Set.notMember` loopWrittenAll
        ]
      -- Arrays extracted from pair proc params via RProj (not RAtom aliases —
      -- those are handled by collectArrayAliases / #define in the kernel body).
      preLoopProjArrays =
        [ v
        | CFG.SAssign v (RProj _ _) <- preLoopStmts,
          v `Set.member` loopUsedVars,
          v `Set.notMember` loopWrittenAll,
          isArrayVar v
        ]
      preLoopInputArrays = nub (preLoopCallArrays ++ preLoopAllocReadArrays ++ preLoopProjArrays)
      -- In-body zero-arg calls: RCall fn [] anywhere inside loop body (recursively).
      -- These are memoized calls that need to be hoisted to CPU pre-loop and passed as buffers.
      -- Split into array calls (input buffers) and scalar calls (constant buffers).
      allZeroArgCalls = collectZeroArgCalls loopBody
      inBodyHoistedAll =
        [ stmt
        | stmt@(CFG.SAssign v (RCall fn [])) <- allZeroArgCalls,
          v `Set.notMember` Set.fromList preLoopInputArrays
        ]
      inBodyHoistedArrays =
        [ stmt
        | stmt@(CFG.SAssign v (RCall fn [])) <- inBodyHoistedAll,
          isArrayFn fn
        ]
      inBodyHoistedScalars =
        [ stmt
        | stmt@(CFG.SAssign v (RCall fn [])) <- inBodyHoistedAll,
          not (isArrayFn fn)
        ]
      inBodyInputArrays = [v | CFG.SAssign v (RCall _ []) <- inBodyHoistedArrays]
      inBodyScalarVars = [v | CFG.SAssign v (RCall _ []) <- inBodyHoistedScalars]
      inputArrays = preLoopInputArrays ++ inBodyInputArrays
      -- Output arrays: RArrayAlloc in pre-loop, written in loop
      outputArrays =
        [ v
        | CFG.SAssign v (RArrayAlloc _) <- preLoopStmts,
          v `Set.member` loopWrittenAll
        ]
      allArrVars = Set.fromList inputArrays `Set.union` Set.fromList outputArrays
      -- Scalar inputs: non-array pre-loop vars used in loop body
      preLoopScalarInputs =
        [ v
        | CFG.SAssign v _ <- preLoopStmts,
          v `Set.member` loopUsedVars,
          v `Set.notMember` allArrVars,
          not (isArrayVar v)
        ]
      scalarInputs = preLoopScalarInputs ++ inBodyScalarVars
   in (nub inputArrays, nub outputArrays, nub scalarInputs, inBodyHoistedAll)
  where
    isArrayVar v = case Map.lookup v typeEnv of
      Just (CTArray _) -> True
      _ -> Map.findWithDefault KScalar v retKinds `elem` [KArray, KFloatArray]
    isArrayFn fn = Map.findWithDefault KScalar fn retKinds `elem` [KArray, KFloatArray]

-- | Recursively collect zero-arg RCall statements from a statement list.
-- Returns one statement per unique function name (the first occurrence).
collectZeroArgCalls :: [CFG.Stmt] -> [CFG.Stmt]
collectZeroArgCalls stmts = Map.elems (Map.fromListWith (\_ first -> first) pairs)
  where
    pairs = [(fn, stmt) | stmt@(CFG.SAssign _ (RCall fn [])) <- allCalls stmts]
    allCalls = concatMap go
    go stmt@(CFG.SAssign _ (RCall _ [])) = [stmt]
    go (CFG.SLoop _ body) = allCalls body
    go (CFG.SIf _ thn els) = allCalls thn ++ allCalls els
    go _ = []

-- | Build a mapping from function name → canonical hoisted variable.
-- Used to replace in-kernel zero-arg calls with the hoisted variable.
buildHoistedCallMap :: [CFG.Stmt] -> Map CVar CVar
buildHoistedCallMap hoisted = Map.fromList [(fn, v) | CFG.SAssign v (RCall fn []) <- hoisted]

-- | Classify which input arrays come from CPU helpers vs. prior GPU kernels.
-- Also returns a map from consumer variable to the producing kernel's output
-- buffer variable, so the harness can reuse the GPU buffer directly.
--
-- @gpuKernelOutputs@ maps kernel proc name → its output arrays (from kaOutputArrays).
classifyBufferOrigins ::
  -- | All GPU kernels, in execution order
  [KernelAnalysis] ->
  -- | Set of GPU kernel proc names
  Set CVar ->
  -- | For each input array across all kernels
  Map CVar BufferOrigin
classifyBufferOrigins kernels gpuNames =
  Map.fromList
    [ (v, origin fn)
    | ka <- kernels,
      CFG.SAssign v (RCall fn []) <- kaEffectivePreLoop ka,
      v `elem` kaInputArrays ka
    ]
  where
    origin fn
      | fn `Set.member` gpuNames = GPUProduced fn
      | otherwise = CPUProduced

-- | Build a mapping from a consumer's input variable to the producing kernel's
-- output buffer variable.  E.g. if kernel B has @SAssign t4 (RCall "arr" [])@
-- and kernel A ("arr") produces output @arr1@, then @t4 → arr1@.
buildGPUBufferAliases ::
  -- | All GPU kernels
  [KernelAnalysis] ->
  -- | GPU kernel proc names
  Set CVar ->
  -- | consumer input var → producer output var
  Map CVar CVar
buildGPUBufferAliases kernels gpuNames =
  Map.fromList
    [ (v, outVar)
    | ka <- kernels,
      CFG.SAssign v (RCall fn []) <- kaEffectivePreLoop ka,
      fn `Set.member` gpuNames,
      -- Find the producing kernel's output array.  For simple (single-array)
      -- returns the first output array is the result.
      let producerOutputs = [kaOutputArrays k | k <- kernels, kaName k == fn],
      outVar <- case producerOutputs of
        ([o] : _) -> [o] -- single output array
        (os : _) -> take 1 os -- multi-output: use first (conservative)
        _ -> []
    ]

-- | Collect array alias assignments: v = RAtom (AVar arr) where arr is a known array.
-- Returns (alias, original) pairs. Recursively follows alias chains.
collectArrayAliases :: Set CVar -> [CFG.Stmt] -> [(CVar, CVar)]
collectArrayAliases knownArrays stmts = go knownArrays stmts []
  where
    go _known [] acc = acc
    go known (stmt : rest) acc = case stmt of
      CFG.SAssign v (RAtom (AVar src))
        | src `Set.member` known ->
            go (Set.insert v known) rest ((v, src) : acc)
      CFG.SLoop _ body ->
        let inner = go known body []
            known' = known `Set.union` Set.fromList (map fst inner)
         in go known' rest (inner ++ acc)
      CFG.SIf _ thn els ->
        let innerT = go known thn []
            innerE = go known els []
            known' =
              known
                `Set.union` Set.fromList (map fst innerT)
                `Set.union` Set.fromList (map fst innerE)
         in go known' rest (innerT ++ innerE ++ acc)
      _ -> go known rest acc

collectWrittenArrays :: [CFG.Stmt] -> Set CVar
collectWrittenArrays = foldMap go
  where
    go (CFG.SArrayWrite (AVar v) _ _) = Set.singleton v
    go (CFG.SLoop _ body) = collectWrittenArrays body
    go (CFG.SIf _ thn els) = collectWrittenArrays thn `Set.union` collectWrittenArrays els
    go _ = Set.empty

-- ---------------------------------------------------------------------------
-- MSL pair struct support

-- | Collect all pair types from the typeEnv for variables assigned in the statement list.
-- Returns pairs in dependency order (leaf types first).
collectPairTypesFromEnv :: TypeEnv -> [CFG.Stmt] -> [(CElemType, CElemType)]
collectPairTypesFromEnv typeEnv stmts =
  let allVars = collectAssignedVars stmts
      pairTypesRaw =
        nub
          [ (ce1, ce2)
          | v <- allVars,
            Just (CTPair t1 t2) <- [Map.lookup v typeEnv],
            Just ce1 <- [ctypeToElemType t1],
            Just ce2 <- [ctypeToElemType t2]
          ]
      -- Transitively collect sub-pair types in dependency order
      transitive = concatMap (\(ce1, ce2) -> transitivePairTypesM ce1 ++ transitivePairTypesM ce2 ++ [(ce1, ce2)]) pairTypesRaw
   in nub transitive

-- | Collect transitive sub-pair types for a CElemType.
transitivePairTypesM :: CElemType -> [(CElemType, CElemType)]
transitivePairTypesM (CEPair ct1 ct2) =
  transitivePairTypesM ct1 ++ transitivePairTypesM ct2 ++ [(ct1, ct2)]
transitivePairTypesM _ = []

-- | Generate MSL struct definitions for all pair types used.
genMSLPairStructDefs :: [(CElemType, CElemType)] -> String
genMSLPairStructDefs [] = ""
genMSLPairStructDefs pairTypes =
  unlines
    [ "// Pair struct definitions"
    ]
    ++ concatMap genDef pairTypes
  where
    genDef (ct1, ct2) =
      "struct "
        ++ mslPairStructName ct1 ct2
        ++ " { "
        ++ mslElemCType ct1
        ++ " fst; "
        ++ mslElemCType ct2
        ++ " snd; };\n"

-- | MSL pair struct name.
mslPairStructName :: CElemType -> CElemType -> String
mslPairStructName ct1 ct2 = "hyd_pair_" ++ celemTypeLetter ct1 ++ celemTypeLetter ct2 ++ "_t"

-- | MSL element type from CElemType.
mslElemCType :: CElemType -> String
mslElemCType CEInt = "long"
mslElemCType CEFloat = "float" -- demoted from double
mslElemCType CEBool = "int"
mslElemCType (CEPair ct1 ct2) = mslPairStructName ct1 ct2
mslElemCType CEArray = "long" -- can't have arrays inside MSL structs

-- ---------------------------------------------------------------------------
-- MSL type helpers

-- | Map a CType to its MSL type name.
-- CTDouble is demoted to float (MSL does not support double on most GPUs).
mslTypeName :: CType -> String
mslTypeName CTInt64 = "long"
mslTypeName CTDouble = "float" -- demoted
mslTypeName CTBool = "int"
mslTypeName CTUnit = "int"
mslTypeName CTTuple = "hyd_tuple_t"
mslTypeName (CTPair t1 t2)
  | Just ce1 <- ctypeToElemType t1,
    Just ce2 <- ctypeToElemType t2 =
      mslPairStructName ce1 ce2
mslTypeName _ = "long"

-- | MSL buffer element type for an array.
mslArrayElemTypeName :: CType -> String
mslArrayElemTypeName (CTArray elt) = mslTypeName elt
mslArrayElemTypeName ct = mslTypeName ct

-- | Whether a CType is a float type (for printf format selection).
isMSLFloatType :: CType -> Bool
isMSLFloatType CTDouble = True
isMSLFloatType (CTArray CTDouble) = True
isMSLFloatType _ = False

-- | Whether a CType contains a @CTDouble@ anywhere — including under arrays and
-- (nested) pairs, e.g. nbody's @((float, float), float)@ element type.
ctypeHasFloat :: CType -> Bool
ctypeHasFloat CTDouble = True
ctypeHasFloat (CTArray t) = ctypeHasFloat t
ctypeHasFloat (CTPair a b) = ctypeHasFloat a || ctypeHasFloat b
ctypeHasFloat _ = False

-- | True when a kernel computes in floating point (so Metal runs it at 32-bit
-- single precision). Any float-typed temporary, array, or pair shows up in the
-- recovered type environment, so scanning it suffices.
kernelUsesFloat :: KernelAnalysis -> Bool
kernelUsesFloat ka =
  any ctypeHasFloat (Map.elems (kaTypeEnv ka))
    || any ctypeHasFloat (Map.elems (kaArrayElemTys ka))

-- | Apple GPUs lack fp64, so the MSL backend demotes every @float@ (a
-- double-precision value in the surface language) to 32-bit single precision.
-- Emit a one-line warning when any kernel computes in floating point so the
-- precision difference from the C backend is not silent. (There is no
-- surface-language way to request 32-bit float explicitly — @float@ is always
-- double on the C backend and always demoted on Metal.)
fp32DemotionWarning :: [KernelAnalysis] -> [String]
fp32DemotionWarning kas
  | any kernelUsesFloat kas =
      [ "Metal: floating-point is computed in 32-bit single precision (Apple GPUs"
          ++ " lack fp64), so results may differ from the C backend by ~1e-6"
          ++ " relative. Hydrangea's `float` is double-precision on the C backend."
      ]
  | otherwise = []

-- ---------------------------------------------------------------------------
-- MSL kernel source generation

-- | Generate the @.metal@ kernel source.
genMSLKernelSrc ::
  -- | Kernel proc name
  CVar ->
  -- | Map loop spec
  CFG.LoopSpec ->
  -- | Map loop body
  [CFG.Stmt] ->
  TypeEnv ->
  -- | Array element types
  Map CVar CType ->
  -- | Input array params (in order)
  [CVar] ->
  -- | Output array params (in order)
  [CVar] ->
  -- | Scalar input params (in order)
  [CVar] ->
  -- | Hoisted zero-arg call map (fn → canonical var)
  Map CVar CVar ->
  -- | Pre-loop array aliases (alias, source)
  [(CVar, CVar)] ->
  String
genMSLKernelSrc
  kernelName
  loopSpec
  loopBody
  typeEnv
  arrayElemTys
  inputArrays
  outputArrays
  scalarInputs
  hoistedCallMap
  preLoopAliases =
    let pairTypes = collectPairTypesFromEnv typeEnv loopBody
        pairDefs = genMSLPairStructDefs pairTypes
     in mslSharedHeader pairDefs
          ++ genMSLKernelFunction
            kernelName
            loopSpec
            loopBody
            typeEnv
            arrayElemTys
            inputArrays
            outputArrays
            scalarInputs
            hoistedCallMap
            preLoopAliases

-- | Shared Metal header (includes, shape helpers, erf).
-- @pairDefs@ is the pair struct definitions needed by the kernel(s).
mslSharedHeader :: String -> String
mslSharedHeader pairDefs =
  unlines
    [ "#include <metal_stdlib>",
      "using namespace metal;",
      "",
      mslShapeHelpers,
      pairDefs,
      ""
    ]

-- | Generate a single @kernel void@ function (no shared header).
genMSLKernelFunction ::
  CVar ->
  CFG.LoopSpec ->
  [CFG.Stmt] ->
  TypeEnv ->
  Map CVar CType ->
  [CVar] ->
  [CVar] ->
  [CVar] ->
  Map CVar CVar ->
  [(CVar, CVar)] ->
  String
genMSLKernelFunction
  kernelName
  loopSpec
  loopBody
  typeEnv
  arrayElemTys
  inputArrays
  outputArrays
  scalarInputs
  hoistedCallMap
  preLoopAliases =
    "kernel void "
      ++ sanitize kernelName
      ++ "(\n"
      ++ intercalate ",\n" (map ("    " ++) allParams)
      ++ ")\n"
      ++ unlines
        [ "{"
        ]
      ++ iterDecompLines
      ++ genMSLBody gidVar (CFG.lsExec loopSpec) hoistedVarSet outputArrSet typeEnv arrayElemTys hoistedCallMap preLoopAliases loopBody
      ++ "}\n"
    where
      -- All hoisted vars (input arrays + scalar inputs): skip their in-body assignments
      hoistedVarSet = Set.fromList inputArrays `Set.union` Set.fromList scalarInputs
      outputArrSet = Set.fromList outputArrays
      iters = CFG.lsIters loopSpec
      bounds = CFG.lsBounds loopSpec
      -- For multi-dim loops, use a synthetic flat gid variable
      isMultiDim = length iters > 1
      gidVar = case iters of
        []    -> error "internal error: MSL kernel loop spec has no iterators"
        (i:_) -> if isMultiDim then "_flat_gid" else i

      -- Decompose flat gid into individual iterators (row-major order)
      -- iter[n-1] = _flat_gid % bound[n-1]; remaining /= bound[n-1]; ...
      iterDecompLines
        | not isMultiDim = ""
        | otherwise =
            "    uint _rem = _flat_gid;\n"
              ++ concatMap
                ( \(it, bd) ->
                    "    uint "
                      ++ sanitize it
                      ++ " = _rem % (uint)("
                      ++ genMSLIndexExpr bd
                      ++ ");\n"
                      ++ "    _rem /= (uint)("
                      ++ genMSLIndexExpr bd
                      ++ ");\n"
                )
                (reverse (zip iters bounds))

      -- Buffer index assignment
      -- Input arrays: data buffer + shape buffer for each
      inputBufs = zipWith (\v i -> (v, i)) inputArrays [0 ..]
      inputShapeBufs = zipWith (\v i -> (v, i)) inputArrays [length inputArrays ..]
      nInputBufs = length inputArrays * 2 -- data + shape per input
      outputBufs = zipWith (\v i -> (v, i)) outputArrays [nInputBufs ..]
      scalarBufs = zipWith (\v i -> (v, i)) scalarInputs [nInputBufs + length outputArrays ..]

      allParams = inputParams ++ inputShapeParams ++ outputParams ++ scalarParams ++ [gidParam]

      inputParams =
        [ "device const "
            ++ mslElemTy v
            ++ "* "
            ++ sanitize v
            ++ "_data [[buffer("
            ++ show i
            ++ ")]]"
        | (v, i) <- inputBufs
        ]
      inputShapeParams =
        [ "constant hyd_tuple_t& "
            ++ sanitize v
            ++ "_shape [[buffer("
            ++ show i
            ++ ")]]"
        | (v, i) <- inputShapeBufs
        ]
      outputParams =
        [ "device "
            ++ mslElemTy v
            ++ "* "
            ++ sanitize v
            ++ "_data [[buffer("
            ++ show i
            ++ ")]]"
        | (v, i) <- outputBufs
        ]
      scalarParams =
        [ "constant "
            ++ mslScalarTy v
            ++ "& "
            ++ sanitize v
            ++ " [[buffer("
            ++ show i
            ++ ")]]"
        | (v, i) <- scalarBufs
        ]
      gidParam = "uint " ++ sanitize gidVar ++ " [[thread_position_in_grid]]"

      mslElemTy v = case Map.lookup v arrayElemTys of
        Just elt -> mslTypeName elt
        Nothing -> case Map.lookup v typeEnv of
          Just (CTArray elt) -> mslTypeName elt
          _ -> "long"

      mslScalarTy v = case Map.lookup v typeEnv of
        Just ct -> mslTypeName ct
        Nothing -> "long"

-- | Generate a @.metal@ source with multiple @kernel void@ functions and
-- a single shared header.
genMultiKernelMSL :: [KernelAnalysis] -> String
genMultiKernelMSL kernels =
  let -- Collect pair types from all kernels (deduplicated)
      allPairTypes =
        nub $
          concatMap
            (\ka -> collectPairTypesFromEnv (kaTypeEnv ka) (kaLoopBody ka))
            kernels
      pairDefs = genMSLPairStructDefs allPairTypes
   in mslSharedHeader pairDefs
        ++ concatMap genOneKernel kernels
  where
    genOneKernel ka =
      genMSLKernelFunction
        (kaName ka)
        (kaLoopSpec ka)
        (kaLoopBody ka)
        (kaTypeEnv ka)
        (kaArrayElemTys ka)
        (kaInputArrays ka)
        (kaOutputArrays ka)
        (kaScalarInputs ka)
        (kaHoistedCallMap ka)
        (kaPreLoopAliases ka)

-- | Collect all variables assigned in a statement list (recursively).
collectAssignedVars :: [CFG.Stmt] -> [CVar]
collectAssignedVars = nub . concatMap go
  where
    go (CFG.SAssign v _) = [v]
    go (CFG.SLoop _ body) = collectAssignedVars body
    go (CFG.SIf _ thn els) = collectAssignedVars thn ++ collectAssignedVars els
    go _ = []

-- | Emit MSL body statements.
-- Pre-declares all variables at the top of the kernel body to avoid
-- block-scoping issues with re-assignments (accumulators, tiled loops).
genMSLBody ::
  -- | Loop iterator (= gid in the kernel)
  CVar ->
  -- | Execution policy of the outermost loop
  CFG.ExecPolicy ->
  -- | Hoisted vars to skip in kernel body
  Set CVar ->
  -- | Output array vars
  Set CVar ->
  TypeEnv ->
  Map CVar CType ->
  -- | Hoisted zero-arg call map (fn → canonical var)
  Map CVar CVar ->
  -- | Pre-loop array aliases (alias, source)
  [(CVar, CVar)] ->
  [CFG.Stmt] ->
  String
genMSLBody iter execPolicy inArrs outArrs typeEnv arrayElemTys hoistedCallMap preLoopAliases stmts =
  let -- Collect array aliases: only from actual array variables (not scalars in inArrs)
      actualArrayVars = Set.filter isArrayInEnv (inArrs `Set.union` outArrs)
      isArrayInEnv v = case Map.lookup v typeEnv of
        Just (CTArray _) -> True
        _ -> case Map.lookup v arrayElemTys of
          Just _ -> True
          _ -> False
      bodyAliases = collectArrayAliases actualArrayVars stmts
      arrayAliases = nub (preLoopAliases ++ bodyAliases)
      allInArrs = inArrs `Set.union` Set.fromList (map fst arrayAliases)
      -- Resolve alias chains to root array
      aliasMap0 = Map.fromList arrayAliases
      resolveAlias v = case Map.lookup v aliasMap0 of
        Just parent -> resolveAlias parent
        Nothing -> v
      -- Emit #define for each alias so v_data → root_data, v_shape → root_shape
      aliasDefLines =
        concatMap
          ( \(v, _src) ->
              let root = resolveAlias v
               in "    #define "
                    ++ sanitize v
                    ++ "_data "
                    ++ sanitize root
                    ++ "_data\n"
                    ++ "    #define "
                    ++ sanitize v
                    ++ "_shape "
                    ++ sanitize root
                    ++ "_shape\n"
          )
          arrayAliases
      allVars = collectAssignedVars stmts
      -- Pre-declare all variables (except input arrays and their aliases)
      declLines =
        concatMap
          ( \v ->
              if v `Set.member` allInArrs
                then ""
                else "    " ++ mslVarDecl v typeEnv ++ " " ++ sanitize v ++ ";\n"
          )
          allVars
      tupDefs = collectTupleDefs stmts
      bodyLines = case scatterStrategy of
        Just _ ->
          case detectAtomicScatterAddLoop stmts of
            Just (outerPrefix, branchPrefix, mGuard, arrAtom, idxAtom, valAtom) ->
              let outerStr = concatMap (genMSLStmt 1 iter allInArrs outArrs typeEnv arrayElemTys hoistedCallMap tupDefs) outerPrefix
                  branchStr depth = concatMap (genMSLStmt depth iter allInArrs outArrs typeEnv arrayElemTys hoistedCallMap tupDefs) branchPrefix
                  atomicLine depth = genMSLAtomicAdd depth iter allInArrs typeEnv arrayElemTys arrAtom idxAtom valAtom
               in case mGuard of
                    Nothing -> outerStr ++ branchStr 1 ++ atomicLine 1
                    Just cond ->
                      outerStr
                        ++ "    if ("
                        ++ genMSLAtom iter cond
                        ++ ") {\n"
                        ++ branchStr 2
                        ++ atomicLine 2
                        ++ "    }\n"
            Nothing ->
              concatMap (genMSLStmt 1 iter allInArrs outArrs typeEnv arrayElemTys hoistedCallMap tupDefs) stmts
        Nothing ->
          concatMap (genMSLStmt 1 iter allInArrs outArrs typeEnv arrayElemTys hoistedCallMap tupDefs) stmts
   in declLines ++ aliasDefLines ++ bodyLines
  where
    -- On the GPU, every scatter-add must use atomics. The CPU parallelizer
    -- prefers *privatization* (per-thread accumulator buffers) for scatter-add,
    -- but that technique does not apply to the GPU's many threads — so we treat
    -- both privatized and atomic scatter-add strategies as atomic here.
    scatterStrategy = case execPolicy of
      CFG.Parallel p -> case CFG.psStrategy p of
        CFG.ParallelScatterAtomicAddInt -> Just CTInt64
        CFG.ParallelScatterAtomicAddFloat -> Just CTDouble
        CFG.ParallelScatterPrivatizedIntAdd -> Just CTInt64
        CFG.ParallelScatterPrivatizedFloatAdd -> Just CTDouble
        _ -> Nothing
      _ -> Nothing

-- | Emit a Metal atomic_fetch_add_explicit for a scatter add.
genMSLAtomicAdd :: Int -> CVar -> Set CVar -> TypeEnv -> Map CVar CType -> Atom -> Atom -> Atom -> String
genMSLAtomicAdd depth iter inArrs typeEnv _arrayElemTys arrAtom idxAtom valAtom =
  let ind = replicate (depth * 4) ' '
      arrName = case arrAtom of
        AVar v -> sanitize v ++ "_data"
        _ -> genMSLAtom iter arrAtom
      idx = genMSLAtom iter idxAtom
      val = genMSLAtom iter valAtom
      -- Determine whether to use atomic_int or atomic_uint based on the scatter type
      isFloat = case arrAtom of
        AVar v -> case Map.lookup v typeEnv of
          Just (CTArray CTDouble) -> True
          _ -> False
        _ -> False
   in if isFloat
        then
          -- Float atomic add via CAS loop
          ind
            ++ "{ device atomic_uint* _ap = (device atomic_uint*)&"
            ++ arrName
            ++ "["
            ++ idx
            ++ "];\n"
            ++ ind
            ++ "  uint _expected = atomic_load_explicit(_ap, memory_order_relaxed);\n"
            ++ ind
            ++ "  uint _desired;\n"
            ++ ind
            ++ "  do {\n"
            ++ ind
            ++ "    float _old = as_type<float>(_expected);\n"
            ++ ind
            ++ "    _desired = as_type<uint>(_old + (float)("
            ++ val
            ++ "));\n"
            ++ ind
            ++ "  } while (!atomic_compare_exchange_weak_explicit(_ap, &_expected, _desired,\n"
            ++ ind
            ++ "            memory_order_relaxed, memory_order_relaxed));\n"
            ++ ind
            ++ "}\n"
        else
          -- Integer atomic add
          ind ++ "atomic_fetch_add_explicit((device atomic_int*)&" ++ arrName ++ "[" ++ idx ++ "], (int)(" ++ val ++ "), memory_order_relaxed);\n"

genMSLStmt :: Int -> CVar -> Set CVar -> Set CVar -> TypeEnv -> Map CVar CType -> Map CVar CVar -> Map CVar [Atom] -> CFG.Stmt -> String
genMSLStmt depth iter inArrs outArrs typeEnv arrayElemTys hoistedCallMap tupleDefs stmt =
  let ind = replicate (depth * 4) ' '
      recurse = genMSLStmt (depth + 1) iter inArrs outArrs typeEnv arrayElemTys hoistedCallMap tupleDefs
   in case stmt of
        CFG.SAssign v _
          | v `Set.member` inArrs ->
              -- This is a hoisted Metal buffer/constant parameter; skip the call in the kernel.
              ""
        -- Zero-arg call whose function was hoisted: replace with canonical hoisted var
        CFG.SAssign v (RCall fn [])
          | Just canonVar <- Map.lookup fn hoistedCallMap,
            v /= canonVar ->
              ind ++ sanitize v ++ " = " ++ sanitize canonVar ++ ";\n"
        CFG.SAssign v rhs ->
          ind
            ++ sanitize v
            ++ " = "
            ++ genMSLRHS v iter inArrs outArrs typeEnv arrayElemTys tupleDefs rhs
            ++ ";\n"
        CFG.SArrayWrite (AVar arr) idx val ->
          let suffix = sanitize arr ++ "_data"
           in ind
                ++ suffix
                ++ "["
                ++ genMSLAtom iter idx
                ++ "] = "
                ++ genMSLAtom iter val
                ++ ";\n"
        CFG.SArrayWrite arr idx val ->
          -- Fallback: shouldn't happen for well-formed kernels
          ind
            ++ genMSLAtom iter arr
            ++ "["
            ++ genMSLAtom iter idx
            ++ "] = "
            ++ genMSLAtom iter val
            ++ ";\n"
        CFG.SLoop spec body ->
          case (CFG.lsIters spec, CFG.lsBounds spec) of
            ([i], [b]) ->
              let ci = sanitize i
                  red = CFG.lsRed spec
                  initLine = case red of
                    Just r ->
                      ind
                        ++ sanitize (CFG.rsAccVar r)
                        ++ " = "
                        ++ genMSLIndexExpr (CFG.rsInit r)
                        ++ ";\n"
                    Nothing -> ""
                  loopLine =
                    ind
                      ++ "for (uint "
                      ++ ci
                      ++ " = 0; "
                      ++ ci
                      ++ " < "
                      ++ genMSLIndexExpr b
                      ++ "; "
                      ++ ci
                      ++ "++) {\n"
                  bodyLines = concatMap recurse body
                  closeLine = ind ++ "}\n"
               in initLine ++ loopLine ++ bodyLines ++ closeLine
            _ ->
              -- Multi-dim inner loops: emit nested for loops
              let bodyLines = concatMap recurse body
                  mkLoop (ci, b) inner =
                    ind
                      ++ "for (uint "
                      ++ sanitize ci
                      ++ " = 0; "
                      ++ sanitize ci
                      ++ " < "
                      ++ genMSLIndexExpr b
                      ++ "; "
                      ++ sanitize ci
                      ++ "++) {\n"
                      ++ inner
                      ++ ind
                      ++ "}\n"
               in foldr mkLoop bodyLines (zip (CFG.lsIters spec) (CFG.lsBounds spec))
        CFG.SIf cond thn els ->
          ind
            ++ "if ("
            ++ genMSLAtom iter cond
            ++ ") {\n"
            ++ concatMap recurse thn
            ++ ( case els of
                   [] -> ind ++ "}\n"
                   _ ->
                     ind
                       ++ "} else {\n"
                       ++ concatMap recurse els
                       ++ ind
                       ++ "}\n"
               )
        CFG.SReturn a ->
          -- In a kernel, SReturn means write to output; handled by SArrayWrite above.
          -- A scalar return at the top level of the kernel body is unusual but emit as comment.
          ind ++ "/* return " ++ genMSLAtom iter a ++ "; */\n"
        CFG.SBreak ->
          ind ++ "break;\n"

-- | MSL variable declaration type (excluding the variable name).
mslVarDecl :: CVar -> TypeEnv -> String
mslVarDecl v typeEnv = case Map.lookup v typeEnv of
  Just ct -> mslTypeName ct
  Nothing -> "long"

-- | Collect all @RTuple@ definitions from a statement list (recursively).
-- Used to inline @RNdToFlat@ for known-arity tuples.
collectTupleDefs :: [CFG.Stmt] -> Map CVar [Atom]
collectTupleDefs = foldMap go
  where
    go (CFG.SAssign v (RTuple atoms)) = Map.singleton v atoms
    go (CFG.SLoop _ body) = collectTupleDefs body
    go (CFG.SIf _ thn els) = collectTupleDefs thn <> collectTupleDefs els
    go _ = Map.empty

-- | Generate MSL for a right-hand-side expression.
genMSLRHS :: CVar -> CVar -> Set CVar -> Set CVar -> TypeEnv -> Map CVar CType -> Map CVar [Atom] -> RHS -> String
genMSLRHS assignedVar iter inArrs outArrs typeEnv arrayElemTys tupleDefs rhs = case rhs of
  RAtom a -> genMSLAtom iter a
  RBinOp op a1 a2 ->
    "(" ++ genMSLAtom iter a1 ++ " " ++ mslBinOp op ++ " " ++ genMSLAtom iter a2 ++ ")"
  RUnOp op a -> case op of
    CNot -> "!" ++ genMSLAtom iter a
    CNeg -> "-" ++ genMSLAtom iter a
    _ -> mslUnOp op ++ "(" ++ genMSLAtom iter a ++ ")"
  RArrayLoad (AVar arr) idx ->
    sanitize arr ++ "_data[" ++ genMSLAtom iter idx ++ "]"
  RArrayLoad arr idx ->
    genMSLAtom iter arr ++ "[" ++ genMSLAtom iter idx ++ "]"
  RArrayShape arr ->
    -- Array shapes need to be available in the kernel.
    -- For arrays with known shapes (passed as constant buffers), reference the shape directly.
    genMSLAtom iter arr ++ "_shape"
  RShapeSize shp ->
    "hyd_shape_size_t(" ++ genMSLAtom iter shp ++ ")"
  RShapeInit shp ->
    genMSLAtom iter shp -- treat as pass-through for scalar shapes
  RShapeLast shp ->
    genMSLAtom iter shp
  R2DToFlat i w ->
    "((uint)(" ++ genMSLAtom iter i ++ ") * (uint)(" ++ genMSLAtom iter w ++ "))"
  RCall fn args ->
    sanitize fn ++ "(" ++ intercalate ", " (map (genMSLAtom iter) args) ++ ")"
  RPairMake ct1 ct2 a1 a2 ->
    let structName = case Map.lookup assignedVar typeEnv of
          Just (CTPair t1 t2)
            | Just ce1 <- ctypeToElemType t1,
              Just ce2 <- ctypeToElemType t2 ->
                mslPairStructName ce1 ce2
          _ -> mslPairStructName ct1 ct2
     in "((" ++ structName ++ "){.fst = " ++ genMSLAtom iter a1 ++ ", .snd = " ++ genMSLAtom iter a2 ++ "})"
  RPairFst _ a -> genMSLAtom iter a ++ ".fst"
  RPairSnd _ a -> genMSLAtom iter a ++ ".snd"
  RProj i (AVar src)
    | Just (CTPair _ _) <- Map.lookup src typeEnv ->
        genMSLAtom iter (AVar src) ++ if i == 0 then ".fst" else ".snd"
  RProj i a -> genMSLAtom iter a ++ ".elems[" ++ show i ++ "]"
  RRecord fields ->
    "{" ++ intercalate ", " ["." ++ sanitize f ++ " = " ++ genMSLAtom iter a | (f, a) <- fields] ++ "}"
  RRecordProj f a -> genMSLAtom iter a ++ "." ++ sanitize f
  -- SIMD ops lowered to scalar
  RVecLoad (AVar arr) idx ->
    sanitize arr ++ "_data[" ++ genMSLAtom iter idx ++ "]"
  RVecLoad arr idx ->
    genMSLAtom iter arr ++ "[" ++ genMSLAtom iter idx ++ "]"
  RVecBinOp op v1 v2 ->
    "(" ++ genMSLAtom iter v1 ++ " " ++ mslBinOp op ++ " " ++ genMSLAtom iter v2 ++ ")"
  RVecUnOp op v ->
    mslUnOp op ++ "(" ++ genMSLAtom iter v ++ ")"
  RVecSplat a -> genMSLAtom iter a
  RVecReduce _ a -> genMSLAtom iter a
  RTuple atoms ->
    let n = length atoms
        elems = intercalate ", " (map (\a -> "(long)(" ++ genMSLAtom iter a ++ ")") atoms)
     in "(hyd_tuple_t){{" ++ elems ++ "}, " ++ show n ++ "}"
  RFlatToNd flat shp ->
    "hyd_flat_to_nd((long)(" ++ genMSLAtom iter flat ++ "), " ++ genMSLAtom iter shp ++ ")"
  RNdToFlat AUnit _ ->
    "0"
  RNdToFlat (AVar nd) shp
    | Just atoms <- Map.lookup nd tupleDefs,
      length atoms >= 2 ->
        -- Inline row-major arithmetic: ((a0 * shape[1] + a1) * shape[2] + a2) ...
        let shpStr = genMSLAtom iter shp
            n = length atoms
            elemStr i = "(long)(" ++ genMSLAtom iter (atoms !! i) ++ ")"
            dimStr i = shpStr ++ ".elems[" ++ show i ++ "]"
            -- Build Horner form: (...((a0 * d1 + a1) * d2 + a2) ... * d_{n-1} + a_{n-1})
            go acc i
              | i >= n = acc
              | otherwise = go ("((" ++ acc ++ ") * " ++ dimStr i ++ " + " ++ elemStr i ++ ")") (i + 1)
         in go (elemStr 0) 1
  RNdToFlat nd shp ->
    "hyd_nd_to_flat(" ++ genMSLAtom iter nd ++ ", " ++ genMSLAtom iter shp ++ ")"
  _ -> "0 /* unhandled RHS */"

-- | Generate MSL for an atom.
genMSLAtom :: CVar -> Atom -> String
genMSLAtom iter atom = case atom of
  AVar v -> if v == iter then sanitize iter else sanitize v
  AInt n -> show n
  AFloat f -> show f ++ "f"
  ABool True -> "1"
  ABool False -> "0"
  AUnit -> "0"
  AString s -> "\"" ++ BS.unpack s ++ "\""
  AVecVar v -> sanitize v

-- | Generate MSL for an index expression.
genMSLIndexExpr :: CFG.IndexExpr -> String
genMSLIndexExpr expr = case CFG.simplifyIndexExpr expr of
  CFG.IVar v -> sanitize v
  CFG.IConst n -> show n
  CFG.IAdd a b -> "(" ++ genMSLIndexExpr a ++ " + " ++ genMSLIndexExpr b ++ ")"
  CFG.ISub a b -> "(" ++ genMSLIndexExpr a ++ " - " ++ genMSLIndexExpr b ++ ")"
  CFG.IMul a b -> "(" ++ genMSLIndexExpr a ++ " * " ++ genMSLIndexExpr b ++ ")"
  CFG.IDiv a b -> "(" ++ genMSLIndexExpr a ++ " / " ++ genMSLIndexExpr b ++ ")"
  CFG.ITuple es ->
    let elems = intercalate ", " (map (\e -> "(long)(" ++ genMSLIndexExpr e ++ ")") es)
     in "(hyd_tuple_t){{" ++ elems ++ "}, " ++ show (length es) ++ "}"
  CFG.IProj i e -> "(" ++ genMSLIndexExpr e ++ ").elems[" ++ show i ++ "]"
  CFG.IFlatToNd flat shp ->
    "hyd_flat_to_nd((long)(" ++ genMSLIndexExpr flat ++ "), " ++ genMSLIndexExpr shp ++ ")"
  CFG.INdToFlat nd shp ->
    "hyd_nd_to_flat(" ++ genMSLIndexExpr nd ++ ", " ++ genMSLIndexExpr shp ++ ")"
  CFG.ICall _ _ -> "0"
  _ -> "0"

mslBinOp :: BinOp -> String
mslBinOp CAdd = "+"
mslBinOp CSub = "-"
mslBinOp CMul = "*"
mslBinOp CDiv = "/"
mslBinOp CMod = "%"
mslBinOp CEq = "=="
mslBinOp CNeq = "!="
mslBinOp CLt = "<"
mslBinOp CLe = "<="
mslBinOp CGt = ">"
mslBinOp CGe = ">="
mslBinOp CAnd = "&&"
mslBinOp COr = "||"
mslBinOp CAddF = "+"
mslBinOp CSubF = "-"
mslBinOp CMulF = "*"
mslBinOp CDivF = "/"
mslBinOp CEqF = "=="
mslBinOp CNeqF = "!="
mslBinOp CLtF = "<"
mslBinOp CLeF = "<="
mslBinOp CGtF = ">"
mslBinOp CGeF = ">="

mslUnOp :: UnOp -> String
mslUnOp CSqrt = "sqrt"
mslUnOp CExpF = "exp"
mslUnOp CLog = "log"
mslUnOp CSin = "sin"
mslUnOp CCos = "cos"
mslUnOp CAbsF = "fabs"
mslUnOp CFloorF = "floor"
mslUnOp CCeilF = "ceil"
mslUnOp CErf = "erf"
mslUnOp CFloatOf = "(float)"
mslUnOp CIntOf = "(long)"
mslUnOp CNot = "!"
mslUnOp CNeg = "-"

isVecRHS :: RHS -> Bool
isVecRHS (RVecLoad {}) = True
isVecRHS (RVecBinOp {}) = True
isVecRHS (RVecUnOp {}) = True
isVecRHS (RVecSplat {}) = True
isVecRHS (RVecReduce {}) = True
isVecRHS _ = False

-- ---------------------------------------------------------------------------
-- ObjC harness generation

-- | Generate helper C code for all procs (including the kernel proc, since
-- other procs may call it transitively). The harness main() uses the Metal
-- version of the kernel and ignores the C version.
genHelperC :: [CFG.Proc] -> CVar -> Map CVar VarKind -> Either String String
genHelperC procs _kernelName _retKinds =
  let helperOpts = defaultCodegenOptions {codegenEmitMain = False, codegenUseFloat = True}
   in case codegenProgramWithOptions helperOpts (CFG.Program procs) of
        Right arts -> Right (codegenSource arts)
        Left err -> Left err

-- | Generate the self-contained ObjC harness source.
genObjCHarnessSrc ::
  CFG.Program ->
  CFG.Proc ->
  CVar ->
  -- | Pre-loop stmts
  [CFG.Stmt] ->
  CFG.LoopSpec ->
  -- | Return atom (the proc's actual result)
  Maybe Atom ->
  -- | Post-loop stmts (CPU work between the kernel loop and the return)
  [CFG.Stmt] ->
  TypeEnv ->
  -- | Array element types
  Map CVar CType ->
  Map CVar VarKind ->
  -- | Program-level return types
  Map CVar CType ->
  -- | Input arrays
  [CVar] ->
  -- | Output arrays
  [CVar] ->
  -- | Scalar inputs
  [CVar] ->
  -- | Proc parameters (declared with defaults)
  [CVar] ->
  -- | Helper C source (already generated)
  String ->
  String
genObjCHarnessSrc
  _prog
  _kernelProc
  kernelName
  preLoopStmts
  loopSpec
  retAtom
  postLoopStmts
  typeEnv
  arrayElemTys
  retKinds
  _retTypes
  inputArrays
  outputArrays
  scalarInputs
  procParams
  helperC =
    unlines $
      [ "// Hydrangea Metal harness — generated by the MSL backend.",
        "#include \"hydrangea_runtime.h\"",
        "#include <stdio.h>",
        "#include <string.h>",
        "#include <stdlib.h>",
        "#import <Foundation/Foundation.h>",
        "#import <Metal/Metal.h>",
        "",
        "// ---- CPU helper procs ----",
        helperC,
        "",
        "// ---- Metal harness main ----",
        "int main(int argc, const char* argv[]) {",
        "    @autoreleasepool {",
        "    const char* _metallib = argc > 1 ? argv[1] : \"kernel.metallib\";",
        "",
        "    // --- Proc parameters (defaults) ---"
      ]
        ++ procParamLines
        ++ [ "    // --- CPU pre-loop setup ---"
           ]
        ++ preLoopLines
        ++ [ "",
             "    // Grid size from loop bound(s)",
             "    long _n = " ++ gridSizeExpr ++ ";",
             "",
             "    // --- Metal device and pipeline setup ---",
             "    id<MTLDevice> _dev = MTLCreateSystemDefaultDevice();",
             "    if (!_dev) { fprintf(stderr, \"hydrangea: Metal not available\\n\"); return 1; }",
             "    NSError* _err = nil;",
             "    NSURL* _libURL = [NSURL fileURLWithPath:[NSString stringWithUTF8String:_metallib]];",
             "    id<MTLLibrary> _lib = [_dev newLibraryWithURL:_libURL error:&_err];",
             "    if (!_lib) {",
             "        fprintf(stderr, \"hydrangea: failed to load metallib: %s\\n\",",
             "                [_err.localizedDescription UTF8String]);",
             "        return 1;",
             "    }",
             "    id<MTLFunction> _fn = [_lib newFunctionWithName:@\"" ++ sanitize kernelName ++ "\"];",
             "    if (!_fn) { fprintf(stderr, \"hydrangea: kernel function not found\\n\"); return 1; }",
             "    id<MTLComputePipelineState> _pso =",
             "        [_dev newComputePipelineStateWithFunction:_fn error:&_err];",
             "    if (!_pso) {",
             "        fprintf(stderr, \"hydrangea: pipeline error: %s\\n\",",
             "                [_err.localizedDescription UTF8String]);",
             "        return 1;",
             "    }",
             "    id<MTLCommandQueue> _queue = [_dev newCommandQueue];",
             "",
             "    // --- Create Metal buffers ---"
           ]
        ++ inputBufLines
        ++ inputShapeBufLines
        ++ outputBufLines
        ++ scalarBufLines
        ++ [ "",
             "    // --- Dispatch ---",
             "    id<MTLCommandBuffer> _cmd = [_queue commandBuffer];",
             "    id<MTLComputeCommandEncoder> _enc = [_cmd computeCommandEncoder];",
             "    [_enc setComputePipelineState:_pso];"
           ]
        ++ setBufLines
        ++ [ "    NSUInteger _tgs = MIN(256UL, _pso.maxTotalThreadsPerThreadgroup);",
             "    if (_tgs == 0) _tgs = 1;",
             "    [_enc dispatchThreads:MTLSizeMake((NSUInteger)_n, 1, 1)",
             "      threadsPerThreadgroup:MTLSizeMake(_tgs, 1, 1)];",
             "    [_enc endEncoding];",
             "    [_cmd commit];",
             "    [_cmd waitUntilCompleted];",
             ""
           ]
        ++ resultLines
        ++ [ "    } // @autoreleasepool",
             "    return 0;",
             "}"
           ]
    where
      iter = case CFG.lsIters loopSpec of
        (i:_) -> i
        []    -> error "internal error: genSingleKernelHarnessSrc: loop spec has no iterators"
      nInputArrays = length inputArrays
      totalBufs = nInputArrays * 2 + length outputArrays + length scalarInputs -- data + shape per input

      -- Proc parameter declarations with default values
      procParamLines = concatMap declProcParam procParams
      declProcParam v =
        let ty = case Map.lookup v typeEnv of
              Just ct -> ct
              Nothing -> CTInt64
            cTy = cTypeName ty
            defVal = case ty of
              CTDouble -> "0.0"
              CTBool -> "0"
              CTArray _ -> "NULL"
              CTPair _ _ -> "((" ++ cTypeName ty ++ "){0})"
              _ -> "0LL"
         in ["    " ++ cTy ++ " " ++ sanitize v ++ " = " ++ defVal ++ ";"]

      -- Grid size expression: product of all loop bounds
      gridSizeExpr = case CFG.lsBounds loopSpec of
        [b] -> "(long)" ++ genMSLIndexExpr b
        bs -> intercalate " * " ["(long)" ++ genMSLIndexExpr b | b <- bs]

      -- Pre-loop C statements (skip output array allocations)
      preLoopLines = concatMap (genPreLoopLine typeEnv retKinds (Set.fromList outputArrays) Set.empty) preLoopStmts

      -- Input buffer creation + fill
      -- Note: CPU arrays storing CTDouble use 8-byte doubles, but Metal uses 4-byte floats.
      -- We must convert element-by-element for float arrays.
      inputBufLines =
        concat
          [ [ "    // Input buffer " ++ show i ++ ": " ++ sanitize v,
              "    long _inN" ++ show i ++ " = hyd_shape_size(" ++ sanitize v ++ "->shape);",
              "    size_t _buf" ++ show i ++ "_sz = (size_t)(_inN" ++ show i ++ " * sizeof(" ++ elemTy ++ "));",
              "    id<MTLBuffer> _buf"
                ++ show i
                ++ " = [_dev newBufferWithLength:_buf"
                ++ show i
                ++ "_sz options:MTLResourceStorageModeShared];"
            ]
              ++ fillLine i v elemTy
          | (v, i) <- zip inputArrays [0 ..],
            let elemTy = mslElemTy v
          ]
        where
          fillLine i v "float" =
            -- CPU stores double (8 bytes); Metal reads float (4 bytes): convert
            [ "    { double* _fsrc = (double*)" ++ sanitize v ++ "->data; float* _fdst = (float*)_buf" ++ show i ++ ".contents;",
              "      for (long _fci = 0; _fci < _inN" ++ show i ++ "; _fci++) _fdst[_fci] = (float)_fsrc[_fci]; }"
            ]
          fillLine i v _ =
            -- Same element size on CPU and GPU (int64 / long): plain memcpy
            ["    memcpy(_buf" ++ show i ++ ".contents, " ++ sanitize v ++ "->data, _buf" ++ show i ++ "_sz);"]

      -- Shape buffers for input arrays
      inputShapeBufLines =
        concat
          [ [ "    // Shape buffer " ++ show i ++ ": " ++ sanitize v ++ " shape",
              "    id<MTLBuffer> _buf"
                ++ show i
                ++ " = [_dev newBufferWithBytes:&"
                ++ sanitize v
                ++ "->shape length:sizeof(hyd_tuple_t) options:MTLResourceStorageModeShared];"
            ]
          | (v, i) <- zip inputArrays [nInputArrays ..]
          ]

      -- Output buffer creation
      -- Map from output array name → shape variable used in its RArrayAlloc
      outputAllocShapes =
        Map.fromList
          [(v, shpAtom) | CFG.SAssign v (RArrayAlloc shpAtom) <- preLoopStmts]
      outputBufLines =
        concat
          [ [ "    // Output buffer " ++ show i ++ ": " ++ sanitize v,
              "    long _outN" ++ show i ++ " = " ++ outSizeExpr v ++ ";",
              "    size_t _buf" ++ show i ++ "_sz = (size_t)(_outN" ++ show i ++ " * sizeof(" ++ elemTy ++ "));",
              "    id<MTLBuffer> _buf"
                ++ show i
                ++ " = [_dev newBufferWithLength:_buf"
                ++ show i
                ++ "_sz options:MTLResourceStorageModeShared];",
              "    memset(_buf" ++ show i ++ ".contents, 0, _buf" ++ show i ++ "_sz);"
            ]
          | (v, i) <- zip outputArrays [nInputArrays * 2 ..],
            let elemTy = mslElemTy v
          ]
      -- Output size: use the allocation shape if known, otherwise fall back to _n
      outSizeExpr v = case Map.lookup v outputAllocShapes of
        Just shpAtom -> "hyd_shape_size(" ++ genCAtom shpAtom ++ ")"
        Nothing -> "_n"

      -- Scalar declarations (passed via setBytes, no buffer allocation)
      scalarBufLines =
        concat
          [ [ "    // Scalar " ++ show i ++ ": " ++ sanitize v,
              "    " ++ scalarTy v ++ " _scalar" ++ show i ++ " = " ++ sanitize v ++ ";"
            ]
          | (v, i) <- zip scalarInputs [nInputArrays * 2 + length outputArrays ..]
          ]

      -- setBuffer for array/shape/output buffers, setBytes for scalars
      scalarStartIdx = nInputArrays * 2 + length outputArrays
      setBufLines =
        [ "    [_enc setBuffer:_buf" ++ show i ++ " offset:0 atIndex:" ++ show i ++ "];"
        | i <- [0 .. scalarStartIdx - 1]
        ]
          ++ [ "    [_enc setBytes:&_scalar" ++ show i ++ " length:sizeof(" ++ scalarTy v ++ ") atIndex:" ++ show i ++ "];"
             | (v, i) <- zip scalarInputs [scalarStartIdx ..]
             ]

      -- Output printing
      printLines =
        concat
          [ genOutputPrintLines v i
          | (v, i) <- zip outputArrays [nInputArrays * 2 ..]
          ]

      -- The proc's result. When the kernel's output array IS the return value
      -- (the common map/reduce case), print the GPU buffer directly. Otherwise
      -- (e.g. scatter→reduce→project, where the GPU loop is an intermediate),
      -- copy the GPU output back to a CPU array, run the post-loop CPU work, and
      -- print the proc's actual return value — not the kernel buffer.
      resultLines
        | retIsKernelOutput =
            ["    // --- Print output (kernel buffer is the result) ---"] ++ printLines
        | otherwise =
            ["    // --- Read GPU output back, finish on CPU, print result ---"]
              ++ readbackLines
              ++ concatMap genRunPostLoopLine postLoopStmts
              ++ printReturnLines

      retIsKernelOutput = retResolvesToOutput retAtom postLoopStmts outputArrays

      -- Copy each GPU output buffer into a CPU hyd_array_t* bound to the proc's
      -- own variable name, so post-loop CPU statements can read it.
      readbackLines =
        concat
          [ let elemTy = mslElemTy v
                sizeVar = "_outN" ++ show i
                shapeExpr = case Map.lookup v outputAllocShapes of
                  Just shpAtom -> genCAtom shpAtom
                  Nothing -> "hyd_tuple_make(1, (int64_t)" ++ sizeVar ++ ")"
             in [ "    // Read back GPU output: " ++ sanitize v,
                  "    hyd_array_t* " ++ sanitize v ++ " = hyd_array_alloc(" ++ shapeExpr ++ ");"
                ]
                  ++ ( if elemTy == "float"
                         then
                           [ "    { float* _gsrc = (float*)_buf" ++ show i ++ ".contents;",
                             "      double* _gdst = (double*)" ++ sanitize v ++ "->data;",
                             "      for (long _ri = 0; _ri < " ++ sizeVar ++ "; _ri++) _gdst[_ri] = (double)_gsrc[_ri]; }"
                           ]
                         else
                           [ "    memcpy("
                               ++ sanitize v
                               ++ "->data, _buf"
                               ++ show i
                               ++ ".contents, (size_t)("
                               ++ sizeVar
                               ++ " * sizeof("
                               ++ elemTy
                               ++ ")));"
                           ]
                     )
          | (v, i) <- zip outputArrays [nInputArrays * 2 ..]
          ]

      -- CPU post-loop statements; reshape_view / pair construction are handled
      -- by the return printing itself.
      genRunPostLoopLine (CFG.SAssign _ (RCall "hyd_array_reshape_view" _)) = []
      genRunPostLoopLine (CFG.SAssign _ (RPairMake {})) = []
      genRunPostLoopLine stmt = genPreLoopLine typeEnv retKinds Set.empty Set.empty stmt

      -- Print the proc's actual return value (scalar, array, or fallback).
      printReturnLines = case retAtom of
        -- A unit return is a pure side-effecting sink (e.g. a CSV file write run
        -- as post-kernel CPU work): there is nothing to print to stdout.
        Just AUnit -> []
        Just (AVar v) -> case Map.lookup v typeEnv of
          Just (CTArray elt) -> genResultArrayPrint v (mslTypeName elt)
          Just CTDouble -> ["    printf(\"%.17g\\n\", (double)" ++ sanitize v ++ ");"]
          Just _ -> ["    printf(\"%ld\\n\", (long)" ++ sanitize v ++ ");"]
          Nothing -> ["    printf(\"%ld\\n\", (long)" ++ sanitize v ++ ");"]
        Just (AInt n) -> ["    printf(\"%ld\\n\", (long)" ++ show n ++ "LL);"]
        Just (AFloat f) -> ["    printf(\"%.17g\\n\", (double)" ++ show f ++ ");"]
        _ -> printLines

      genResultArrayPrint v eTy =
        let fmt = if eTy == "float" then "%.17g" else "%ld"
            cast = if eTy == "float" then "(double)" else "(long)"
            nm = sanitize v
         in [ "    { " ++ eTy ++ "* _rp = (" ++ eTy ++ "*)" ++ nm ++ "->data;",
              "      long _rn = hyd_shape_size(" ++ nm ++ "->shape);",
              "      printf(\"[\");",
              "      for (long _pi = 0; _pi < _rn; _pi++) { if (_pi > 0) printf(\", \"); printf(\"" ++ fmt ++ "\", " ++ cast ++ "_rp[_pi]); }",
              "      printf(\"]\\n\"); }"
            ]

      mslElemTy v = case Map.lookup v arrayElemTys of
        Just elt -> mslTypeName elt
        Nothing -> case Map.lookup v typeEnv of
          Just (CTArray elt) -> mslTypeName elt
          _ -> "long"

      scalarTy v = case Map.lookup v typeEnv of
        Just ct -> mslTypeName ct
        Nothing -> "long"

      genOutputPrintLines v i =
        let eTy = mslElemTy v
            fmt = if eTy == "float" then "%.17g" else "%ld"
            cast = if eTy == "float" then "(double)" else "(long)"
            sizeVar = "_outN" ++ show i
         in [ "    // Print output: " ++ sanitize v,
              "    " ++ eTy ++ "* _out" ++ show i ++ " = (" ++ eTy ++ "*)_buf" ++ show i ++ ".contents;",
              "    printf(\"[\");",
              "    for (long _pi = 0; _pi < " ++ sizeVar ++ "; _pi++) {",
              "        if (_pi > 0) printf(\", \");",
              "        printf(\"" ++ fmt ++ "\", " ++ cast ++ "_out" ++ show i ++ "[_pi]);",
              "    }",
              "    printf(\"]\\n\");"
            ]

-- ---------------------------------------------------------------------------
-- Multi-kernel ObjC harness generation

-- | Generate a harness that dispatches multiple GPU kernels in sequence.
-- Intermediate buffers stay on GPU; only the last kernel's outputs are read back.
genMultiKernelHarnessSrc ::
  CFG.Program ->
  -- | Kernels in execution order
  [KernelAnalysis] ->
  -- | Buffer origin for each input array
  Map CVar BufferOrigin ->
  -- | GPU buffer aliases (consumer var → producer output var)
  Map CVar CVar ->
  -- | Return kinds
  Map CVar VarKind ->
  -- | Program-level return types
  Map CVar CType ->
  -- | Helper C source
  String ->
  String
genMultiKernelHarnessSrc _prog kernels bufferOrigins gpuAliases retKinds _retTypes helperC =
  unlines $
    -- Header
    [ "// Hydrangea Metal harness (multi-kernel) — generated by the MSL backend.",
      "#include \"hydrangea_runtime.h\"",
      "#include <stdio.h>",
      "#include <string.h>",
      "#include <stdlib.h>",
      "#import <Foundation/Foundation.h>",
      "#import <Metal/Metal.h>",
      "",
      "// ---- CPU helper procs ----",
      helperC,
      "",
      "// ---- Metal harness main ----",
      "int main(int argc, const char* argv[]) {",
      "    @autoreleasepool {",
      "    const char* _metallib = argc > 1 ? argv[1] : \"kernel.metallib\";",
      "",
      "    // --- Metal device and library setup ---",
      "    id<MTLDevice> _dev = MTLCreateSystemDefaultDevice();",
      "    if (!_dev) { fprintf(stderr, \"hydrangea: Metal not available\\n\"); return 1; }",
      "    NSError* _err = nil;",
      "    NSURL* _libURL = [NSURL fileURLWithPath:[NSString stringWithUTF8String:_metallib]];",
      "    id<MTLLibrary> _lib = [_dev newLibraryWithURL:_libURL error:&_err];",
      "    if (!_lib) {",
      "        fprintf(stderr, \"hydrangea: failed to load metallib: %s\\n\",",
      "                [_err.localizedDescription UTF8String]);",
      "        return 1;",
      "    }",
      "    id<MTLCommandQueue> _queue = [_dev newCommandQueue];",
      ""
    ]
      ++
      -- Pipeline state objects (one per kernel)
      concatMap genPSO indexedKernels
      ++ [""]
      ++
      -- Per-kernel dispatch blocks
      concatMap genKernelBlock indexedKernels
      ++
      -- Print output (from last kernel only)
      ["    // --- Print output ---"]
      ++ genLastKernelPrint
      ++ [ "    } // @autoreleasepool",
           "    return 0;",
           "}"
         ]
  where
    indexedKernels = zip [0 ..] kernels
    lastIdx = length kernels - 1

    -- Naming: _k<idx>_<buftype><sub>
    kPrefix :: Int -> String
    kPrefix ki = "_k" ++ show ki ++ "_"

    -- Generate pipeline state object for one kernel
    genPSO (ki, ka) =
      let p = kPrefix ki
       in [ "    // Pipeline: " ++ sanitize (kaName ka),
            "    id<MTLFunction> " ++ p ++ "fn = [_lib newFunctionWithName:@\"" ++ sanitize (kaName ka) ++ "\"];",
            "    if (!"
              ++ p
              ++ "fn) { fprintf(stderr, \"hydrangea: kernel function '"
              ++ sanitize (kaName ka)
              ++ "' not found\\n\"); return 1; }",
            "    id<MTLComputePipelineState> " ++ p ++ "pso =",
            "        [_dev newComputePipelineStateWithFunction:" ++ p ++ "fn error:&_err];",
            "    if (!" ++ p ++ "pso) { fprintf(stderr, \"hydrangea: pipeline error: %s\\n\",",
            "            [_err.localizedDescription UTF8String]); return 1; }"
          ]

    -- Generate the full dispatch block for one kernel
    genKernelBlock (ki, ka) =
      let p = kPrefix ki
          te = kaTypeEnv ka
          ae = kaArrayElemTys ka
          inArrs = kaInputArrays ka
          outArrs = kaOutputArrays ka
          scals = kaScalarInputs ka
          nInArrs = length inArrs
          -- Skip set: output arrays (Metal buffers, not CPU-allocated)
          skipAlloc = Set.fromList outArrs
          -- Also skip RCall to GPU-produced arrays in pre-loop
          gpuProducedVars =
            Set.fromList
              [ v
              | CFG.SAssign v (RCall fn []) <- kaEffectivePreLoop ka,
                case Map.lookup v bufferOrigins of
                  Just (GPUProduced _) -> True
                  _ -> False
              ]
          -- Shape expressions for GPU-produced vars (from producer output shapes)
          gpuKernelNames = Set.fromList [kaName k | k <- kernels]
          gpuShapeExprs =
            Map.fromList
              [ (v, findProducerShapeVar fn)
              | CFG.SAssign v (RCall fn []) <- kaEffectivePreLoop ka,
                fn `Set.member` gpuKernelNames
              ]
       in ["", "    // ======== Kernel " ++ show ki ++ ": " ++ sanitize (kaName ka) ++ " ========"]
            ++
            -- Pre-loop (skip GPU-produced array calls and output allocs)
            ["    // CPU pre-loop"]
            ++ concatMap (genMultiPreLoopLine te retKinds skipAlloc gpuProducedVars gpuShapeExprs Set.empty) (kaEffectivePreLoop ka)
            ++
            -- Grid size
            ["    long " ++ p ++ "n = " ++ gridSizeExprFor ka ++ ";"]
            ++
            -- Input buffers
            concatMap (genInputBuf ki p te ae) (zip inArrs [0 ..])
            ++
            -- Input shape buffers
            concatMap (genInputShapeBuf ki p) (zip inArrs [nInArrs ..])
            ++
            -- Output buffers
            concatMap (genOutputBuf ki p ka te ae) (zip outArrs [nInArrs * 2 ..])
            ++
            -- Scalar buffers
            concatMap (genScalarBuf ki p te) (zip scals [nInArrs * 2 + length outArrs ..])
            ++
            -- Dispatch
            genDispatch ki p ka (nInArrs * 2 + length outArrs + length scals)

    gridSizeExprFor ka = case CFG.lsBounds (kaLoopSpec ka) of
      [b] -> "(long)" ++ genMSLIndexExpr b
      bs -> intercalate " * " ["(long)" ++ genMSLIndexExpr b | b <- bs]

    -- Input buffer for one array. Reuse GPU buffer if the origin is a prior kernel.
    genInputBuf ki p te ae (v, sub) =
      let bufName = p ++ "buf" ++ show sub
          elemTy = mslElemTyFor ae te v
       in case Map.lookup v gpuAliases of
            Just producerVar ->
              -- Reuse the GPU buffer from the producing kernel
              let producerBuf = findProducerBufName producerVar
               in [ "    // Input " ++ sanitize v ++ " — reuse GPU buffer from " ++ sanitize producerVar,
                    "    id<MTLBuffer> " ++ bufName ++ " = " ++ producerBuf ++ ";"
                  ]
            Nothing ->
              -- CPU-produced: upload from hyd_array_t*
              [ "    // Input " ++ sanitize v,
                "    long " ++ p ++ "inN" ++ show sub ++ " = hyd_shape_size(" ++ sanitize v ++ "->shape);",
                "    size_t " ++ bufName ++ "_sz = (size_t)(" ++ p ++ "inN" ++ show sub ++ " * sizeof(" ++ elemTy ++ "));",
                "    id<MTLBuffer> " ++ bufName ++ " = [_dev newBufferWithLength:" ++ bufName ++ "_sz options:MTLResourceStorageModeShared];"
              ]
                ++ fillLine (p ++ "inN" ++ show sub) bufName v elemTy

    fillLine sizeVar bufName v "float" =
      [ "    { double* _fsrc = (double*)" ++ sanitize v ++ "->data; float* _fdst = (float*)" ++ bufName ++ ".contents;",
        "      for (long _fci = 0; _fci < " ++ sizeVar ++ "; _fci++) _fdst[_fci] = (float)_fsrc[_fci]; }"
      ]
    fillLine _sizeVar bufName v _ =
      ["    memcpy(" ++ bufName ++ ".contents, " ++ sanitize v ++ "->data, " ++ bufName ++ "_sz);"]

    genInputShapeBuf ki p (v, sub) =
      let bufName = p ++ "buf" ++ show sub
       in case Map.lookup v gpuAliases of
            Just producerVar ->
              -- Reuse shape from producing kernel's output shape
              let producerShapeBuf = findProducerShapeBufName producerVar
               in ["    id<MTLBuffer> " ++ bufName ++ " = " ++ producerShapeBuf ++ ";"]
            Nothing ->
              [ "    // Shape: " ++ sanitize v,
                "    id<MTLBuffer> "
                  ++ bufName
                  ++ " = [_dev newBufferWithBytes:&"
                  ++ sanitize v
                  ++ "->shape length:sizeof(hyd_tuple_t) options:MTLResourceStorageModeShared];"
              ]

    genOutputBuf ki p ka te ae (v, sub) =
      let bufName = p ++ "buf" ++ show sub
          elemTy = mslElemTyFor ae te v
          outputAllocShapes =
            Map.fromList
              [(ov, shpAtom) | CFG.SAssign ov (RArrayAlloc shpAtom) <- kaEffectivePreLoop ka]
          sizeExpr = case Map.lookup v outputAllocShapes of
            Just shpAtom -> "hyd_shape_size(" ++ genCAtom shpAtom ++ ")"
            Nothing -> p ++ "n"
       in [ "    // Output " ++ sanitize v,
            "    long " ++ p ++ "outN" ++ show sub ++ " = " ++ sizeExpr ++ ";",
            "    size_t " ++ bufName ++ "_sz = (size_t)(" ++ p ++ "outN" ++ show sub ++ " * sizeof(" ++ elemTy ++ "));",
            "    id<MTLBuffer> " ++ bufName ++ " = [_dev newBufferWithLength:" ++ bufName ++ "_sz options:MTLResourceStorageModeShared];",
            "    memset(" ++ bufName ++ ".contents, 0, " ++ bufName ++ "_sz);",
            -- Also create a shape buffer for this output (needed if a downstream kernel references it)
            "    hyd_tuple_t " ++ p ++ "outShape" ++ show sub ++ " = " ++ outputShapeInit ka v ++ ";",
            "    id<MTLBuffer> "
              ++ p
              ++ "outShapeBuf"
              ++ show sub
              ++ " = [_dev newBufferWithBytes:&"
              ++ p
              ++ "outShape"
              ++ show sub
              ++ " length:sizeof(hyd_tuple_t) options:MTLResourceStorageModeShared];"
          ]

    -- Build the shape from the allocation, or from a post-loop reshape if present.
    -- Scatter kernels allocate a 1D buffer but reshape to N-D before returning;
    -- downstream kernels need the N-D shape for correct indexing.
    outputShapeInit ka v =
      let outputAllocShapes =
            Map.fromList
              [(ov, shpAtom) | CFG.SAssign ov (RArrayAlloc shpAtom) <- kaEffectivePreLoop ka]
          -- Check if the output array is reshaped in the post-loop before returning
          reshapeShape =
            listToMaybe
              [ shpAtom
              | CFG.SAssign _ (RCall "hyd_array_reshape_view" [arrAtom, shpAtom]) <-
                  kaPostLoopStmts ka,
                arrAtom == AVar v
              ]
       in case reshapeShape of
            Just shpAtom -> genCAtom shpAtom
            Nothing -> case Map.lookup v outputAllocShapes of
              Just shpAtom -> genCAtom shpAtom
              Nothing ->
                -- Fallback: build from loop bounds
                let bs = CFG.lsBounds (kaLoopSpec ka)
                 in "hyd_tuple_make("
                      ++ show (length bs)
                      ++ ", "
                      ++ intercalate ", " ["(int64_t)" ++ genMSLIndexExpr b | b <- bs]
                      ++ ")"

    genScalarBuf ki p te (v, sub) =
      let bufName = p ++ "buf" ++ show sub
          sty = case Map.lookup v te of
            Just ct -> mslTypeName ct
            Nothing -> "long"
       in [ "    // Scalar " ++ sanitize v,
            "    " ++ sty ++ " " ++ p ++ "scalar" ++ show sub ++ " = " ++ sanitize v ++ ";",
            "    id<MTLBuffer> "
              ++ bufName
              ++ " = [_dev newBufferWithBytes:&"
              ++ p
              ++ "scalar"
              ++ show sub
              ++ " length:sizeof("
              ++ sty
              ++ ") options:MTLResourceStorageModeShared];"
          ]
      where
        te = kaTypeEnv (snd (indexedKernels !! ki))

    genDispatch ki p ka nBufs =
      [ "    // Dispatch " ++ sanitize (kaName ka),
        "    id<MTLCommandBuffer> " ++ p ++ "cmd = [_queue commandBuffer];",
        "    id<MTLComputeCommandEncoder> " ++ p ++ "enc = [" ++ p ++ "cmd computeCommandEncoder];",
        "    [" ++ p ++ "enc setComputePipelineState:" ++ p ++ "pso];"
      ]
        ++ [ "    [" ++ p ++ "enc setBuffer:" ++ p ++ "buf" ++ show i ++ " offset:0 atIndex:" ++ show i ++ "];"
           | i <- [0 .. nBufs - 1]
           ]
        ++ [ "    NSUInteger " ++ p ++ "tgs = MIN(256UL, " ++ p ++ "pso.maxTotalThreadsPerThreadgroup);",
             "    if (" ++ p ++ "tgs == 0) " ++ p ++ "tgs = 1;",
             "    [" ++ p ++ "enc dispatchThreads:MTLSizeMake((NSUInteger)" ++ p ++ "n, 1, 1)",
             "      threadsPerThreadgroup:MTLSizeMake(" ++ p ++ "tgs, 1, 1)];",
             "    [" ++ p ++ "enc endEncoding];",
             "    [" ++ p ++ "cmd commit];",
             "    [" ++ p ++ "cmd waitUntilCompleted];"
           ]

    -- Find the buffer name for a producer's output variable
    findProducerBufName :: CVar -> String
    findProducerBufName prodVar =
      head $
        [ kPrefix ki ++ "buf" ++ show sub
        | (ki, ka) <- indexedKernels,
          let outArrs = kaOutputArrays ka
              nIn = length (kaInputArrays ka),
          (ov, sub) <- zip outArrs [nIn * 2 ..],
          ov == prodVar
        ]
          ++ ["/* ERROR: producer buffer not found for " ++ sanitize prodVar ++ " */"]

    findProducerShapeBufName :: CVar -> String
    findProducerShapeBufName prodVar =
      head $
        [ kPrefix ki ++ "outShapeBuf" ++ show sub
        | (ki, ka) <- indexedKernels,
          let outArrs = kaOutputArrays ka
              nIn = length (kaInputArrays ka),
          (ov, sub) <- zip outArrs [nIn * 2 ..],
          ov == prodVar
        ]
          ++ ["/* ERROR: producer shape buffer not found for " ++ sanitize prodVar ++ " */"]

    -- Find the C variable name holding the shape for a producer kernel's output
    findProducerShapeVar :: CVar -> String
    findProducerShapeVar prodKernelName =
      head $
        [ kPrefix ki ++ "outShape" ++ show sub
        | (ki, ka) <- indexedKernels,
          kaName ka == prodKernelName,
          let outArrs = kaOutputArrays ka
              nIn = length (kaInputArrays ka),
          (_ov, sub) <- zip outArrs [nIn * 2 ..]
        ]
          ++ ["((hyd_tuple_t){0})"]

    -- Print output from the last kernel only
    genLastKernelPrint = case reverse indexedKernels of
      [] -> []
      ((ki, ka) : _) ->
        let p = kPrefix ki
            ae = kaArrayElemTys ka
            te = kaTypeEnv ka
            outArrs = kaOutputArrays ka
            nIn = length (kaInputArrays ka)
         in concat
              [ let sub = nIn * 2 + idx
                    bufName = p ++ "buf" ++ show sub
                    sizeVar = p ++ "outN" ++ show sub
                    elemTy = mslElemTyFor ae te v
                    fmt = if elemTy == "float" then "%.17g" else "%ld"
                    cast = if elemTy == "float" then "(double)" else "(long)"
                 in [ "    // Print output: " ++ sanitize v,
                      "    " ++ elemTy ++ "* " ++ p ++ "out" ++ show sub ++ " = (" ++ elemTy ++ "*)" ++ bufName ++ ".contents;",
                      "    printf(\"[\");",
                      "    for (long _pi = 0; _pi < " ++ sizeVar ++ "; _pi++) {",
                      "        if (_pi > 0) printf(\", \");",
                      "        printf(\"" ++ fmt ++ "\", " ++ cast ++ p ++ "out" ++ show sub ++ "[_pi]);",
                      "    }",
                      "    printf(\"]\\n\");"
                    ]
              | (idx, v) <- zip [0 ..] outArrs
              ]

    mslElemTyFor ae te v = case Map.lookup v ae of
      Just elt -> mslTypeName elt
      Nothing -> case Map.lookup v te of
        Just (CTArray elt) -> mslTypeName elt
        _ -> "long"

-- | Pre-loop line generator for multi-kernel harness.
-- For GPU-produced arrays, emits a shape-only stub instead of calling the
-- CPU helper, so downstream code can access @v->shape@.
genMultiPreLoopLine ::
  TypeEnv ->
  Map CVar VarKind ->
  Set CVar ->
  -- | GPU-produced variables to stub
  Set CVar ->
  -- | GPU-produced var → shape expression
  Map CVar String ->
  Set CVar ->
  CFG.Stmt ->
  [String]
genMultiPreLoopLine typeEnv retKinds skipAlloc gpuProduced gpuShapeExprs preDeclared stmt = case stmt of
  CFG.SAssign v (RCall _ [])
    | v `Set.member` gpuProduced ->
        -- Emit a shape-only stub so v->shape is valid
        let shapeExpr = Map.findWithDefault "((hyd_tuple_t){0})" v gpuShapeExprs
         in [ "    // GPU-produced array (stub for shape access)",
              "    hyd_array_t _stub_" ++ sanitize v ++ " = { .shape = " ++ shapeExpr ++ ", .data = NULL };",
              "    hyd_array_t* " ++ sanitize v ++ " = &_stub_" ++ sanitize v ++ ";"
            ]
  _ -> genPreLoopLine typeEnv retKinds skipAlloc preDeclared stmt

-- | Generate a single pre-loop C statement for the ObjC harness main.
-- Returns empty list for output array allocations (replaced by Metal buffers).
-- @preDeclared@ is the set of variables already declared (e.g. by an outer SIf).
genPreLoopLine :: TypeEnv -> Map CVar VarKind -> Set CVar -> Set CVar -> CFG.Stmt -> [String]
genPreLoopLine typeEnv retKinds skipAlloc preDeclared stmt = case stmt of
  CFG.SAssign v (RArrayAlloc _)
    | v `Set.member` skipAlloc ->
        [] -- output array: skip CPU alloc, will be Metal buffer
  CFG.SAssign v rhs
    | v `Set.member` preDeclared ->
        [ "    "
            ++ sanitize v
            ++ " = "
            ++ genPreLoopRHS typeEnv retKinds v rhs
            ++ ";"
        ]
    | otherwise ->
        [ "    "
            ++ preLoopCType typeEnv retKinds v rhs
            ++ " "
            ++ sanitize v
            ++ " = "
            ++ genPreLoopRHS typeEnv retKinds v rhs
            ++ ";"
        ]
  CFG.SLoop spec body
    -- Skip pre-loop loops that only write to output arrays (Metal buffers)
    | let written = collectWrittenArrays body,
      not (Set.null written),
      written `Set.isSubsetOf` skipAlloc ->
        []
  CFG.SLoop spec body ->
    genPreLoopCLoop 1 typeEnv retKinds skipAlloc preDeclared spec body
  CFG.SArrayWrite arr idx val ->
    ["    " ++ genPreLoopCArrayWrite typeEnv arr idx val]
  CFG.SIf cond thn els ->
    let -- Pre-declare all variables assigned inside the if branches
        ifVars = Set.fromList (collectAssignedVars thn ++ collectAssignedVars els)
        newDecls = ifVars `Set.difference` preDeclared
        declLines =
          [ "    "
              ++ preLoopCTypeVar typeEnv retKinds v (thn ++ els)
              ++ " "
              ++ sanitize v
              ++ ";"
          | v <- Set.toList newDecls
          ]
        inner = preDeclared `Set.union` newDecls
     in declLines
          ++ ["    if (" ++ genCAtom cond ++ ") {"]
          ++ concatMap (genPreLoopLine typeEnv retKinds skipAlloc inner) thn
          ++ ( case els of
                 [] -> ["    }"]
                 _ ->
                   ["    } else {"]
                     ++ concatMap (genPreLoopLine typeEnv retKinds skipAlloc inner) els
                     ++ ["    }"]
             )
  _ -> []

-- | Emit a C for-loop for pre-loop execution.
genPreLoopCLoop :: Int -> TypeEnv -> Map CVar VarKind -> Set CVar -> Set CVar -> CFG.LoopSpec -> [CFG.Stmt] -> [String]
genPreLoopCLoop depth typeEnv retKinds skipAlloc preDeclared spec body =
  let ind = replicate ((depth + 1) * 4) ' '
      red = CFG.lsRed spec
      initLines = case red of
        Just r ->
          let accTy = case Map.lookup (CFG.rsAccVar r) typeEnv of
                Just ct -> cTypeName ct
                Nothing -> error ("genPreLoopCLoop: reduction accumulator type unknown for " ++ show (CFG.rsAccVar r))
           in [ ind
                  ++ accTy
                  ++ " "
                  ++ sanitize (CFG.rsAccVar r)
                  ++ " = "
                  ++ genPreLoopCIndexExpr (CFG.rsInit r)
                  ++ ";"
              ]
        Nothing -> []
      loopLines = case (CFG.lsIters spec, CFG.lsBounds spec) of
        ([i], [b]) ->
          [ ind
              ++ "for (int64_t "
              ++ sanitize i
              ++ " = 0; "
              ++ sanitize i
              ++ " < "
              ++ genPreLoopCIndexExpr b
              ++ "; "
              ++ sanitize i
              ++ "++) {"
          ]
            ++ concatMap (genPreLoopLine typeEnv retKinds skipAlloc preDeclared) body
            ++ [ind ++ "}"]
        (iters, bounds) ->
          let mkLoop (ci, b) inner =
                [ ind
                    ++ "for (int64_t "
                    ++ sanitize ci
                    ++ " = 0; "
                    ++ sanitize ci
                    ++ " < "
                    ++ genPreLoopCIndexExpr b
                    ++ "; "
                    ++ sanitize ci
                    ++ "++) {"
                ]
                  ++ inner
                  ++ [ind ++ "}"]
           in foldr mkLoop (concatMap (genPreLoopLine typeEnv retKinds skipAlloc preDeclared) body) (zip iters bounds)
   in initLines ++ loopLines

-- | Render a C array write for pre-loop.
genPreLoopCArrayWrite :: TypeEnv -> Atom -> Atom -> Atom -> String
genPreLoopCArrayWrite typeEnv (AVar arr) idx val =
  "((" ++ cpuArrayElemCType typeEnv arr ++ "*)" ++ sanitize arr ++ "->data)[" ++ genCAtom idx ++ "] = " ++ genCAtom val ++ ";"
genPreLoopCArrayWrite _ arr idx val =
  genCAtom arr ++ "[" ++ genCAtom idx ++ "] = " ++ genCAtom val ++ ";"

-- | CPU storage element type for an array variable's @->data@ pointer.
-- The host-side @hyd_array_t@ stores floats as @double@ and ints as @int64_t@,
-- so a projection/read must cast to the element's CPU C type — not a blanket
-- @int64_t*@, which would reinterpret a double's bit pattern as an integer.
cpuArrayElemCType :: TypeEnv -> CVar -> String
cpuArrayElemCType typeEnv arr = case Map.lookup arr typeEnv of
  Just (CTArray elt) -> cTypeName elt
  _ -> "int64_t"

-- | Render an IndexExpr as C code for pre-loop.
genPreLoopCIndexExpr :: CFG.IndexExpr -> String
genPreLoopCIndexExpr expr = case CFG.simplifyIndexExpr expr of
  CFG.IVar v -> sanitize v
  CFG.IConst n -> show n ++ "LL"
  CFG.IAdd a b -> "(" ++ genPreLoopCIndexExpr a ++ " + " ++ genPreLoopCIndexExpr b ++ ")"
  CFG.ISub a b -> "(" ++ genPreLoopCIndexExpr a ++ " - " ++ genPreLoopCIndexExpr b ++ ")"
  CFG.IMul a b -> "(" ++ genPreLoopCIndexExpr a ++ " * " ++ genPreLoopCIndexExpr b ++ ")"
  CFG.IDiv a b -> "(" ++ genPreLoopCIndexExpr a ++ " / " ++ genPreLoopCIndexExpr b ++ ")"
  CFG.INdToFlat nd shp -> "hyd_nd_to_flat(" ++ genPreLoopCIndexExpr nd ++ ", " ++ genPreLoopCIndexExpr shp ++ ")"
  CFG.IFlatToNd flat shp -> "hyd_flat_to_nd(" ++ genPreLoopCIndexExpr flat ++ ", " ++ genPreLoopCIndexExpr shp ++ ")"
  CFG.ITuple es ->
    "hyd_tuple_make("
      ++ show (length es)
      ++ (if null es then "" else ", " ++ intercalate ", " (map (\e -> "(int64_t)" ++ genPreLoopCIndexExpr e) es))
      ++ ")"
  CFG.IProj i e -> genPreLoopCIndexExpr e ++ ".elems[" ++ show i ++ "]"
  CFG.ICall fn args -> sanitize fn ++ "(" ++ intercalate ", " (map genPreLoopCIndexExpr args) ++ ")"
  _ -> "0"

-- | C type string for a pre-loop variable.
preLoopCType :: TypeEnv -> Map CVar VarKind -> CVar -> RHS -> String
preLoopCType typeEnv retKinds v rhs =
  case Map.lookup v typeEnv of
    Just ct -> cTypeName ct
    Nothing -> case rhs of
      RArrayAlloc {} -> "hyd_array_t*"
      RArrayCopy {} -> "hyd_array_t*"
      RArrayShape {} -> "hyd_tuple_t"
      RShapeInit {} -> "hyd_tuple_t"
      RTuple {} -> "hyd_tuple_t"
      RShapeSize {} -> "int64_t"
      RShapeLast {} -> "int64_t"
      RCall fn _ -> case Map.findWithDefault KScalar fn retKinds of
        KArray -> "hyd_array_t*"
        KFloatArray -> "hyd_array_t*"
        KTuple -> "hyd_tuple_t"
        KFloat -> "double"
        _ -> "int64_t"
      RBinOp op _ _
        | isFloatArithBinOp op -> "double"
        | otherwise -> "int64_t"
      RUnOp op _
        | isMathFloatOp op -> "double"
        | otherwise -> "int64_t"
      RArrayLoad arr _ -> case lookupArrayElemType typeEnv arr of
        Just eltTy -> cTypeName eltTy
        Nothing -> error ("preLoopCType: RArrayLoad element type unknown for array " ++ show arr)
      RFlatToNd {} -> "hyd_tuple_t"
      RNdToFlat {} -> "int64_t"
      R2DToFlat {} -> "int64_t"
      RPairMake ct1 ct2 _ _ -> pairStructName ct1 ct2
      RPairFst ct _ -> celemTypeCType ct
      RPairSnd ct _ -> celemTypeCType ct
      _ -> error ("preLoopCType: cannot determine type for variable " ++ show v ++ " with rhs " ++ show rhs)

-- | Determine the C type for a variable from typeEnv, falling back to scanning
-- statements for an assignment to find the RHS-based type.
preLoopCTypeVar :: TypeEnv -> Map CVar VarKind -> CVar -> [CFG.Stmt] -> String
preLoopCTypeVar typeEnv retKinds v stmts =
  case Map.lookup v typeEnv of
    Just ct -> cTypeName ct
    Nothing ->
      -- Scan statements for an assignment to v to find the RHS type
      case findAssignRHS v stmts of
        Just rhs -> preLoopCType typeEnv retKinds v rhs
        Nothing -> error ("preLoopCTypeVar: no assignment found and type unknown for variable " ++ show v)
  where
    findAssignRHS target = go
      where
        go [] = Nothing
        go (CFG.SAssign v' rhs : _) | v' == target = Just rhs
        go (CFG.SIf _ thn els : rest) = go thn <|> go els <|> go rest
        go (CFG.SLoop _ body : rest) = go body <|> go rest
        go (_ : rest) = go rest

-- | C expression for a pre-loop RHS (used in ObjC harness main).
-- Takes the typeEnv and assigned variable to resolve pair struct types.
genPreLoopRHS :: TypeEnv -> Map CVar VarKind -> CVar -> RHS -> String
genPreLoopRHS typeEnv retKinds v rhs = case rhs of
  RAtom a -> genCAtom a
  RCall fn [] -> sanitize fn ++ "()"
  RCall fn args -> sanitize fn ++ "(" ++ intercalate ", " (map genCAtom args) ++ ")"
  RArrayShape arr -> genCAtom arr ++ "->shape"
  RShapeSize shp -> "hyd_shape_size(" ++ genCAtom shp ++ ")"
  RShapeInit shp -> "hyd_shape_init(" ++ genCAtom shp ++ ")"
  RShapeLast shp -> "hyd_shape_last(" ++ genCAtom shp ++ ")"
  RArrayAlloc shp -> "hyd_array_alloc(" ++ genCAtom shp ++ ")"
  RArrayCopy src -> "hyd_array_alloc_copy(" ++ genCAtom src ++ ")"
  RBinOp op a1 a2 -> "(" ++ genCAtom a1 ++ " " ++ mslBinOp op ++ " " ++ genCAtom a2 ++ ")"
  RUnOp CNeg a -> "(-" ++ genCAtom a ++ ")"
  RUnOp op a -> mslUnOp op ++ "(" ++ genCAtom a ++ ")"
  RTuple atoms ->
    "hyd_tuple_make("
      ++ show (length atoms)
      ++ ( if null atoms
             then ""
             else
               ", "
                 ++ intercalate ", " (map (\a -> "(int64_t)" ++ genCAtom a) atoms)
         )
      ++ ")"
  RProj i a ->
    let isPair = case a of
          AVar src -> case Map.lookup src typeEnv of
            Just (CTPair _ _) -> True
            _ -> False
          _ -> False
     in if isPair
          then genCAtom a ++ (if i == 0 then ".fst" else ".snd")
          else genCAtom a ++ ".elems[" ++ show i ++ "]"
  RArrayLoad (AVar arr) idx ->
    "((" ++ cpuArrayElemCType typeEnv arr ++ "*)" ++ sanitize arr ++ "->data)[" ++ genCAtom idx ++ "]"
  RArrayLoad arr idx ->
    genCAtom arr ++ "[" ++ genCAtom idx ++ "]"
  RFlatToNd flat shp -> "hyd_flat_to_nd(" ++ genCAtom flat ++ ", " ++ genCAtom shp ++ ")"
  RNdToFlat AUnit _ -> "0LL"
  RNdToFlat nd shp -> "hyd_nd_to_flat(" ++ genCAtom nd ++ ", " ++ genCAtom shp ++ ")"
  RPairMake ct1 ct2 a1 a2 ->
    let structName = case Map.lookup v typeEnv of
          Just (CTPair et1 et2)
            | Just ce1 <- ctypeToElemType et1,
              Just ce2 <- ctypeToElemType et2 ->
                pairStructName ce1 ce2
          _ -> pairStructName ct1 ct2
     in "((" ++ structName ++ "){.fst = " ++ genCAtom a1 ++ ", .snd = " ++ genCAtom a2 ++ "})"
  RPairFst _ a -> genCAtom a ++ ".fst"
  RPairSnd _ a -> genCAtom a ++ ".snd"
  RRecord fields ->
    "{" ++ intercalate ", " ["." ++ sanitize f ++ " = " ++ genCAtom a | (f, a) <- fields] ++ "}"
  RRecordProj f a -> genCAtom a ++ "." ++ sanitize f
  R2DToFlat i w -> "(" ++ genCAtom i ++ " * " ++ genCAtom w ++ ")"
  _ -> "0 /* unhandled pre-loop RHS */"

-- | Plain C atom rendering (same syntax as C, unlike genMSLAtom which maps gid).
genCAtom :: Atom -> String
genCAtom (AVar v) = sanitize v
genCAtom (AInt n) = show n ++ "LL"
genCAtom (AFloat f) = show f
genCAtom (ABool True) = "1"
genCAtom (ABool False) = "0"
genCAtom AUnit = "0"
genCAtom (AString s) = "\"" ++ BS.unpack s ++ "\""
genCAtom (AVecVar v) = sanitize v

-- ---------------------------------------------------------------------------
-- Boilerplate Metal shape helpers emitted in every .metal file

mslShapeHelpers :: String
mslShapeHelpers =
  unlines
    [ "// Tuple / shape type",
      "#define HYD_MAX_DIMS 8",
      "struct hyd_tuple_t {",
      "    long elems[HYD_MAX_DIMS];",
      "    int ndims;",
      "};",
      "",
      "// Shape helpers",
      "static inline uint hyd_metal_shape_size(constant uint* s, int n) {",
      "    uint r = 1;",
      "    for (int i = 0; i < n; i++) r *= s[i];",
      "    return r;",
      "}",
      "",
      "static inline long hyd_shape_size_t(hyd_tuple_t shape) {",
      "    long r = 1;",
      "    for (int i = 0; i < shape.ndims; i++) r *= shape.elems[i];",
      "    return r;",
      "}",
      "",
      "static inline hyd_tuple_t hyd_flat_to_nd(long flat, hyd_tuple_t shape) {",
      "    hyd_tuple_t idx;",
      "    idx.ndims = shape.ndims;",
      "    long remaining = flat;",
      "    for (int i = shape.ndims - 1; i >= 0; i--) {",
      "        idx.elems[i] = remaining % shape.elems[i];",
      "        remaining /= shape.elems[i];",
      "    }",
      "    return idx;",
      "}",
      "",
      "static inline long hyd_nd_to_flat(hyd_tuple_t nd, hyd_tuple_t shape) {",
      "    long flat = 0;",
      "    long stride = 1;",
      "    for (int i = shape.ndims - 1; i >= 0; i--) {",
      "        flat += nd.elems[i] * stride;",
      "        stride *= shape.elems[i];",
      "    }",
      "    return flat;",
      "}",
      "",
      "// erf approximation (Abramowitz & Stegun 7.1.26, max error 1.5e-7)",
      "static inline float erf(float x) {",
      "    const float a1 =  0.254829592f, a2 = -0.284496736f, a3 =  1.421413741f;",
      "    const float a4 = -1.453152027f, a5 =  1.061405429f, p  =  0.3275911f;",
      "    float sign = x >= 0.0f ? 1.0f : -1.0f;",
      "    float ax = fabs(x);",
      "    float t = 1.0f / (1.0f + p * ax);",
      "    float y = 1.0f - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1) * t * exp(-(ax * ax));",
      "    return sign * y;",
      "}"
    ]
