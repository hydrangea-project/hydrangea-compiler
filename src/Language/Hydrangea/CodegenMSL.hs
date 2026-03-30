{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.CodegenMSL
--
-- Metal Shading Language (MSL) compute backend for macOS / Apple Silicon.
--
-- Translates the same @C2.Program@ IR consumed by the C backend into:
--
-- * A @.metal@ kernel source file (scalar compute kernel).
-- * A self-contained Objective-C harness (@.m@) that invokes CPU helper
--   procs, fills Metal buffers, dispatches the GPU kernel, and prints the
--   result.
--
-- Scope of the initial implementation:
--
-- * 1-D parallel \/ vector map kernels (outermost @LoopMap@ loop).
-- * Scalar and 1-D array inputs\/outputs.
-- * @CTInt64@ (mapped to @long@) and @CTDouble@ (demoted to @float@).
-- * Inner serial loops and conditionals inside the kernel body.
-- * No dynamic allocation inside the kernel.
-- * No @RFlatToNd@ \/ @RNdToFlat@ inside the kernel body.
module Language.Hydrangea.CodegenMSL
  ( MSLArtifacts(..)
  , MSLOptions(..)
  , defaultMSLOptions
  , codegenMSL
  ) where

import Control.Applicative ((<|>))
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Char (toUpper)
import Data.List (intercalate, maximumBy, nub)
import Data.Maybe (listToMaybe)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Language.Hydrangea.CFGCore
  ( Atom(..), BinOp(..), CElemType(..), CType(..), CVar, RHS(..), UnOp(..)
  , ctypeToElemType
  )
import Language.Hydrangea.CFG qualified as C2
import Language.Hydrangea.CFGAnalysis (usedVarsStmts2)
import Language.Hydrangea.CFGTyping
  ( TypeEnv, inferProgramReturnTypes2, recoverProcTypeEnv2, buildCallParamTypes
  , lookupArrayElemType2
  )
import Language.Hydrangea.CodegenC
  ( CodegenOptions(..), CodegenArtifacts(..), VarKind(..)
  , ExportSpec(..), resolveExportSpec, procReturnTypeName, sanitizeExportName
  , defaultCodegenOptions, codegenProgram2WithOptions
  , sanitize, splitFinalReturn
  , typeEnvToVarSets, classifyVarKinds2
  , arrayVarsProc2, pairVarsProc2
  , inferArrayElemTypesFromStmts
  , procReturnKinds2
  , isFloatArithBinOp, isMathFloatOp
  , pairStructName, celemTypeLetter, celemTypeCType
  , cTypeName
  , detectAtomicScatterAddLoop
  , genPairStructDefs, genRecordStructDefs
  )

-- ---------------------------------------------------------------------------
-- Public types

-- | Artifacts produced by the MSL codegen.
data MSLArtifacts = MSLArtifacts
  { mslKernelSource  :: String        -- ^ Contents of the @.metal@ kernel file.
  , mslHarnessSource :: String        -- ^ Contents of the self-contained ObjC harness.
  , mslHeaderSource  :: Maybe String  -- ^ Header file (present in export mode).
  }

-- | Options controlling which proc is selected as the GPU kernel.
data MSLOptions = MSLOptions
  { mslKernelToEmit :: Maybe CVar
    -- ^ If 'Just name', use that proc as the kernel. If 'Nothing', use the
    -- first proc with a top-level @LoopMap@ loop.
  , mslMultiKernel  :: Bool
    -- ^ When 'True' (default), automatically dispatch multiple GPU kernels
    -- when the program has more than one GPU-eligible proc.
  , mslExportKernel :: Maybe CVar
    -- ^ If 'Just name', generate a reusable library with init/dispatch/cleanup
    -- lifecycle functions instead of a standalone executable.
  }

defaultMSLOptions :: MSLOptions
defaultMSLOptions = MSLOptions
  { mslKernelToEmit = Nothing
  , mslMultiKernel = True
  , mslExportKernel = Nothing
  }

-- | Per-kernel analysis record.  Captures everything needed to generate the
-- Metal kernel source and harness dispatch for one proc.
data KernelAnalysis = KernelAnalysis
  { kaProc           :: C2.Proc
  , kaName           :: CVar
  , kaPreLoopStmts   :: [C2.Stmt]
  , kaLoopSpec       :: C2.LoopSpec
  , kaLoopBody       :: [C2.Stmt]
  , kaRetAtom        :: Maybe Atom
  , kaTypeEnv        :: TypeEnv
  , kaArrayElemTys   :: Map CVar CType
  , kaInputArrays    :: [CVar]
  , kaOutputArrays   :: [CVar]
  , kaScalarInputs   :: [CVar]         -- ^ Includes proc-param scalars
  , kaEffectivePreLoop :: [C2.Stmt]    -- ^ preLoopStmts ++ inBodyHoisted
  , kaHoistedCallMap :: Map CVar CVar
  , kaPreLoopAliases :: [(CVar, CVar)]
  , kaPostLoopStmts  :: [C2.Stmt]      -- ^ Statements between kernel loop and return
  , kaProcParams     :: [CVar]         -- ^ All proc params (for harness defaults)
  }

-- | Whether an input array originates from a CPU helper or a prior GPU kernel.
data BufferOrigin = CPUProduced | GPUProduced CVar  -- ^ producing kernel name

-- ---------------------------------------------------------------------------
-- Top-level entry point

-- | Translate a @C2.Program@ to Metal artifacts.
-- Uses multi-kernel dispatch when multiple zero-arg procs have parallelizable
-- loops; falls back to single-kernel for explicit @--metal-kernel@ selection
-- or when only one proc is GPU-eligible.
codegenMSL :: MSLOptions -> C2.Program -> Either String MSLArtifacts
codegenMSL opts prog@(C2.Program procs) =
  case mslExportKernel opts of
    Just _  -> codegenMSLExport opts prog  -- export-kernel mode
    Nothing ->
      case mslKernelToEmit opts of
        Just _ -> codegenMSLSingle opts prog  -- explicit kernel: single-kernel path
        Nothing
          | not (mslMultiKernel opts) -> codegenMSLSingle opts prog
          | otherwise ->
            let gpuProcs = findGPUEligibleProcs procs
            in case gpuProcs of
              []  -> Left "MSL backend: no proc with a parallelizable loop found"
              [_] -> codegenMSLSingle opts prog  -- one GPU proc: existing path
              _   -> codegenMSLMulti prog gpuProcs

-- | Multi-kernel code path: dispatch multiple GPU kernels in sequence.
codegenMSLMulti :: C2.Program -> [C2.Proc] -> Either String MSLArtifacts
codegenMSLMulti prog@(C2.Program procs) gpuProcs = do
  -- Analyze each GPU-eligible proc; silently drop those that fail analysis
  let analyses = [ ka | p <- gpuProcs, Right ka <- [analyzeOneKernel prog p] ]
  case analyses of
    [] -> Left "MSL backend: no GPU-eligible proc passed kernel analysis"
    [ka] -> codegenMSLSingle defaultMSLOptions prog  -- degenerate: one survived
    kas -> do
      let gpuNames = Set.fromList [kaName ka | ka <- kas]
          bufOrigins = classifyBufferOrigins kas gpuNames
          gpuBufAliases = buildGPUBufferAliases kas gpuNames
          retKinds = procReturnKinds2 prog
          retTypes = inferProgramReturnTypes2 prog
      helperC <- genHelperC procs (kaName (last kas)) retKinds
      let kernelSrc = genMultiKernelMSL kas
          harnessSrc = genMultiKernelHarnessSrc prog kas bufOrigins gpuBufAliases
                         retKinds retTypes helperC
      Right MSLArtifacts
        { mslKernelSource  = kernelSrc
        , mslHarnessSource = harnessSrc
        , mslHeaderSource  = Nothing
        }

-- | Single-kernel code path (existing behaviour).
codegenMSLSingle :: MSLOptions -> C2.Program -> Either String MSLArtifacts
codegenMSLSingle opts prog@(C2.Program procs) = do
  kernelProc <- findKernelProc opts procs
  ka <- analyzeOneKernel prog kernelProc
  let retKinds = procReturnKinds2 prog
      retTypes = inferProgramReturnTypes2 prog
  helperC <- genHelperC procs (kaName ka) retKinds
  let kernelSrc  = genMSLKernelSrc (kaName ka) (kaLoopSpec ka) (kaLoopBody ka) (kaTypeEnv ka)
                     (kaArrayElemTys ka) (kaInputArrays ka) (kaOutputArrays ka) (kaScalarInputs ka)
                     (kaHoistedCallMap ka) (kaPreLoopAliases ka)
      harnessSrc = genObjCHarnessSrc prog (kaProc ka) (kaName ka) (kaEffectivePreLoop ka)
                     (kaLoopSpec ka) (kaRetAtom ka) (kaTypeEnv ka) (kaArrayElemTys ka)
                     retKinds retTypes
                     (kaInputArrays ka) (kaOutputArrays ka) (kaScalarInputs ka)
                     (kaProcParams ka) helperC
  Right MSLArtifacts
    { mslKernelSource  = kernelSrc
    , mslHarnessSource = harnessSrc
    , mslHeaderSource  = Nothing
    }

-- ---------------------------------------------------------------------------
-- Export-kernel code path
-- ---------------------------------------------------------------------------

-- | Export mode: generate a reusable Metal library with init/dispatch/cleanup.
codegenMSLExport :: MSLOptions -> C2.Program -> Either String MSLArtifacts
codegenMSLExport opts prog@(C2.Program procs) = do
  exportName <- case mslExportKernel opts of
    Just n  -> Right n
    Nothing -> Left "MSL export: no kernel name specified"
  -- Find the proc to export
  kp <- case filter (\p -> C2.procName p == exportName) procs of
    (p:_) -> Right p
    []    -> Left $ "MSL export: proc not found: " ++ BS.unpack exportName
  ka <- analyzeOneKernel prog kp
  let retKinds = procReturnKinds2 prog
      retTypes = inferProgramReturnTypes2 prog
      callParamTys = buildCallParamTypes retTypes procs
  -- Resolve export spec (reuse CodegenC infrastructure)
  spec <- resolveExportSpec retKinds retTypes callParamTys procs exportName
  -- Build metal export spec with hyd_metal_ prefix
  let metalSpec = spec { exportWrapperName = "hyd_metal_" ++ sanitizeExportName exportName }
  -- Identify which input arrays are cached (zero-arg RCall returning arrays,
  -- not derived from proc params). Only arrays get cached as GPU buffers;
  -- scalar zero-arg calls are just CPU helper functions run each frame.
  let procParamSet = Set.fromList (kaProcParams ka)
      inputArraySet = Set.fromList (kaInputArrays ka)
      cachedArrayBindings =
        [ (v, fn) | C2.SAssign v (RCall fn []) <- kaEffectivePreLoop ka
                   , v `Set.member` inputArraySet      -- must be a kernel input array
                   , v `Set.notMember` procParamSet     -- not a proc param
                   -- not derived from a proc param via RProj
                   , not (any (derivesFromParam procParamSet v) (kaEffectivePreLoop ka))
        ]
  -- Generate .metal kernel source
  helperC <- genHelperC procs (kaName ka) retKinds
  let kernelSrc = genMSLKernelSrc (kaName ka) (kaLoopSpec ka) (kaLoopBody ka) (kaTypeEnv ka)
                    (kaArrayElemTys ka) (kaInputArrays ka) (kaOutputArrays ka) (kaScalarInputs ka)
                    (kaHoistedCallMap ka) (kaPreLoopAliases ka)
      headerSrc = genMetalExportHeader metalSpec
      harnessSrc = genExportObjCHarness prog ka metalSpec cachedArrayBindings
                     retKinds retTypes helperC
  Right MSLArtifacts
    { mslKernelSource  = kernelSrc
    , mslHarnessSource = harnessSrc
    , mslHeaderSource  = Just headerSrc
    }

-- | Check if a variable derives from a proc parameter (e.g., via RProj).
derivesFromParam :: Set CVar -> CVar -> C2.Stmt -> Bool
derivesFromParam paramSet target (C2.SAssign v (RProj _ (AVar p))) =
  v == target && p `Set.member` paramSet
derivesFromParam paramSet target (C2.SAssign v (RAtom (AVar p))) =
  v == target && p `Set.member` paramSet
derivesFromParam _ _ _ = False

-- | Generate the export header file (.h).
genMetalExportHeader :: ExportSpec -> String
genMetalExportHeader spec =
  let guardName = "HYDRANGEA_METAL_EXPORT_" ++ map toUpper (sanitizeExportName (exportKernelName spec)) ++ "_H"
      ps = exportParams spec
      cParams = case ps of
        [] -> "void"
        _  -> intercalate ", " [ct ++ " " ++ cn | (cn, ct) <- ps]
      -- Collect all type strings from return type and params, emit pair struct
      -- typedefs for any that match the hyd_pair_*_t pattern.
      allTypeStrs = exportReturnType spec : map snd ps
      pairDefs = nub [ genPairTypedef name
                     | name <- allTypeStrs
                     , "hyd_pair_" `isPrefixOfStr` name
                     ]
  in unlines $
    [ "#ifndef " ++ guardName
    , "#define " ++ guardName
    , ""
    , "#include \"hydrangea_runtime.h\""
    , ""
    , "#ifdef __cplusplus"
    , "extern \"C\" {"
    , "#endif"
    ]
    ++ (if null pairDefs then [] else "" : pairDefs)
    ++
    [ ""
    , "int hyd_metal_init(const char* metallib_path);"
    , exportReturnType spec ++ " " ++ exportWrapperName spec ++ "(" ++ cParams ++ ");"
    , "void hyd_metal_cleanup(void);"
    , ""
    , "#ifdef __cplusplus"
    , "}"
    , "#endif"
    , ""
    , "#endif"
    ]

-- | Check if a string starts with a prefix.
isPrefixOfStr :: String -> String -> Bool
isPrefixOfStr prefix str = take (length prefix) str == prefix

-- | Generate a typedef for a pair struct from its name (e.g., "hyd_pair_aa_t").
-- Parses the letter codes between "hyd_pair_" and "_t" to determine field types.
genPairTypedef :: String -> String
genPairTypedef name =
  let -- Extract letter codes: "hyd_pair_XY_t" → "XY"
      stripped = drop 9 name  -- drop "hyd_pair_"
      codes = takeWhile (/= '_') stripped
      letterToCType 'a' = "hyd_array_t*"
      letterToCType 'i' = "int64_t"
      letterToCType 'f' = "double"
      letterToCType 'b' = "int64_t"
      letterToCType _   = "int64_t"
  in case codes of
       [c1, c2] -> "typedef struct { " ++ letterToCType c1 ++ " fst; "
                    ++ letterToCType c2 ++ " snd; } " ++ name ++ ";"
       _ -> "/* unknown pair type: " ++ name ++ " */"

-- | Generate the export ObjC harness (.m) with init/dispatch/cleanup lifecycle.
--
-- Architecture:
-- * @hyd_metal_init@: set up Metal device/pipeline/queue, compute and upload
--   cached array bindings (zero-arg array-returning procs) to persistent GPU buffers.
-- * Dispatch function: run pre-loop to compute scalars and decompose params,
--   upload dynamic input arrays, create output/scalar buffers, dispatch, read back.
-- * @hyd_metal_cleanup@: nil out persistent Metal objects.
genExportObjCHarness
  :: C2.Program
  -> KernelAnalysis
  -> ExportSpec            -- ^ Metal export spec (hyd_metal_ prefix)
  -> [(CVar, CVar)]       -- ^ Cached array bindings: (var, proc_name) pairs
  -> Map CVar VarKind      -- ^ Return kinds
  -> Map CVar CType        -- ^ Program-level return types
  -> String               -- ^ Helper C source
  -> String
genExportObjCHarness _prog ka spec cachedArrayBindings retKinds _retTypes helperC =
  unlines $
    -- Header
    [ "// Hydrangea Metal export harness — generated by the MSL backend."
    , "#include \"hydrangea_runtime.h\""
    , "#include <stdio.h>"
    , "#include <string.h>"
    , "#include <stdlib.h>"
    , "#import <Foundation/Foundation.h>"
    , "#import <Metal/Metal.h>"
    , ""
    , "// ---- CPU helper procs ----"
    , helperC
    , ""
    , "// ---- Static Metal state ----"
    , "static id<MTLDevice> _hyd_dev;"
    , "static id<MTLLibrary> _hyd_lib;"
    , "static id<MTLComputePipelineState> _hyd_pso;"
    , "static id<MTLCommandQueue> _hyd_queue;"
    ] ++
    -- Static cached buffer declarations
    concatMap genCachedBufDecl cachedArrayBindings ++
    [ ""
    , "// ---- hyd_metal_init ----"
    , "int hyd_metal_init(const char* metallib_path) {"
    , "    @autoreleasepool {"
    , "    _hyd_dev = MTLCreateSystemDefaultDevice();"
    , "    if (!_hyd_dev) { fprintf(stderr, \"hydrangea: Metal not available\\n\"); return 1; }"
    , "    NSError* _err = nil;"
    , "    NSURL* _libURL = [NSURL fileURLWithPath:[NSString stringWithUTF8String:metallib_path]];"
    , "    _hyd_lib = [_hyd_dev newLibraryWithURL:_libURL error:&_err];"
    , "    if (!_hyd_lib) {"
    , "        fprintf(stderr, \"hydrangea: failed to load metallib: %s\\n\","
    , "                [_err.localizedDescription UTF8String]);"
    , "        return 1;"
    , "    }"
    , "    id<MTLFunction> _fn = [_hyd_lib newFunctionWithName:@\"" ++ sanitize (kaName ka) ++ "\"];"
    , "    if (!_fn) { fprintf(stderr, \"hydrangea: kernel function not found\\n\"); return 1; }"
    , "    _hyd_pso = [_hyd_dev newComputePipelineStateWithFunction:_fn error:&_err];"
    , "    if (!_hyd_pso) {"
    , "        fprintf(stderr, \"hydrangea: pipeline error: %s\\n\","
    , "                [_err.localizedDescription UTF8String]);"
    , "        return 1;"
    , "    }"
    , "    _hyd_queue = [_hyd_dev newCommandQueue];"
    ] ++
    -- Compute and upload cached array bindings
    concatMap genCachedBufInit cachedArrayBindings ++
    [ "    } // @autoreleasepool"
    , "    return 0;"
    , "}"
    , ""
    , "// ---- dispatch function ----"
    ] ++
    genDispatchFn ++
    [ ""
    , "// ---- hyd_metal_cleanup ----"
    , "void hyd_metal_cleanup(void) {"
    , "    _hyd_pso = nil;"
    , "    _hyd_queue = nil;"
    , "    _hyd_lib = nil;"
    ] ++
    concatMap genCachedBufCleanup cachedArrayBindings ++
    [ "    _hyd_dev = nil;"
    , "}"
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
      Nothing  -> case Map.lookup v typeEnv of
        Just (CTArray elt) -> mslTypeName elt
        _                  -> "long"

    -- Static declarations for cached GPU buffers
    genCachedBufDecl (v, _fn) =
      [ "static id<MTLBuffer> _hyd_cached_buf_" ++ sanitize v ++ ";"
      , "static hyd_tuple_t _hyd_cached_shape_" ++ sanitize v ++ ";"
      ]

    -- Init: compute cached binding on CPU, upload to persistent GPU buffer
    genCachedBufInit (v, fn) =
      let elemTy = mslElemTy v
      in [ "    // Cached binding: " ++ sanitize v ++ " = " ++ sanitize fn ++ "()"
         , "    hyd_array_t* _cpu_" ++ sanitize v ++ " = " ++ sanitize fn ++ "();"
         , "    _hyd_cached_shape_" ++ sanitize v ++ " = _cpu_" ++ sanitize v ++ "->shape;"
         , "    long _cachedN_" ++ sanitize v ++ " = hyd_shape_size(_cpu_" ++ sanitize v ++ "->shape);"
         , "    size_t _cachedSz_" ++ sanitize v ++ " = (size_t)(_cachedN_" ++ sanitize v
           ++ " * sizeof(" ++ elemTy ++ "));"
         , "    _hyd_cached_buf_" ++ sanitize v ++ " = [_hyd_dev newBufferWithLength:_cachedSz_"
           ++ sanitize v ++ " options:MTLResourceStorageModeShared];"
         ] ++ uploadCachedBuf v elemTy ++
         [ "    hyd_array_free(_cpu_" ++ sanitize v ++ ");"
         ]

    uploadCachedBuf v "float" =
      [ "    { double* _src = (double*)_cpu_" ++ sanitize v ++ "->data;"
      , "      float* _dst = (float*)_hyd_cached_buf_" ++ sanitize v ++ ".contents;"
      , "      for (long _i = 0; _i < _cachedN_" ++ sanitize v ++ "; _i++) _dst[_i] = (float)_src[_i]; }"
      ]
    uploadCachedBuf v _ =
      [ "    memcpy(_hyd_cached_buf_" ++ sanitize v ++ ".contents, _cpu_" ++ sanitize v
        ++ "->data, _cachedSz_" ++ sanitize v ++ ");"
      ]

    genCachedBufCleanup (v, _fn) =
      [ "    _hyd_cached_buf_" ++ sanitize v ++ " = nil;" ]

    -- The dispatch function runs the pre-loop (for scalars + pair decomposition),
    -- then creates Metal buffers, dispatches, and reads back output.
    genDispatchFn :: [String]
    genDispatchFn =
      let ps = exportParams spec
          cParams = case ps of
            [] -> "void"
            _  -> intercalate ", " [ct ++ " " ++ cn | (cn, ct) <- ps]
      in [ exportReturnType spec ++ " " ++ exportWrapperName spec ++ "(" ++ cParams ++ ") {"
         , "    @autoreleasepool {"
         ] ++
         -- Run pre-loop: computes scalars, decomposes pairs, allocates output arrays.
         -- Cached array RCalls are replaced with shape stubs (data comes from GPU buffer).
         -- Output array allocations are skipped (replaced by Metal buffers).
         concatMap (genExportPreLoopLine (Set.fromList outputArrs) cachedSet) (kaEffectivePreLoop ka) ++
         [ ""
         , "    // Grid size"
         , "    long _n = " ++ gridSizeExpr ++ ";"
         , ""
         , "    // --- Create Metal buffers ---"
         ] ++
         -- Input buffers (cached or dynamic)
         concatMap genInputBuf (zip inputArrs [0..]) ++
         -- Shape buffers
         concatMap genInputShapeBuf (zip inputArrs [nIn..]) ++
         -- Output buffers
         concatMap genOutputBuf (zip outputArrs [nIn * 2..]) ++
         -- Scalar buffers
         concatMap genScalarBuf (zip scals [nIn * 2 + length outputArrs..]) ++
         [ ""
         , "    // --- Dispatch ---"
         , "    id<MTLCommandBuffer> _cmd = [_hyd_queue commandBuffer];"
         , "    id<MTLComputeCommandEncoder> _enc = [_cmd computeCommandEncoder];"
         , "    [_enc setComputePipelineState:_hyd_pso];"
         ] ++
         [ "    [_enc setBuffer:_buf" ++ show i ++ " offset:0 atIndex:" ++ show i ++ "];"
         | i <- [0 .. nIn * 2 + length outputArrs + length scals - 1]
         ] ++
         [ "    NSUInteger _tgs = MIN(256UL, _hyd_pso.maxTotalThreadsPerThreadgroup);"
         , "    if (_tgs == 0) _tgs = 1;"
         , "    [_enc dispatchThreads:MTLSizeMake((NSUInteger)_n, 1, 1)"
         , "      threadsPerThreadgroup:MTLSizeMake(_tgs, 1, 1)];"
         , "    [_enc endEncoding];"
         , "    [_cmd commit];"
         , "    [_cmd waitUntilCompleted];"
         , ""
         ] ++
         -- Read back outputs and construct return value
         genReadback ++
         -- Post-loop statements (dummy array allocations, reshaping, etc.)
         -- Skip RPairMake (handled by return construction) and reshape_view
         -- (handled by readback shape logic).
         concatMap genPostLoopLine (kaPostLoopStmts ka) ++
         genReturnConstruction ++
         [ "    } // @autoreleasepool"
         , "}"
         ]

    -- Pre-loop line for export dispatch: like genPreLoopLine but:
    -- 1. Skips output array allocations (Metal buffers replace them)
    -- 2. For cached array RCalls: emits a shape-only stub (GPU buffer has data)
    genExportPreLoopLine :: Set CVar -> Set CVar -> C2.Stmt -> [String]
    genExportPreLoopLine skipAlloc cachedVars stmt = case stmt of
      C2.SAssign v (RArrayAlloc _) | v `Set.member` skipAlloc ->
        []  -- output arrays: Metal buffer, skip CPU alloc
      C2.SAssign v (RCall fn []) | v `Set.member` cachedVars ->
        -- Cached array: use shape stub so downstream shape accesses work
        [ "    // Cached array (GPU buffer): " ++ sanitize v
        , "    hyd_array_t _stub_" ++ sanitize v ++ " = { .shape = _hyd_cached_shape_" ++ sanitize v ++ ", .data = NULL };"
        , "    hyd_array_t* " ++ sanitize v ++ " = &_stub_" ++ sanitize v ++ ";"
        ]
      _ -> genPreLoopLine typeEnv retKinds skipAlloc Set.empty stmt

    -- Grid size expression
    gridSizeExpr = case C2.lsBounds (kaLoopSpec ka) of
      [b] -> "(long)" ++ genMSLIndexExpr b
      bs  -> intercalate " * " ["(long)" ++ genMSLIndexExpr b | b <- bs]

    -- Input buffer: cached → reference static GPU buffer, dynamic → upload from hyd_array_t*
    genInputBuf (v, i) =
      let elemTy = mslElemTy v
      in if v `Set.member` cachedSet
         then [ "    // Cached input: " ++ sanitize v
              , "    id<MTLBuffer> _buf" ++ show i ++ " = _hyd_cached_buf_" ++ sanitize v ++ ";"
              ]
         else [ "    // Dynamic input: " ++ sanitize v
              , "    long _inN" ++ show i ++ " = hyd_shape_size(" ++ sanitize v ++ "->shape);"
              , "    size_t _buf" ++ show i ++ "_sz = (size_t)(_inN" ++ show i ++ " * sizeof(" ++ elemTy ++ "));"
              , "    id<MTLBuffer> _buf" ++ show i ++ " = [_hyd_dev newBufferWithLength:_buf"
                ++ show i ++ "_sz options:MTLResourceStorageModeShared];"
              ] ++ fillBuf i v elemTy

    fillBuf i v "float" =
      [ "    { double* _fsrc = (double*)" ++ sanitize v ++ "->data; float* _fdst = (float*)_buf" ++ show i ++ ".contents;"
      , "      for (long _fci = 0; _fci < _inN" ++ show i ++ "; _fci++) _fdst[_fci] = (float)_fsrc[_fci]; }"
      ]
    fillBuf i v _ =
      [ "    memcpy(_buf" ++ show i ++ ".contents, " ++ sanitize v ++ "->data, _buf" ++ show i ++ "_sz);" ]

    -- Shape buffer: cached → stored shape, dynamic → array->shape
    genInputShapeBuf (v, i) =
      if v `Set.member` cachedSet
      then [ "    id<MTLBuffer> _buf" ++ show i ++ " = [_hyd_dev newBufferWithBytes:&_hyd_cached_shape_"
               ++ sanitize v ++ " length:sizeof(hyd_tuple_t) options:MTLResourceStorageModeShared];"
           ]
      else [ "    id<MTLBuffer> _buf" ++ show i ++ " = [_hyd_dev newBufferWithBytes:&" ++ sanitize v
               ++ "->shape length:sizeof(hyd_tuple_t) options:MTLResourceStorageModeShared];"
           ]

    -- Output buffer
    genOutputBuf (v, i) =
      let elemTy = mslElemTy v
          outputAllocShapes = Map.fromList
            [ (ov, shpAtom) | C2.SAssign ov (RArrayAlloc shpAtom) <- kaEffectivePreLoop ka ]
          sizeExpr = case Map.lookup v outputAllocShapes of
            Just shpAtom -> "hyd_shape_size(" ++ genCAtom shpAtom ++ ")"
            Nothing -> "_n"
      in [ "    // Output: " ++ sanitize v
         , "    long _outN" ++ show i ++ " = " ++ sizeExpr ++ ";"
         , "    size_t _buf" ++ show i ++ "_sz = (size_t)(_outN" ++ show i ++ " * sizeof(" ++ elemTy ++ "));"
         , "    id<MTLBuffer> _buf" ++ show i ++ " = [_hyd_dev newBufferWithLength:_buf"
           ++ show i ++ "_sz options:MTLResourceStorageModeShared];"
         , "    memset(_buf" ++ show i ++ ".contents, 0, _buf" ++ show i ++ "_sz);"
         ]

    -- Scalar buffer
    genScalarBuf (v, i) =
      let sty = case Map.lookup v typeEnv of
                  Just ct -> mslTypeName ct
                  Nothing -> "long"
      in [ "    // Scalar: " ++ sanitize v
         , "    " ++ sty ++ " _scalar" ++ show i ++ " = " ++ sanitize v ++ ";"
         , "    id<MTLBuffer> _buf" ++ show i ++ " = [_hyd_dev newBufferWithBytes:&_scalar"
           ++ show i ++ " length:sizeof(" ++ sty ++ ") options:MTLResourceStorageModeShared];"
         ]

    -- Read back GPU output arrays from Metal buffers (float→double).
    genReadback :: [String]
    genReadback = concat
      [ let i = nIn * 2 + idx
            elemTy = mslElemTy v
            sizeVar = "_outN" ++ show i
            outputAllocShapes = Map.fromList
              [ (ov, shpAtom) | C2.SAssign ov (RArrayAlloc shpAtom) <- kaEffectivePreLoop ka ]
            reshapeShape = listToMaybe
              [ shpAtom
              | C2.SAssign _ (RCall "hyd_array_reshape_view" [arrAtom, shpAtom])
                  <- kaPostLoopStmts ka
              , arrAtom == AVar v
              ]
            shapeExpr = case reshapeShape of
              Just shpAtom -> genCAtom shpAtom
              Nothing -> case Map.lookup v outputAllocShapes of
                Just shpAtom -> genCAtom shpAtom
                Nothing -> "hyd_tuple_make(1, (int64_t)" ++ sizeVar ++ ")"
        in [ "    // Read back: " ++ sanitize v
           , "    hyd_array_t* _result_" ++ sanitize v ++ " = hyd_array_alloc(" ++ shapeExpr ++ ");"
           ] ++
           (if elemTy == "float"
            then [ "    { float* _gsrc = (float*)_buf" ++ show i ++ ".contents;"
                 , "      double* _gdst = (double*)_result_" ++ sanitize v ++ "->data;"
                 , "      for (long _ri = 0; _ri < " ++ sizeVar ++ "; _ri++) _gdst[_ri] = (double)_gsrc[_ri]; }"
                 ]
            else [ "    memcpy(_result_" ++ sanitize v ++ "->data, _buf" ++ show i
                     ++ ".contents, " ++ sizeVar ++ " * sizeof(" ++ elemTy ++ "));"
                 ])
      | (idx, v) <- zip [0..] outputArrs
      ]

    -- Generate post-loop statements needed before return (e.g., dummy array allocs).
    -- Skip RPairMake and reshape_view (handled by return construction / readback).
    genPostLoopLine :: C2.Stmt -> [String]
    genPostLoopLine (C2.SAssign _ (RPairMake {})) = []  -- handled by genReturnConstruction
    genPostLoopLine (C2.SAssign _ (RCall "hyd_array_reshape_view" _)) = []  -- handled by readback
    genPostLoopLine stmt = genPreLoopLine typeEnv retKinds Set.empty Set.empty stmt

    -- Construct the return value from post-loop RPairMake or output arrays.
    genReturnConstruction :: [String]
    genReturnConstruction =
      let retTy = exportReturnType spec
          outputSet = Set.fromList outputArrs
          pairMake = listToMaybe
            [ (a1, a2) | C2.SAssign _ (RPairMake _ _ a1 a2) <- kaPostLoopStmts ka ]
          atomToRetExpr (AVar v)
            | v `Set.member` outputSet = "_result_" ++ sanitize v
            | otherwise                = sanitize v  -- CPU-produced array
          atomToRetExpr _ = "NULL"
      in case pairMake of
           Just (a1, a2) ->
             [ "    " ++ retTy ++ " _ret = { .fst = " ++ atomToRetExpr a1
               ++ ", .snd = " ++ atomToRetExpr a2 ++ " };"
             , "    return _ret;"
             ]
           Nothing -> case outputArrs of
             [v] | retTy == "hyd_array_t*" ->
                     [ "    return _result_" ++ sanitize v ++ ";" ]
                 | otherwise ->
                     [ "    " ++ retTy ++ " _ret = { .fst = _result_" ++ sanitize v
                       ++ ", .snd = _result_" ++ sanitize v ++ " };"
                     , "    return _ret;"
                     ]
             [v1, v2] ->
                     [ "    " ++ retTy ++ " _ret = { .fst = _result_" ++ sanitize v1
                       ++ ", .snd = _result_" ++ sanitize v2 ++ " };"
                     , "    return _ret;"
                     ]
             _ -> [ "    return _result_" ++ sanitize (head outputArrs) ++ ";" ]

-- | Analyze a single proc for Metal kernel generation.
analyzeOneKernel :: C2.Program -> C2.Proc -> Either String KernelAnalysis
analyzeOneKernel prog@(C2.Program procs) kernelProc = do
  let kernelName = C2.procName kernelProc
      body       = C2.procBody kernelProc
  (preLoopStmts, mapLoopSpec, mapLoopBody, postLoopStmts, retAtom) <- analyzeKernelProc body
  () <- validateMSLLoopBody mapLoopBody
  let retTypes     = inferProgramReturnTypes2 prog
      retKinds     = procReturnKinds2 prog
      callParamTys = buildCallParamTypes retTypes procs
      typeEnv      = recoverProcTypeEnv2 retTypes callParamTys kernelProc
      arrayElemTys = inferArrayElemTypesFromStmts retTypes body
                       (Map.fromList [(v, elt) | (v, CTArray elt) <- Map.toList typeEnv])
      (inputArrays, outputArrays, scalarInputs, inBodyHoisted) =
        classifyKernelParams preLoopStmts mapLoopBody mapLoopSpec typeEnv retKinds
      effectivePreLoop = preLoopStmts ++ inBodyHoisted
      hoistedCallMap = buildHoistedCallMap inBodyHoisted
      procParams' = C2.procParams kernelProc
      procParamScalars = [ v | v <- procParams'
                             , case Map.lookup v typeEnv of
                                 Just (CTArray _) -> False
                                 Just (CTPair _ _) -> False
                                 _ -> True ]
      procParamScalarsUsed = [ v | v <- procParamScalars
                                 , v `Set.member` usedVarsStmts2 (effectivePreLoop ++ mapLoopBody) ]
      allScalarInputs = scalarInputs ++ procParamScalarsUsed
      inputArraySet = Set.fromList inputArrays
      preLoopAliases = collectArrayAliases inputArraySet effectivePreLoop
  Right KernelAnalysis
    { kaProc           = kernelProc
    , kaName           = kernelName
    , kaPreLoopStmts   = preLoopStmts
    , kaLoopSpec       = mapLoopSpec
    , kaLoopBody       = mapLoopBody
    , kaRetAtom        = retAtom
    , kaTypeEnv        = typeEnv
    , kaArrayElemTys   = arrayElemTys
    , kaInputArrays    = inputArrays
    , kaOutputArrays   = outputArrays
    , kaScalarInputs   = allScalarInputs
    , kaEffectivePreLoop = effectivePreLoop
    , kaHoistedCallMap = hoistedCallMap
    , kaPreLoopAliases = preLoopAliases
    , kaPostLoopStmts  = postLoopStmts
    , kaProcParams     = procParams'
    }

-- ---------------------------------------------------------------------------
-- Analysis helpers

-- | Find the proc to use as the Metal kernel.
findKernelProc :: MSLOptions -> [C2.Proc] -> Either String C2.Proc
findKernelProc opts procs =
  case mslKernelToEmit opts of
    Just name ->
      case filter (\p -> C2.procName p == name) procs of
        []    -> Left $ "MSL backend: kernel proc not found: " ++ BS.unpack name
        (p:_) -> Right p
    Nothing ->
      let candidates = filter (procHasMapLoop . C2.procBody) procs
          -- Prefer the "main" proc if it's a candidate; otherwise use last candidate
          -- (last is typically the final top-level binding, the program output)
      in case candidates of
        []    -> Left "MSL backend: no proc with a map loop found in program"
        _     -> case filter (\p -> C2.procName p == "main") candidates of
                   (p:_) -> Right p
                   []    -> Right (last candidates)
  where
    procHasMapLoop = any isKernelLoopStmt
    isKernelLoopStmt (C2.SLoop spec _) =
      C2.lsRole spec `elem` [C2.LoopMap, C2.LoopMapReduction] ||
      (case C2.lsExec spec of C2.Parallel _ -> True; _ -> False)
    isKernelLoopStmt _ = False

-- | Find all zero-arg procs with parallelizable loops (GPU-eligible).
-- Returns procs in original definition order (callees before callers).
findGPUEligibleProcs :: [C2.Proc] -> [C2.Proc]
findGPUEligibleProcs procs =
  [ p | p <- procs
      , null (C2.procParams p)     -- zero-arg only
      , gpuKernelCapturesAllWork (C2.procBody p)
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
      let indexed = zip [0..] stmts
          candidates = [ (i, stmtCount lb)
                       | (i, C2.SLoop spec lb) <- indexed
                       , isKernelLoopStmt (C2.SLoop spec lb)
                       ]
          stmtCount = length . concatMap collectAllStmts
          collectAllStmts (C2.SLoop _ b) = b ++ concatMap collectAllStmts b
          collectAllStmts (C2.SIf _ t e) = t ++ e ++ concatMap collectAllStmts t ++ concatMap collectAllStmts e
          collectAllStmts s = [s]
      in case candidates of
           [] -> Nothing
           _  -> Just (fst (maximumBy (\a b -> compare (snd a) (snd b)) candidates))
    isKernelLoopStmt (C2.SLoop spec _) =
      C2.lsRole spec `elem` [C2.LoopMap, C2.LoopMapReduction] ||
      (case C2.lsExec spec of C2.Parallel _ -> True; _ -> False)
    isKernelLoopStmt _ = False
    hasLoopNested (C2.SLoop _ _) = True
    hasLoopNested (C2.SIf _ thn els) = any hasLoopNested thn || any hasLoopNested els
    hasLoopNested _ = False

-- | Split proc body into (preLoopStmts, loopSpec, loopBody, postLoopStmts, retAtom).
-- Selects the largest kernel-eligible loop (by body size) in the proc body,
-- treating everything before and after it as pre-loop / post-loop CPU work.
analyzeKernelProc :: [C2.Stmt] -> Either String ([C2.Stmt], C2.LoopSpec, [C2.Stmt], [C2.Stmt], Maybe Atom)
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
      let indexed = zip [0..] stmts
          candidates = [ (i, spec, lb)
                       | (i, C2.SLoop spec lb) <- indexed
                       , isKernelLoopStmt (C2.SLoop spec lb)
                       ]
          stmtCount = length . concatMap collectAllStmts
          collectAllStmts (C2.SLoop _ b) = b ++ concatMap collectAllStmts b
          collectAllStmts (C2.SIf _ t e) = t ++ e ++ concatMap collectAllStmts t ++ concatMap collectAllStmts e
          collectAllStmts s = [s]
      in case candidates of
           [] -> Nothing
           _  -> let best = maximumBy (\(_, _, b1) (_, _, b2) -> compare (stmtCount b1) (stmtCount b2)) candidates
                     (i, spec, lb) = best
                 in Just (take i stmts, spec, lb, drop (i + 1) stmts)
    isKernelLoopStmt (C2.SLoop spec _) =
      C2.lsRole spec `elem` [C2.LoopMap, C2.LoopMapReduction] ||
      (case C2.lsExec spec of C2.Parallel _ -> True; _ -> False)
    isKernelLoopStmt _ = False

-- | Validation: reject unsupported MSL constructs in the kernel loop body.
validateMSLLoopBody :: [C2.Stmt] -> Either String ()
validateMSLLoopBody stmts = mapM_ checkStmt stmts
  where
    checkStmt stmt = case stmt of
      C2.SAssign _ rhs -> checkRHS rhs
      C2.SArrayWrite {} -> Right ()
      C2.SLoop _ body  -> mapM_ checkStmt body
      C2.SIf _ thn els -> mapM_ checkStmt thn >> mapM_ checkStmt els
      C2.SReturn {}    -> Right ()
      C2.SBreak        -> Right ()
    checkRHS rhs = case rhs of
      RArrayAlloc {} ->
        Left "MSL backend: dynamic array allocation inside kernel loop not supported"
      _ -> Right ()

-- | Classify kernel parameters from pre-loop stmts and loop body.
-- Returns (inputArrays, outputArrays, scalarInputs, inBodyHoisted).
-- inBodyHoisted contains zero-arg array calls found inside the loop body
-- that must be pre-fetched on CPU and passed as Metal buffers.
classifyKernelParams
  :: [C2.Stmt]     -- ^ Pre-loop stmts
  -> [C2.Stmt]     -- ^ Loop body
  -> C2.LoopSpec   -- ^ Loop spec (for iterator names)
  -> TypeEnv
  -> Map CVar VarKind
  -> ([CVar], [CVar], [CVar], [C2.Stmt])
classifyKernelParams preLoopStmts loopBody loopSpec typeEnv retKinds =
  let iterVars       = Set.fromList (C2.lsIters loopSpec)
      -- Include variables from loop bounds (needed for multi-dim decomposition)
      boundVars      = Set.fromList [ v | C2.IVar v <- C2.lsBounds loopSpec ]
      loopUsedVars   = (usedVarsStmts2 loopBody `Set.union` boundVars) `Set.difference` iterVars
      -- Also scan for written arrays inside nested loops
      loopWrittenAll = collectWrittenArrays loopBody
      -- Input arrays: results of zero-arg RCall in pre-loop, or allocated and
      -- filled by earlier loops but only read (not written) in kernel loop.
      preLoopCallArrays = [ v | C2.SAssign v (RCall _ []) <- preLoopStmts
                              , v `Set.member` loopUsedVars
                              , isArrayVar v ]
      -- Arrays allocated in pre-loop, used in kernel loop but not written there
      preLoopAllocReadArrays = [ v | C2.SAssign v (RArrayAlloc _) <- preLoopStmts
                                   , v `Set.member` loopUsedVars
                                   , v `Set.notMember` loopWrittenAll ]
      -- Arrays extracted from pair proc params via RProj (not RAtom aliases —
      -- those are handled by collectArrayAliases / #define in the kernel body).
      preLoopProjArrays = [ v | C2.SAssign v (RProj _ _) <- preLoopStmts
                              , v `Set.member` loopUsedVars
                              , v `Set.notMember` loopWrittenAll
                              , isArrayVar v ]
      preLoopInputArrays = nub (preLoopCallArrays ++ preLoopAllocReadArrays ++ preLoopProjArrays)
      -- In-body zero-arg calls: RCall fn [] anywhere inside loop body (recursively).
      -- These are memoized calls that need to be hoisted to CPU pre-loop and passed as buffers.
      -- Split into array calls (input buffers) and scalar calls (constant buffers).
      allZeroArgCalls = collectZeroArgCalls loopBody
      inBodyHoistedAll = [ stmt
                         | stmt@(C2.SAssign v (RCall fn [])) <- allZeroArgCalls
                         , v `Set.notMember` Set.fromList preLoopInputArrays ]
      inBodyHoistedArrays = [ stmt
                            | stmt@(C2.SAssign v (RCall fn [])) <- inBodyHoistedAll
                            , isArrayFn fn ]
      inBodyHoistedScalars = [ stmt
                             | stmt@(C2.SAssign v (RCall fn [])) <- inBodyHoistedAll
                             , not (isArrayFn fn) ]
      inBodyInputArrays = [ v | C2.SAssign v (RCall _ []) <- inBodyHoistedArrays ]
      inBodyScalarVars  = [ v | C2.SAssign v (RCall _ []) <- inBodyHoistedScalars ]
      inputArrays    = preLoopInputArrays ++ inBodyInputArrays
      -- Output arrays: RArrayAlloc in pre-loop, written in loop
      outputArrays = [ v | C2.SAssign v (RArrayAlloc _) <- preLoopStmts
                         , v `Set.member` loopWrittenAll ]
      allArrVars   = Set.fromList inputArrays `Set.union` Set.fromList outputArrays
      -- Scalar inputs: non-array pre-loop vars used in loop body
      preLoopScalarInputs = [ v | C2.SAssign v _ <- preLoopStmts
                                , v `Set.member` loopUsedVars
                                , v `Set.notMember` allArrVars
                                , not (isArrayVar v) ]
      scalarInputs = preLoopScalarInputs ++ inBodyScalarVars
  in (nub inputArrays, nub outputArrays, nub scalarInputs, inBodyHoistedAll)
  where
    isArrayVar v = case Map.lookup v typeEnv of
      Just (CTArray _) -> True
      _ -> Map.findWithDefault KScalar v retKinds `elem` [KArray, KFloatArray]
    isArrayFn fn = Map.findWithDefault KScalar fn retKinds `elem` [KArray, KFloatArray]

-- | Recursively collect zero-arg RCall statements from a statement list.
-- Returns one statement per unique function name (the first occurrence).
collectZeroArgCalls :: [C2.Stmt] -> [C2.Stmt]
collectZeroArgCalls stmts = Map.elems (Map.fromListWith (\_ first -> first) pairs)
  where
    pairs = [(fn, stmt) | stmt@(C2.SAssign _ (RCall fn [])) <- allCalls stmts]
    allCalls = concatMap go
    go stmt@(C2.SAssign _ (RCall _ [])) = [stmt]
    go (C2.SLoop _ body)       = allCalls body
    go (C2.SIf _ thn els)      = allCalls thn ++ allCalls els
    go _                       = []

-- | Build a mapping from function name → canonical hoisted variable.
-- Used to replace in-kernel zero-arg calls with the hoisted variable.
buildHoistedCallMap :: [C2.Stmt] -> Map CVar CVar
buildHoistedCallMap hoisted = Map.fromList [(fn, v) | C2.SAssign v (RCall fn []) <- hoisted]

-- | Classify which input arrays come from CPU helpers vs. prior GPU kernels.
-- Also returns a map from consumer variable to the producing kernel's output
-- buffer variable, so the harness can reuse the GPU buffer directly.
--
-- @gpuKernelOutputs@ maps kernel proc name → its output arrays (from kaOutputArrays).
classifyBufferOrigins
  :: [KernelAnalysis]        -- ^ All GPU kernels, in execution order
  -> Set CVar                -- ^ Set of GPU kernel proc names
  -> Map CVar BufferOrigin   -- ^ For each input array across all kernels
classifyBufferOrigins kernels gpuNames =
  Map.fromList
    [ (v, origin fn)
    | ka <- kernels
    , C2.SAssign v (RCall fn []) <- kaEffectivePreLoop ka
    , v `elem` kaInputArrays ka
    ]
  where
    origin fn
      | fn `Set.member` gpuNames = GPUProduced fn
      | otherwise                = CPUProduced

-- | Build a mapping from a consumer's input variable to the producing kernel's
-- output buffer variable.  E.g. if kernel B has @SAssign t4 (RCall "arr" [])@
-- and kernel A ("arr") produces output @arr1@, then @t4 → arr1@.
buildGPUBufferAliases
  :: [KernelAnalysis]        -- ^ All GPU kernels
  -> Set CVar                -- ^ GPU kernel proc names
  -> Map CVar CVar           -- ^ consumer input var → producer output var
buildGPUBufferAliases kernels gpuNames =
  Map.fromList
    [ (v, outVar)
    | ka <- kernels
    , C2.SAssign v (RCall fn []) <- kaEffectivePreLoop ka
    , fn `Set.member` gpuNames
    -- Find the producing kernel's output array.  For simple (single-array)
    -- returns the first output array is the result.
    , let producerOutputs = [ kaOutputArrays k | k <- kernels, kaName k == fn ]
    , outVar <- case producerOutputs of
        ([o]:_) -> [o]          -- single output array
        (os:_)  -> take 1 os    -- multi-output: use first (conservative)
        _       -> []
    ]

-- | Collect array alias assignments: v = RAtom (AVar arr) where arr is a known array.
-- Returns (alias, original) pairs. Recursively follows alias chains.
collectArrayAliases :: Set CVar -> [C2.Stmt] -> [(CVar, CVar)]
collectArrayAliases knownArrays stmts = go knownArrays stmts []
  where
    go _known [] acc = acc
    go known (stmt : rest) acc = case stmt of
      C2.SAssign v (RAtom (AVar src)) | src `Set.member` known ->
        go (Set.insert v known) rest ((v, src) : acc)
      C2.SLoop _ body ->
        let inner = go known body []
            known' = known `Set.union` Set.fromList (map fst inner)
        in go known' rest (inner ++ acc)
      C2.SIf _ thn els ->
        let innerT = go known thn []
            innerE = go known els []
            known' = known `Set.union` Set.fromList (map fst innerT)
                       `Set.union` Set.fromList (map fst innerE)
        in go known' rest (innerT ++ innerE ++ acc)
      _ -> go known rest acc

collectWrittenArrays :: [C2.Stmt] -> Set CVar
collectWrittenArrays = foldMap go
  where
    go (C2.SArrayWrite (AVar v) _ _) = Set.singleton v
    go (C2.SLoop _ body)             = collectWrittenArrays body
    go (C2.SIf _ thn els)            = collectWrittenArrays thn `Set.union` collectWrittenArrays els
    go _                             = Set.empty

-- ---------------------------------------------------------------------------
-- MSL pair struct support

-- | Collect all pair types from the typeEnv for variables assigned in the statement list.
-- Returns pairs in dependency order (leaf types first).
collectPairTypesFromEnv :: TypeEnv -> [C2.Stmt] -> [(CElemType, CElemType)]
collectPairTypesFromEnv typeEnv stmts =
  let allVars = collectAssignedVars stmts
      pairTypesRaw = nub
        [ (ce1, ce2)
        | v <- allVars
        , Just (CTPair t1 t2) <- [Map.lookup v typeEnv]
        , Just ce1 <- [ctypeToElemType t1]
        , Just ce2 <- [ctypeToElemType t2]
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
genMSLPairStructDefs pairTypes = unlines
  [ "// Pair struct definitions"
  ] ++ concatMap genDef pairTypes
  where
    genDef (ct1, ct2) =
      "struct " ++ mslPairStructName ct1 ct2 ++ " { " ++
      mslElemCType ct1 ++ " fst; " ++ mslElemCType ct2 ++ " snd; };\n"

-- | MSL pair struct name.
mslPairStructName :: CElemType -> CElemType -> String
mslPairStructName ct1 ct2 = "hyd_pair_" ++ celemTypeLetter ct1 ++ celemTypeLetter ct2 ++ "_t"

-- | MSL element type from CElemType.
mslElemCType :: CElemType -> String
mslElemCType CEInt = "long"
mslElemCType CEFloat = "float"  -- demoted from double
mslElemCType CEBool = "int"
mslElemCType (CEPair ct1 ct2) = mslPairStructName ct1 ct2
mslElemCType CEArray = "long"  -- can't have arrays inside MSL structs

-- ---------------------------------------------------------------------------
-- MSL type helpers

-- | Map a CType to its MSL type name.
-- CTDouble is demoted to float (MSL does not support double on most GPUs).
mslTypeName :: CType -> String
mslTypeName CTInt64   = "long"
mslTypeName CTDouble  = "float"   -- demoted
mslTypeName CTBool    = "int"
mslTypeName CTUnit    = "int"
mslTypeName CTTuple   = "hyd_tuple_t"
mslTypeName (CTPair t1 t2)
  | Just ce1 <- ctypeToElemType t1
  , Just ce2 <- ctypeToElemType t2 = mslPairStructName ce1 ce2
mslTypeName _         = "long"

-- | MSL buffer element type for an array.
mslArrayElemTypeName :: CType -> String
mslArrayElemTypeName (CTArray elt) = mslTypeName elt
mslArrayElemTypeName ct            = mslTypeName ct

-- | Whether a CType is a float type (for printf format selection).
isMSLFloatType :: CType -> Bool
isMSLFloatType CTDouble = True
isMSLFloatType (CTArray CTDouble) = True
isMSLFloatType _ = False

-- ---------------------------------------------------------------------------
-- MSL kernel source generation

-- | Generate the @.metal@ kernel source.
genMSLKernelSrc
  :: CVar          -- ^ Kernel proc name
  -> C2.LoopSpec   -- ^ Map loop spec
  -> [C2.Stmt]     -- ^ Map loop body
  -> TypeEnv
  -> Map CVar CType  -- ^ Array element types
  -> [CVar]          -- ^ Input array params (in order)
  -> [CVar]          -- ^ Output array params (in order)
  -> [CVar]          -- ^ Scalar input params (in order)
  -> Map CVar CVar   -- ^ Hoisted zero-arg call map (fn → canonical var)
  -> [(CVar, CVar)]  -- ^ Pre-loop array aliases (alias, source)
  -> String
genMSLKernelSrc kernelName loopSpec loopBody typeEnv arrayElemTys
                inputArrays outputArrays scalarInputs hoistedCallMap preLoopAliases =
  let pairTypes = collectPairTypesFromEnv typeEnv loopBody
      pairDefs = genMSLPairStructDefs pairTypes
  in
  mslSharedHeader pairDefs ++
  genMSLKernelFunction kernelName loopSpec loopBody typeEnv arrayElemTys
    inputArrays outputArrays scalarInputs hoistedCallMap preLoopAliases

-- | Shared Metal header (includes, shape helpers, erf).
-- @pairDefs@ is the pair struct definitions needed by the kernel(s).
mslSharedHeader :: String -> String
mslSharedHeader pairDefs =
  unlines
    [ "#include <metal_stdlib>"
    , "using namespace metal;"
    , ""
    , mslShapeHelpers
    , pairDefs
    , ""
    ]

-- | Generate a single @kernel void@ function (no shared header).
genMSLKernelFunction
  :: CVar -> C2.LoopSpec -> [C2.Stmt] -> TypeEnv -> Map CVar CType
  -> [CVar] -> [CVar] -> [CVar] -> Map CVar CVar -> [(CVar, CVar)]
  -> String
genMSLKernelFunction kernelName loopSpec loopBody typeEnv arrayElemTys
                     inputArrays outputArrays scalarInputs hoistedCallMap preLoopAliases =
  "kernel void " ++ sanitize kernelName ++ "(\n" ++
  intercalate ",\n" (map ("    " ++) allParams) ++ ")\n" ++
  unlines
    [ "{"
    ] ++
  iterDecompLines ++
  genMSLBody gidVar (C2.lsExec loopSpec) hoistedVarSet outputArrSet typeEnv arrayElemTys hoistedCallMap preLoopAliases loopBody ++
  "}\n"
  where
    -- All hoisted vars (input arrays + scalar inputs): skip their in-body assignments
    hoistedVarSet = Set.fromList inputArrays `Set.union` Set.fromList scalarInputs
    outputArrSet = Set.fromList outputArrays
    iters  = C2.lsIters loopSpec
    bounds = C2.lsBounds loopSpec
    -- For multi-dim loops, use a synthetic flat gid variable
    isMultiDim = length iters > 1
    gidVar = if isMultiDim then "_flat_gid" else head iters

    -- Decompose flat gid into individual iterators (row-major order)
    -- iter[n-1] = _flat_gid % bound[n-1]; remaining /= bound[n-1]; ...
    iterDecompLines
      | not isMultiDim = ""
      | otherwise =
          "    uint _rem = _flat_gid;\n" ++
          concatMap (\(it, bd) ->
            "    uint " ++ sanitize it ++ " = _rem % (uint)(" ++ genMSLIndexExpr bd ++ ");\n" ++
            "    _rem /= (uint)(" ++ genMSLIndexExpr bd ++ ");\n"
          ) (reverse (zip iters bounds))

    -- Buffer index assignment
    -- Input arrays: data buffer + shape buffer for each
    inputBufs  = zipWith (\v i -> (v, i)) inputArrays [0..]
    inputShapeBufs = zipWith (\v i -> (v, i)) inputArrays [length inputArrays..]
    nInputBufs = length inputArrays * 2  -- data + shape per input
    outputBufs = zipWith (\v i -> (v, i)) outputArrays [nInputBufs..]
    scalarBufs = zipWith (\v i -> (v, i)) scalarInputs [nInputBufs + length outputArrays..]

    allParams = inputParams ++ inputShapeParams ++ outputParams ++ scalarParams ++ [gidParam]

    inputParams  = [ "device const " ++ mslElemTy v ++ "* " ++ sanitize v ++ "_data [[buffer("
                     ++ show i ++ ")]]"
                   | (v, i) <- inputBufs ]
    inputShapeParams = [ "constant hyd_tuple_t& " ++ sanitize v ++ "_shape [[buffer("
                         ++ show i ++ ")]]"
                       | (v, i) <- inputShapeBufs ]
    outputParams = [ "device " ++ mslElemTy v ++ "* " ++ sanitize v ++ "_data [[buffer("
                     ++ show i ++ ")]]"
                   | (v, i) <- outputBufs ]
    scalarParams = [ "constant " ++ mslScalarTy v ++ "& " ++ sanitize v ++ " [[buffer("
                     ++ show i ++ ")]]"
                   | (v, i) <- scalarBufs ]
    gidParam     = "uint " ++ sanitize gidVar ++ " [[thread_position_in_grid]]"

    mslElemTy v = case Map.lookup v arrayElemTys of
      Just elt -> mslTypeName elt
      Nothing  -> case Map.lookup v typeEnv of
        Just (CTArray elt) -> mslTypeName elt
        _                  -> "long"

    mslScalarTy v = case Map.lookup v typeEnv of
      Just ct -> mslTypeName ct
      Nothing -> "long"

-- | Generate a @.metal@ source with multiple @kernel void@ functions and
-- a single shared header.
genMultiKernelMSL :: [KernelAnalysis] -> String
genMultiKernelMSL kernels =
  let -- Collect pair types from all kernels (deduplicated)
      allPairTypes = nub $ concatMap
        (\ka -> collectPairTypesFromEnv (kaTypeEnv ka) (kaLoopBody ka)) kernels
      pairDefs = genMSLPairStructDefs allPairTypes
  in mslSharedHeader pairDefs ++
     concatMap genOneKernel kernels
  where
    genOneKernel ka =
      genMSLKernelFunction (kaName ka) (kaLoopSpec ka) (kaLoopBody ka)
        (kaTypeEnv ka) (kaArrayElemTys ka) (kaInputArrays ka) (kaOutputArrays ka)
        (kaScalarInputs ka) (kaHoistedCallMap ka) (kaPreLoopAliases ka)

-- | Collect all variables assigned in a statement list (recursively).
collectAssignedVars :: [C2.Stmt] -> [CVar]
collectAssignedVars = nub . concatMap go
  where
    go (C2.SAssign v _)   = [v]
    go (C2.SLoop _ body)  = collectAssignedVars body
    go (C2.SIf _ thn els) = collectAssignedVars thn ++ collectAssignedVars els
    go _                  = []

-- | Emit MSL body statements.
-- Pre-declares all variables at the top of the kernel body to avoid
-- block-scoping issues with re-assignments (accumulators, tiled loops).
genMSLBody
  :: CVar            -- ^ Loop iterator (= gid in the kernel)
  -> C2.ExecPolicy   -- ^ Execution policy of the outermost loop
  -> Set CVar        -- ^ Hoisted vars to skip in kernel body
  -> Set CVar        -- ^ Output array vars
  -> TypeEnv
  -> Map CVar CType
  -> Map CVar CVar   -- ^ Hoisted zero-arg call map (fn → canonical var)
  -> [(CVar, CVar)]  -- ^ Pre-loop array aliases (alias, source)
  -> [C2.Stmt]
  -> String
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
      aliasDefLines = concatMap (\(v, _src) ->
        let root = resolveAlias v
        in "    #define " ++ sanitize v ++ "_data " ++ sanitize root ++ "_data\n" ++
           "    #define " ++ sanitize v ++ "_shape " ++ sanitize root ++ "_shape\n"
        ) arrayAliases
      allVars = collectAssignedVars stmts
      -- Pre-declare all variables (except input arrays and their aliases)
      declLines = concatMap (\v ->
        if v `Set.member` allInArrs
        then ""
        else "    " ++ mslVarDecl v typeEnv ++ " " ++ sanitize v ++ ";\n"
        ) allVars
      bodyLines = case scatterStrategy of
        Just _ ->
          case detectAtomicScatterAddLoop stmts of
            Just (prefix, mGuard, arrAtom, idxAtom, valAtom) ->
              let prefixStr = concatMap (genMSLStmt 1 iter allInArrs outArrs typeEnv arrayElemTys hoistedCallMap) prefix
                  atomicLine = genMSLAtomicAdd 1 iter allInArrs typeEnv arrayElemTys arrAtom idxAtom valAtom
              in case mGuard of
                   Nothing -> prefixStr ++ atomicLine
                   Just cond ->
                     prefixStr ++
                     "    if (" ++ genMSLAtom iter cond ++ ") {\n" ++
                     genMSLAtomicAdd 2 iter allInArrs typeEnv arrayElemTys arrAtom idxAtom valAtom ++
                     "    }\n"
            Nothing ->
              concatMap (genMSLStmt 1 iter allInArrs outArrs typeEnv arrayElemTys hoistedCallMap) stmts
        Nothing ->
          concatMap (genMSLStmt 1 iter allInArrs outArrs typeEnv arrayElemTys hoistedCallMap) stmts
  in declLines ++ aliasDefLines ++ bodyLines
  where
    scatterStrategy = case execPolicy of
      C2.Parallel p -> case C2.psStrategy p of
        C2.ParallelScatterAtomicAddInt   -> Just CTInt64
        C2.ParallelScatterAtomicAddFloat -> Just CTDouble
        _ -> Nothing
      _ -> Nothing

-- | Emit a Metal atomic_fetch_add_explicit for a scatter add.
genMSLAtomicAdd :: Int -> CVar -> Set CVar -> TypeEnv -> Map CVar CType -> Atom -> Atom -> Atom -> String
genMSLAtomicAdd depth iter inArrs typeEnv _arrayElemTys arrAtom idxAtom valAtom =
  let ind = replicate (depth * 4) ' '
      arrName = case arrAtom of
        AVar v -> sanitize v ++ "_data"
        _      -> genMSLAtom iter arrAtom
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
       ind ++ "{ device atomic_uint* _ap = (device atomic_uint*)&" ++ arrName ++ "[" ++ idx ++ "];\n" ++
       ind ++ "  uint _expected = atomic_load_explicit(_ap, memory_order_relaxed);\n" ++
       ind ++ "  uint _desired;\n" ++
       ind ++ "  do {\n" ++
       ind ++ "    float _old = as_type<float>(_expected);\n" ++
       ind ++ "    _desired = as_type<uint>(_old + (float)(" ++ val ++ "));\n" ++
       ind ++ "  } while (!atomic_compare_exchange_weak_explicit(_ap, &_expected, _desired,\n" ++
       ind ++ "            memory_order_relaxed, memory_order_relaxed));\n" ++
       ind ++ "}\n"
     else
       -- Integer atomic add
       ind ++ "atomic_fetch_add_explicit((device atomic_int*)&" ++ arrName ++ "[" ++ idx ++ "], (int)(" ++ val ++ "), memory_order_relaxed);\n"

genMSLStmt :: Int -> CVar -> Set CVar -> Set CVar -> TypeEnv -> Map CVar CType -> Map CVar CVar -> C2.Stmt -> String
genMSLStmt depth iter inArrs outArrs typeEnv arrayElemTys hoistedCallMap stmt =
  let ind = replicate (depth * 4) ' '
      recurse = genMSLStmt (depth+1) iter inArrs outArrs typeEnv arrayElemTys hoistedCallMap
  in case stmt of
    C2.SAssign v _
      | v `Set.member` inArrs ->
          -- This is a hoisted Metal buffer/constant parameter; skip the call in the kernel.
          ""
    -- Zero-arg call whose function was hoisted: replace with canonical hoisted var
    C2.SAssign v (RCall fn [])
      | Just canonVar <- Map.lookup fn hoistedCallMap
      , v /= canonVar ->
          ind ++ sanitize v ++ " = " ++ sanitize canonVar ++ ";\n"
    C2.SAssign v rhs ->
          ind ++ sanitize v ++ " = " ++
          genMSLRHS v iter inArrs outArrs typeEnv arrayElemTys rhs ++ ";\n"

    C2.SArrayWrite (AVar arr) idx val ->
      let suffix = sanitize arr ++ "_data"
      in ind ++ suffix ++ "[" ++ genMSLAtom iter idx ++ "] = " ++
         genMSLAtom iter val ++ ";\n"

    C2.SArrayWrite arr idx val ->
      -- Fallback: shouldn't happen for well-formed kernels
      ind ++ genMSLAtom iter arr ++ "[" ++ genMSLAtom iter idx ++ "] = " ++
      genMSLAtom iter val ++ ";\n"

    C2.SLoop spec body ->
      case (C2.lsIters spec, C2.lsBounds spec) of
        ([i], [b]) ->
          let ci = sanitize i
              red = C2.lsRed spec
              initLine = case red of
                Just r ->
                  ind ++ sanitize (C2.rsAccVar r) ++ " = " ++
                  genMSLIndexExpr (C2.rsInit r) ++ ";\n"
                Nothing -> ""
              loopLine = ind ++ "for (uint " ++ ci ++ " = 0; " ++ ci ++ " < " ++
                         genMSLIndexExpr b ++ "; " ++ ci ++ "++) {\n"
              bodyLines = concatMap recurse body
              closeLine = ind ++ "}\n"
          in initLine ++ loopLine ++ bodyLines ++ closeLine
        _ ->
          -- Multi-dim inner loops: emit nested for loops
          let bodyLines = concatMap recurse body
              mkLoop (ci, b) inner =
                ind ++ "for (uint " ++ sanitize ci ++ " = 0; " ++ sanitize ci ++ " < " ++
                genMSLIndexExpr b ++ "; " ++ sanitize ci ++ "++) {\n" ++ inner ++ ind ++ "}\n"
          in foldr mkLoop bodyLines (zip (C2.lsIters spec) (C2.lsBounds spec))

    C2.SIf cond thn els ->
      ind ++ "if (" ++ genMSLAtom iter cond ++ ") {\n" ++
      concatMap recurse thn ++
      (case els of
         [] -> ind ++ "}\n"
         _  -> ind ++ "} else {\n" ++
               concatMap recurse els ++
               ind ++ "}\n")

    C2.SReturn a ->
      -- In a kernel, SReturn means write to output; handled by SArrayWrite above.
      -- A scalar return at the top level of the kernel body is unusual but emit as comment.
      ind ++ "/* return " ++ genMSLAtom iter a ++ "; */\n"

    C2.SBreak ->
      ind ++ "break;\n"

-- | MSL variable declaration type (excluding the variable name).
mslVarDecl :: CVar -> TypeEnv -> String
mslVarDecl v typeEnv = case Map.lookup v typeEnv of
  Just ct -> mslTypeName ct
  Nothing -> "long"

-- | Generate MSL for a right-hand-side expression.
genMSLRHS :: CVar -> CVar -> Set CVar -> Set CVar -> TypeEnv -> Map CVar CType -> RHS -> String
genMSLRHS assignedVar iter inArrs outArrs typeEnv arrayElemTys rhs = case rhs of
  RAtom a -> genMSLAtom iter a
  RBinOp op a1 a2 ->
    "(" ++ genMSLAtom iter a1 ++ " " ++ mslBinOp op ++ " " ++ genMSLAtom iter a2 ++ ")"
  RUnOp op a -> case op of
    CNot -> "!" ++ genMSLAtom iter a
    CNeg -> "-" ++ genMSLAtom iter a
    _    -> mslUnOp op ++ "(" ++ genMSLAtom iter a ++ ")"
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
    genMSLAtom iter shp  -- treat as pass-through for scalar shapes
  RShapeLast shp ->
    genMSLAtom iter shp
  R2DToFlat i w ->
    "((uint)(" ++ genMSLAtom iter i ++ ") * (uint)(" ++ genMSLAtom iter w ++ "))"
  RCall fn args ->
    sanitize fn ++ "(" ++ intercalate ", " (map (genMSLAtom iter) args) ++ ")"
  RPairMake ct1 ct2 a1 a2 ->
    let structName = case Map.lookup assignedVar typeEnv of
          Just (CTPair t1 t2) | Just ce1 <- ctypeToElemType t1
                              , Just ce2 <- ctypeToElemType t2 ->
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
  RNdToFlat nd shp ->
    "hyd_nd_to_flat(" ++ genMSLAtom iter nd ++ ", " ++ genMSLAtom iter shp ++ ")"
  _ -> "0 /* unhandled RHS */"

-- | Generate MSL for an atom.
genMSLAtom :: CVar -> Atom -> String
genMSLAtom iter atom = case atom of
  AVar v    -> if v == iter then sanitize iter else sanitize v
  AInt n    -> show n
  AFloat f  -> show f ++ "f"
  ABool True  -> "1"
  ABool False -> "0"
  AUnit     -> "0"
  AString s -> "\"" ++ BS.unpack s ++ "\""
  AVecVar v -> sanitize v

-- | Generate MSL for an index expression.
genMSLIndexExpr :: C2.IndexExpr -> String
genMSLIndexExpr expr = case C2.simplifyIndexExpr expr of
  C2.IVar v    -> sanitize v
  C2.IConst n  -> show n
  C2.IAdd a b  -> "(" ++ genMSLIndexExpr a ++ " + " ++ genMSLIndexExpr b ++ ")"
  C2.ISub a b  -> "(" ++ genMSLIndexExpr a ++ " - " ++ genMSLIndexExpr b ++ ")"
  C2.IMul a b  -> "(" ++ genMSLIndexExpr a ++ " * " ++ genMSLIndexExpr b ++ ")"
  C2.IDiv a b  -> "(" ++ genMSLIndexExpr a ++ " / " ++ genMSLIndexExpr b ++ ")"
  C2.ITuple es ->
    let elems = intercalate ", " (map (\e -> "(long)(" ++ genMSLIndexExpr e ++ ")") es)
    in "(hyd_tuple_t){{" ++ elems ++ "}, " ++ show (length es) ++ "}"
  C2.IProj i e -> "(" ++ genMSLIndexExpr e ++ ").elems[" ++ show i ++ "]"
  C2.IFlatToNd flat shp ->
    "hyd_flat_to_nd((long)(" ++ genMSLIndexExpr flat ++ "), " ++ genMSLIndexExpr shp ++ ")"
  C2.INdToFlat nd shp ->
    "hyd_nd_to_flat(" ++ genMSLIndexExpr nd ++ ", " ++ genMSLIndexExpr shp ++ ")"
  C2.ICall _ _ -> "0"
  _            -> "0"

mslBinOp :: BinOp -> String
mslBinOp CAdd  = "+"
mslBinOp CSub  = "-"
mslBinOp CMul  = "*"
mslBinOp CDiv  = "/"
mslBinOp CMod  = "%"
mslBinOp CEq   = "=="
mslBinOp CNeq  = "!="
mslBinOp CLt   = "<"
mslBinOp CLe   = "<="
mslBinOp CGt   = ">"
mslBinOp CGe   = ">="
mslBinOp CAnd  = "&&"
mslBinOp COr   = "||"
mslBinOp CAddF = "+"
mslBinOp CSubF = "-"
mslBinOp CMulF = "*"
mslBinOp CDivF = "/"
mslBinOp CEqF  = "=="
mslBinOp CNeqF = "!="
mslBinOp CLtF  = "<"
mslBinOp CLeF  = "<="
mslBinOp CGtF  = ">"
mslBinOp CGeF  = ">="

mslUnOp :: UnOp -> String
mslUnOp CSqrt   = "sqrt"
mslUnOp CExpF   = "exp"
mslUnOp CLog    = "log"
mslUnOp CSin    = "sin"
mslUnOp CCos    = "cos"
mslUnOp CAbsF   = "fabs"
mslUnOp CFloorF = "floor"
mslUnOp CCeilF  = "ceil"
mslUnOp CErf    = "erf"
mslUnOp CFloatOf = "(float)"
mslUnOp CIntOf   = "(long)"
mslUnOp CNot    = "!"
mslUnOp CNeg    = "-"

isVecRHS :: RHS -> Bool
isVecRHS (RVecLoad {})   = True
isVecRHS (RVecBinOp {})  = True
isVecRHS (RVecUnOp {})   = True
isVecRHS (RVecSplat {})  = True
isVecRHS (RVecReduce {}) = True
isVecRHS _               = False

-- ---------------------------------------------------------------------------
-- ObjC harness generation

-- | Generate helper C code for all procs (including the kernel proc, since
-- other procs may call it transitively). The harness main() uses the Metal
-- version of the kernel and ignores the C version.
genHelperC :: [C2.Proc] -> CVar -> Map CVar VarKind -> Either String String
genHelperC procs _kernelName _retKinds =
  let helperOpts = defaultCodegenOptions { codegenEmitMain = False }
  in case codegenProgram2WithOptions helperOpts (C2.Program procs) of
       Right arts -> Right (codegenSource arts)
       Left  err  -> Left err

-- | Generate the self-contained ObjC harness source.
genObjCHarnessSrc
  :: C2.Program
  -> C2.Proc
  -> CVar
  -> [C2.Stmt]          -- ^ Pre-loop stmts
  -> C2.LoopSpec
  -> Maybe Atom         -- ^ Return atom (for output identification)
  -> TypeEnv
  -> Map CVar CType     -- ^ Array element types
  -> Map CVar VarKind
  -> Map CVar CType     -- ^ Program-level return types
  -> [CVar]             -- ^ Input arrays
  -> [CVar]             -- ^ Output arrays
  -> [CVar]             -- ^ Scalar inputs
  -> [CVar]             -- ^ Proc parameters (declared with defaults)
  -> String             -- ^ Helper C source (already generated)
  -> String
genObjCHarnessSrc _prog _kernelProc kernelName preLoopStmts loopSpec _retAtom
                  typeEnv arrayElemTys retKinds _retTypes
                  inputArrays outputArrays scalarInputs procParams helperC =
  unlines $
    [ "// Hydrangea Metal harness — generated by the MSL backend."
    , "#include \"hydrangea_runtime.h\""
    , "#include <stdio.h>"
    , "#include <string.h>"
    , "#include <stdlib.h>"
    , "#import <Foundation/Foundation.h>"
    , "#import <Metal/Metal.h>"
    , ""
    , "// ---- CPU helper procs ----"
    , helperC
    , ""
    , "// ---- Metal harness main ----"
    , "int main(int argc, const char* argv[]) {"
    , "    @autoreleasepool {"
    , "    const char* _metallib = argc > 1 ? argv[1] : \"kernel.metallib\";"
    , ""
    , "    // --- Proc parameters (defaults) ---"
    ] ++
    procParamLines ++
    [ "    // --- CPU pre-loop setup ---"
    ] ++
    preLoopLines ++
    [ ""
    , "    // Grid size from loop bound(s)"
    , "    long _n = " ++ gridSizeExpr ++ ";"
    , ""
    , "    // --- Metal device and pipeline setup ---"
    , "    id<MTLDevice> _dev = MTLCreateSystemDefaultDevice();"
    , "    if (!_dev) { fprintf(stderr, \"hydrangea: Metal not available\\n\"); return 1; }"
    , "    NSError* _err = nil;"
    , "    NSURL* _libURL = [NSURL fileURLWithPath:[NSString stringWithUTF8String:_metallib]];"
    , "    id<MTLLibrary> _lib = [_dev newLibraryWithURL:_libURL error:&_err];"
    , "    if (!_lib) {"
    , "        fprintf(stderr, \"hydrangea: failed to load metallib: %s\\n\","
    , "                [_err.localizedDescription UTF8String]);"
    , "        return 1;"
    , "    }"
    , "    id<MTLFunction> _fn = [_lib newFunctionWithName:@\"" ++ sanitize kernelName ++ "\"];"
    , "    if (!_fn) { fprintf(stderr, \"hydrangea: kernel function not found\\n\"); return 1; }"
    , "    id<MTLComputePipelineState> _pso ="
    , "        [_dev newComputePipelineStateWithFunction:_fn error:&_err];"
    , "    if (!_pso) {"
    , "        fprintf(stderr, \"hydrangea: pipeline error: %s\\n\","
    , "                [_err.localizedDescription UTF8String]);"
    , "        return 1;"
    , "    }"
    , "    id<MTLCommandQueue> _queue = [_dev newCommandQueue];"
    , ""
    , "    // --- Create Metal buffers ---"
    ] ++
    inputBufLines ++
    inputShapeBufLines ++
    outputBufLines ++
    scalarBufLines ++
    [ ""
    , "    // --- Dispatch ---"
    , "    id<MTLCommandBuffer> _cmd = [_queue commandBuffer];"
    , "    id<MTLComputeCommandEncoder> _enc = [_cmd computeCommandEncoder];"
    , "    [_enc setComputePipelineState:_pso];"
    ] ++
    setBufLines ++
    [ "    NSUInteger _tgs = MIN(256UL, _pso.maxTotalThreadsPerThreadgroup);"
    , "    if (_tgs == 0) _tgs = 1;"
    , "    [_enc dispatchThreads:MTLSizeMake((NSUInteger)_n, 1, 1)"
    , "      threadsPerThreadgroup:MTLSizeMake(_tgs, 1, 1)];"
    , "    [_enc endEncoding];"
    , "    [_cmd commit];"
    , "    [_cmd waitUntilCompleted];"
    , ""
    , "    // --- Print output ---"
    ] ++
    printLines ++
    [ "    } // @autoreleasepool"
    , "    return 0;"
    , "}"
    ]
  where
    iter = head (C2.lsIters loopSpec)
    nInputArrays = length inputArrays
    totalBufs = nInputArrays * 2 + length outputArrays + length scalarInputs  -- data + shape per input

    -- Proc parameter declarations with default values
    procParamLines = concatMap declProcParam procParams
    declProcParam v =
      let ty = case Map.lookup v typeEnv of
                 Just ct -> ct
                 Nothing -> CTInt64
          cTy = cTypeName ty
          defVal = case ty of
            CTDouble -> "0.0"
            CTBool   -> "0"
            CTArray _ -> "NULL"
            CTPair _ _ -> "((" ++ cTypeName ty ++ "){0})"
            _        -> "0LL"
      in [ "    " ++ cTy ++ " " ++ sanitize v ++ " = " ++ defVal ++ ";" ]

    -- Grid size expression: product of all loop bounds
    gridSizeExpr = case C2.lsBounds loopSpec of
      [b] -> "(long)" ++ genMSLIndexExpr b
      bs  -> intercalate " * " ["(long)" ++ genMSLIndexExpr b | b <- bs]

    -- Pre-loop C statements (skip output array allocations)
    preLoopLines = concatMap (genPreLoopLine typeEnv retKinds (Set.fromList outputArrays) Set.empty) preLoopStmts

    -- Input buffer creation + fill
    -- Note: CPU arrays storing CTDouble use 8-byte doubles, but Metal uses 4-byte floats.
    -- We must convert element-by-element for float arrays.
    inputBufLines = concat
      [ [ "    // Input buffer " ++ show i ++ ": " ++ sanitize v
        , "    long _inN" ++ show i ++ " = hyd_shape_size(" ++ sanitize v ++ "->shape);"
        , "    size_t _buf" ++ show i ++ "_sz = (size_t)(_inN" ++ show i ++ " * sizeof(" ++ elemTy ++ "));"
        , "    id<MTLBuffer> _buf" ++ show i ++ " = [_dev newBufferWithLength:_buf" ++ show i
          ++ "_sz options:MTLResourceStorageModeShared];"
        ] ++ fillLine i v elemTy
      | (v, i) <- zip inputArrays [0..]
      , let elemTy = mslElemTy v
      ]
      where
        fillLine i v "float" =
          -- CPU stores double (8 bytes); Metal reads float (4 bytes): convert
          [ "    { double* _fsrc = (double*)" ++ sanitize v ++ "->data; float* _fdst = (float*)_buf" ++ show i ++ ".contents;"
          , "      for (long _fci = 0; _fci < _inN" ++ show i ++ "; _fci++) _fdst[_fci] = (float)_fsrc[_fci]; }"
          ]
        fillLine i v _ =
          -- Same element size on CPU and GPU (int64 / long): plain memcpy
          [ "    memcpy(_buf" ++ show i ++ ".contents, " ++ sanitize v ++ "->data, _buf" ++ show i ++ "_sz);" ]

    -- Shape buffers for input arrays
    inputShapeBufLines = concat
      [ [ "    // Shape buffer " ++ show i ++ ": " ++ sanitize v ++ " shape"
        , "    id<MTLBuffer> _buf" ++ show i ++ " = [_dev newBufferWithBytes:&" ++ sanitize v
          ++ "->shape length:sizeof(hyd_tuple_t) options:MTLResourceStorageModeShared];"
        ]
      | (v, i) <- zip inputArrays [nInputArrays..]
      ]

    -- Output buffer creation
    -- Map from output array name → shape variable used in its RArrayAlloc
    outputAllocShapes = Map.fromList
      [ (v, shpAtom) | C2.SAssign v (RArrayAlloc shpAtom) <- preLoopStmts ]
    outputBufLines = concat
      [ [ "    // Output buffer " ++ show i ++ ": " ++ sanitize v
        , "    long _outN" ++ show i ++ " = " ++ outSizeExpr v ++ ";"
        , "    size_t _buf" ++ show i ++ "_sz = (size_t)(_outN" ++ show i ++ " * sizeof(" ++ elemTy ++ "));"
        , "    id<MTLBuffer> _buf" ++ show i ++ " = [_dev newBufferWithLength:_buf" ++ show i
          ++ "_sz options:MTLResourceStorageModeShared];"
        , "    memset(_buf" ++ show i ++ ".contents, 0, _buf" ++ show i ++ "_sz);"
        ]
      | (v, i) <- zip outputArrays [nInputArrays * 2..]
      , let elemTy = mslElemTy v
      ]
    -- Output size: use the allocation shape if known, otherwise fall back to _n
    outSizeExpr v = case Map.lookup v outputAllocShapes of
      Just shpAtom -> "hyd_shape_size(" ++ genCAtom shpAtom ++ ")"
      Nothing -> "_n"

    -- Scalar buffer creation (constant buffers)
    scalarBufLines = concat
      [ [ "    // Scalar buffer " ++ show i ++ ": " ++ sanitize v
        , "    " ++ scalarTy v ++ " _scalar" ++ show i ++ " = " ++ sanitize v ++ ";"
        , "    id<MTLBuffer> _buf" ++ show i ++ " = [_dev newBufferWithBytes:&_scalar" ++ show i
          ++ " length:sizeof(" ++ scalarTy v ++ ") options:MTLResourceStorageModeShared];"
        ]
      | (v, i) <- zip scalarInputs [nInputArrays * 2 + length outputArrays..]
      ]

    -- setBuffer calls
    setBufLines =
      [ "    [_enc setBuffer:_buf" ++ show i ++ " offset:0 atIndex:" ++ show i ++ "];"
      | i <- [0 .. totalBufs - 1]
      ]

    -- Output printing
    printLines = concat
      [ genOutputPrintLines v i
      | (v, i) <- zip outputArrays [nInputArrays * 2..]
      ]

    mslElemTy v = case Map.lookup v arrayElemTys of
      Just elt -> mslTypeName elt
      Nothing  -> case Map.lookup v typeEnv of
        Just (CTArray elt) -> mslTypeName elt
        _                  -> "long"

    scalarTy v = case Map.lookup v typeEnv of
      Just ct -> mslTypeName ct
      Nothing -> "long"

    genOutputPrintLines v i =
      let eTy = mslElemTy v
          fmt = if eTy == "float" then "%.17g" else "%ld"
          cast = if eTy == "float" then "(double)" else "(long)"
          sizeVar = "_outN" ++ show i
      in [ "    // Print output: " ++ sanitize v
         , "    " ++ eTy ++ "* _out" ++ show i ++ " = (" ++ eTy ++ "*)_buf" ++ show i ++ ".contents;"
         , "    printf(\"[\");"
         , "    for (long _pi = 0; _pi < " ++ sizeVar ++ "; _pi++) {"
         , "        if (_pi > 0) printf(\", \");"
         , "        printf(\"" ++ fmt ++ "\", " ++ cast ++ "_out" ++ show i ++ "[_pi]);"
         , "    }"
         , "    printf(\"]\\n\");"
         ]

-- ---------------------------------------------------------------------------
-- Multi-kernel ObjC harness generation

-- | Generate a harness that dispatches multiple GPU kernels in sequence.
-- Intermediate buffers stay on GPU; only the last kernel's outputs are read back.
genMultiKernelHarnessSrc
  :: C2.Program
  -> [KernelAnalysis]       -- ^ Kernels in execution order
  -> Map CVar BufferOrigin  -- ^ Buffer origin for each input array
  -> Map CVar CVar          -- ^ GPU buffer aliases (consumer var → producer output var)
  -> Map CVar VarKind       -- ^ Return kinds
  -> Map CVar CType         -- ^ Program-level return types
  -> String                 -- ^ Helper C source
  -> String
genMultiKernelHarnessSrc _prog kernels bufferOrigins gpuAliases retKinds _retTypes helperC =
  unlines $
    -- Header
    [ "// Hydrangea Metal harness (multi-kernel) — generated by the MSL backend."
    , "#include \"hydrangea_runtime.h\""
    , "#include <stdio.h>"
    , "#include <string.h>"
    , "#include <stdlib.h>"
    , "#import <Foundation/Foundation.h>"
    , "#import <Metal/Metal.h>"
    , ""
    , "// ---- CPU helper procs ----"
    , helperC
    , ""
    , "// ---- Metal harness main ----"
    , "int main(int argc, const char* argv[]) {"
    , "    @autoreleasepool {"
    , "    const char* _metallib = argc > 1 ? argv[1] : \"kernel.metallib\";"
    , ""
    , "    // --- Metal device and library setup ---"
    , "    id<MTLDevice> _dev = MTLCreateSystemDefaultDevice();"
    , "    if (!_dev) { fprintf(stderr, \"hydrangea: Metal not available\\n\"); return 1; }"
    , "    NSError* _err = nil;"
    , "    NSURL* _libURL = [NSURL fileURLWithPath:[NSString stringWithUTF8String:_metallib]];"
    , "    id<MTLLibrary> _lib = [_dev newLibraryWithURL:_libURL error:&_err];"
    , "    if (!_lib) {"
    , "        fprintf(stderr, \"hydrangea: failed to load metallib: %s\\n\","
    , "                [_err.localizedDescription UTF8String]);"
    , "        return 1;"
    , "    }"
    , "    id<MTLCommandQueue> _queue = [_dev newCommandQueue];"
    , ""
    ] ++
    -- Pipeline state objects (one per kernel)
    concatMap genPSO indexedKernels ++
    [""] ++
    -- Per-kernel dispatch blocks
    concatMap genKernelBlock indexedKernels ++
    -- Print output (from last kernel only)
    [ "    // --- Print output ---" ] ++
    genLastKernelPrint ++
    [ "    } // @autoreleasepool"
    , "    return 0;"
    , "}"
    ]
  where
    indexedKernels = zip [0..] kernels
    lastIdx = length kernels - 1

    -- Naming: _k<idx>_<buftype><sub>
    kPrefix :: Int -> String
    kPrefix ki = "_k" ++ show ki ++ "_"

    -- Generate pipeline state object for one kernel
    genPSO (ki, ka) =
      let p = kPrefix ki
      in [ "    // Pipeline: " ++ sanitize (kaName ka)
         , "    id<MTLFunction> " ++ p ++ "fn = [_lib newFunctionWithName:@\"" ++ sanitize (kaName ka) ++ "\"];"
         , "    if (!" ++ p ++ "fn) { fprintf(stderr, \"hydrangea: kernel function '" ++ sanitize (kaName ka)
           ++ "' not found\\n\"); return 1; }"
         , "    id<MTLComputePipelineState> " ++ p ++ "pso ="
         , "        [_dev newComputePipelineStateWithFunction:" ++ p ++ "fn error:&_err];"
         , "    if (!" ++ p ++ "pso) { fprintf(stderr, \"hydrangea: pipeline error: %s\\n\","
         , "            [_err.localizedDescription UTF8String]); return 1; }"
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
          gpuProducedVars = Set.fromList
            [ v | C2.SAssign v (RCall fn []) <- kaEffectivePreLoop ka
                , case Map.lookup v bufferOrigins of
                    Just (GPUProduced _) -> True
                    _ -> False ]
          -- Shape expressions for GPU-produced vars (from producer output shapes)
          gpuKernelNames = Set.fromList [kaName k | k <- kernels]
          gpuShapeExprs = Map.fromList
            [ (v, findProducerShapeVar fn)
            | C2.SAssign v (RCall fn []) <- kaEffectivePreLoop ka
            , fn `Set.member` gpuKernelNames
            ]
      in
      [ "", "    // ======== Kernel " ++ show ki ++ ": " ++ sanitize (kaName ka) ++ " ========" ] ++
      -- Pre-loop (skip GPU-produced array calls and output allocs)
      [ "    // CPU pre-loop" ] ++
      concatMap (genMultiPreLoopLine te retKinds skipAlloc gpuProducedVars gpuShapeExprs Set.empty) (kaEffectivePreLoop ka) ++
      -- Grid size
      [ "    long " ++ p ++ "n = " ++ gridSizeExprFor ka ++ ";" ] ++
      -- Input buffers
      concatMap (genInputBuf ki p te ae) (zip inArrs [0..]) ++
      -- Input shape buffers
      concatMap (genInputShapeBuf ki p) (zip inArrs [nInArrs..]) ++
      -- Output buffers
      concatMap (genOutputBuf ki p ka te ae) (zip outArrs [nInArrs * 2..]) ++
      -- Scalar buffers
      concatMap (genScalarBuf ki p te) (zip scals [nInArrs * 2 + length outArrs..]) ++
      -- Dispatch
      genDispatch ki p ka (nInArrs * 2 + length outArrs + length scals)

    gridSizeExprFor ka = case C2.lsBounds (kaLoopSpec ka) of
      [b] -> "(long)" ++ genMSLIndexExpr b
      bs  -> intercalate " * " ["(long)" ++ genMSLIndexExpr b | b <- bs]

    -- Input buffer for one array. Reuse GPU buffer if the origin is a prior kernel.
    genInputBuf ki p te ae (v, sub) =
      let bufName = p ++ "buf" ++ show sub
          elemTy = mslElemTyFor ae te v
      in case Map.lookup v gpuAliases of
        Just producerVar ->
          -- Reuse the GPU buffer from the producing kernel
          let producerBuf = findProducerBufName producerVar
          in [ "    // Input " ++ sanitize v ++ " — reuse GPU buffer from " ++ sanitize producerVar
             , "    id<MTLBuffer> " ++ bufName ++ " = " ++ producerBuf ++ ";"
             ]
        Nothing ->
          -- CPU-produced: upload from hyd_array_t*
          [ "    // Input " ++ sanitize v
          , "    long " ++ p ++ "inN" ++ show sub ++ " = hyd_shape_size(" ++ sanitize v ++ "->shape);"
          , "    size_t " ++ bufName ++ "_sz = (size_t)(" ++ p ++ "inN" ++ show sub ++ " * sizeof(" ++ elemTy ++ "));"
          , "    id<MTLBuffer> " ++ bufName ++ " = [_dev newBufferWithLength:" ++ bufName ++ "_sz options:MTLResourceStorageModeShared];"
          ] ++ fillLine (p ++ "inN" ++ show sub) bufName v elemTy

    fillLine sizeVar bufName v "float" =
      [ "    { double* _fsrc = (double*)" ++ sanitize v ++ "->data; float* _fdst = (float*)" ++ bufName ++ ".contents;"
      , "      for (long _fci = 0; _fci < " ++ sizeVar ++ "; _fci++) _fdst[_fci] = (float)_fsrc[_fci]; }"
      ]
    fillLine _sizeVar bufName v _ =
      [ "    memcpy(" ++ bufName ++ ".contents, " ++ sanitize v ++ "->data, " ++ bufName ++ "_sz);" ]

    genInputShapeBuf ki p (v, sub) =
      let bufName = p ++ "buf" ++ show sub
      in case Map.lookup v gpuAliases of
        Just producerVar ->
          -- Reuse shape from producing kernel's output shape
          let producerShapeBuf = findProducerShapeBufName producerVar
          in [ "    id<MTLBuffer> " ++ bufName ++ " = " ++ producerShapeBuf ++ ";" ]
        Nothing ->
          [ "    // Shape: " ++ sanitize v
          , "    id<MTLBuffer> " ++ bufName ++ " = [_dev newBufferWithBytes:&" ++ sanitize v
            ++ "->shape length:sizeof(hyd_tuple_t) options:MTLResourceStorageModeShared];"
          ]

    genOutputBuf ki p ka te ae (v, sub) =
      let bufName = p ++ "buf" ++ show sub
          elemTy = mslElemTyFor ae te v
          outputAllocShapes = Map.fromList
            [ (ov, shpAtom) | C2.SAssign ov (RArrayAlloc shpAtom) <- kaEffectivePreLoop ka ]
          sizeExpr = case Map.lookup v outputAllocShapes of
            Just shpAtom -> "hyd_shape_size(" ++ genCAtom shpAtom ++ ")"
            Nothing -> p ++ "n"
      in [ "    // Output " ++ sanitize v
         , "    long " ++ p ++ "outN" ++ show sub ++ " = " ++ sizeExpr ++ ";"
         , "    size_t " ++ bufName ++ "_sz = (size_t)(" ++ p ++ "outN" ++ show sub ++ " * sizeof(" ++ elemTy ++ "));"
         , "    id<MTLBuffer> " ++ bufName ++ " = [_dev newBufferWithLength:" ++ bufName ++ "_sz options:MTLResourceStorageModeShared];"
         , "    memset(" ++ bufName ++ ".contents, 0, " ++ bufName ++ "_sz);"
         -- Also create a shape buffer for this output (needed if a downstream kernel references it)
         , "    hyd_tuple_t " ++ p ++ "outShape" ++ show sub ++ " = " ++ outputShapeInit ka v ++ ";"
         , "    id<MTLBuffer> " ++ p ++ "outShapeBuf" ++ show sub
           ++ " = [_dev newBufferWithBytes:&" ++ p ++ "outShape" ++ show sub
           ++ " length:sizeof(hyd_tuple_t) options:MTLResourceStorageModeShared];"
         ]

    -- Build the shape from the allocation, or from a post-loop reshape if present.
    -- Scatter kernels allocate a 1D buffer but reshape to N-D before returning;
    -- downstream kernels need the N-D shape for correct indexing.
    outputShapeInit ka v =
      let outputAllocShapes = Map.fromList
            [ (ov, shpAtom) | C2.SAssign ov (RArrayAlloc shpAtom) <- kaEffectivePreLoop ka ]
          -- Check if the output array is reshaped in the post-loop before returning
          reshapeShape = listToMaybe
            [ shpAtom
            | C2.SAssign _ (RCall "hyd_array_reshape_view" [arrAtom, shpAtom])
                <- kaPostLoopStmts ka
            , arrAtom == AVar v
            ]
      in case reshapeShape of
        Just shpAtom -> genCAtom shpAtom
        Nothing -> case Map.lookup v outputAllocShapes of
          Just shpAtom -> genCAtom shpAtom
          Nothing ->
            -- Fallback: build from loop bounds
            let bs = C2.lsBounds (kaLoopSpec ka)
            in "hyd_tuple_make(" ++ show (length bs) ++ ", " ++
               intercalate ", " ["(int64_t)" ++ genMSLIndexExpr b | b <- bs] ++ ")"

    genScalarBuf ki p te (v, sub) =
      let bufName = p ++ "buf" ++ show sub
          sty = case Map.lookup v te of
                  Just ct -> mslTypeName ct
                  Nothing -> "long"
      in [ "    // Scalar " ++ sanitize v
         , "    " ++ sty ++ " " ++ p ++ "scalar" ++ show sub ++ " = " ++ sanitize v ++ ";"
         , "    id<MTLBuffer> " ++ bufName ++ " = [_dev newBufferWithBytes:&" ++ p ++ "scalar" ++ show sub
           ++ " length:sizeof(" ++ sty ++ ") options:MTLResourceStorageModeShared];"
         ]
      where te = kaTypeEnv (snd (indexedKernels !! ki))

    genDispatch ki p ka nBufs =
      [ "    // Dispatch " ++ sanitize (kaName ka)
      , "    id<MTLCommandBuffer> " ++ p ++ "cmd = [_queue commandBuffer];"
      , "    id<MTLComputeCommandEncoder> " ++ p ++ "enc = [" ++ p ++ "cmd computeCommandEncoder];"
      , "    [" ++ p ++ "enc setComputePipelineState:" ++ p ++ "pso];"
      ] ++
      [ "    [" ++ p ++ "enc setBuffer:" ++ p ++ "buf" ++ show i ++ " offset:0 atIndex:" ++ show i ++ "];"
      | i <- [0 .. nBufs - 1]
      ] ++
      [ "    NSUInteger " ++ p ++ "tgs = MIN(256UL, " ++ p ++ "pso.maxTotalThreadsPerThreadgroup);"
      , "    if (" ++ p ++ "tgs == 0) " ++ p ++ "tgs = 1;"
      , "    [" ++ p ++ "enc dispatchThreads:MTLSizeMake((NSUInteger)" ++ p ++ "n, 1, 1)"
      , "      threadsPerThreadgroup:MTLSizeMake(" ++ p ++ "tgs, 1, 1)];"
      , "    [" ++ p ++ "enc endEncoding];"
      , "    [" ++ p ++ "cmd commit];"
      , "    [" ++ p ++ "cmd waitUntilCompleted];"
      ]

    -- Find the buffer name for a producer's output variable
    findProducerBufName :: CVar -> String
    findProducerBufName prodVar =
      head $ [ kPrefix ki ++ "buf" ++ show sub
             | (ki, ka) <- indexedKernels
             , let outArrs = kaOutputArrays ka
                   nIn = length (kaInputArrays ka)
             , (ov, sub) <- zip outArrs [nIn * 2..]
             , ov == prodVar
             ] ++ ["/* ERROR: producer buffer not found for " ++ sanitize prodVar ++ " */"]

    findProducerShapeBufName :: CVar -> String
    findProducerShapeBufName prodVar =
      head $ [ kPrefix ki ++ "outShapeBuf" ++ show sub
             | (ki, ka) <- indexedKernels
             , let outArrs = kaOutputArrays ka
                   nIn = length (kaInputArrays ka)
             , (ov, sub) <- zip outArrs [nIn * 2..]
             , ov == prodVar
             ] ++ ["/* ERROR: producer shape buffer not found for " ++ sanitize prodVar ++ " */"]

    -- Find the C variable name holding the shape for a producer kernel's output
    findProducerShapeVar :: CVar -> String
    findProducerShapeVar prodKernelName =
      head $ [ kPrefix ki ++ "outShape" ++ show sub
             | (ki, ka) <- indexedKernels
             , kaName ka == prodKernelName
             , let outArrs = kaOutputArrays ka
                   nIn = length (kaInputArrays ka)
             , (_ov, sub) <- zip outArrs [nIn * 2..]
             ] ++ ["((hyd_tuple_t){0})"]

    -- Print output from the last kernel only
    genLastKernelPrint =
      let (ki, ka) = last indexedKernels
          p = kPrefix ki
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
          in [ "    // Print output: " ++ sanitize v
             , "    " ++ elemTy ++ "* " ++ p ++ "out" ++ show sub ++ " = (" ++ elemTy ++ "*)" ++ bufName ++ ".contents;"
             , "    printf(\"[\");"
             , "    for (long _pi = 0; _pi < " ++ sizeVar ++ "; _pi++) {"
             , "        if (_pi > 0) printf(\", \");"
             , "        printf(\"" ++ fmt ++ "\", " ++ cast ++ p ++ "out" ++ show sub ++ "[_pi]);"
             , "    }"
             , "    printf(\"]\\n\");"
             ]
        | (idx, v) <- zip [0..] outArrs
        ]

    mslElemTyFor ae te v = case Map.lookup v ae of
      Just elt -> mslTypeName elt
      Nothing  -> case Map.lookup v te of
        Just (CTArray elt) -> mslTypeName elt
        _                  -> "long"

-- | Pre-loop line generator for multi-kernel harness.
-- For GPU-produced arrays, emits a shape-only stub instead of calling the
-- CPU helper, so downstream code can access @v->shape@.
genMultiPreLoopLine
  :: TypeEnv -> Map CVar VarKind -> Set CVar
  -> Set CVar            -- ^ GPU-produced variables to stub
  -> Map CVar String     -- ^ GPU-produced var → shape expression
  -> Set CVar -> C2.Stmt -> [String]
genMultiPreLoopLine typeEnv retKinds skipAlloc gpuProduced gpuShapeExprs preDeclared stmt = case stmt of
  C2.SAssign v (RCall _ []) | v `Set.member` gpuProduced ->
    -- Emit a shape-only stub so v->shape is valid
    let shapeExpr = Map.findWithDefault "((hyd_tuple_t){0})" v gpuShapeExprs
    in [ "    // GPU-produced array (stub for shape access)"
       , "    hyd_array_t _stub_" ++ sanitize v ++ " = { .shape = " ++ shapeExpr ++ ", .data = NULL };"
       , "    hyd_array_t* " ++ sanitize v ++ " = &_stub_" ++ sanitize v ++ ";"
       ]
  _ -> genPreLoopLine typeEnv retKinds skipAlloc preDeclared stmt

-- | Generate a single pre-loop C statement for the ObjC harness main.
-- Returns empty list for output array allocations (replaced by Metal buffers).
-- @preDeclared@ is the set of variables already declared (e.g. by an outer SIf).
genPreLoopLine :: TypeEnv -> Map CVar VarKind -> Set CVar -> Set CVar -> C2.Stmt -> [String]
genPreLoopLine typeEnv retKinds skipAlloc preDeclared stmt = case stmt of
  C2.SAssign v (RArrayAlloc _) | v `Set.member` skipAlloc ->
    []  -- output array: skip CPU alloc, will be Metal buffer
  C2.SAssign v rhs
    | v `Set.member` preDeclared ->
        [ "    " ++ sanitize v ++ " = " ++
          genPreLoopRHS typeEnv retKinds v rhs ++ ";" ]
    | otherwise ->
        [ "    " ++ preLoopCType typeEnv retKinds v rhs ++ " " ++ sanitize v ++ " = " ++
          genPreLoopRHS typeEnv retKinds v rhs ++ ";" ]
  C2.SLoop spec body
    -- Skip pre-loop loops that only write to output arrays (Metal buffers)
    | let written = collectWrittenArrays body
    , not (Set.null written)
    , written `Set.isSubsetOf` skipAlloc ->
        []
  C2.SLoop spec body ->
    genPreLoopCLoop 1 typeEnv retKinds skipAlloc preDeclared spec body
  C2.SArrayWrite arr idx val ->
    [ "    " ++ genPreLoopCArrayWrite arr idx val ]
  C2.SIf cond thn els ->
    let -- Pre-declare all variables assigned inside the if branches
        ifVars = Set.fromList (collectAssignedVars thn ++ collectAssignedVars els)
        newDecls = ifVars `Set.difference` preDeclared
        declLines = [ "    " ++ preLoopCTypeVar typeEnv retKinds v (thn ++ els)
                      ++ " " ++ sanitize v ++ ";"
                    | v <- Set.toList newDecls ]
        inner = preDeclared `Set.union` newDecls
    in declLines ++
       [ "    if (" ++ genCAtom cond ++ ") {" ] ++
       concatMap (genPreLoopLine typeEnv retKinds skipAlloc inner) thn ++
       (case els of
          [] -> [ "    }" ]
          _  -> [ "    } else {" ] ++
                 concatMap (genPreLoopLine typeEnv retKinds skipAlloc inner) els ++
                 [ "    }" ])
  _ -> []

-- | Emit a C for-loop for pre-loop execution.
genPreLoopCLoop :: Int -> TypeEnv -> Map CVar VarKind -> Set CVar -> Set CVar -> C2.LoopSpec -> [C2.Stmt] -> [String]
genPreLoopCLoop depth typeEnv retKinds skipAlloc preDeclared spec body =
  let ind = replicate ((depth + 1) * 4) ' '
      red = C2.lsRed spec
      initLines = case red of
        Just r ->
          let accTy = case Map.lookup (C2.rsAccVar r) typeEnv of
                Just ct -> cTypeName ct
                Nothing -> error ("genPreLoopCLoop: reduction accumulator type unknown for " ++ show (C2.rsAccVar r))
          in [ind ++ accTy ++ " " ++ sanitize (C2.rsAccVar r) ++ " = " ++
              genPreLoopCIndexExpr (C2.rsInit r) ++ ";"]
        Nothing -> []
      loopLines = case (C2.lsIters spec, C2.lsBounds spec) of
        ([i], [b]) ->
          [ind ++ "for (int64_t " ++ sanitize i ++ " = 0; " ++
           sanitize i ++ " < " ++ genPreLoopCIndexExpr b ++ "; " ++
           sanitize i ++ "++) {"] ++
          concatMap (genPreLoopLine typeEnv retKinds skipAlloc preDeclared) body ++
          [ind ++ "}"]
        (iters, bounds) ->
          let mkLoop (ci, b) inner =
                [ind ++ "for (int64_t " ++ sanitize ci ++ " = 0; " ++
                 sanitize ci ++ " < " ++ genPreLoopCIndexExpr b ++ "; " ++
                 sanitize ci ++ "++) {"] ++ inner ++ [ind ++ "}"]
          in foldr mkLoop (concatMap (genPreLoopLine typeEnv retKinds skipAlloc preDeclared) body) (zip iters bounds)
  in initLines ++ loopLines

-- | Render a C array write for pre-loop.
genPreLoopCArrayWrite :: Atom -> Atom -> Atom -> String
genPreLoopCArrayWrite (AVar arr) idx val =
  "((int64_t*)" ++ sanitize arr ++ "->data)[" ++ genCAtom idx ++ "] = " ++ genCAtom val ++ ";"
genPreLoopCArrayWrite arr idx val =
  genCAtom arr ++ "[" ++ genCAtom idx ++ "] = " ++ genCAtom val ++ ";"

-- | Render an IndexExpr as C code for pre-loop.
genPreLoopCIndexExpr :: C2.IndexExpr -> String
genPreLoopCIndexExpr expr = case C2.simplifyIndexExpr expr of
  C2.IVar v    -> sanitize v
  C2.IConst n  -> show n ++ "LL"
  C2.IAdd a b  -> "(" ++ genPreLoopCIndexExpr a ++ " + " ++ genPreLoopCIndexExpr b ++ ")"
  C2.ISub a b  -> "(" ++ genPreLoopCIndexExpr a ++ " - " ++ genPreLoopCIndexExpr b ++ ")"
  C2.IMul a b  -> "(" ++ genPreLoopCIndexExpr a ++ " * " ++ genPreLoopCIndexExpr b ++ ")"
  C2.IDiv a b  -> "(" ++ genPreLoopCIndexExpr a ++ " / " ++ genPreLoopCIndexExpr b ++ ")"
  C2.INdToFlat nd shp -> "hyd_nd_to_flat(" ++ genPreLoopCIndexExpr nd ++ ", " ++ genPreLoopCIndexExpr shp ++ ")"
  C2.IFlatToNd flat shp -> "hyd_flat_to_nd(" ++ genPreLoopCIndexExpr flat ++ ", " ++ genPreLoopCIndexExpr shp ++ ")"
  C2.ITuple es -> "hyd_tuple_make(" ++ show (length es) ++
    (if null es then "" else ", " ++ intercalate ", " (map (\e -> "(int64_t)" ++ genPreLoopCIndexExpr e) es)) ++ ")"
  C2.IProj i e -> genPreLoopCIndexExpr e ++ ".elems[" ++ show i ++ "]"
  C2.ICall fn args -> sanitize fn ++ "(" ++ intercalate ", " (map genPreLoopCIndexExpr args) ++ ")"
  _ -> "0"

-- | C type string for a pre-loop variable.
preLoopCType :: TypeEnv -> Map CVar VarKind -> CVar -> RHS -> String
preLoopCType typeEnv retKinds v rhs =
  case Map.lookup v typeEnv of
    Just ct -> cTypeName ct
    Nothing -> case rhs of
      RArrayAlloc {}  -> "hyd_array_t*"
      RArrayShape {}  -> "hyd_tuple_t"
      RShapeInit {}   -> "hyd_tuple_t"
      RTuple {}       -> "hyd_tuple_t"
      RShapeSize {}   -> "int64_t"
      RShapeLast {}   -> "int64_t"
      RCall fn _ -> case Map.findWithDefault KScalar fn retKinds of
        KArray      -> "hyd_array_t*"
        KFloatArray -> "hyd_array_t*"
        KTuple      -> "hyd_tuple_t"
        KFloat      -> "double"
        _           -> "int64_t"
      RBinOp op _ _
        | isFloatArithBinOp op -> "double"
        | otherwise            -> "int64_t"
      RUnOp op _
        | isMathFloatOp op -> "double"
        | otherwise        -> "int64_t"
      RArrayLoad arr _ -> case lookupArrayElemType2 typeEnv arr of
        Just eltTy -> cTypeName eltTy
        Nothing    -> error ("preLoopCType: RArrayLoad element type unknown for array " ++ show arr)
      RFlatToNd {}  -> "hyd_tuple_t"
      RNdToFlat {}  -> "int64_t"
      R2DToFlat {}  -> "int64_t"
      RPairMake ct1 ct2 _ _ -> pairStructName ct1 ct2
      RPairFst ct _   -> celemTypeCType ct
      RPairSnd ct _   -> celemTypeCType ct
      _ -> error ("preLoopCType: cannot determine type for variable " ++ show v ++ " with rhs " ++ show rhs)

-- | Determine the C type for a variable from typeEnv, falling back to scanning
-- statements for an assignment to find the RHS-based type.
preLoopCTypeVar :: TypeEnv -> Map CVar VarKind -> CVar -> [C2.Stmt] -> String
preLoopCTypeVar typeEnv retKinds v stmts =
  case Map.lookup v typeEnv of
    Just ct -> cTypeName ct
    Nothing ->
      -- Scan statements for an assignment to v to find the RHS type
      case findAssignRHS v stmts of
        Just rhs -> preLoopCType typeEnv retKinds v rhs
        Nothing  -> error ("preLoopCTypeVar: no assignment found and type unknown for variable " ++ show v)
  where
    findAssignRHS target = go
      where
        go [] = Nothing
        go (C2.SAssign v' rhs : _) | v' == target = Just rhs
        go (C2.SIf _ thn els : rest) = go thn <|> go els <|> go rest
        go (C2.SLoop _ body : rest) = go body <|> go rest
        go (_ : rest) = go rest

-- | C expression for a pre-loop RHS (used in ObjC harness main).
-- Takes the typeEnv and assigned variable to resolve pair struct types.
genPreLoopRHS :: TypeEnv -> Map CVar VarKind -> CVar -> RHS -> String
genPreLoopRHS typeEnv retKinds v rhs = case rhs of
  RAtom a           -> genCAtom a
  RCall fn []       -> sanitize fn ++ "()"
  RCall fn args     -> sanitize fn ++ "(" ++ intercalate ", " (map genCAtom args) ++ ")"
  RArrayShape arr   -> genCAtom arr ++ "->shape"
  RShapeSize shp    -> "hyd_shape_size(" ++ genCAtom shp ++ ")"
  RShapeInit shp    -> "hyd_shape_init(" ++ genCAtom shp ++ ")"
  RShapeLast shp    -> "hyd_shape_last(" ++ genCAtom shp ++ ")"
  RArrayAlloc shp   -> "hyd_array_alloc(" ++ genCAtom shp ++ ")"
  RBinOp op a1 a2   -> "(" ++ genCAtom a1 ++ " " ++ mslBinOp op ++ " " ++ genCAtom a2 ++ ")"
  RUnOp CNeg a      -> "(-" ++ genCAtom a ++ ")"
  RUnOp op a        -> mslUnOp op ++ "(" ++ genCAtom a ++ ")"
  RTuple atoms      -> "hyd_tuple_make(" ++ show (length atoms) ++
                       (if null atoms then "" else ", " ++
                         intercalate ", " (map (\a -> "(int64_t)" ++ genCAtom a) atoms)) ++ ")"
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
    "((int64_t*)" ++ sanitize arr ++ "->data)[" ++ genCAtom idx ++ "]"
  RArrayLoad arr idx ->
    genCAtom arr ++ "[" ++ genCAtom idx ++ "]"
  RFlatToNd flat shp -> "hyd_flat_to_nd(" ++ genCAtom flat ++ ", " ++ genCAtom shp ++ ")"
  RNdToFlat AUnit _ -> "0LL"
  RNdToFlat nd shp  -> "hyd_nd_to_flat(" ++ genCAtom nd ++ ", " ++ genCAtom shp ++ ")"
  RPairMake ct1 ct2 a1 a2 ->
    let structName = case Map.lookup v typeEnv of
          Just (CTPair et1 et2) | Just ce1 <- ctypeToElemType et1
                                , Just ce2 <- ctypeToElemType et2 ->
            pairStructName ce1 ce2
          _ -> pairStructName ct1 ct2
    in "((" ++ structName ++ "){.fst = " ++ genCAtom a1 ++ ", .snd = " ++ genCAtom a2 ++ "})"
  RPairFst _ a -> genCAtom a ++ ".fst"
  RPairSnd _ a -> genCAtom a ++ ".snd"
  RRecord fields ->
    "{" ++ intercalate ", " ["." ++ sanitize f ++ " = " ++ genCAtom a | (f, a) <- fields] ++ "}"
  RRecordProj f a -> genCAtom a ++ "." ++ sanitize f
  R2DToFlat i w -> "(" ++ genCAtom i ++ " * " ++ genCAtom w ++ ")"
  _                 -> "0 /* unhandled pre-loop RHS */"

-- | Plain C atom rendering (same syntax as C, unlike genMSLAtom which maps gid).
genCAtom :: Atom -> String
genCAtom (AVar v)    = sanitize v
genCAtom (AInt n)    = show n ++ "LL"
genCAtom (AFloat f)  = show f
genCAtom (ABool True)  = "1"
genCAtom (ABool False) = "0"
genCAtom AUnit       = "0"
genCAtom (AString s) = "\"" ++ BS.unpack s ++ "\""
genCAtom (AVecVar v) = sanitize v

-- ---------------------------------------------------------------------------
-- Boilerplate Metal shape helpers emitted in every .metal file

mslShapeHelpers :: String
mslShapeHelpers = unlines
  [ "// Tuple / shape type"
  , "#define HYD_MAX_DIMS 8"
  , "struct hyd_tuple_t {"
  , "    long elems[HYD_MAX_DIMS];"
  , "    int ndims;"
  , "};"
  , ""
  , "// Shape helpers"
  , "static inline uint hyd_metal_shape_size(constant uint* s, int n) {"
  , "    uint r = 1;"
  , "    for (int i = 0; i < n; i++) r *= s[i];"
  , "    return r;"
  , "}"
  , ""
  , "static inline long hyd_shape_size_t(hyd_tuple_t shape) {"
  , "    long r = 1;"
  , "    for (int i = 0; i < shape.ndims; i++) r *= shape.elems[i];"
  , "    return r;"
  , "}"
  , ""
  , "static inline hyd_tuple_t hyd_flat_to_nd(long flat, hyd_tuple_t shape) {"
  , "    hyd_tuple_t idx;"
  , "    idx.ndims = shape.ndims;"
  , "    long remaining = flat;"
  , "    for (int i = shape.ndims - 1; i >= 0; i--) {"
  , "        idx.elems[i] = remaining % shape.elems[i];"
  , "        remaining /= shape.elems[i];"
  , "    }"
  , "    return idx;"
  , "}"
  , ""
  , "static inline long hyd_nd_to_flat(hyd_tuple_t nd, hyd_tuple_t shape) {"
  , "    long flat = 0;"
  , "    long stride = 1;"
  , "    for (int i = shape.ndims - 1; i >= 0; i--) {"
  , "        flat += nd.elems[i] * stride;"
  , "        stride *= shape.elems[i];"
  , "    }"
  , "    return flat;"
  , "}"
  , ""
  , "// erf approximation (Abramowitz & Stegun 7.1.26, max error 1.5e-7)"
  , "static inline float erf(float x) {"
  , "    const float a1 =  0.254829592f, a2 = -0.284496736f, a3 =  1.421413741f;"
  , "    const float a4 = -1.453152027f, a5 =  1.061405429f, p  =  0.3275911f;"
  , "    float sign = x >= 0.0f ? 1.0f : -1.0f;"
  , "    float ax = fabs(x);"
  , "    float t = 1.0f / (1.0f + p * ax);"
  , "    float y = 1.0f - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1) * t * exp(-(ax * ax));"
  , "    return sign * y;"
  , "}"
  ]
