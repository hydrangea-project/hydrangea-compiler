{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.CodegenCUDA
--
-- CUDA compute backend (single-kernel path) for NVIDIA GPUs.
--
-- This is a stub port of the Metal/MSL backend ("Language.Hydrangea.CodegenMSL").
-- It reuses that module's backend-neutral pieces wholesale:
--
--   * /Kernel analysis/ — 'analyzeOneKernel' \/ 'findKernelProc' classify a proc
--     into pre-loop CPU work, the parallel map\/scatter loop, and post-loop work.
--     None of this mentions Metal.
--   * /Statement \/ expression emitters/ — 'genMSLStmt', 'genMSLAtom',
--     'genMSLIndexExpr' emit plain C (no @device@\/@constant@ address spaces and
--     no @[[buffer(n)]]@ attributes), so they are valid CUDA device code as-is.
--
-- The CUDA-specific surface this module supplies is small and corresponds
-- exactly to a would-be @GpuDialect@ record:
--
--   * Kernel signature: @extern \"C\" __global__ void@ with positional pointer
--     arguments and a @blockIdx.x * blockDim.x + threadIdx.x@ thread index
--     (instead of @kernel void@ + @[[buffer(n)]]@ + @[[thread_position_in_grid]]@).
--   * Atomics: native @atomicAdd@ (instead of @atomic_fetch_add_explicit@ and the
--     float CAS loop Metal needs).
--   * Headers: @<cuda_runtime.h>@ + @__host__ __device__@ shape helpers (CUDA
--     provides @erf@\/@sqrt@\/… as device builtins, so none are redefined).
--   * Host harness: @cudaMalloc@ \/ @cudaMemcpy@ \/ @<<<grid, block>>>@ \/
--     @cudaDeviceSynchronize@ (instead of @MTLDevice@\/@MTLCommandBuffer@).
--
-- Unlike Metal, CUDA supports @double@ natively, so this backend does /not/
-- demote @float@ to 32-bit single precision and does no element-wise conversion
-- when staging host buffers to the device.
--
-- Implemented: the single-kernel path (one parallel map\/map-reduction\/scatter
-- loop per proc). Multi-phase dispatch, temporal @iterate@, and export-kernel
-- mode are not yet ported and produce an explanatory 'Left'.
--
-- NOTE: the host machine has no NVIDIA toolchain, so the emitted @.cu@ sources
-- are not compile-checked here. They are produced by close analogy to the
-- verified MSL output; see "CUDABackend" for the @nvcc@ driver.
module Language.Hydrangea.CodegenCUDA
  ( CUDAArtifacts (..),
    CUDAOptions (..),
    defaultCUDAOptions,
    codegenCUDA,
  )
where

import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List (intercalate, nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Language.Hydrangea.CFG qualified as CFG
import Language.Hydrangea.CFGCore
  ( Atom (..),
    CElemType (..),
    CType (..),
    CVar,
    RHS (..),
    ctypeToElemType,
  )
import Language.Hydrangea.CFGTyping (TypeEnv)
import Language.Hydrangea.CodegenC
  ( VarKind (..),
    cTypeName,
    detectAtomicScatterAddLoop,
    procReturnKinds,
    sanitize,
  )
import Language.Hydrangea.CodegenMSL
  ( KernelAnalysis (..),
    MSLOptions (..),
    analyzeOneKernel,
    collectArrayAliases,
    collectAssignedVars,
    collectTupleDefs,
    findKernelProc,
    genCAtom,
    genHelperC,
    genMSLAtom,
    genMSLIndexExpr,
    genMSLStmt,
    genPreLoopLine,
    mslPairStructName,
    retResolvesToOutput,
    topLevelLoopSpecs,
  )

-- ---------------------------------------------------------------------------
-- Public types

-- | Artifacts produced by the CUDA codegen.  The two-file split mirrors Metal's
-- (@.metal@ kernel + @.m@ harness): a self-contained device @.cu@ and a host
-- @.cu@ that launches it.
data CUDAArtifacts = CUDAArtifacts
  { -- | Contents of the device @kernel.cu@ (the @__global__@ kernel).
    cudaKernelSource :: String,
    -- | Contents of the host @harness.cu@ (CPU helpers + @main@).
    cudaHarnessSource :: String,
    -- | Diagnostics about work that falls back to the CPU.
    cudaWarnings :: [String]
  }

-- | Options controlling which proc is selected as the GPU kernel.
newtype CUDAOptions = CUDAOptions
  { -- | If 'Just name', use that proc as the kernel; otherwise pick the first
    -- proc with a top-level parallel loop (same policy as the Metal backend).
    cudaKernelToEmit :: Maybe CVar
  }

defaultCUDAOptions :: CUDAOptions
defaultCUDAOptions = CUDAOptions {cudaKernelToEmit = Nothing}

-- ---------------------------------------------------------------------------
-- Top-level entry point

-- | Translate a @CFG.Program@ to CUDA artifacts via the single-kernel path.
--
-- This mirrors @codegenMSLSingle@: select the kernel proc, analyze it, refuse
-- if a serial loop (e.g. a reduction) survives after the kernel that the host
-- harness cannot finish, then emit the device kernel and host harness.
codegenCUDA :: CUDAOptions -> CFG.Program -> Either String CUDAArtifacts
codegenCUDA opts prog@(CFG.Program procs) = do
  kernelProc <- findKernelProc mslOpts procs
  ka <- analyzeOneKernel prog kernelProc
  let postLoopHasLoop = not (null (topLevelLoopSpecs (kaPostLoopStmts ka)))
      retPrintable = retResolvesToOutput (kaRetAtom ka) (kaPostLoopStmts ka) (kaOutputArrays ka)
  () <-
    if postLoopHasLoop && not retPrintable
      then
        Left $
          "CUDA backend: kernel `"
            ++ BS.unpack (kaName ka)
            ++ "` leaves a loop (e.g. a reduction) running after it that the host"
            ++ " harness cannot finish on the CPU. Select a different kernel"
            ++ " (--cuda-kernel=<name>) or run on the C backend."
      else Right ()
  let retKinds = procReturnKinds prog
  helperC <- genHelperC procs (kaName ka) retKinds
  let kernelSrc = genCUDAKernelSrc ka
      harnessSrc = genCUDAHarnessSrc ka retKinds helperC
  Right
    CUDAArtifacts
      { cudaKernelSource = kernelSrc,
        cudaHarnessSource = harnessSrc,
        cudaWarnings = cudaFallbackWarnings ka
      }
  where
    mslOpts =
      MSLOptions
        { mslKernelToEmit = cudaKernelToEmit opts,
          mslMultiKernel = False,
          mslExportKernel = Nothing
        }

-- | Host-side CPU-fallback warnings: the GPU runs the kernel loop; any pre-loop
-- producers or post-loop consumers run on the CPU.
cudaFallbackWarnings :: KernelAnalysis -> [String]
cudaFallbackWarnings ka =
  let specs = topLevelLoopSpecs (kaEffectivePreLoop ka) ++ topLevelLoopSpecs (kaPostLoopStmts ka)
   in [ "CUDA: in `"
          ++ BS.unpack (kaName ka)
          ++ "`, "
          ++ show (length specs)
          ++ " loop(s) run on the host CPU, not the GPU."
      | not (null specs)
      ]

-- ---------------------------------------------------------------------------
-- CUDA type mapping
--
-- Unlike Metal, CUDA has native fp64, so @float@ (a double-precision value in
-- the surface language) stays @double@ — matching the host @hyd_array_t@
-- storage, so device staging is a plain @cudaMemcpy@ with no conversion.

cudaTypeName :: CType -> String
cudaTypeName CTInt64 = "long"
cudaTypeName CTDouble = "double"
cudaTypeName CTBool = "int"
cudaTypeName CTUnit = "int"
cudaTypeName CTTuple = "hyd_tuple_t"
cudaTypeName (CTPair t1 t2)
  | Just ce1 <- ctypeToElemType t1,
    Just ce2 <- ctypeToElemType t2 =
      mslPairStructName ce1 ce2
cudaTypeName _ = "long"

cudaElemCType :: CElemType -> String
cudaElemCType CEInt = "long"
cudaElemCType CEFloat = "double"
cudaElemCType CEBool = "int"
cudaElemCType (CEPair ct1 ct2) = mslPairStructName ct1 ct2
cudaElemCType CEArray = "long"

-- | Device buffer element type for an array variable.
cudaArrayElemTyName :: TypeEnv -> Map CVar CType -> CVar -> String
cudaArrayElemTyName typeEnv arrayElemTys v = case Map.lookup v arrayElemTys of
  Just elt -> cudaTypeName elt
  Nothing -> case Map.lookup v typeEnv of
    Just (CTArray elt) -> cudaTypeName elt
    _ -> "long"

cudaScalarTyName :: TypeEnv -> CVar -> String
cudaScalarTyName typeEnv v = case Map.lookup v typeEnv of
  Just ct -> cudaTypeName ct
  Nothing -> "long"

-- | A pair-struct's variable declaration type (mirrors @mslVarDecl@).
cudaVarDecl :: CVar -> TypeEnv -> String
cudaVarDecl v typeEnv = case Map.lookup v typeEnv of
  Just ct -> cudaTypeName ct
  Nothing -> "long"

-- ---------------------------------------------------------------------------
-- Pair struct definitions (double-precision element types)

cudaPairTypesOf :: TypeEnv -> [CFG.Stmt] -> [(CElemType, CElemType)]
cudaPairTypesOf typeEnv stmts =
  let allVars = collectAssignedVars stmts
      raw =
        nub
          [ (ce1, ce2)
          | v <- allVars,
            Just (CTPair t1 t2) <- [Map.lookup v typeEnv],
            Just ce1 <- [ctypeToElemType t1],
            Just ce2 <- [ctypeToElemType t2]
          ]
      transitive = concatMap (\(a, b) -> subPairs a ++ subPairs b ++ [(a, b)]) raw
   in nub transitive
  where
    subPairs (CEPair a b) = subPairs a ++ subPairs b ++ [(a, b)]
    subPairs _ = []

genCUDAPairStructDefs :: [(CElemType, CElemType)] -> String
genCUDAPairStructDefs [] = ""
genCUDAPairStructDefs pairTypes =
  "// Pair struct definitions\n"
    ++ concatMap genDef pairTypes
  where
    genDef (ct1, ct2) =
      "struct "
        ++ mslPairStructName ct1 ct2
        ++ " { "
        ++ cudaElemCType ct1
        ++ " fst; "
        ++ cudaElemCType ct2
        ++ " snd; };\n"

-- ---------------------------------------------------------------------------
-- Device kernel source

-- | Boilerplate header emitted in the device @.cu@: the tuple type and the
-- @__host__ __device__@ shape helpers the body may call.  The kernel file is
-- self-contained and does NOT include @hydrangea_runtime.h@ (whose @static@
-- host @hyd_flat_to_nd@\/@hyd_nd_to_flat@ would otherwise clash with these and
-- are not callable from device code).
cudaSharedHeader :: String -> String
cudaSharedHeader pairDefs =
  unlines
    [ "#include <cuda_runtime.h>",
      "#include <math.h>",
      "",
      "// Tuple / shape type (8-byte elems to match host int64_t).",
      "#define HYD_MAX_DIMS 8",
      "struct hyd_tuple_t {",
      "    long elems[HYD_MAX_DIMS];",
      "    int ndims;",
      "};",
      "",
      "__host__ __device__ static inline long hyd_shape_size_t(hyd_tuple_t shape) {",
      "    long r = 1;",
      "    for (int i = 0; i < shape.ndims; i++) r *= shape.elems[i];",
      "    return r;",
      "}",
      "",
      "__host__ __device__ static inline hyd_tuple_t hyd_flat_to_nd(long flat, hyd_tuple_t shape) {",
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
      "__host__ __device__ static inline long hyd_nd_to_flat(hyd_tuple_t nd, hyd_tuple_t shape) {",
      "    long flat = 0;",
      "    long stride = 1;",
      "    for (int i = shape.ndims - 1; i >= 0; i--) {",
      "        flat += nd.elems[i] * stride;",
      "        stride *= shape.elems[i];",
      "    }",
      "    return flat;",
      "}",
      "",
      pairDefs,
      ""
    ]

genCUDAKernelSrc :: KernelAnalysis -> String
genCUDAKernelSrc ka =
  let pairDefs = genCUDAPairStructDefs (cudaPairTypesOf (kaTypeEnv ka) (kaLoopBody ka))
   in cudaSharedHeader pairDefs ++ genCUDAKernelFunction ka

genCUDAKernelFunction :: KernelAnalysis -> String
genCUDAKernelFunction ka =
  "extern \"C\" __global__ void "
    ++ sanitize kernelName
    ++ "(\n"
    ++ intercalate ",\n" (map ("    " ++) allParams)
    ++ ")\n{\n"
    ++ gidInit
    ++ iterDecompLines
    ++ genCUDABody
      gidVar
      (CFG.lsExec loopSpec)
      hoistedVarSet
      outputArrSet
      typeEnv
      arrayElemTys
      (kaHoistedCallMap ka)
      (kaPreLoopAliases ka)
      loopBody
    ++ "}\n"
  where
    kernelName = kaName ka
    loopSpec = kaLoopSpec ka
    loopBody = kaLoopBody ka
    typeEnv = kaTypeEnv ka
    arrayElemTys = kaArrayElemTys ka
    inputArrays = kaInputArrays ka
    outputArrays = kaOutputArrays ka
    scalarInputs = kaScalarInputs ka

    hoistedVarSet = Set.fromList inputArrays `Set.union` Set.fromList scalarInputs
    outputArrSet = Set.fromList outputArrays
    iters = CFG.lsIters loopSpec
    bounds = CFG.lsBounds loopSpec
    isMultiDim = length iters > 1
    gidVar = case iters of
      [] -> error "internal error: CUDA kernel loop spec has no iterators"
      (i : _) -> if isMultiDim then "_flat_gid" else i

    -- Round-up grid means some threads run past the logical bound; guard them.
    gidInit =
      "    long "
        ++ sanitize gidVar
        ++ " = (long)blockIdx.x * blockDim.x + threadIdx.x;\n"
        ++ "    if ("
        ++ sanitize gidVar
        ++ " >= _hyd_n) return;\n"

    iterDecompLines
      | not isMultiDim = ""
      | otherwise =
          "    long _rem = _flat_gid;\n"
            ++ concatMap
              ( \(it, bd) ->
                  "    long "
                    ++ sanitize it
                    ++ " = _rem % (long)("
                    ++ genMSLIndexExpr bd
                    ++ ");\n"
                    ++ "    _rem /= (long)("
                    ++ genMSLIndexExpr bd
                    ++ ");\n"
              )
              (reverse (zip iters bounds))

    -- Positional kernel arguments: input data ptrs, input shapes (by value),
    -- output data ptrs, scalars (by value), then the grid bound for the guard.
    allParams = inputParams ++ inputShapeParams ++ outputParams ++ scalarParams ++ ["long _hyd_n"]

    inputParams =
      [ "const " ++ cudaArrayElemTyName typeEnv arrayElemTys v ++ "* " ++ sanitize v ++ "_data"
      | v <- inputArrays
      ]
    inputShapeParams =
      ["hyd_tuple_t " ++ sanitize v ++ "_shape" | v <- inputArrays]
    outputParams =
      [ cudaArrayElemTyName typeEnv arrayElemTys v ++ "* " ++ sanitize v ++ "_data"
      | v <- outputArrays
      ]
    scalarParams =
      [cudaScalarTyName typeEnv v ++ " " ++ sanitize v | v <- scalarInputs]

-- | Emit the kernel body.  Mirrors @genMSLBody@: pre-declare locals, emit alias
-- @#define@s, then the statements — except scatter-add uses CUDA atomics.
-- Ordinary statements are emitted by the shared (backend-neutral) 'genMSLStmt'.
genCUDABody ::
  CVar ->
  CFG.ExecPolicy ->
  Set CVar ->
  Set CVar ->
  TypeEnv ->
  Map CVar CType ->
  Map CVar CVar ->
  [(CVar, CVar)] ->
  [CFG.Stmt] ->
  String
genCUDABody iter execPolicy inArrs outArrs typeEnv arrayElemTys hoistedCallMap preLoopAliases stmts =
  declLines ++ aliasDefLines ++ bodyLines
  where
    actualArrayVars = Set.filter isArrayInEnv (inArrs `Set.union` outArrs)
    isArrayInEnv v = case Map.lookup v typeEnv of
      Just (CTArray _) -> True
      _ -> case Map.lookup v arrayElemTys of
        Just _ -> True
        _ -> False
    bodyAliases = collectArrayAliases actualArrayVars stmts
    arrayAliases = nub (preLoopAliases ++ bodyAliases)
    allInArrs = inArrs `Set.union` Set.fromList (map fst arrayAliases)
    aliasMap0 = Map.fromList arrayAliases
    resolveAlias v = case Map.lookup v aliasMap0 of
      Just parent -> resolveAlias parent
      Nothing -> v
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
    declLines =
      concatMap
        ( \v ->
            if v `Set.member` allInArrs
              then ""
              else "    " ++ cudaVarDecl v typeEnv ++ " " ++ sanitize v ++ ";\n"
        )
        allVars
    tupDefs = collectTupleDefs stmts
    emit depth = genMSLStmt depth iter allInArrs outArrs typeEnv arrayElemTys hoistedCallMap tupDefs
    bodyLines = case scatterStrategy of
      Just _ ->
        case detectAtomicScatterAddLoop stmts of
          Just (outerPrefix, branchPrefix, mGuard, arrAtom, idxAtom, valAtom) ->
            let outerStr = concatMap (emit 1) outerPrefix
                branchStr depth = concatMap (emit depth) branchPrefix
                atomicLine depth = genCUDAAtomicAdd depth iter typeEnv arrAtom idxAtom valAtom
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
          Nothing -> concatMap (emit 1) stmts
      Nothing -> concatMap (emit 1) stmts

    -- On the GPU every scatter-add is atomic; the CPU parallelizer's privatized
    -- strategies map to atomics here too (same reasoning as the MSL backend).
    scatterStrategy = case execPolicy of
      CFG.Parallel p -> case CFG.psStrategy p of
        CFG.ParallelScatterAtomicAddInt -> Just CTInt64
        CFG.ParallelScatterAtomicAddFloat -> Just CTDouble
        CFG.ParallelScatterPrivatizedIntAdd -> Just CTInt64
        CFG.ParallelScatterPrivatizedFloatAdd -> Just CTDouble
        _ -> Nothing
      _ -> Nothing

-- | CUDA scatter-add. Native @atomicAdd@ — no CAS loop (unlike Metal's float
-- path) and full 64-bit integers (Metal truncates to 32-bit). @double@ atomics
-- require compute capability >= 6.0.
genCUDAAtomicAdd :: Int -> CVar -> TypeEnv -> Atom -> Atom -> Atom -> String
genCUDAAtomicAdd depth iter typeEnv arrAtom idxAtom valAtom =
  let ind = replicate (depth * 4) ' '
      arrName = case arrAtom of
        AVar v -> sanitize v ++ "_data"
        _ -> genMSLAtom iter arrAtom
      idx = genMSLAtom iter idxAtom
      val = genMSLAtom iter valAtom
      isFloat = case arrAtom of
        AVar v -> case Map.lookup v typeEnv of
          Just (CTArray CTDouble) -> True
          _ -> False
        _ -> False
   in if isFloat
        then ind ++ "atomicAdd((double*)&" ++ arrName ++ "[" ++ idx ++ "], (double)(" ++ val ++ "));\n"
        else ind ++ "atomicAdd((unsigned long long*)&" ++ arrName ++ "[" ++ idx ++ "], (unsigned long long)(" ++ val ++ "));\n"

-- ---------------------------------------------------------------------------
-- Host harness source

-- | Generate the host @harness.cu@: CPU helper procs, then a @main@ that runs
-- the pre-loop CPU work, stages inputs to the device, launches the kernel, and
-- reads back / prints the result.
genCUDAHarnessSrc :: KernelAnalysis -> Map CVar VarKind -> String -> String
genCUDAHarnessSrc ka retKinds helperC =
  unlines $
    [ "// Hydrangea CUDA harness — generated by the CUDA backend.",
      "#include \"hydrangea_runtime.h\"",
      "#include <stdio.h>",
      "#include <string.h>",
      "#include <stdlib.h>",
      "#include <cuda_runtime.h>",
      "",
      "// ---- CPU helper procs ----",
      helperC,
      "",
      "#define HYD_CUDA_CHECK(call) do { \\",
      "    cudaError_t _e = (call); \\",
      "    if (_e != cudaSuccess) { \\",
      "        fprintf(stderr, \"hydrangea: CUDA error %s at %s:%d\\n\", \\",
      "                cudaGetErrorString(_e), __FILE__, __LINE__); \\",
      "        return 1; \\",
      "    } \\",
      "} while (0)",
      "",
      "// ---- Kernel prototype (defined in kernel.cu) ----",
      "extern \"C\" __global__ void " ++ sanitize kernelName ++ "(" ++ intercalate ", " protoParams ++ ");",
      "",
      "int main(int argc, char* argv[]) {",
      "    (void)argc; (void)argv;",
      "    // --- Proc parameters (defaults) ---"
    ]
      ++ procParamLines
      ++ ["    // --- CPU pre-loop setup ---"]
      ++ preLoopLines
      ++ [ "",
           "    // Grid size from loop bound(s)",
           "    long _n = " ++ gridSizeExpr ++ ";",
           "",
           "    // --- Stage input arrays to the device ---"
         ]
      ++ inputBufLines
      ++ ["", "    // --- Allocate output buffers on the device ---"]
      ++ outputBufLines
      ++ [ "",
           "    // --- Launch ---",
           "    int _block = 256;",
           "    long _grid = (_n + _block - 1) / _block;",
           "    if (_grid < 1) _grid = 1;",
           "    " ++ sanitize kernelName ++ "<<<(unsigned int)_grid, (unsigned int)_block>>>(" ++ intercalate ", " launchArgs ++ ");",
           "    HYD_CUDA_CHECK(cudaGetLastError());",
           "    HYD_CUDA_CHECK(cudaDeviceSynchronize());",
           ""
         ]
      ++ resultLines
      ++ ["    return 0;", "}"]
  where
    kernelName = kaName ka
    typeEnv = kaTypeEnv ka
    arrayElemTys = kaArrayElemTys ka
    preLoopStmts = kaEffectivePreLoop ka
    postLoopStmts = kaPostLoopStmts ka
    loopSpec = kaLoopSpec ka
    retAtom = kaRetAtom ka
    inputArrays = kaInputArrays ka
    outputArrays = kaOutputArrays ka
    scalarInputs = kaScalarInputs ka
    procParams = kaProcParams ka
    nInputArrays = length inputArrays

    elemTy v = cudaArrayElemTyName typeEnv arrayElemTys v
    scalarTy v = cudaScalarTyName typeEnv v

    -- Kernel prototype / launch argument lists (must match genCUDAKernelFunction).
    protoParams =
      ["const " ++ elemTy v ++ "*" | v <- inputArrays]
        ++ ["hyd_tuple_t" | _ <- inputArrays]
        ++ [elemTy v ++ "*" | v <- outputArrays]
        ++ [scalarTy v | v <- scalarInputs]
        ++ ["long"]
    launchArgs =
      ["_din" ++ show i | i <- [0 .. nInputArrays - 1]]
        ++ [sanitize v ++ "->shape" | v <- inputArrays]
        ++ ["_dout" ++ show i | i <- [0 .. length outputArrays - 1]]
        ++ [sanitize v | v <- scalarInputs]
        ++ ["_n"]

    -- Proc parameter declarations with default values.
    procParamLines = concatMap declProcParam procParams
    declProcParam v =
      let ty = Map.findWithDefault CTInt64 v typeEnv
          defVal = case ty of
            CTDouble -> "0.0"
            CTBool -> "0"
            CTArray _ -> "NULL"
            CTPair _ _ -> "((" ++ cTypeName ty ++ "){0})"
            _ -> "0LL"
       in ["    " ++ cTypeName ty ++ " " ++ sanitize v ++ " = " ++ defVal ++ ";"]

    gridSizeExpr = case CFG.lsBounds loopSpec of
      [b] -> "(long)" ++ genMSLIndexExpr b
      bs -> intercalate " * " ["(long)" ++ genMSLIndexExpr b | b <- bs]

    preLoopLines = concatMap (genPreLoopLine typeEnv retKinds (Set.fromList outputArrays) Set.empty) preLoopStmts

    -- Input arrays: device alloc + plain HtoD copy (no float conversion needed).
    inputBufLines =
      concat
        [ [ "    // Input " ++ show i ++ ": " ++ sanitize v,
            "    long _inN" ++ show i ++ " = hyd_shape_size(" ++ sanitize v ++ "->shape);",
            "    size_t _din" ++ show i ++ "_sz = (size_t)(_inN" ++ show i ++ " * sizeof(" ++ et ++ "));",
            "    " ++ et ++ "* _din" ++ show i ++ " = NULL;",
            "    HYD_CUDA_CHECK(cudaMalloc((void**)&_din" ++ show i ++ ", _din" ++ show i ++ "_sz));",
            "    HYD_CUDA_CHECK(cudaMemcpy(_din" ++ show i ++ ", " ++ sanitize v ++ "->data, _din" ++ show i ++ "_sz, cudaMemcpyHostToDevice));"
          ]
        | (v, i) <- zip inputArrays [0 :: Int ..],
          let et = elemTy v
        ]

    -- Output allocation shapes harvested from pre-loop RArrayAlloc.
    outputAllocShapes =
      Map.fromList [(v, shpAtom) | CFG.SAssign v (RArrayAlloc shpAtom) <- preLoopStmts]
    outSizeExpr v = case Map.lookup v outputAllocShapes of
      Just shpAtom -> "hyd_shape_size(" ++ genCAtom shpAtom ++ ")"
      Nothing -> "_n"

    outputBufLines =
      concat
        [ [ "    // Output " ++ show i ++ ": " ++ sanitize v,
            "    long _outN" ++ show i ++ " = " ++ outSizeExpr v ++ ";",
            "    size_t _dout" ++ show i ++ "_sz = (size_t)(_outN" ++ show i ++ " * sizeof(" ++ et ++ "));",
            "    " ++ et ++ "* _dout" ++ show i ++ " = NULL;",
            "    HYD_CUDA_CHECK(cudaMalloc((void**)&_dout" ++ show i ++ ", _dout" ++ show i ++ "_sz));",
            "    HYD_CUDA_CHECK(cudaMemset(_dout" ++ show i ++ ", 0, _dout" ++ show i ++ "_sz));"
          ]
        | (v, i) <- zip outputArrays [0 :: Int ..],
          let et = elemTy v
        ]

    retIsKernelOutput = retResolvesToOutput retAtom postLoopStmts outputArrays

    -- When the kernel output IS the result, copy it back and print directly.
    -- Otherwise read it into a host array, finish the post-loop CPU work, and
    -- print the proc's actual return value.
    resultLines
      | retIsKernelOutput =
          ["    // --- Copy kernel output back and print ---"] ++ printOutputLines
      | otherwise =
          ["    // --- Read GPU output back, finish on CPU, print result ---"]
            ++ readbackLines
            ++ concatMap genRunPostLoopLine postLoopStmts
            ++ printReturnLines

    printOutputLines =
      concat
        [ let et = elemTy v
              fmt = if et == "double" then "%.17g" else "%ld"
              cast = if et == "double" then "(double)" else "(long)"
           in [ "    // Print output: " ++ sanitize v,
                "    " ++ et ++ "* _hout" ++ show i ++ " = (" ++ et ++ "*)malloc(_dout" ++ show i ++ "_sz);",
                "    HYD_CUDA_CHECK(cudaMemcpy(_hout" ++ show i ++ ", _dout" ++ show i ++ ", _dout" ++ show i ++ "_sz, cudaMemcpyDeviceToHost));",
                "    printf(\"[\");",
                "    for (long _pi = 0; _pi < _outN" ++ show i ++ "; _pi++) {",
                "        if (_pi > 0) printf(\", \");",
                "        printf(\"" ++ fmt ++ "\", " ++ cast ++ "_hout" ++ show i ++ "[_pi]);",
                "    }",
                "    printf(\"]\\n\");"
              ]
        | (v, i) <- zip outputArrays [0 :: Int ..]
        ]

    readbackLines =
      concat
        [ let et = elemTy v
              sizeVar = "_outN" ++ show i
              shapeExpr = case Map.lookup v outputAllocShapes of
                Just shpAtom -> genCAtom shpAtom
                Nothing -> "hyd_tuple_make(1, (int64_t)" ++ sizeVar ++ ")"
           in [ "    // Read back GPU output: " ++ sanitize v,
                "    hyd_array_t* " ++ sanitize v ++ " = hyd_array_alloc(" ++ shapeExpr ++ ");",
                "    HYD_CUDA_CHECK(cudaMemcpy(" ++ sanitize v ++ "->data, _dout" ++ show i ++ ", (size_t)(" ++ sizeVar ++ " * sizeof(" ++ et ++ ")), cudaMemcpyDeviceToHost));"
              ]
        | (v, i) <- zip outputArrays [0 :: Int ..]
        ]

    -- reshape_view / pair construction are realized by the return printing.
    genRunPostLoopLine (CFG.SAssign _ (RCall "hyd_array_reshape_view" _)) = []
    genRunPostLoopLine (CFG.SAssign _ (RPairMake {})) = []
    genRunPostLoopLine stmt = genPreLoopLine typeEnv retKinds Set.empty Set.empty stmt

    printReturnLines = case retAtom of
      Just AUnit -> []
      Just (AVar v) -> case Map.lookup v typeEnv of
        Just (CTArray elt) -> genResultArrayPrint v (cudaTypeName elt)
        Just CTDouble -> ["    printf(\"%.17g\\n\", (double)" ++ sanitize v ++ ");"]
        Just _ -> ["    printf(\"%ld\\n\", (long)" ++ sanitize v ++ ");"]
        Nothing -> ["    printf(\"%ld\\n\", (long)" ++ sanitize v ++ ");"]
      Just (AInt n) -> ["    printf(\"%ld\\n\", (long)" ++ show n ++ "LL);"]
      Just (AFloat f) -> ["    printf(\"%.17g\\n\", (double)" ++ show f ++ ");"]
      _ -> printOutputLines

    genResultArrayPrint v eTy =
      let fmt = if eTy == "double" then "%.17g" else "%ld"
          cast = if eTy == "double" then "(double)" else "(long)"
          nm = sanitize v
       in [ "    { " ++ eTy ++ "* _rp = (" ++ eTy ++ "*)" ++ nm ++ "->data;",
            "      long _rn = hyd_shape_size(" ++ nm ++ "->shape);",
            "      printf(\"[\");",
            "      for (long _pi = 0; _pi < _rn; _pi++) { if (_pi > 0) printf(\", \"); printf(\"" ++ fmt ++ "\", " ++ cast ++ "_rp[_pi]); }",
            "      printf(\"]\\n\"); }"
          ]
