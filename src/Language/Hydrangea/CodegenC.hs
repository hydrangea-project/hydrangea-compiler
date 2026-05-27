{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.CodegenC
--
-- Native C code generation backend for CFG.
module Language.Hydrangea.CodegenC
  ( codegenProgram,
    codegenProgramPrune,
    codegenProgramWithOptions,
    codegenProgramWithOptionsPrune,
    CodegenArtifacts (..),
    CodegenOptions (..),
    BenchmarkConfig (..),
    defaultCodegenOptions,
    VarKind (..),
    ctypeToVarKind,

    -- * Shared utilities (used by alternative backends)
    sanitize,
    sanitizeFieldName,
    celemTypeLetter,
    celemTypeCType,
    pairStructName,
    recordStructName,
    cTypeName,
    splitFinalReturn,
    allAssignedVars,
    assignedRHSMap,
    typeEnvToVarSets,
    classifyVarKinds,
    arrayVarsProc,
    tupleVarsProc,
    pairVarsProc,
    inferArrayElemTypesFromStmts,
    detectAtomicScatterAddLoop,
    procReturnKinds,
    isFloatArithBinOp,
    isMathFloatOp,
    genAtom,
    genIndexExpr,
    genPairStructDefs,
    genRecordStructDefs,
    ExportSpec (..),
    resolveExportSpec,
    procReturnTypeName,
    sanitizeExportName,
  )
where

import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Char (isAlphaNum, toUpper)
import Data.List (nub, nubBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Language.Hydrangea.CFG qualified as CFG
import Language.Hydrangea.CFGAnalysis (usedVarsAtom, usedVarsStmts)
import Language.Hydrangea.CFGCore (Atom (..), BinOp (..), CElemType (..), CType (..), CVar, RHS (..), Redop (..), UnOp (..), ctypeToElemType)
import Language.Hydrangea.CFGTyping (CallParamTypes, TypeEnv, buildCallParamTypes, inferProgramReturnTypes, recoverProcTypeEnv)
import Language.Hydrangea.Vectorize (defaultVectorWidth)
import Text.PrettyPrint.HughesPJClass
import Prelude hiding ((<>))

data CodegenArtifacts = CodegenArtifacts
  { codegenSource :: String,
    codegenHeader :: Maybe String
  }
  deriving (Eq, Show)

data CodegenOptions = CodegenOptions
  { codegenEmitMain :: Bool,
    codegenExportKernel :: Maybe CVar,
    codegenBenchmark :: Maybe BenchmarkConfig,
    -- | SIMD vector lane width (2 or 4; default 4)
    codegenSimdWidth :: Int,
    -- | Use float instead of double for scalar types
    codegenUseFloat :: Bool
  }
  deriving (Eq, Show)

-- | Configuration for benchmark scaffolding mode.
data BenchmarkConfig = BenchmarkConfig
  { -- | Name of the zero-argument proc to benchmark.
    bcKernelName :: CVar,
    -- | Number of warmup iterations (not timed).
    bcWarmupIters :: Int,
    -- | Number of timed measurement iterations.
    bcMeasureIters :: Int
  }
  deriving (Eq, Show)

defaultCodegenOptions :: CodegenOptions
defaultCodegenOptions =
  CodegenOptions
    { codegenEmitMain = True,
      codegenExportKernel = Nothing,
      codegenBenchmark = Nothing,
      codegenSimdWidth = defaultVectorWidth,
      codegenUseFloat = False
    }

data ExportSpec = ExportSpec
  { exportKernelName :: CVar,
    exportProcName :: String,
    exportWrapperName :: String,
    exportReturnType :: String,
    -- | [(sanitized_c_name, c_type_string)]
    exportParams :: [(String, String)]
  }

-- | Codegen-oriented classification of CFG variables.
data VarKind = KScalar | KFloat | KArray | KFloatArray | KTuple | KPair CElemType CElemType | KRecord [(BS.ByteString, CType)]
  deriving (Eq, Ord, Show)

-- | Classify a 'CType' into a 'VarKind' for codegen.
ctypeToVarKind :: CType -> Maybe VarKind
ctypeToVarKind CTInt64 = Just KScalar
ctypeToVarKind CTDouble = Just KFloat
ctypeToVarKind CTBool = Just KScalar
ctypeToVarKind CTUnit = Just KScalar
ctypeToVarKind CTTuple = Just KTuple
ctypeToVarKind (CTArray CTDouble) = Just KFloatArray
ctypeToVarKind (CTArray _) = Just KArray
ctypeToVarKind (CTPair t1 t2) = do
  et1 <- ctypeToElemType t1
  et2 <- ctypeToElemType t2
  return (KPair et1 et2)
ctypeToVarKind (CTRecord fields) = Just (KRecord fields)
ctypeToVarKind CTUnknown = Nothing

-- | Per-procedure environment threaded through statement code generation.
-- Bundles the classification maps and sets that are invariant within a single
-- procedure and would otherwise be passed as individual parameters to every
-- recursive helper.
data CodegenEnv = CodegenEnv
  { ceOpts :: CodegenOptions,
    ceRetKinds :: Map CVar VarKind,
    ceRetTypes :: Map CVar CType,
    ceArrayElemTypes :: Map CVar CType,
    ceArrVars :: Set CVar,
    ceTupVars :: Set CVar,
    cePairVars :: Map CVar (CElemType, CElemType),
    ceRecordVars :: Map CVar [(BS.ByteString, CType)],
    ceFloatVars :: Set CVar,
    ceFloatArrVars :: Set CVar,
    ceVecVars :: Map CVar Int,
    ceTupleDefs :: Map CVar [Atom],
    ceInsideParallelRegion :: Bool
  }

-- | Derive the classification sets/maps used by genStmts from a type environment.
-- Returns (arrVars, tupVars, pairVars, recordVars, floatVars, floatArrVars).
typeEnvToVarSets ::
  Map CVar CType ->
  (Set CVar, Set CVar, Map CVar (CElemType, CElemType), Map CVar [(BS.ByteString, CType)], Set CVar, Set CVar)
typeEnvToVarSets env = Map.foldlWithKey' go (S.empty, S.empty, Map.empty, Map.empty, S.empty, S.empty) env
  where
    go acc@(av, tv, pv, rv, fv, fav) var ct = case ct of
      CTDouble -> (av, tv, pv, rv, S.insert var fv, fav)
      CTTuple -> (av, S.insert var tv, pv, rv, fv, fav)
      CTArray CTDouble -> (S.insert var av, tv, pv, rv, fv, S.insert var fav)
      CTArray _ -> (S.insert var av, tv, pv, rv, fv, fav)
      CTPair t1 t2
        | Just et1 <- ctypeToElemType t1,
          Just et2 <- ctypeToElemType t2 ->
            (av, tv, Map.insert var (et1, et2) pv, rv, fv, fav)
      CTRecord fields -> (av, tv, pv, Map.insert var fields rv, fv, fav)
      _ -> acc

-- | Render a CFG program as C source code.
codegenProgram :: CFG.Program -> String
codegenProgram prog =
  codegenSource $
    case codegenProgramWithOptions defaultCodegenOptions prog of
      Right artifacts -> artifacts
      Left err -> error err

-- | Codegen with optional dead-proc pruning. When prune=True, remove
-- top-level procs that are not reachable from zero-arg entry procs via
-- direct RCall edges. If the program uses indirect calls via "__apply",
-- pruning is skipped conservatively.
codegenProgramPrune :: Bool -> CFG.Program -> String
codegenProgramPrune prune prog =
  codegenSource $
    case codegenProgramWithOptionsPrune defaultCodegenOptions prune prog of
      Right artifacts -> artifacts
      Left err -> error err

codegenProgramWithOptions :: CodegenOptions -> CFG.Program -> Either String CodegenArtifacts
codegenProgramWithOptions opts = codegenProgramWithOptionsPrune opts False

codegenProgramWithOptionsPrune :: CodegenOptions -> Bool -> CFG.Program -> Either String CodegenArtifacts
codegenProgramWithOptionsPrune opts prune prog = do
  let prepared = pruneProgram opts prune prog
  genProgram opts prepared

pruneProgram :: CodegenOptions -> Bool -> CFG.Program -> CFG.Program
pruneProgram _ False prog = prog
pruneProgram opts True prog@(CFG.Program procs) =
  if usesApply then prog else CFG.Program keptProcs
  where
    -- detect any use of the dynamic apply operator; if present, skip pruning
    usesApply = any procUsesApply procs
    procNames = S.fromList [CFG.procName p | p <- procs]

    procUsesApply (CFG.Proc {CFG.procBody = body}) = any stmtUsesApply body
    stmtUsesApply st = case st of
      CFG.SAssign _ rhs -> rhsUsesApply rhs
      CFG.SLoop _ body' -> any stmtUsesApply body'
      CFG.SIf _ thn els -> any stmtUsesApply thn || any stmtUsesApply els
      _ -> False
    rhsUsesApply (RCall fn _) = fn == "__apply"
    rhsUsesApply _ = False

    -- build direct call graph (ignore __apply as handled above)
    callEdges = Map.fromList [(CFG.procName p, calledByProc p) | p <- procs]

    calledByProc (CFG.Proc {CFG.procBody = body}) = foldMap stmtCalledNames body

    stmtCalledNames st = case st of
      CFG.SAssign _ rhs -> rhsCalledNames rhs
      CFG.SLoop _ body' -> foldMap stmtCalledNames body'
      CFG.SIf _ thn els -> foldMap stmtCalledNames thn `S.union` foldMap stmtCalledNames els
      _ -> S.empty

    rhsCalledNames (RCall fn _) = if fn == "__apply" then S.empty else S.singleton fn
    rhsCalledNames _ = S.empty

    -- roots: explicit entrypoints first (main/export/benchmark); fallback to
    -- all zero-arg procs when no explicit root exists in the program.
    explicitRootCandidates =
      S.fromList $
        mapMaybe
          id
          ( [if codegenEmitMain opts then Just "main" else Nothing]
              ++ [codegenExportKernel opts]
              ++ [bcKernelName <$> codegenBenchmark opts]
          )
    explicitRoots = explicitRootCandidates `S.intersection` procNames
    rootNames
      | S.null explicitRoots = S.fromList [CFG.procName p | p <- procs, null (CFG.procParams p)]
      | otherwise = explicitRoots

    -- traverse call graph to find reachable procs
    reachable = closure callEdges rootNames

    keptProcs = [p | p <- procs, CFG.procName p `S.member` reachable]

    closure edges roots = go roots S.empty
      where
        go frontier visited
          | S.null frontier = visited
          | otherwise =
              let (x, rest) = S.deleteFindMin frontier
                  visited' = S.insert x visited
                  succs = Map.findWithDefault S.empty x edges
                  new = succs `S.difference` visited'
                in go (rest `S.union` new) visited'

procCallEdges :: [CFG.Proc] -> Map CVar (Set CVar)
procCallEdges procs = Map.fromList [(CFG.procName p, calledByProc p) | p <- procs]
  where
    procNames = S.fromList (map CFG.procName procs)
    calledByProc (CFG.Proc {CFG.procBody = body}) = foldMap (stmtCalledNames procNames) body

stmtCalledNames :: Set CVar -> CFG.Stmt -> Set CVar
stmtCalledNames procNames st = case st of
  CFG.SAssign _ rhs -> rhsCalledNames procNames rhs
  CFG.SLoop _ body' -> foldMap (stmtCalledNames procNames) body'
  CFG.SIf _ thn els -> foldMap (stmtCalledNames procNames) thn `S.union` foldMap (stmtCalledNames procNames) els
  _ -> S.empty

rhsCalledNames :: Set CVar -> RHS -> Set CVar
rhsCalledNames procNames rhs = case rhs of
  RCall fn _ | fn `S.member` procNames && fn /= "__apply" -> S.singleton fn
  _ -> S.empty

procUsesIO :: [CFG.Stmt] -> Bool
procUsesIO = any stmtUsesIO
  where
    stmtUsesIO st = case st of
      CFG.SAssign _ rhs -> rhsUsesIO rhs
      CFG.SLoop _ body' -> procUsesIO body'
      CFG.SIf _ thn els -> procUsesIO thn || procUsesIO els
      _ -> False
    rhsUsesIO (RCall fn _) =
      fn == "hyd_read_array_csv"
        || fn == "hyd_read_float_array_csv"
        || fn == "hyd_write_array_csv"
        || fn == "hyd_write_array_csv_float"
    rhsUsesIO _ = False

stripBenchmarkIOStmts :: [CFG.Stmt] -> [CFG.Stmt]
stripBenchmarkIOStmts = mapMaybe stripStmt
  where
    stripStmt st = case st of
      CFG.SAssign _ rhs | rhsIsBenchmarkIO rhs -> Nothing
      CFG.SLoop spec body -> Just (CFG.SLoop spec (stripBenchmarkIOStmts body))
      CFG.SIf cond thn els -> Just (CFG.SIf cond (stripBenchmarkIOStmts thn) (stripBenchmarkIOStmts els))
      _ -> Just st

    rhsIsBenchmarkIO (RCall fn _) =
      fn == "hyd_write_array_csv" || fn == "hyd_write_array_csv_float"
    rhsIsBenchmarkIO _ = False

reachableClosure :: Map CVar (Set CVar) -> Set CVar -> Set CVar
reachableClosure edges roots = go roots S.empty
  where
    go frontier visited
      | S.null frontier = visited
      | otherwise =
          let (x, rest) = S.deleteFindMin frontier
              visited' = S.insert x visited
              succs = Map.findWithDefault S.empty x edges
              new = succs `S.difference` visited'
           in go (rest `S.union` new) visited'

genProgram :: CodegenOptions -> CFG.Program -> Either String CodegenArtifacts
genProgram opts prog@(CFG.Program procs) = do
  let retKinds = procReturnKinds prog
      retTypes = inferProgramReturnTypes prog
      callParamTypes = buildCallParamTypes retTypes procs
      callEdges = procCallEdges procs
      missingArrayElemVars = collectMissingArrayElemVars retTypes callParamTypes procs
      needsOmp = any (\(CFG.Proc {CFG.procBody = body}) -> hasParallelStmt body) procs
      needsMath = any (\(CFG.Proc {CFG.procBody = body}) -> hasMathOp body) procs
      ompInclude = if needsOmp then text "#include <omp.h>" else empty
      mathInclude = if needsMath then text "#include <math.h>" else empty
      mBench = codegenBenchmark opts
      timeInclude = case mBench of
        Nothing -> empty
        Just _ -> text "#include <time.h>"
      benchReachable = fmap (\bc -> reachableClosure callEdges (S.singleton (bcKernelName bc))) mBench
      recoveredEnvs = map (recoverProcTypeEnv retTypes callParamTypes) procs
      pairStructs = genPairStructDefs opts prog recoveredEnvs
      recordStructs = genRecordStructDefs prog
      structuredArrayPrints = genStructuredArrayPrintDefs retTypes
      missingArrayElemDoc
        | null missingArrayElemVars = empty
        | otherwise =
            text "#error \"Hydrangea C backend missing monomorphic array element type information for:"
              <+> hsep (punctuate comma (map text missingArrayElemVars))
              <> text "\""
              $$ text ""
      -- File-scope cache variable for the benchmark kernel (array-returning only).
      benchCacheGlobal = case mBench of
        Nothing -> empty
        Just bc ->
          let kName = bcKernelName bc
              kCName = sanitize kName
              isArrayRet = case Map.lookup kName retTypes of
                Just (CTArray _) -> True
                _ -> case Map.findWithDefault KScalar kName retKinds of
                  KArray -> True
                  KFloatArray -> True
                  _ -> False
           in if isArrayRet
                then text ("static hyd_array_t* __bench_cache_" ++ kCName ++ " = NULL;") $$ text ""
                else empty
  exportSpec <- traverse (resolveExportSpec retKinds retTypes callParamTypes procs) (codegenExportKernel opts)
  -- Validate benchmark kernel existence when requested.
  () <- case mBench of
    Just bc ->
      let benchName = bcKernelName bc
          zeroArgProcNames = [CFG.procName p | p <- procs, null (CFG.procParams p)]
       in if benchName `notElem` zeroArgProcNames
            then
              Left
                ( "Unknown benchmark kernel: '"
                    ++ BS.unpack benchName
                    ++ "'. "
                    ++ "Available zero-argument procs: "
                    ++ show (map BS.unpack zeroArgProcNames)
                )
            else Right ()
    Nothing -> Right ()
  let sourceDoc =
        missingArrayElemDoc
          $$ text "#include \"hydrangea_runtime.h\""
          $$ ompInclude
          $$ mathInclude
          $$ timeInclude
          $$ pairStructs
          $$ recordStructs
          $$ structuredArrayPrints
          $$ text ""
          $$ benchCacheGlobal
          $$ vcat (punctuate (text "") (map (genProc opts benchReachable retKinds retTypes callParamTypes) procs))
          $$ maybe empty (\spec -> text "" $$ genExportWrapper spec) exportSpec
          $$ if codegenEmitMain opts
            then text "" $$ genMain mBench retKinds retTypes procs
            else empty
      headerDoc = fmap (genHeader pairStructs recordStructs) exportSpec
  pure
    CodegenArtifacts
      { codegenSource = render sourceDoc,
        codegenHeader = render <$> headerDoc
      }

resolveExportSpec :: Map CVar VarKind -> Map CVar CType -> CallParamTypes -> [CFG.Proc] -> CVar -> Either String ExportSpec
resolveExportSpec retKinds retTypes callParamTypes procs kernelName =
  case filter (\proc -> CFG.procName proc == kernelName) procs of
    [] ->
      Left $ "Unknown exported kernel: " ++ BS.unpack kernelName
    proc : _ ->
      let typeEnv = recoverProcTypeEnv retTypes callParamTypes proc
          (arrVarsTE, tupVarsTE, pairVarsTE, recordVarsTE, floatVarsTE, _) = typeEnvToVarSets typeEnv
          resolveParamType p
            | p `S.member` floatVarsTE = "double"
            | p `S.member` arrVarsTE = "hyd_array_t*"
            | p `S.member` tupVarsTE = "hyd_tuple_t"
            | Just (ct1, ct2) <- Map.lookup p pairVarsTE = pairStructName ct1 ct2
            | Just fields <- Map.lookup p recordVarsTE = recordStructName fields
            | otherwise = "int64_t"
          params = [(sanitize p, resolveParamType p) | p <- CFG.procParams proc]
       in pure
            ExportSpec
              { exportKernelName = kernelName,
                exportProcName = sanitize (CFG.procName proc),
                exportWrapperName = "hyd_export_" ++ sanitizeExportName kernelName,
                exportReturnType = procReturnTypeName retKinds retTypes (CFG.procName proc),
                exportParams = params
              }

procReturnTypeName :: Map CVar VarKind -> Map CVar CType -> CVar -> String
procReturnTypeName retKinds retTypes name =
  case Map.lookup name retTypes of
    Just cty -> cTypeName cty
    Nothing ->
      case Map.findWithDefault KScalar name retKinds of
        KFloatArray -> "hyd_array_t*"
        KArray -> "hyd_array_t*"
        KTuple -> "hyd_tuple_t"
        KPair ct1 ct2 -> pairStructName ct1 ct2
        KRecord fields -> recordStructName fields
        KFloat -> "double"
        KScalar -> "int64_t"

genExportWrapper :: ExportSpec -> Doc
genExportWrapper spec =
  let ps = exportParams spec
      cParams = case ps of
        [] -> text "void"
        _ -> hsep (punctuate comma [text ct <+> text cn | (cn, ct) <- ps])
      cArgs = case ps of
        [] -> empty
        _ -> hsep (punctuate comma (map (text . fst) ps))
   in text (exportReturnType spec)
        <+> text (exportWrapperName spec)
        <> parens cParams
        <+> text "{"
        $$ nest 4 (text "return" <+> text (exportProcName spec) <> parens cArgs <> text ";")
        $$ text "}"

genHeader :: Doc -> Doc -> ExportSpec -> Doc
genHeader pairStructs recordStructs spec =
  let guardName = exportHeaderGuard spec
      ps = exportParams spec
      cParams = case ps of
        [] -> text "void"
        _ -> hsep (punctuate comma [text ct <+> text cn | (cn, ct) <- ps])
   in text "#ifndef"
        <+> text guardName
        $$ text "#define"
        <+> text guardName
        $$ text ""
        $$ text "#include \"hydrangea_runtime.h\""
        $$ text ""
        $$ text "#ifdef __cplusplus"
        $$ text "extern \"C\" {"
        $$ text "#endif"
        $$ text ""
        $$ pairStructs
        $$ recordStructs
        $$ text (exportReturnType spec)
        <+> text (exportWrapperName spec)
        <> parens cParams
        <> text ";"
        $$ text ""
        $$ text "#ifdef __cplusplus"
        $$ text "}"
        $$ text "#endif"
        $$ text ""
        $$ text "#endif"

exportHeaderGuard :: ExportSpec -> String
exportHeaderGuard spec =
  "HYDRANGEA_EXPORT_" ++ map toUpper (sanitizeExportName (exportKernelName spec)) ++ "_H"

genMain :: Maybe BenchmarkConfig -> Map CVar VarKind -> Map CVar CType -> [CFG.Proc] -> Doc
genMain mBench retKinds retTypes procs =
  text "int main(void) {"
    $$ nest 4 body
    $$ nest 4 (text "return 0;")
    $$ text "}"
  where
    zeroArgProcs = filter (\(CFG.Proc {CFG.procParams = params}) -> null params) procs

    body = case mBench of
      Nothing -> vcat (map callAndPrint zeroArgProcs)
      Just bc ->
        let benchName = bcKernelName bc
            benchAndAfter = dropWhile (\p -> CFG.procName p /= benchName) zeroArgProcs
         in case benchAndAfter of
               -- Fallback if kernel not found among zero-arg procs (should have been caught earlier).
               [] -> vcat (map callAndPrint zeroArgProcs)
               (benchProc : _) ->
                -- In benchmark mode, time only the selected benchmark kernel in
                -- main; do not run unrelated zero-arg top-level procedures.
                benchScaffold bc benchProc

    -- Generate warmup + measure loops + stats report + final call-and-print.
    benchScaffold bc proc =
      let name = CFG.procName proc
          cName = sanitize name
          bsName = BS.unpack name
          warmupStr = show (bcWarmupIters bc)
          itersStr = show (bcMeasureIters bc)
          retKind = Map.findWithDefault KScalar name retKinds
          isArrayRet = case Map.lookup name retTypes of
            Just (CTArray _) -> True
            _ -> case Map.findWithDefault KScalar name retKinds of
              KArray -> True
              KFloatArray -> True
              _ -> False
          benchCacheVar = "__bench_cache_" ++ cName
          resetBenchCache =
            if isArrayRet
              then text (benchCacheVar ++ " = NULL;")
              else empty
          benchSinkDecl = case retKind of
            KFloat -> text "volatile double __bench_sink_f = 0.0;"
            KScalar -> text "volatile int64_t __bench_sink_i = 0;"
            _ -> empty
          benchInvoke = case retKind of
            KFloat -> text ("__bench_sink_f += " ++ cName ++ "();")
            KScalar -> text ("__bench_sink_i += " ++ cName ++ "();")
            _ -> text ("(void) " ++ cName ++ "();")
       in -- Warmup loop
          benchSinkDecl
            $$ text ("for (int __bench_w = 0; __bench_w < " ++ warmupStr ++ "; __bench_w++) {")
            $$ nest 4 (resetBenchCache $$ text ("(void) " ++ cName ++ "();"))
            $$ text "}"
            -- Measurement loop
            $$ text ("double __bench_times[" ++ itersStr ++ "];")
            $$ text ("for (int __bench_i = 0; __bench_i < " ++ itersStr ++ "; __bench_i++) {")
            $$ nest
              4
              ( resetBenchCache
                  $$ text "struct timespec __ts0, __ts1;"
                  $$ text "clock_gettime(CLOCK_MONOTONIC, &__ts0);"
                  $$ benchInvoke
                  $$ text "clock_gettime(CLOCK_MONOTONIC, &__ts1);"
                  $$ text ("__bench_times[__bench_i] = (double)(__ts1.tv_sec - __ts0.tv_sec) * 1.0e9")
                  $$ text "    + (double)(__ts1.tv_nsec - __ts0.tv_nsec);"
              )
            $$ text "}"
            -- Stats report
            $$ text "{"
            $$ nest
              4
              ( text "double __bench_min = __bench_times[0], __bench_sum = 0.0;"
                  $$ text ("for (int __bench_j = 0; __bench_j < " ++ itersStr ++ "; __bench_j++) {")
                  $$ nest
                    4
                    ( text "if (__bench_times[__bench_j] < __bench_min)"
                        $$ nest 4 (text "__bench_min = __bench_times[__bench_j];")
                        $$ text "__bench_sum += __bench_times[__bench_j];"
                    )
                  $$ text "}"
                  $$ text ("fprintf(stderr, \"benchmark[" ++ bsName ++ "]: min=%.3f ms  mean=%.3f ms\\n\",")
                  $$ text ("    __bench_min / 1.0e6, __bench_sum / " ++ itersStr ++ ".0 / 1.0e6);")
              )
            $$ text "}"
            -- Final call: populate cache (for arrays) or call-and-print normally.
            $$ if isArrayRet
              then benchFinalArrayCall cName benchCacheVar (Map.lookup name retTypes) (Map.findWithDefault KScalar name retKinds)
              else callAndPrint proc

    -- Assign to the pre-declared global cache then print.
    benchFinalArrayCall cName benchCacheVar mtype kind =
      text benchCacheVar
        <+> text "="
        <+> text cName
        <> text "();"
        $$ arrayPrintCall benchCacheVar mtype kind

    -- Print an array variable given its type/kind (no declaration).
    arrayPrintCall varName mtype kind =
      case mtype of
        Just (CTArray CTDouble) -> text "hyd_print_float_array(" <> text varName <> text ");"
        Just (CTArray CTInt64) -> text "hyd_print_array(" <> text varName <> text ");"
        Just (CTArray CTBool) -> text "hyd_print_array(" <> text varName <> text ");"
        Just (CTArray CTUnit) -> text "hyd_print_array(" <> text varName <> text ");"
        Just (CTArray eltTy)
          | isStructuredPrintableArrayElem eltTy ->
              text (structuredArrayPrintName eltTy) <> text "(" <> text varName <> text ");"
        Just (CTArray _) -> text "(void)" <> parens (text varName) <> text ";"
        _ -> case kind of
          KFloatArray -> text "hyd_print_float_array(" <> text varName <> text ");"
          KArray -> text "hyd_print_array(" <> text varName <> text ");"
          _ -> text "(void)" <> parens (text varName) <> text ";"

    callAndPrint (CFG.Proc {CFG.procName = name}) =
      let cName = sanitize name
          kind = Map.findWithDefault KScalar name retKinds
          result = cName ++ "_result"
          invoke ctype printStmt =
            text ctype
              <+> text result
              <+> text "="
              <+> text cName
              <> text "();"
              $$ printStmt
          printFn fn = invoke "hyd_array_t*" (text fn <> parens (text result) <> text ";")
       in case Map.lookup name retTypes of
            Just (CTArray CTDouble) -> printFn "hyd_print_float_array"
            Just (CTArray CTInt64) -> printFn "hyd_print_array"
            Just (CTArray CTBool) -> printFn "hyd_print_array"
            Just (CTArray CTUnit) -> printFn "hyd_print_array"
            Just (CTArray eltTy)
              | isStructuredPrintableArrayElem eltTy ->
                  invoke "hyd_array_t*" (text (structuredArrayPrintName eltTy) <> parens (text result) <> text ";")
              | otherwise ->
                  invoke "hyd_array_t*" (text "(void)" <> parens (text result) <> text ";")
            _ -> case kind of
              KFloatArray -> printFn "hyd_print_float_array"
              KArray -> printFn "hyd_print_array"
              KTuple -> invoke "hyd_tuple_t" (text "hyd_print_tuple" <> parens (text result) <> text ";")
              KPair cty1 cty2 ->
                invoke
                  (pairStructName cty1 cty2)
                  (text "hyd_print_pair(" <> text result <> text ".fst, " <> text result <> text ".snd);")
              KRecord fields ->
                invoke (recordStructName fields) (text "(void)" <> parens (text result) <> text ";")
              KFloat -> invoke "double" (text "hyd_print_double" <> parens (text result) <> text ";")
              KScalar -> invoke "int64_t" (text "hyd_print_int" <> parens (text result) <> text ";")

genProc :: CodegenOptions -> Maybe (Set CVar) -> Map CVar VarKind -> Map CVar CType -> CallParamTypes -> CFG.Proc -> Doc
genProc opts mBenchReachable retKinds retTypes callParamTypes proc@(CFG.Proc {CFG.procName = name, CFG.procParams = params, CFG.procBody = body}) =
  let mBench = codegenBenchmark opts
      typeEnv = recoverProcTypeEnv retTypes callParamTypes proc
      -- Authoritative classification from the recovered type environment.
      (arrVarsTE, tupVarsTE, pairVarsTE, recordVarsTE, floatVarsTE, floatArrVarsTE) = typeEnvToVarSets typeEnv
      arrayElemTypes = inferArrayElemTypesFromStmts retTypes body (Map.fromList [(v, eltTy) | (v, CTArray eltTy) <- Map.toList typeEnv])
      -- Fallback heuristic classification for optimizer-introduced temporaries
      -- not present in the type env.
      arrVarsH = arrayVarsProc proc
      tupVarsH = tupleVarsProc proc
      pairVarsH = pairVarsProc proc
      (floatVarsH, floatArrVarsH) = classifyVarKinds retKinds body
      -- Merge: type env entries take priority (left-biased union for maps).
      arrVars = arrVarsTE `S.union` arrVarsH
      tupVars = tupVarsTE `S.union` tupVarsH
      pairVars = pairVarsTE `Map.union` pairVarsH
      recordVars = recordVarsTE
      floatVars = floatVarsTE `S.union` floatVarsH
      floatArrVars = floatArrVarsTE `S.union` floatArrVarsH
      procRetKind = Map.findWithDefault KScalar name retKinds
      -- Emit the correct C type for each formal parameter.
      genParam p
        | p `S.member` floatVars = text "double" <+> text (sanitize p)
        | p `S.member` arrVars = text "hyd_array_t*" <+> text (sanitize p)
        | p `S.member` tupVars = text "hyd_tuple_t" <+> text (sanitize p)
        | Just (ct1, ct2) <- Map.lookup p pairVars = text (pairStructName ct1 ct2) <+> text (sanitize p)
        | Just fields <- Map.lookup p recordVars = text (recordStructName fields) <+> text (sanitize p)
        | otherwise = text "int64_t" <+> text (sanitize p)
      retType = case procRetKind of
        KArray -> "hyd_array_t*"
        KFloatArray -> "hyd_array_t*"
        KTuple -> "hyd_tuple_t"
        KPair ct1 ct2 -> pairStructName ct1 ct2
        KRecord fields -> recordStructName fields
        KFloat -> "double"
        KScalar -> "int64_t"
      cName = sanitize name
      cParams = case params of
        [] -> text "void"
        _ -> hsep (punctuate comma (map genParam params))
      -- Zero-argument array-returning procs are memoized with a static cache
      -- to prevent O(n) re-reads when called from inside generate loops.
      -- Exception: the benchmark kernel uses a file-scope cache instead so
      -- it can be called repeatedly without short-circuiting during the loop.
      isBenchKernel = case mBench of
        Just bc -> bcKernelName bc == name
        Nothing -> False
      benchClosureHit = maybe False (name `S.member`) mBenchReachable
      benchmarkUsesMemo = benchClosureHit && not (procUsesIO body)
      useMemo = null params && retType == "hyd_array_t*" && not isBenchKernel && not benchmarkUsesMemo
      -- Scalar zero-arg procs (e.g. getenv-based config procs like iters(), w_f()) are
      -- memoized with a static bool+value pair so callers inside hot loops don't re-invoke
      -- the body (which may call getenv) on every iteration.
      useScalarMemo =
        null params
          && (procRetKind == KScalar || procRetKind == KFloat)
          && not isBenchKernel
          && not (procUsesIO body)
      cacheVar = text "__cache_" <> text cName
      scalarComputedVar = text "__scalar_computed_" <> text cName
      scalarCacheVar = text "__scalar_cache_" <> text cName
      benchCacheVar = text "__bench_cache_" <> text cName
      benchmarkBody = if isBenchKernel then stripBenchmarkIOStmts body else body
      (bodyWithoutReturn, finalReturn) = splitFinalReturn benchmarkBody
      localArrayVars = localArrayVarsProc typeEnv proc
      vecVars = classifyVecVars (codegenSimdWidth opts) body
      tupleDefs = collectTupleDefs body
      env =
        CodegenEnv
          { ceOpts = opts,
            ceRetKinds = retKinds,
            ceRetTypes = retTypes,
            ceArrayElemTypes = arrayElemTypes,
            ceArrVars = arrVars,
            ceTupVars = tupVars,
            cePairVars = pairVars,
            ceRecordVars = recordVars,
            ceFloatVars = floatVars,
            ceFloatArrVars = floatArrVars,
            ceVecVars = vecVars,
            ceTupleDefs = tupleDefs,
            ceInsideParallelRegion = False
          }
      returnedArrayVars retAtom =
        case retAtom of
          AVar v -> case Map.lookup v typeEnv of
            Just (CTArray _) -> S.singleton v
            _ -> S.empty
          _ -> S.empty
      cleanupDoc retAtom =
        let returnedVars = returnedArrayVars retAtom
            aliasedToReturn = case retAtom of
              AVar retVar -> S.fromList
                [ src
                | CFG.SAssign tgt (RAtom (AVar src)) <- bodyWithoutReturn
                , tgt == retVar
                , case Map.lookup src typeEnv of
                    Just (CTArray _) -> True
                    _ -> False
                ]
              _ -> S.empty
            varsToFree =
              case procRetKind of
                KArray -> localArrayVars `S.difference` returnedVars `S.difference` aliasedToReturn
                KFloatArray -> localArrayVars `S.difference` returnedVars `S.difference` aliasedToReturn
                KPair _ _ -> S.empty
                KRecord _ -> S.empty
                _ -> localArrayVars
            -- Free the non-returned component of any pair-of-arrays accumulator.
            discardedPairDoc =
              discardedArrayPairComponentDoc bodyWithoutReturn retAtom pairVars
         in cleanupArrayVarsDoc varsToFree $$ discardedPairDoc
      returnDoc retAtom = text "return" <+> genAtom retAtom <> text ";"
      procBodyMemo =
        case finalReturn of
          Just retAtom ->
            genStmts env S.empty (usedVarsAtom retAtom) bodyWithoutReturn
              $$ cleanupDoc retAtom
              $$ cacheVar
              <+> text "="
              <+> genAtom retAtom
              <> text ";"
          Nothing ->
            genStmts env S.empty S.empty body
      -- Body for scalar-memoized proc: compute once, cache in static variable.
      procBodyScalarMemo =
        case finalReturn of
          Just retAtom ->
            genStmts env S.empty (usedVarsAtom retAtom) bodyWithoutReturn
              $$ cleanupDoc retAtom
              $$ scalarCacheVar <+> text "=" <+> genAtom retAtom <> text ";"
          Nothing ->
            genStmts env S.empty S.empty body
      procBodyNormal =
        case finalReturn of
          Just retAtom ->
            genStmts env S.empty (usedVarsAtom retAtom) bodyWithoutReturn
              $$ cleanupDoc retAtom
              $$ returnDoc retAtom
          Nothing ->
            genStmts env S.empty S.empty body
   in if isBenchKernel && retType == "hyd_array_t*"
        then
          -- Benchmark kernel: check the file-scope cache (set after the timing loop
          -- by main); if NULL, compute fresh without writing to any cache so that
          -- the timing loop can call us repeatedly.
          text retType
            <+> text cName
            <> text "(void) {"
            $$ nest
              4
              ( text "if ("
                  <> benchCacheVar
                  <> text ") return hyd_array_reshape_view("
                  <> benchCacheVar
                  <> text ", "
                  <> benchCacheVar
                  <> text "->shape);"
                  $$ procBodyNormal
              )
            $$ text "}"
        else
          if useMemo
            then
              text "static hyd_array_t*"
                <+> cacheVar
                <+> text "= NULL;"
                $$ text retType
                <+> text cName
                <> text "(void) {"
                $$ nest
                  4
                  ( text "if ("
                      <> cacheVar
                      <+> text "== NULL) {"
                      $$ nest 4 procBodyMemo
                      $$ text "}"
                      $$ text "return hyd_array_reshape_view("
                      <> cacheVar
                      <> text ", "
                      <> cacheVar
                      <> text "->shape);"
                  )
                $$ text "}"
            else if useScalarMemo
              then
                -- Scalar memoization: static bool + static value; compute once.
                text "static int"
                  <+> scalarComputedVar
                  <+> text "= 0;"
                  $$ text "static"
                  <+> text retType
                  <+> scalarCacheVar
                  <> text ";"
                  $$ text retType
                  <+> text cName
                  <> text "(void) {"
                  $$ nest
                    4
                    ( text "if (!"
                        <> scalarComputedVar
                        <> text ") {"
                        $$ nest
                          4
                          ( scalarComputedVar <+> text "= 1;"
                              $$ procBodyScalarMemo
                          )
                        $$ text "}"
                        $$ text "return"
                        <+> scalarCacheVar
                        <> text ";"
                    )
                  $$ text "}"
              else
                text retType
                  <+> text cName
                  <> parens cParams
                  <+> text "{"
                  $$ nest 4 procBodyNormal
                  $$ text "}"

genStmts :: CodegenEnv -> Set CVar -> Set CVar -> [CFG.Stmt] -> Doc
genStmts env declared liveAfter stmts =
  case stmts of
    [] -> empty
    stmt : rest ->
      let stmtLiveAfter = usedVarsStmts rest `S.union` liveAfter
          doc = genStmt env declared stmtLiveAfter stmt
          declared' = case stmt of
            CFG.SAssign _ (RVecStore _ _ _) -> declared
            CFG.SAssign _ (RArrayFree _) -> declared
            CFG.SAssign v _ -> S.insert v declared
            CFG.SLoop spec body ->
              let declaredWithHoists = declared `S.union` memoizedZeroArgArrayCallVars (ceRetKinds env) (ceRetTypes env) body
               in case CFG.lsRed spec of
                    Just r -> S.insert (CFG.rsAccVar r) declaredWithHoists
                    Nothing -> declaredWithHoists
            CFG.SParallelRegion body ->
              let regionVars = allAssignedVars body `S.intersection` stmtLiveAfter
              in declared `S.union` regionVars
            CFG.SIf _ thn els ->
              let branchVars = allAssignedVars (thn ++ els) `S.intersection` stmtLiveAfter
               in declared `S.union` branchVars
            _ -> declared
       in doc $$ genStmts env declared' liveAfter rest

genStmt :: CodegenEnv -> Set CVar -> Set CVar -> CFG.Stmt -> Doc
genStmt env declared liveAfter stmt = case stmt of
  CFG.SAssign _ (RArrayFree a) -> text "hyd_array_free(" <> genAtom a <> text ");"
  CFG.SAssign _ rhs@(RVecStore _ _ val) ->
    let mw = case val of
          AVecVar vv -> Map.lookup vv (ceVecVars env)
          _ -> Nothing
     in genRHS (ceArrayElemTypes env) (ceFloatArrVars env) mw (ceTupleDefs env) rhs <> text ";"
  CFG.SAssign v rhs
    | v `S.member` declared ->
        -- When a declared pair-of-arrays variable is overwritten, free the old
        -- array components first to prevent a heap leak.
        let freeOldPair = case Map.lookup v (cePairVars env) of
              Just (CEArray, CEArray) ->
                text "hyd_array_free("
                  <> text (sanitize v)
                  <> text ".fst);"
                  $$ text "hyd_array_free("
                  <> text (sanitize v)
                  <> text ".snd);"
              Just (CEArray, _) ->
                text "hyd_array_free(" <> text (sanitize v) <> text ".fst);"
              Just (_, CEArray) ->
                text "hyd_array_free(" <> text (sanitize v) <> text ".snd);"
              _ -> empty
         in freeOldPair $$ text (sanitize v) <+> text "=" <+> genAssignRHS rhs <> text ";"
    | otherwise ->
        let cv = sanitize v
            decl = varDecl env v rhs
         in decl <+> text cv <+> text "=" <+> genAssignRHS rhs <> text ";"
    where
      genAssignRHS assignedRhs = case assignedRhs of
        RArrayAlloc shp -> genArrayAllocExpr (ceArrayElemTypes env) v shp
        RArrayCopy src -> text "hyd_array_alloc_copy(" <> genAtom src <> text ")"
        RProj 0 (AVar src) | Map.member src (cePairVars env) -> genAtom (AVar src) <> text ".fst"
        RProj 1 (AVar src) | Map.member src (cePairVars env) -> genAtom (AVar src) <> text ".snd"
        _ -> genRHS (ceArrayElemTypes env) (ceFloatArrVars env) (Map.lookup v (ceVecVars env)) (ceTupleDefs env) assignedRhs
  CFG.SArrayWrite arr idx val
    | AVar arrV <- arr,
      Just eltTy <- Map.lookup arrV (ceArrayElemTypes env) ->
        genArrayAccess eltTy arr idx <+> text "=" <+> genAtom val <> text ";"
    | AVar arrV <- arr,
      arrV `S.member` ceFloatArrVars env ->
        text "hyd_array_set_float(" <> genAtom arr <> text "," <+> genAtom idx <> text "," <+> genAtom val <> text ");"
    | otherwise ->
        genAtom arr <> text "->data[" <> genAtom idx <> text "]" <+> text "=" <+> genAtom val <> text ";"
  CFG.SLoop spec body -> genLoop env declared spec body
  CFG.SParallelRegion body ->
    let branchRHS = assignedRHSMap body
        branchVars =
          S.toList ((allAssignedVars body `S.intersection` liveAfter) `S.difference` declared)
        preDecls =
          vcat
            [ case Map.lookup v branchRHS of
                Just rhs ->
                  varDecl env v rhs <+> text (sanitize v) <> text ";"
                Nothing -> empty
            | v <- branchVars
            ]
        declared' = declared `S.union` S.fromList branchVars
        regionEnv = env { ceInsideParallelRegion = True }
     in preDecls
          $$ text "#pragma omp parallel"
          $$ text "{"
          $$ nest 4 (genStmts regionEnv declared' liveAfter body)
          $$ text "}"
  CFG.SIf cond thn els ->
    let branchRHS = assignedRHSMap (thn ++ els)
        branchVars =
          S.toList ((allAssignedVars (thn ++ els) `S.intersection` liveAfter) `S.difference` declared)
        preDecls =
          vcat
            [ case Map.lookup v branchRHS of
                Just rhs ->
                  varDecl env v rhs <+> text (sanitize v) <> text ";"
                Nothing -> empty
            | v <- branchVars
            ]
        declared' = declared `S.union` S.fromList branchVars
     in preDecls
          $$ text "if ("
          <> genAtom cond
          <> text ") {"
          $$ nest 4 (genStmts env declared' liveAfter thn)
          $$ case els of
            [] -> text "}"
            _ ->
              text "} else {"
                $$ nest 4 (genStmts env declared' liveAfter els)
                $$ text "}"
  CFG.SReturn a -> text "return" <+> genAtom a <> text ";"
  CFG.SBreak -> text "break;"

genLoop :: CodegenEnv -> Set CVar -> CFG.LoopSpec -> [CFG.Stmt] -> Doc
genLoop env declared spec body =
  let iters = CFG.lsIters spec
      bounds = CFG.lsBounds spec
      red = CFG.lsRed spec
      role = CFG.lsRole spec
      (hoistedMemoCalls, loopBody) = hoistMemoizedZeroArgArrayCalls (ceRetKinds env) (ceRetTypes env) body
      hoistedMemoVars =
        S.fromList [v | CFG.SAssign v (RCall _ args) <- hoistedMemoCalls, isZeroArgCallArgs args]
      declaredWithHoisted = declared `S.union` hoistedMemoVars
      hoistedMemoDoc =
        genStmts env declared (usedVarsStmts loopBody) hoistedMemoCalls
      bodyDoc d = genStmts env d S.empty loopBody
      parallelPolicyClause = case CFG.lsExec spec of
        CFG.Parallel p ->
          let strategyComment = case CFG.psStrategy p of
                CFG.ParallelGeneric -> empty
                CFG.ParallelScatterDirect -> text " /* scatter-direct */"
                CFG.ParallelScatterAtomicAddInt -> text " /* scatter-atomic-add-int */"
                CFG.ParallelScatterAtomicAddFloat -> text " /* scatter-atomic-add-float */"
                CFG.ParallelScatterPrivatizedIntAdd -> text " /* scatter-privatized-int-add */"
           in strategyComment <> maybe empty (\pol -> space <> text (BS.unpack pol)) (CFG.psPolicy p)
        CFG.Workshare p ->
          let strategyComment = case CFG.psStrategy p of
                CFG.ParallelGeneric -> empty
                CFG.ParallelScatterDirect -> text " /* scatter-direct */"
                CFG.ParallelScatterAtomicAddInt -> text " /* scatter-atomic-add-int */"
                CFG.ParallelScatterAtomicAddFloat -> text " /* scatter-atomic-add-float */"
                CFG.ParallelScatterPrivatizedIntAdd -> text " /* scatter-privatized-int-add */"
           in strategyComment <> maybe empty (\pol -> space <> text (BS.unpack pol)) (CFG.psPolicy p)
        _ -> empty
      origins = CFG.lsOrigins spec
      collapseClause
        | length iters > 1 = space <> text "collapse(" <> int (length iters) <> text ")"
        | otherwise = empty
      originFor i = case drop i (origins ++ repeat (CFG.IConst 0)) of
        (o : _) -> o
        [] -> CFG.IConst 0
      originDoc i = case originFor i of
        CFG.IConst 0 -> text "0"
        o           -> genIndexExpr o
      parallelPragma extraClauses =
        let simdClause = case CFG.lsExec spec of
              CFG.Parallel p -> maybe empty (\w -> text " simd simdlen(" <> int w <> text ")") (CFG.psSimdLen p)
              CFG.Workshare p -> maybe empty (\w -> text " simd simdlen(" <> int w <> text ")") (CFG.psSimdLen p)
              _             -> empty
            schedClause
              | role == CFG.LoopSegRedOuter = text " schedule(dynamic)"
              | otherwise                  = empty
            basePragma = case CFG.lsExec spec of
              CFG.Workshare {} ->
                if ceInsideParallelRegion env
                  then text "#pragma omp for"
                  else text "#pragma omp parallel for"
              _ ->
                text "#pragma omp parallel for"
        in basePragma <> simdClause <> schedClause <> parallelPolicyClause <> collapseClause <> extraClauses
      simdPragma extraClauses = text "#pragma omp simd simdlen(" <> int defaultSimdLen <> text ")" <> extraClauses
      roleComment = case role of
        CFG.LoopPlain -> text "loop"
        CFG.LoopFold -> text "fold loop"
        CFG.LoopMap -> text "map loop"
        CFG.LoopReductionWrapper -> text "reduction wrapper loop"
        CFG.LoopReduction -> text "reduction loop"
        CFG.LoopMapReduction -> text "map-reduction outer loop"
        CFG.LoopIterate -> text "iterate temporal loop"
        CFG.LoopSegRedOuter -> text "segmented-reduce outer loop"
      defaultSimdLen = case CFG.lsExec spec of
        CFG.Vector v -> CFG.vsWidth v
        _ -> 1
      minSimdTripCount = 4
      constantTripCount ie = case CFG.simplifyIndexExpr ie of
        CFG.IConst n -> Just n
        _ -> Nothing
      shouldEmitSimd ie = maybe True (>= minSimdTripCount) (constantTripCount ie)
      atomicScatterBodyDoc elemTy declared' =
        case detectAtomicScatterAddLoop loopBody of
          Just (prefix, mGuard, arr, idx, val) ->
            let prefixLiveAfter =
                  maybe S.empty usedVarsAtom mGuard
                    `S.union` usedVarsAtom arr
                    `S.union` usedVarsAtom idx
                    `S.union` usedVarsAtom val
                atomicUpdate =
                  text "#pragma omp atomic update"
                    $$ genArrayAccess elemTy arr idx
                    <+> text "+="
                    <+> genAtom val
                    <> text ";"
                guardedUpdate =
                  case mGuard of
                    Nothing -> atomicUpdate
                    Just cond ->
                      text "if ("
                        <> genAtom cond
                        <> text ") {"
                        $$ nest 4 atomicUpdate
                        $$ text "}"
             in genStmts env declared' prefixLiveAfter prefix $$ guardedUpdate
          Nothing ->
            bodyDoc declared'
      privatizedScatterLoopDoc declared' iter bound =
        case detectAtomicScatterAddLoop loopBody of
          Just (prefix, mGuard, arr, idx, val) ->
            let arrStem = case arr of
                  AVar arrV -> sanitize arrV
                  _ -> "scatter"
                prefixLiveAfter =
                  maybe S.empty usedVarsAtom mGuard
                    `S.union` usedVarsAtom arr
                    `S.union` usedVarsAtom idx
                    `S.union` usedVarsAtom val
                sizeVar = text ("__hyd_priv_size_" ++ arrStem)
                privVar = text ("__hyd_priv_buf_" ++ arrStem)
                mergeIxName = "__hyd_priv_merge_" ++ arrStem
                mergeIx = text mergeIxName
                ci = sanitize iter
                privUpdate =
                  privVar <> text "[" <> genAtom idx <> text "]" <+> text "+=" <+> genAtom val <> text ";"
                guardedPrivUpdate =
                  case mGuard of
                    Nothing -> privUpdate
                    Just cond ->
                      text "if ("
                        <> genAtom cond
                        <> text ") {"
                        $$ nest 4 privUpdate
                        $$ text "}"
             in text "/* parallel"
                  <+> roleComment
                  <+> text "*/"
                  $$ text "#pragma omp parallel /* scatter-privatized-int-add */"
                  $$ text "{"
                  $$ nest
                    4
                    ( text "int64_t"
                        <+> sizeVar
                        <+> text "="
                        <+> text "hyd_shape_size("
                        <> genAtom arr
                        <> text "->shape);"
                        $$ text "int64_t*"
                        <+> privVar
                        <+> text "="
                        <+> text "(int64_t*)calloc((size_t)"
                        <> sizeVar
                        <> text ", sizeof(int64_t));"
                        $$ text "if ("
                        <> privVar
                        <+> text "== NULL) {"
                        $$ nest
                          4
                          ( text "fprintf(stderr, \"hydrangea: failed to allocate privatized scatter buffer\\n\");"
                              $$ text "exit(1);"
                          )
                        $$ text "}"
                        $$ text "#pragma omp for"
                        $$ text "for (int64_t"
                        <+> text ci
                        <+> text "=" <+> originDoc 0 <> text ";"
                        <+> text ci
                        <+> text "<"
                        <+> genIndexExpr bound
                        <> text ";"
                        <+> text ci
                        <> text "++) {"
                        $$ nest
                          4
                          ( genStmts env declared' prefixLiveAfter prefix
                              $$ guardedPrivUpdate
                          )
                        $$ text "}"
                        $$ text "for (int64_t"
                        <+> mergeIx
                        <+> text "= 0;"
                        <+> mergeIx
                        <+> text "<"
                        <+> sizeVar
                        <> text ";"
                        <+> mergeIx
                        <> text "++) {"
                        $$ nest
                          4
                          ( text "#pragma omp atomic"
                              $$ (genArrayAccess CTInt64 arr (AVar (BS.pack mergeIxName)) <+> text "+=" <+> privVar <> text "[" <> mergeIx <> text "];")
                          )
                        $$ text "}"
                        $$ text "free("
                        <> privVar
                        <> text ");"
                    )
                  $$ text "}"
          Nothing ->
            text "/* parallel"
              <+> roleComment
              <+> text "*/"
              $$ parallelPragma empty
              $$ text "for (int64_t"
              <+> text (sanitize iter)
              <+> text "=" <+> originDoc 0 <> text ";"
              <+> text (sanitize iter)
              <+> text "<"
              <+> genIndexExpr bound
              <> text ";"
              <+> text (sanitize iter)
              <> text "++) {"
              $$ nest 4 (bodyDoc declared')
              $$ text "}"
   in hoistedMemoDoc $$ case (CFG.lsExec spec, red, iters, bounds) of
        (CFG.Serial, Just r, [i], [b]) ->
          let cacc = sanitize (CFG.rsAccVar r)
              ci = sanitize i
              declared' = S.insert (CFG.rsAccVar r) declaredWithHoisted
              alreadyDeclared = CFG.rsAccVar r `S.member` declared
              accCType = if CFG.rsAccVar r `S.member` ceFloatVars env then "double" else "int64_t"
              accInit =
                if alreadyDeclared
                  then empty
                  else text "/* acc */" $$ text accCType <+> text cacc <+> text "=" <+> genIndexExpr (CFG.rsInit r) <> text ";"
           in accInit
                $$ text "for (int64_t"
                <+> text ci
                <+> text "=" <+> originDoc 0 <> text ";"
                <+> text ci
                <+> text "<"
                <+> genIndexExpr b
                <> text ";"
                <+> text ci
                <> text "++) {"
                $$ nest 4 (bodyDoc declared')
                $$ text "}"
        (execPolicy, Just r, [i], [b]) | isOmpLoopExec execPolicy ->
          let cacc = sanitize (CFG.rsAccVar r)
              ci = sanitize i
              declared' = S.insert (CFG.rsAccVar r) declaredWithHoisted
              alreadyDeclared = CFG.rsAccVar r `S.member` declared
              redClause = case CFG.rsRedop r of
                RAdd -> text "reduction(+:" <> text cacc <> text ")"
                RMul -> text "reduction(*:" <> text cacc <> text ")"
              accCType = if CFG.rsAccVar r `S.member` ceFloatVars env then "double" else "int64_t"
              accInit =
                if alreadyDeclared
                  then empty
                  else text "/* parallel reduction */" $$ text accCType <+> text cacc <+> text "=" <+> genIndexExpr (CFG.rsInit r) <> text ";"
           in accInit
                $$ parallelPragma (space <> redClause)
                $$ text "for (int64_t"
                <+> text ci
                <+> text "=" <+> originDoc 0 <> text ";"
                <+> text ci
                <+> text "<"
                <+> genIndexExpr b
                <> text ";"
                <+> text ci
                <> text "++) {"
                $$ nest 4 (bodyDoc declared')
                $$ text "}"
        (execPolicy, Just r, _, _) | isOmpLoopExec execPolicy ->
          let cacc = sanitize (CFG.rsAccVar r)
              declared' = S.insert (CFG.rsAccVar r) declaredWithHoisted
              alreadyDeclared = CFG.rsAccVar r `S.member` declared
              redClause = case CFG.rsRedop r of
                RAdd -> text "reduction(+:" <> text cacc <> text ")"
                RMul -> text "reduction(*:" <> text cacc <> text ")"
              accCType = if CFG.rsAccVar r `S.member` ceFloatVars env then "double" else "int64_t"
              accInit =
                if alreadyDeclared
                  then empty
                  else text "/* parallel reduction */" $$ text accCType <+> text cacc <+> text "=" <+> genIndexExpr (CFG.rsInit r) <> text ";"
           in accInit
                $$ parallelPragma (space <> redClause)
                 $$ genNestedLoops iters bounds origins (bodyDoc declared')
        (CFG.Vector v, Just r, [i], [b]) ->
          let ci = sanitize i
              cacc = sanitize (CFG.rsAccVar r)
              declared' = S.insert (CFG.rsAccVar r) declaredWithHoisted
              redClause = case CFG.rsRedop r of
                RAdd -> text " reduction(+:" <> text cacc <> text ")"
                RMul -> text " reduction(*:" <> text cacc <> text ")"
              accCType = if CFG.rsAccVar r `S.member` ceFloatVars env then "double" else "int64_t"
              accInit =
                if CFG.rsAccVar r `S.member` declared
                  then empty
                  else text "/* simd reduction */" $$ text accCType <+> text cacc <+> text "=" <+> genIndexExpr (CFG.rsInit r) <> text ";"
              vecComment = case CFG.vsTail v of
                CFG.TailNone -> text "/* vectorized reduction loop, width =" <+> int (CFG.vsWidth v) <> text " */"
                _ -> text "/* vectorized reduction loop, width =" <+> int (CFG.vsWidth v) <+> text "(compiler handles tail) */"
           in if shouldEmitSimd b
                then
                  accInit
                    $$ vecComment
                    $$ simdPragma redClause
                    $$ text "for (int64_t"
                    <+> text ci
                    <+> text "=" <+> originDoc 0 <> text ";"
                    <+> text ci
                    <+> text "<"
                    <+> genIndexExpr b
                    <> text ";"
                    <+> text ci
                    <> text "++) {"
                    $$ nest 4 (bodyDoc declared')
                    $$ text "}"
                else
                  accInit
                    $$ text "/* short vector reduction loop; simd pragma omitted */"
                    $$ text "for (int64_t"
                    <+> text ci
                    <+> text "=" <+> originDoc 0 <> text ";"
                    <+> text ci
                    <+> text "<"
                    <+> genIndexExpr b
                    <> text ";"
                    <+> text ci
                    <> text "++) {"
                    $$ nest 4 (bodyDoc declared')
                    $$ text "}"
        (CFG.Vector v, _, [i], [b]) ->
          let ci = sanitize i
              vecComment = case CFG.vsTail v of
                CFG.TailNone -> text "/* vectorized loop, width =" <+> int (CFG.vsWidth v) <> text " */"
                _ -> text "/* vectorized loop, width =" <+> int (CFG.vsWidth v) <+> text "(compiler handles tail) */"
           in if shouldEmitSimd b
                then
                  vecComment
                    $$ simdPragma empty
                    $$ text "for (int64_t"
                    <+> text ci
                    <+> text "=" <+> originDoc 0 <> text ";"
                    <+> text ci
                    <+> text "<"
                    <+> genIndexExpr b
                    <> text ";"
                    <+> text ci
                    <> text "++) {"
                    $$ nest 4 (bodyDoc declaredWithHoisted)
                    $$ text "}"
                else
                  text "/* short vector loop; simd pragma omitted */"
                    $$ text "for (int64_t"
                    <+> text ci
                    <+> text "=" <+> originDoc 0 <> text ";"
                    <+> text ci
                    <+> text "<"
                    <+> genIndexExpr b
                    <> text ";"
                    <+> text ci
                    <> text "++) {"
                    $$ nest 4 (bodyDoc declaredWithHoisted)
                    $$ text "}"
        (execPolicy, _, [i], [b])
          | Just p <- ompLoopParallelSpec execPolicy
          , CFG.psStrategy p == CFG.ParallelScatterPrivatizedIntAdd ->
              privatizedScatterLoopDoc declaredWithHoisted i b
        (execPolicy, _, [i], [b]) | isOmpLoopExec execPolicy ->
          let ci = sanitize i
           in text "/* parallel"
                <+> roleComment
                <+> text "*/"
                $$ parallelPragma empty
                $$ text "for (int64_t"
                <+> text ci
                <+> text "=" <+> originDoc 0 <> text ";"
                <+> text ci
                <+> text "<"
                <+> genIndexExpr b
                <> text ";"
                <+> text ci
                <> text "++) {"
                $$ nest
                  4
                  ( case CFG.lsExec spec of
                      CFG.Parallel p
                        | Just elemTy <- atomicScatterElemType p -> atomicScatterBodyDoc elemTy declaredWithHoisted
                      CFG.Workshare p
                        | Just elemTy <- atomicScatterElemType p -> atomicScatterBodyDoc elemTy declaredWithHoisted
                      _ -> bodyDoc declaredWithHoisted
                  )
                $$ text "}"
        (execPolicy, _, _, _) | isOmpLoopExec execPolicy ->
          text "/* parallel"
            <+> roleComment
            <+> text "*/"
            $$ parallelPragma empty
            $$ genNestedLoops
              iters
              bounds
              origins
              ( case CFG.lsExec spec of
                  CFG.Parallel p
                    | Just elemTy <- atomicScatterElemType p -> atomicScatterBodyDoc elemTy declaredWithHoisted
                  CFG.Workshare p
                    | Just elemTy <- atomicScatterElemType p -> atomicScatterBodyDoc elemTy declaredWithHoisted
                  _ -> bodyDoc declaredWithHoisted
              )
        (_, _, _, _) -> genNestedLoops iters bounds origins (bodyDoc declaredWithHoisted)
  where
    isOmpLoopExec execPolicy = case execPolicy of
      CFG.Parallel {} -> True
      CFG.Workshare {} -> True
      _ -> False

    ompLoopParallelSpec execPolicy = case execPolicy of
      CFG.Parallel p -> Just p
      CFG.Workshare p -> Just p
      _ -> Nothing

hoistMemoizedZeroArgArrayCalls :: Map CVar VarKind -> Map CVar CType -> [CFG.Stmt] -> ([CFG.Stmt], [CFG.Stmt])
hoistMemoizedZeroArgArrayCalls retKinds retTypes = go
  where
    go [] = ([], [])
    go (stmt : rest) =
      let (hoistedStmt, keptStmt) = step stmt
          (hoistedRest, keptRest) = go rest
       in (hoistedStmt ++ hoistedRest, keptStmt ++ keptRest)

    step stmt
      | isMemoizedZeroArgArrayCall retKinds retTypes stmt = ([stmt], [])
    step (CFG.SLoop spec body) =
      let (hoistedBody, keptBody) = go body
       in (hoistedBody, [CFG.SLoop spec keptBody])
    step (CFG.SParallelRegion body) =
      let (hoistedBody, keptBody) = go body
       in ([], [CFG.SParallelRegion (hoistedBody ++ keptBody)])
    step (CFG.SIf cond thn els) =
      let (hoistedThn, keptThn) = go thn
          (hoistedEls, keptEls) = go els
       in ([], [CFG.SIf cond (hoistedThn ++ keptThn) (hoistedEls ++ keptEls)])
    step other = ([], [other])

memoizedZeroArgArrayCallVars :: Map CVar VarKind -> Map CVar CType -> [CFG.Stmt] -> Set CVar
memoizedZeroArgArrayCallVars retKinds retTypes body =
  S.fromList [v | CFG.SAssign v (RCall _ args) <- fst (hoistMemoizedZeroArgArrayCalls retKinds retTypes body), isZeroArgCallArgs args]

isMemoizedZeroArgArrayCall :: Map CVar VarKind -> Map CVar CType -> CFG.Stmt -> Bool
isMemoizedZeroArgArrayCall retKinds retTypes stmt = case stmt of
  CFG.SAssign _ (RCall f args)
    | isZeroArgCallArgs args ->
    case Map.findWithDefault KScalar f retKinds of
      KArray -> True
      KFloatArray -> True
      _ -> case Map.lookup f retTypes of
        Just (CTArray _) -> True
        _ -> False
  _ -> False

isZeroArgCallArgs :: [Atom] -> Bool
isZeroArgCallArgs args = null args || all (== AUnit) args

genNestedLoops :: [CVar] -> [CFG.IndexExpr] -> [CFG.IndexExpr] -> Doc -> Doc
genNestedLoops iters bounds origins body =
  foldr mk body (zip3 (map sanitize iters) bounds (origins ++ repeat (CFG.IConst 0)))
  where
    mk (ci, b, o) inner =
      let startDoc = case o of
            CFG.IConst _ -> text "0"
            _           -> genIndexExpr o
      in text "for (int64_t"
        <+> text ci
        <+> text "="
        <+> startDoc
        <> text ";"
        <+> text ci
        <+> text "<"
        <+> genIndexExpr (CFG.simplifyIndexExpr (CFG.IAdd o b))
        <> text ";"
        <+> text ci
        <> text "++) {"
        $$ nest 4 inner
        $$ text "}"

detectAtomicScatterAddLoop :: [CFG.Stmt] -> Maybe ([CFG.Stmt], Maybe Atom, Atom, Atom, Atom)
detectAtomicScatterAddLoop body =
  case reverse body of
    (CFG.SIf cond thn [] : revPrefix) -> do
      (branchPrefix, arrWrite, idxWrite, val) <- detectAtomicScatterAddCore thn
      pure (reverse revPrefix ++ branchPrefix, Just cond, arrWrite, idxWrite, val)
    _ -> do
      (prefix, arrWrite, idxWrite, val) <- detectAtomicScatterAddCore body
      pure (prefix, Nothing, arrWrite, idxWrite, val)
  where
    detectAtomicScatterAddCore stmts =
      case reverse stmts of
        ( CFG.SArrayWrite arrWrite idxWrite (AVar newVar)
            : CFG.SAssign newVar' (RBinOp op a b)
            : CFG.SAssign oldVar (RArrayLoad arrRead idxRead)
            : revPrefix
          )
            | newVar == newVar',
              isAtomicAddOp op,
              arrWrite == arrRead,
              idxWrite == idxRead,
              Just val <- nonOldOperand oldVar a b ->
                Just (reverse revPrefix, arrWrite, idxWrite, val)
        _ -> Nothing

    nonOldOperand oldVar a b = case (a, b) of
      (AVar oldV, other) | oldV == oldVar -> Just other
      (other, AVar oldV) | oldV == oldVar -> Just other
      _ -> Nothing

    isAtomicAddOp op = case op of
      CAdd -> True
      CAddF -> True
      _ -> False

atomicScatterElemType :: CFG.ParallelSpec -> Maybe CType
atomicScatterElemType p = case CFG.psStrategy p of
  CFG.ParallelScatterAtomicAddInt -> Just CTInt64
  CFG.ParallelScatterAtomicAddFloat -> Just CTDouble
  _ -> Nothing

genIndexExpr :: CFG.IndexExpr -> Doc
genIndexExpr expr = case CFG.simplifyIndexExpr expr of
  CFG.IVar v -> text (sanitize v)
  CFG.IConst n -> text (show n ++ "LL")
  CFG.IAdd a b -> parens (genIndexExpr a <+> text "+" <+> genIndexExpr b)
  CFG.ISub a b -> parens (genIndexExpr a <+> text "-" <+> genIndexExpr b)
  CFG.IMul a b -> parens (genIndexExpr a <+> text "*" <+> genIndexExpr b)
  CFG.IDiv a b -> parens (genIndexExpr a <+> text "/" <+> genIndexExpr b)
  CFG.INdToFlat nd shp -> text "hyd_nd_to_flat(" <> genIndexExpr nd <> text "," <+> genIndexExpr shp <> text ")"
  CFG.ICall _ _ -> text "0LL"
  _ -> text "0LL"

hasParallelStmt :: [CFG.Stmt] -> Bool
hasParallelStmt = any go
  where
    go st = case st of
      CFG.SLoop spec body -> case CFG.lsExec spec of
        CFG.Parallel _ -> True
        CFG.Workshare _ -> True
        _ -> hasParallelStmt body
      CFG.SParallelRegion body -> hasParallelStmt body
      CFG.SIf _ thn els -> hasParallelStmt thn || hasParallelStmt els
      _ -> False

-- | Returns True when any statement in the body uses a math-library UnOp
-- (CSqrt, CExpF, CLog, etc.) so we can conditionally emit @#include \<math.h\>@.
hasMathOp :: [CFG.Stmt] -> Bool
hasMathOp = any go
  where
    go st = case st of
      CFG.SAssign _ rhs -> rhsHasMath rhs
      CFG.SArrayWrite _ _ a -> atomHasMath a
      CFG.SLoop _ body -> hasMathOp body
      CFG.SIf _ thn els -> hasMathOp thn || hasMathOp els
      _ -> False
    rhsHasMath (RUnOp op _) = isMathOp op
    rhsHasMath _ = False
    atomHasMath _ = False
    isMathOp op = op `elem` [CSqrt, CExpF, CLog, CSin, CCos, CAbsF, CFloorF, CCeilF, CErf]

findReturnVar :: [CFG.Stmt] -> Maybe CVar
findReturnVar [] = Nothing
findReturnVar stmts = case last stmts of
  CFG.SReturn (AVar v) -> Just v
  _ -> Nothing

splitFinalReturn :: [CFG.Stmt] -> ([CFG.Stmt], Maybe Atom)
splitFinalReturn stmts = case reverse stmts of
  (CFG.SReturn a : rest) -> (reverse rest, Just a)
  _ -> (stmts, Nothing)

-- | True when a proc body ends with a direct float-literal return,
-- i.e. the body is just `SReturn (AFloat _)` with no intermediate variable.
-- This handles top-level float constants like `let g = 6.674e-11`.
isDirectFloatReturn :: [CFG.Stmt] -> Bool
isDirectFloatReturn stmts = case stmts of
  [CFG.SReturn (AFloat _)] -> True
  _ -> False

arrayVarsProc :: CFG.Proc -> Set CVar
arrayVarsProc (CFG.Proc {CFG.procBody = body}) = arrayVarsStmts body

localArrayVarsProc :: Map CVar CType -> CFG.Proc -> Set CVar
localArrayVarsProc typeEnv (CFG.Proc {CFG.procBody = body}) =
  rawLocals `S.difference` consumedByArrayPair `S.difference` explicitlyFreedLocals
  where
    rawLocals =
      S.fromList
        [v | CFG.SAssign v rhs <- body, isLocalArray v, ownsArrayHeader rhs]
    isLocalArray v = case Map.lookup v typeEnv of
      Just (CTArray _) -> True
      _ -> False
    ownsArrayHeader rhs = case rhs of
      RArrayAlloc {} -> True
      RArrayCopy {} -> True
      RCall {} -> True
      _ -> False
    -- Arrays passed as CEArray components to RPairMake are owned by the
    -- resulting pair and freed during pair-update in the enclosing loop.
    -- Excluding them here prevents a double-free at the post-return cleanup.
    consumedByArrayPair =
      S.fromList
        [ v
        | CFG.SAssign _ (RPairMake ct1 ct2 a1 a2) <- body,
          (ct, a) <- [(ct1, a1), (ct2, a2)],
          ct == CEArray,
          AVar v <- [a]
        ]
    explicitlyFreedLocals =
      rawLocals `S.intersection` explicitlyFreedArrayVarsStmts body

explicitlyFreedArrayVarsStmts :: [CFG.Stmt] -> Set CVar
explicitlyFreedArrayVarsStmts = foldMap go
  where
    go stmt = case stmt of
      CFG.SAssign "__hyd_discard" (RArrayFree (AVar v)) -> S.singleton v
      CFG.SLoop _ inner -> explicitlyFreedArrayVarsStmts inner
      CFG.SIf _ thn els -> explicitlyFreedArrayVarsStmts thn `S.union` explicitlyFreedArrayVarsStmts els
      _ -> S.empty

-- | Emit frees for the non-returned array component(s) of any pair-of-arrays
-- variable whose one component is the returned atom.  This handles the case
-- where a foldl accumulator of type (Array, Array) has its snd (or fst)
-- extracted as the return value — the other component must be freed to avoid
-- a post-loop heap leak.
discardedArrayPairComponentDoc :: [CFG.Stmt] -> Atom -> Map CVar (CElemType, CElemType) -> Doc
discardedArrayPairComponentDoc stmts retAtom pairVars =
  case retAtom of
    AVar retVar -> vcat (fstFrees ++ sndFrees)
      where
        fstFrees =
          [ text "hyd_array_free(" <> text (sanitize pairVar) <> text ".fst);"
          | CFG.SAssign rv (RPairSnd _ (AVar pairVar)) <- stmts,
            rv == retVar,
            Just (CEArray, _) <- [Map.lookup pairVar pairVars]
          ]
        sndFrees =
          [ text "hyd_array_free(" <> text (sanitize pairVar) <> text ".snd);"
          | CFG.SAssign rv (RPairFst _ (AVar pairVar)) <- stmts,
            rv == retVar,
            Just (_, CEArray) <- [Map.lookup pairVar pairVars]
          ]
    _ -> empty

arrayVarsStmts :: [CFG.Stmt] -> Set CVar
arrayVarsStmts = foldMap arrayVarsStmt

arrayVarsStmt :: CFG.Stmt -> Set CVar
arrayVarsStmt st = case st of
  CFG.SAssign v rhs -> case rhs of
    RArrayAlloc {} -> S.singleton v
    RArrayCopy {} -> S.singleton v
    RCall fn _ | fn == "hyd_read_array_csv" -> S.singleton v
    RCall fn _ | fn == "hyd_read_float_array_csv" -> S.singleton v
    _ -> S.empty
  CFG.SLoop _ body -> arrayVarsStmts body
  CFG.SIf _ thn els -> arrayVarsStmts thn `S.union` arrayVarsStmts els
  _ -> S.empty

cleanupArrayVarsDoc :: Set CVar -> Doc
cleanupArrayVarsDoc vars =
  vcat [text "hyd_array_free(" <> text (sanitize v) <> text ");" | v <- S.toList vars]

tupleVarsProc :: CFG.Proc -> Set CVar
tupleVarsProc (CFG.Proc {CFG.procBody = body}) = tupleVarsStmts body

tupleVarsStmts :: [CFG.Stmt] -> Set CVar
tupleVarsStmts = foldMap tupleVarsStmt

tupleVarsStmt :: CFG.Stmt -> Set CVar
tupleVarsStmt st = case st of
  CFG.SAssign v rhs -> case rhs of
    RTuple {} -> S.singleton v
    RArrayShape {} -> S.singleton v
    RShapeInit {} -> S.singleton v
    RFlatToNd {} -> S.singleton v
    _ -> S.empty
  CFG.SLoop _ body -> tupleVarsStmts body
  CFG.SIf _ thn els -> tupleVarsStmts thn `S.union` tupleVarsStmts els
  _ -> S.empty

-- | Collect the variables that hold pair values and their element types.
pairVarsProc :: CFG.Proc -> Map CVar (CElemType, CElemType)
pairVarsProc (CFG.Proc {CFG.procBody = body}) = pairVarsStmts Map.empty body

-- | Forward-propagating pass: process statements in order, accumulating
-- the map of known pair-typed variables. Propagates pair type through
-- plain @RAtom (AVar src)@ copies (e.g. the foldl accumulator init).
pairVarsStmts :: Map CVar (CElemType, CElemType) -> [CFG.Stmt] -> Map CVar (CElemType, CElemType)
pairVarsStmts known = foldl pairVarsStmt known

pairVarsStmt :: Map CVar (CElemType, CElemType) -> CFG.Stmt -> Map CVar (CElemType, CElemType)
pairVarsStmt known st = case st of
  CFG.SAssign v (RPairMake cty1 cty2 _ _) -> Map.insert v (cty1, cty2) known
  -- Propagate pair type through plain copies (e.g. foldl acc initialisation).
  CFG.SAssign v (RAtom (AVar src))
    | Just tys <- Map.lookup src known -> Map.insert v tys known
  -- RPairFst/RPairSnd carrying a pair CElemType yield another pair variable.
  CFG.SAssign v (RPairFst (CEPair ct1 ct2) _) -> Map.insert v (ct1, ct2) known
  CFG.SAssign v (RPairSnd (CEPair ct1 ct2) _) -> Map.insert v (ct1, ct2) known
  CFG.SLoop _ body -> pairVarsStmts known body
  CFG.SIf _ thn els ->
    let k1 = pairVarsStmts known thn
        k2 = pairVarsStmts known els
     in Map.union k1 k2
  _ -> known

isFloatArithBinOp :: BinOp -> Bool
isFloatArithBinOp CAddF = True
isFloatArithBinOp CSubF = True
isFloatArithBinOp CMulF = True
isFloatArithBinOp CDivF = True
isFloatArithBinOp _ = False

-- | Returns True for UnOps whose result type is always Float (math functions).
isMathFloatOp :: UnOp -> Bool
isMathFloatOp op = op `elem` [CSqrt, CExpF, CLog, CSin, CCos, CAbsF, CFloorF, CCeilF, CErf, CFloatOf]

-- | Collect variable names from an atom (empty for literals).
atomVars :: Atom -> Set CVar
atomVars (AVar v) = S.singleton v
atomVars (AVecVar v) = S.singleton v
atomVars _ = S.empty

isFloatAtom :: Set CVar -> Atom -> Bool
isFloatAtom _ (AFloat _) = True
isFloatAtom fv (AVar v) = v `S.member` fv
isFloatAtom _ _ = False

classifyVarKinds :: Map CVar VarKind -> [CFG.Stmt] -> (Set CVar, Set CVar)
classifyVarKinds retKinds stmts = fixpoint S.empty (initFloatArrays stmts)
  where
    isFloatArrayCall fn =
      fn == "hyd_read_float_array_csv"
        || Map.lookup fn retKinds == Just KFloatArray
    initFloatArrays = foldMap initFloatArray
    initFloatArray st = case st of
      CFG.SAssign v (RCall fn _) | isFloatArrayCall fn -> S.singleton v
      CFG.SLoop _ body -> foldMap initFloatArray body
      CFG.SIf _ thn els -> foldMap initFloatArray thn `S.union` foldMap initFloatArray els
      _ -> S.empty

    fixpoint fv fa =
      let fa' = collectFloatArrayVars fv stmts `S.union` fa
          fv' = collectFloatVars fv fa' stmts
       in if fv' == fv && fa' == fa then (fv, fa) else fixpoint fv' fa'

    collectFloatVars fv fa = foldMap (floatVarStmt fv fa)
    floatVarStmt fv fa st = case st of
      CFG.SAssign v rhs ->
        let isFloat = case rhs of
              RAtom a -> isFloatAtom fv a
              RBinOp op _ _ -> isFloatArithBinOp op
              RUnOp CNeg a -> isFloatAtom fv a
              RUnOp op _ -> isMathFloatOp op
              RArrayLoad (AVar arr) _ -> arr `S.member` fa
              RVecUnOp {} -> True
              RVecReduce {} -> True
              _ -> False
            -- Backward propagation: variables USED in float operations are floats.
            -- This is needed to correctly type function parameters (which are never
            -- assigned in the body, so they can't be discovered by the forward pass).
            -- Note: CFloatOf converts int→float, so its argument stays an int.
            usedAsFloat = case rhs of
              RBinOp op a1 a2
                | isFloatArithBinOp op ->
                    atomVars a1 `S.union` atomVars a2
              RUnOp op a
                | op `elem` [CSqrt, CExpF, CLog, CSin, CCos, CAbsF, CFloorF, CCeilF, CErf, CIntOf] ->
                    atomVars a
              RUnOp CNeg a | isFloatAtom fv a -> atomVars a
              _ -> S.empty
         in (if isFloat then S.singleton v else S.empty) `S.union` usedAsFloat
      CFG.SLoop _ body -> collectFloatVars fv fa body
      CFG.SIf _ thn els -> collectFloatVars fv fa thn `S.union` collectFloatVars fv fa els
      _ -> S.empty

    collectFloatArrayVars fv = foldMap (floatArrayVarStmt fv)
    floatArrayVarStmt fv st = case st of
      CFG.SAssign v (RCall fn _) | isFloatArrayCall fn -> S.singleton v
      CFG.SArrayWrite (AVar arr) _ val | isFloatAtom fv val -> S.singleton arr
      CFG.SLoop _ body -> collectFloatArrayVars fv body
      CFG.SIf _ thn els -> collectFloatArrayVars fv thn `S.union` collectFloatArrayVars fv els
      _ -> S.empty

classifyVecVars :: Int -> [CFG.Stmt] -> Map CVar Int
classifyVecVars defaultW stmts = fixpoint Map.empty stmts
  where
    fixpoint vv body =
      let vv' = collectVecVars vv defaultW body
       in if vv' == vv then vv else fixpoint vv' body

    collectVecVars known w = foldMap (goStmt known w)

    goStmt known w st = case st of
      CFG.SAssign v rhs -> case rhs of
        RVecLoad {} -> Map.singleton v w
        RVecBinOp {} -> Map.singleton v w
        RVecUnOp {} -> Map.singleton v w
        RVecSplat {} -> Map.singleton v w
        RAtom (AVecVar src) | Just sw <- Map.lookup src known -> Map.singleton v sw
        _ -> Map.empty
      CFG.SLoop spec body ->
        let w' = case CFG.lsExec spec of
              CFG.Vector vs -> CFG.vsWidth vs
              _ -> w
         in collectVecVars known w' body
      CFG.SIf _ thn els ->
        collectVecVars known w thn `Map.union` collectVecVars known w els
      _ -> Map.empty

procReturnKinds :: CFG.Program -> Map CVar VarKind
-- Build return-kind map incrementally so callees are known when their callers
-- are classified. Procs are emitted in definition order (callees before callers),
-- so a left fold accumulates enough context by the time each proc is processed.
procReturnKinds prog@(CFG.Program procs) = foldl addProc Map.empty procs
  where
    callTypes = inferProgramReturnTypes prog
    callParamTypes = buildCallParamTypes callTypes procs
    addProc rk proc@(CFG.Proc {CFG.procName = name}) = Map.insert name (retKindOf rk proc) rk
    retKindOf rk proc@(CFG.Proc {CFG.procBody = body}) =
      let recoveredTypeEnv = recoverProcTypeEnv callTypes callParamTypes proc
          (arrVarsTE, tupVarsTE, pairVarsTE, recordVarsTE, floatVarsTE, floatArrVarsTE) =
            -- Use recoveredTypeEnv directly: it corrects stale lowering-time types
            -- (e.g. RPairMake tags that defaulted to CEInt).
            typeEnvToVarSets recoveredTypeEnv
          arrVars = arrVarsTE `S.union` arrayVarsProc proc
          tupVars = tupVarsTE `S.union` tupleVarsProc proc
          pairVars = pairVarsTE `Map.union` pairVarsProc proc
          recordVars = recordVarsTE
          (floatVarsH, floatArrVarsH) = classifyVarKinds rk body
          floatVars = floatVarsTE `S.union` floatVarsH
          floatArrVars = floatArrVarsTE `S.union` floatArrVarsH
       in case findReturnVar body of
            Just rv | rv `S.member` floatArrVars -> KFloatArray
            Just rv | rv `S.member` arrVars -> KArray
            Just rv | rv `S.member` tupVars -> KTuple
            Just rv | Just (ct1, ct2) <- Map.lookup rv pairVars -> KPair ct1 ct2
            Just rv | Just fields <- Map.lookup rv recordVars -> KRecord fields
            Just rv | rv `S.member` floatVars -> KFloat
            Nothing | isDirectFloatReturn body -> KFloat
            _ -> KScalar

-- | Reserved C / POSIX identifiers that would clash with system headers.
cReservedNames :: S.Set String
cReservedNames =
  S.fromList
    [ "main",
      "kill",
      "exit",
      "abort",
      "signal",
      "read",
      "write",
      "open",
      "close",
      "stat",
      "time",
      "wait",
      "link",
      "fork",
      "exec",
      "free",
      "malloc",
      "calloc",
      "realloc",
      "printf",
      "scanf",
      "puts",
      "gets",
      "strlen",
      "strcmp",
      "memcpy",
      "memset",
      "remove",
      "rename"
    ]

sanitize :: CVar -> String
sanitize bs = case BS.unpack bs of
  [] -> "_empty"
  s
    | s `S.member` cReservedNames -> "hyd_" ++ s
    | otherwise -> map (\c -> if isAlphaNum c || c == '_' then c else '_') s

sanitizeExportName :: BS.ByteString -> String
sanitizeExportName bs = case BS.unpack bs of
  [] -> "_empty"
  s -> map (\c -> if isAlphaNum c || c == '_' then c else '_') s

sanitizeFieldName :: BS.ByteString -> String
sanitizeFieldName bs = case sanitize bs of
  [] -> "field"
  s@(c : _)
    | isAlphaNum c || c == '_' -> s
    | otherwise -> '_' : s

cTypeTag :: CType -> String
cTypeTag CTInt64 = "i"
cTypeTag CTDouble = "f"
cTypeTag CTBool = "b"
cTypeTag CTUnit = "u"
cTypeTag CTTuple = "t"
cTypeTag (CTArray elt) = "a" ++ cTypeTag elt
cTypeTag (CTPair t1 t2) = "p" ++ cTypeTag t1 ++ cTypeTag t2
cTypeTag (CTRecord fields) = "r" ++ concatMap (\(field, fieldTy) -> sanitizeFieldName field ++ "_" ++ cTypeTag fieldTy ++ "_") fields
cTypeTag CTUnknown = error "internal error: CTUnknown reached codegen type tag"

cTypeName :: CType -> String
cTypeName CTInt64 = "int64_t"
cTypeName CTDouble = "double"
cTypeName CTBool = "int64_t"
cTypeName CTUnit = "int64_t"
cTypeName CTTuple = "hyd_tuple_t"
cTypeName (CTArray _) = "hyd_array_t*"
cTypeName (CTPair t1 t2)
  | Just et1 <- ctypeToElemType t1,
    Just et2 <- ctypeToElemType t2 =
      pairStructName et1 et2
  | otherwise = error ("internal error: CTPair with non-representable component in codegen: " ++ show t1 ++ ", " ++ show t2)
cTypeName (CTRecord fields) = recordStructName fields
cTypeName CTUnknown = error "internal error: CTUnknown reached codegen (type was not propagated)"

recordStructName :: [(BS.ByteString, CType)] -> String
recordStructName fields = "hyd_record_" ++ concatMap (\(field, fieldTy) -> sanitizeFieldName field ++ "_" ++ cTypeTag fieldTy ++ "_") fields ++ "t"

-- | Letter abbreviation for a CElemType, used in struct name mangling.
-- For nested pairs this recurses: CEPair CEFloat CEInt → "pfi".
celemTypeLetter :: CElemType -> String
celemTypeLetter CEInt = "i"
celemTypeLetter CEFloat = "f"
celemTypeLetter CEBool = "b"
celemTypeLetter (CEPair ct1 ct2) = "p" ++ celemTypeLetter ct1 ++ celemTypeLetter ct2
celemTypeLetter CEArray = "a"

-- | C type string for a CElemType.
celemTypeCType :: CElemType -> String
celemTypeCType CEInt = "int64_t"
celemTypeCType CEFloat = "double"
celemTypeCType CEBool = "int64_t"
celemTypeCType (CEPair ct1 ct2) = pairStructName ct1 ct2
celemTypeCType CEArray = "hyd_array_t*"

-- | Struct typedef name for a pair type, e.g. @hyd_pair_fi_t@.
pairStructName :: CElemType -> CElemType -> String
pairStructName ct1 ct2 = "hyd_pair_" ++ celemTypeLetter ct1 ++ celemTypeLetter ct2 ++ "_t"

-- | Emit a typedef for a pair struct.
genPairStructDef :: CodegenOptions -> CElemType -> CElemType -> Doc
genPairStructDef opts ct1 ct2 =
  text "typedef struct {"
    <+> text (celemTypeCType' opts ct1)
    <+> text "fst;"
    <+> text (celemTypeCType' opts ct2)
    <+> text "snd;"
    <+> text "}"
    <+> text (pairStructName ct1 ct2)
    <> text ";"

-- | C type string for a CElemType, respecting codegenUseFloat.
celemTypeCType' :: CodegenOptions -> CElemType -> String
celemTypeCType' opts CEInt = "int64_t"
celemTypeCType' opts CEFloat = if codegenUseFloat opts then "float" else "double"
celemTypeCType' opts CEBool = "int64_t"
celemTypeCType' opts (CEPair ct1 ct2) = pairStructName ct1 ct2
celemTypeCType' opts CEArray = "hyd_array_t*"

-- | Collect the transitive closure of sub-pair types used by a CElemType.
-- Returns all (ct1, ct2) pairs that need struct definitions, in dependency order
-- (innermost pairs first).
transitivePairTypes :: CElemType -> [(CElemType, CElemType)]
transitivePairTypes (CEPair ct1 ct2) =
  transitivePairTypes ct1 ++ transitivePairTypes ct2 ++ [(ct1, ct2)]
transitivePairTypes _ = []

collectPairTypesFromCType :: CType -> [(CElemType, CElemType)]
collectPairTypesFromCType (CTPair t1 t2)
  | Just et1 <- ctypeToElemType t1,
    Just et2 <- ctypeToElemType t2 =
      transitivePairTypes (CEPair et1 et2)
collectPairTypesFromCType (CTArray elt) = collectPairTypesFromCType elt
collectPairTypesFromCType (CTRecord fields) = concatMap (collectPairTypesFromCType . snd) fields
collectPairTypesFromCType _ = []

-- | Collect all distinct pair kinds used in the program and emit their struct
-- typedefs in dependency order (inner structs before outer structs).
-- Scans the CFG body (for RPairMake sites), the procTypeEnv (for pair
-- parameters/return values that never appear in a make statement), and the
-- recovered type environments (which may include pair types discovered by
-- backward type inference not present at lowering time).
genPairStructDefs :: CodegenOptions -> CFG.Program -> [TypeEnv] -> Doc
genPairStructDefs opts (CFG.Program procs) recoveredEnvs =
  let allPairs = concatMap transitivePairTypes topLevelTypes
      ordered = nubBy (\a b -> fst a == fst b && snd a == snd b) allPairs
   in if null ordered
        then empty
        else text "" $$ vcat (map (uncurry (genPairStructDef opts)) ordered)
  where
    topLevelTypes = concatMap collectPairTypesProc (zip procs recoveredEnvs)
    collectPairKinds (CFG.Proc {CFG.procBody = body, CFG.procTypeEnv = tenv}, recEnv) =
      collectPairKindsStmts body
        `S.union` collectPairKindsTypeEnv tenv
        `S.union` collectPairKindsTypeEnv recEnv
    collectPairTypesProc procEnv = map (\(ct1, ct2) -> CEPair ct1 ct2) (S.toList (collectPairKinds procEnv))
    -- Collect pairs from RPairMake in the body.
    collectPairKindsStmts = foldMap collectPairKindsStmt
    collectPairKindsStmt st = case st of
      CFG.SAssign _ (RPairMake ct1 ct2 _ _) -> S.singleton (ct1, ct2)
      CFG.SLoop _ body -> collectPairKindsStmts body
      CFG.SIf _ thn els -> collectPairKindsStmts thn `S.union` collectPairKindsStmts els
      _ -> S.empty
    -- Collect pairs from the type environment (covers pair params/returns).
    collectPairKindsTypeEnv tenv =
      S.fromList
        [ pairTy
        | (_, ct) <- Map.toList tenv,
          pairTy <- collectPairTypesFromCType ct
        ]

transitiveRecordTypes :: CType -> [[(BS.ByteString, CType)]]
transitiveRecordTypes (CTRecord fields) =
  concatMap (transitiveRecordTypes . snd) fields ++ [fields]
transitiveRecordTypes (CTArray elt) = transitiveRecordTypes elt
transitiveRecordTypes (CTPair t1 t2) = transitiveRecordTypes t1 ++ transitiveRecordTypes t2
transitiveRecordTypes _ = []

genRecordStructDef :: [(BS.ByteString, CType)] -> Doc
genRecordStructDef fields =
  text "typedef struct {"
    $$ nest 2 (vcat [text (cTypeName fieldTy) <+> text (sanitizeFieldName field) <> text ";" | (field, fieldTy) <- fields])
    $$ text "}"
    <+> text (recordStructName fields)
    <> text ";"

genRecordStructDefs :: CFG.Program -> Doc
genRecordStructDefs (CFG.Program procs) =
  let allRecords = concatMap collectProcRecords procs
      ordered = nub allRecords
   in if null ordered then empty else text "" $$ vcat (map genRecordStructDef ordered)
  where
    collectProcRecords (CFG.Proc {CFG.procTypeEnv = tenv}) =
      concatMap transitiveRecordTypes (Map.elems tenv)

isStructuredPrintableArrayElem :: CType -> Bool
isStructuredPrintableArrayElem ct = case ct of
  CTPair {} -> True
  CTRecord {} -> True
  _ -> False

structuredArrayPrintName :: CType -> String
structuredArrayPrintName eltTy = "hyd_print_array_" ++ cTypeTag eltTy

genStructuredArrayPrintDefs :: Map CVar CType -> Doc
genStructuredArrayPrintDefs retTypes =
  let arrayElts =
        nub
          [ eltTy
          | (_, CTArray eltTy) <- Map.toList retTypes,
            isStructuredPrintableArrayElem eltTy
          ]
   in if null arrayElts then empty else text "" $$ vcat (map genStructuredArrayPrintDef arrayElts)

genStructuredArrayPrintDef :: CType -> Doc
genStructuredArrayPrintDef eltTy =
  let fnName = structuredArrayPrintName eltTy
      eltName = cTypeName eltTy
   in text "static void"
        <+> text fnName
        <> parens (text "hyd_array_t* arr")
        <+> text "{"
        $$ nest
          4
          ( text "int64_t size = hyd_shape_size(arr->shape);"
              $$ text "printf(\"[\");"
              $$ text "for (int64_t i = 0; i < size; i++) {"
              $$ nest
                4
                ( text "if (i > 0) printf(\", \");"
                    $$ text eltName
                    <+> text "v = ((("
                    <> text eltName
                    <> text "*)(void*)arr->data)[i]);"
                    $$ genPrintValue eltTy (text "v")
                )
              $$ text "}"
              $$ text "printf(\"] (shape: [\");"
              $$ text "for (int i = 0; i < arr->shape.ndims; i++) {"
              $$ nest
                4
                ( text "if (i > 0) printf(\", \");"
                    $$ text "printf(\"%lld\", (long long)arr->shape.elems[i]);"
                )
              $$ text "}"
              $$ text "printf(\"])\\n\");"
          )
        $$ text "}"

genPrintValue :: CType -> Doc -> Doc
genPrintValue ct expr = case ct of
  CTInt64 -> text "printf(\"%lld\", (long long)(" <> expr <> text "));"
  CTBool -> text "printf(\"%lld\", (long long)(" <> expr <> text "));"
  CTUnit -> text "printf(\"%lld\", (long long)(" <> expr <> text "));"
  CTDouble -> text "printf(\"%.17g\", (double)(" <> expr <> text "));"
  CTPair _ _ ->
    text "printf(\"(\");"
      $$ genPrintValuePairFst ct expr
      $$ text "printf(\", \");"
      $$ genPrintValuePairSnd ct expr
      $$ text "printf(\")\");"
  CTRecord fields ->
    text "printf(\"{\");"
      $$ vcat
        [ (if ix == 0 then empty else text "printf(\", \");")
            $$ text "printf(\""
            <> text (sanitizeFieldName field ++ " = ")
            <> text "\");"
            $$ genPrintValue fieldTy (expr <> text "." <> text (sanitizeFieldName field))
        | (ix, (field, fieldTy)) <- zip [0 :: Int ..] fields
        ]
      $$ text "printf(\"}\");"
  _ -> text "printf(\"<unprintable>\");"
  where
    genPrintValuePairFst (CTPair t1 _) base = genPrintValue t1 (base <> text ".fst")
    genPrintValuePairFst _ _ = text "printf(\"<unprintable>\");"
    genPrintValuePairSnd (CTPair _ t2) base = genPrintValue t2 (base <> text ".snd")
    genPrintValuePairSnd _ _ = text "printf(\"<unprintable>\");"

vecTypeName :: Int -> String
vecTypeName 4 = "hyd_float64x4_t"
vecTypeName _ = "hyd_float64x2_t"

vecFuncSuffix :: Int -> String
vecFuncSuffix 4 = "_f64x4"
vecFuncSuffix _ = "_f64"

varDecl :: CodegenEnv -> CVar -> RHS -> Doc
varDecl env v rhs
  | v `S.member` ceArrVars env = text "hyd_array_t*"
  | v `S.member` ceTupVars env = text "hyd_tuple_t"
  | Just (ct1, ct2) <- Map.lookup v (cePairVars env) = text (pairStructName ct1 ct2)
  | Just fields <- Map.lookup v (ceRecordVars env) = text (recordStructName fields)
  | Just w <- Map.lookup v (ceVecVars env) = text (vecTypeName w)
  | v `S.member` ceFloatVars env = text "double"
  | otherwise = case rhs of
      RAtom (AVar src)
        | src `S.member` ceArrVars env -> text "hyd_array_t*"
        | src `S.member` ceTupVars env -> text "hyd_tuple_t"
        | Just (ct1, ct2) <- Map.lookup src (cePairVars env) -> text (pairStructName ct1 ct2)
        | Just fields <- Map.lookup src (ceRecordVars env) -> text (recordStructName fields)
        | Just w <- Map.lookup src (ceVecVars env) -> text (vecTypeName w)
        | src `S.member` ceFloatVars env -> text "double"
      RAtom (AVecVar src) -> text (vecTypeName (Map.findWithDefault defaultVectorWidth src (ceVecVars env)))
      RArrayAlloc {} -> text "hyd_array_t*"
      RArrayCopy {} -> text "hyd_array_t*"
      RArrayShape {} -> text "hyd_tuple_t"
      RShapeInit {} -> text "hyd_tuple_t"
      RTuple {} -> text "hyd_tuple_t"
      RFlatToNd {} -> text "hyd_tuple_t"
      RVecLoad {} -> text (vecTypeName defaultVectorWidth)
      RVecBinOp {} -> text (vecTypeName defaultVectorWidth)
      RVecUnOp {} -> text (vecTypeName defaultVectorWidth)
      RVecSplat {} -> text (vecTypeName defaultVectorWidth)
      RVecReduce {} -> text "double"
      RCall fn _ | fn == "hyd_slice_shape" -> text "hyd_tuple_t"
      RCall fn _ -> case Map.lookup fn (ceRetKinds env) of
        Just KArray -> text "hyd_array_t*"
        Just KFloatArray -> text "hyd_array_t*"
        Just KTuple -> text "hyd_tuple_t"
        Just KFloat -> text "double"
        Just (KPair ct1 ct2) -> text (pairStructName ct1 ct2)
        Just (KRecord fields) -> text (recordStructName fields)
        _ -> text "int64_t"
      RArrayFree{} -> text "void"
      RPairMake ct1 ct2 _ _ -> text (pairStructName ct1 ct2)
      RPairFst ct _ -> text (celemTypeCType' (ceOpts env) ct)
      RPairSnd ct _ -> text (celemTypeCType' (ceOpts env) ct)
      _ -> text "int64_t"

-- | Collect all RTuple definitions from a statement list (pre-pass).
-- Used for inlining RNdToFlat with known-arity tuples.
collectTupleDefs :: [CFG.Stmt] -> Map CVar [Atom]
collectTupleDefs = foldMap go
  where
    go (CFG.SAssign v (RTuple atoms)) = Map.singleton v atoms
    go (CFG.SLoop _ body) = collectTupleDefs body
    go (CFG.SIf _ thn els) = collectTupleDefs thn `Map.union` collectTupleDefs els
    go _ = Map.empty

genRHS :: Map CVar CType -> Set CVar -> Maybe Int -> Map CVar [Atom] -> RHS -> Doc
genRHS _ _ _ _ (RAtom a) = genAtom a
genRHS _ _ _ _ (RBinOp op a1 a2) = parens (genAtom a1 <+> genBinOp op <+> genAtom a2)
genRHS _ _ _ _ (RUnOp op a) = case op of
  CNot -> parens (text "!" <> genAtom a)
  CNeg -> parens (text "-" <> genAtom a)
  _ -> genUnOp op <> parens (genAtom a) -- math function call: f(arg)
-- Emit a compound struct literal instead of hyd_tuple_make() so GCC sees
-- no memory-clobbering side effects.  Dead tuple assignments then become
-- pure stack stores that GCC's DCE eliminates, allowing the surrounding
-- #pragma omp simd loop to be vectorized.
genRHS _ _ _ _ (RTuple atoms) =
  text "(hyd_tuple_t){.ndims="
    <> int (length atoms)
    <> (if null atoms then empty else text ",.elems={" <> hsep (punctuate (text ",") (map (\a -> text "(int64_t)" <> genAtom a) atoms)) <> text "}")
    <> text "}"
genRHS _ _ _ _ (RProj i a) = genAtom a <> text ".elems[" <> integer i <> text "]"
genRHS _ _ _ _ (RRecord fields) =
  text "{"
    <> hsep (punctuate comma [text "." <> text (sanitizeFieldName field) <+> text "=" <+> genAtom atom | (field, atom) <- fields])
    <> text "}"
genRHS _ _ _ _ (RRecordProj field a) = genAtom a <> text "." <> text (sanitizeFieldName field)
genRHS _ _ _ _ (RArrayAlloc shp) = text "hyd_array_alloc(" <> genAtom shp <> text ")"
genRHS _ _ _ _ (RArrayCopy src) = text "hyd_array_alloc_copy(" <> genAtom src <> text ")"
genRHS arrayElemTypes fa _ _ (RArrayLoad arr idx)
  | AVar v <- arr, Just eltTy <- Map.lookup v arrayElemTypes = genArrayAccess eltTy arr idx
  | AVar v <- arr, v `S.member` fa = text "hyd_array_get_float(" <> genAtom arr <> text "," <+> genAtom idx <> text ")"
  | otherwise = genAtom arr <> text "->data[" <> genAtom idx <> text "]"
genRHS _ _ _ _ (RArrayShape arr) = genAtom arr <> text "->shape"
genRHS _ _ _ _ (RShapeSize shp) = text "hyd_shape_size(" <> genAtom shp <> text ")"
genRHS _ _ _ _ (RShapeInit shp) = text "hyd_shape_init(" <> genAtom shp <> text ")"
genRHS _ _ _ _ (RShapeLast shp) = text "hyd_shape_last(" <> genAtom shp <> text ")"
genRHS _ _ _ _ (RFlatToNd flat shp) = text "hyd_flat_to_nd(" <> genAtom flat <> text "," <+> genAtom shp <> text ")"
genRHS _ _ _ _ (RNdToFlat AUnit _) = text "0LL" -- 0D index always maps to flat position 0
genRHS _ _ _ tupleDefs (RNdToFlat (AVar nd) shp)
  | Just atoms <- Map.lookup nd tupleDefs,
    length atoms >= 2 =
      let shpStr = genAtom shp
          n = length atoms
          elemStr i = text "(long)(" <> genAtom (atoms !! i) <> text ")"
          dimStr i = shpStr <> text ".elems[" <> text (show i) <> text "]"
          go acc i
            | i >= n = acc
            | otherwise = go (text "((" <> acc <> text ") * " <> dimStr i <+> text "+" <+> elemStr i <> text ")") (i + 1)
       in go (elemStr 0) 1
genRHS _ _ _ _ (RNdToFlat nd shp) = text "hyd_nd_to_flat(" <> genAtom nd <> text "," <+> genAtom shp <> text ")"
genRHS _ _ _ _ (R2DToFlat i w) = genAtom i <+> text "*" <+> genAtom w
genRHS _ _ _ _ (RCall fn args)
  | fn == "hyd_slice_shape" = text "hyd_slice_shape(" <> genArgs <> text ")"
  | otherwise = text (sanitize fn) <> parens genArgs
  where
    genArgs = hsep (punctuate comma (map genAtom args))
genRHS _ _ mw _ (RVecLoad arr idx) =
  text ("hyd_vec_loadu" ++ vecFuncSuffix w ++ "(((double*)(void*)")
    <> genAtom arr
    <> text "->data) + "
    <> genAtom idx
    <> text ")"
  where
    w = fromMaybe defaultVectorWidth mw
genRHS _ _ mw _ (RVecStore arr idx val) =
  text ("hyd_vec_storeu" ++ vecFuncSuffix w ++ "(((double*)(void*)")
    <> genAtom arr
    <> text "->data) + "
    <> genAtom idx
    <> text ", "
    <> genAtom val
    <> text ")"
  where
    w = fromMaybe defaultVectorWidth mw
genRHS _ _ mw _ (RVecBinOp op v1 v2) =
  text "hyd_vec_"
    <> genVecBinOp op
    <> text (vecFuncSuffix w)
    <> text "("
    <> genAtom v1
    <> text ", "
    <> genAtom v2
    <> text ")"
  where
    w = fromMaybe defaultVectorWidth mw
genRHS _ _ mw _ (RVecUnOp op v) =
  text "hyd_vec_"
    <> genVecUnOp op
    <> text (vecFuncSuffix w)
    <> text "("
    <> genAtom v
    <> text ")"
  where
    w = fromMaybe defaultVectorWidth mw
genRHS _ _ mw _ (RVecSplat a) =
  text ("hyd_vec_set1" ++ vecFuncSuffix w ++ "(") <> genAtom a <> text ")"
  where
    w = fromMaybe defaultVectorWidth mw
genRHS _ _ mw _ (RVecReduce op v) =
  text "hyd_vec_reduce_"
    <> genVecBinOp op
    <> text (vecFuncSuffix w)
    <> text "("
    <> genAtom v
    <> text ")"
  where
    w = fromMaybe defaultVectorWidth mw
genRHS _ _ _ _ (RPairMake _ _ a1 a2) =
  text "{.fst =" <+> genAtom a1 <> text ", .snd =" <+> genAtom a2 <> text "}"
genRHS _ _ _ _ (RPairFst _ a) = genAtom a <> text ".fst"
genRHS _ _ _ _ (RPairSnd _ a) = genAtom a <> text ".snd"
genRHS _ _ _ _ (RArrayFree a) = text "hyd_array_free(" <> genAtom a <> text ")"

genArrayAllocExpr :: Map CVar CType -> CVar -> Atom -> Doc
genArrayAllocExpr arrayElemTypes arrVar shp =
  case Map.lookup arrVar arrayElemTypes of
    Just eltTy ->
      text "hyd_array_alloc_bytes(" <> genAtom shp <> text ", sizeof(" <> text (cTypeName eltTy) <> text "))"
    Nothing ->
      text "hyd_array_alloc(" <> genAtom shp <> text ")"

genArrayAccess :: CType -> Atom -> Atom -> Doc
genArrayAccess eltTy arr idx =
  text "(((" <> text (cTypeName eltTy) <> text "*)(void*)" <> genAtom arr <> text "->data)[" <> genAtom idx <> text "])"

inferArrayElemTypesFromStmts :: Map CVar CType -> [CFG.Stmt] -> Map CVar CType -> Map CVar CType
inferArrayElemTypesFromStmts retTypes stmts initial = go initial
  where
    go known =
      let known' = foldl step known stmts
       in if known' == known then known else go known'

    step known stmt = case stmt of
      CFG.SAssign v (RCall fn _)
        | Just (CTArray eltTy) <- Map.lookup fn retTypes -> Map.insert v eltTy known
      CFG.SAssign v (RAtom (AVar src))
        | Just eltTy <- Map.lookup src known -> Map.insert v eltTy known
      CFG.SAssign v (RArrayCopy (AVar src))
        | Just eltTy <- Map.lookup src known -> Map.insert v eltTy known
      CFG.SLoop _ body -> inferArrayElemTypesFromStmts retTypes body known
      CFG.SIf _ thn els ->
        let knownThn = inferArrayElemTypesFromStmts retTypes thn known
            knownEls = inferArrayElemTypesFromStmts retTypes els known
         in Map.union knownThn knownEls
      _ -> known

collectMissingArrayElemVars :: Map CVar CType -> CallParamTypes -> [CFG.Proc] -> [String]
collectMissingArrayElemVars retTypes callParamTypes procs =
  map BS.unpack (S.toList (foldMap missingForProc procs))
  where
    missingForProc proc =
      let typeEnv = recoverProcTypeEnv retTypes callParamTypes proc
          arrayElemTypes = inferArrayElemTypesFromStmts retTypes (CFG.procBody proc) (Map.fromList [(v, eltTy) | (v, CTArray eltTy) <- Map.toList typeEnv])
       in missingArrayVarsInStmts arrayElemTypes (CFG.procBody proc)

    missingArrayVarsInStmts known = foldMap go
      where
        go stmt = case stmt of
          CFG.SAssign _ (RArrayLoad (AVar arr) _) ->
            if Map.member arr known then S.empty else S.singleton arr
          CFG.SAssign _ (RVecLoad (AVar arr) _) ->
            if Map.member arr known then S.empty else S.singleton arr
          CFG.SAssign _ (RVecStore (AVar arr) _ _) ->
            if Map.member arr known then S.empty else S.singleton arr
          CFG.SArrayWrite (AVar arr) _ _ ->
            if Map.member arr known then S.empty else S.singleton arr
          CFG.SLoop _ body -> missingArrayVarsInStmts known body
          CFG.SIf _ thn els -> missingArrayVarsInStmts known thn `S.union` missingArrayVarsInStmts known els
          _ -> S.empty

genAtom :: Atom -> Doc
genAtom (AVar v) = text (sanitize v)
genAtom (AInt n) = text (show n ++ "LL")
genAtom (AFloat f) = text (show f)
genAtom (ABool True) = text "1"
genAtom (ABool False) = text "0"
genAtom AUnit = text "0"
genAtom (AString s) = doubleQuotes (text (BS.unpack s))
genAtom (AVecVar v) = text (sanitize v)

genBinOp :: BinOp -> Doc
genBinOp CAdd = text "+"
genBinOp CSub = text "-"
genBinOp CMul = text "*"
genBinOp CDiv = text "/"
genBinOp CMod = text "%"
genBinOp CEq = text "=="
genBinOp CNeq = text "!="
genBinOp CLt = text "<"
genBinOp CLe = text "<="
genBinOp CGt = text ">"
genBinOp CGe = text ">="
genBinOp CAnd = text "&&"
genBinOp COr = text "||"
genBinOp CAddF = text "+"
genBinOp CSubF = text "-"
genBinOp CMulF = text "*"
genBinOp CDivF = text "/"
genBinOp CEqF = text "=="
genBinOp CNeqF = text "!="
genBinOp CLtF = text "<"
genBinOp CLeF = text "<="
genBinOp CGtF = text ">"
genBinOp CGeF = text ">="

genVecBinOp :: BinOp -> Doc
genVecBinOp CAdd = text "add"
genVecBinOp CSub = text "sub"
genVecBinOp CMul = text "mul"
genVecBinOp CDiv = text "div"
genVecBinOp CAddF = text "add"
genVecBinOp CSubF = text "sub"
genVecBinOp CMulF = text "mul"
genVecBinOp CDivF = text "div"
genVecBinOp _ = text "add"

genVecUnOp :: UnOp -> Doc
genVecUnOp CSqrt = text "sqrt"
genVecUnOp CExpF = text "exp"
genVecUnOp CLog = text "log"
genVecUnOp CErf = text "erf"
genVecUnOp _ = text "sqrt"

genUnOp :: UnOp -> Doc
genUnOp CNot = text "!"
genUnOp CNeg = text "-"
genUnOp CSqrt = text "sqrt"
genUnOp CExpF = text "exp"
genUnOp CLog = text "log"
genUnOp CSin = text "sin"
genUnOp CCos = text "cos"
genUnOp CAbsF = text "fabs"
genUnOp CFloorF = text "floor"
genUnOp CCeilF = text "ceil"
genUnOp CErf = text "erf"
genUnOp CFloatOf = text "(double)"
genUnOp CIntOf = text "(int64_t)"

-- | Collect all variables assigned anywhere in a list of statements,
-- including inside nested SIf and SLoop bodies. Used to hoist variable
-- declarations before if-blocks to avoid C scoping issues.
allAssignedVars :: [CFG.Stmt] -> Set CVar
allAssignedVars = foldMap go
  where
    go (CFG.SAssign v _) = S.singleton v
    go (CFG.SIf _ thn els) = allAssignedVars thn `S.union` allAssignedVars els
    go (CFG.SLoop _ body) = allAssignedVars body
    go _ = S.empty

assignedRHSMap :: [CFG.Stmt] -> Map CVar RHS
assignedRHSMap = foldr go Map.empty
  where
    go stmt acc = case stmt of
      CFG.SAssign v rhs -> Map.insertWith (\_ existing -> existing) v rhs acc
      CFG.SIf _ thn els -> assignedRHSMap thn `Map.union` assignedRHSMap els `Map.union` acc
      CFG.SLoop _ body -> assignedRHSMap body `Map.union` acc
      _ -> acc
