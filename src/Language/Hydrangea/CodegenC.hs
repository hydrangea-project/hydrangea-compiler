{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.CodegenC
--
-- Native C code generation backend for CFG.
module Language.Hydrangea.CodegenC
  ( codegenProgram2,
    codegenProgram2Prune,
    codegenProgram2WithOptions,
    codegenProgram2WithOptionsPrune,
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
    classifyVarKinds2,
    arrayVarsProc2,
    tupleVarsProc2,
    pairVarsProc2,
    inferArrayElemTypesFromStmts,
    detectAtomicScatterAddLoop,
    procReturnKinds2,
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
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Language.Hydrangea.CFG qualified as C2
import Language.Hydrangea.CFGAnalysis (usedVarsAtom2, usedVarsStmts2)
import Language.Hydrangea.CFGCore (Atom (..), BinOp (..), CElemType (..), CType (..), CVar, RHS (..), Redop (..), UnOp (..), ctypeToElemType)
import Language.Hydrangea.CFGTyping (CallParamTypes, TypeEnv, buildCallParamTypes, inferProgramReturnTypes2, recoverProcTypeEnv2)
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
    ceArrayElemTypes :: Map CVar CType,
    ceArrVars :: Set CVar,
    ceTupVars :: Set CVar,
    cePairVars :: Map CVar (CElemType, CElemType),
    ceRecordVars :: Map CVar [(BS.ByteString, CType)],
    ceFloatVars :: Set CVar,
    ceFloatArrVars :: Set CVar,
    ceVecVars :: Map CVar Int,
    ceTupleDefs :: Map CVar [Atom]
  }

-- | Derive the classification sets/maps used by genStmts2 from a type environment.
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
codegenProgram2 :: C2.Program -> String
codegenProgram2 prog =
  codegenSource $
    case codegenProgram2WithOptions defaultCodegenOptions prog of
      Right artifacts -> artifacts
      Left err -> error err

-- | Codegen with optional dead-proc pruning. When prune=True, remove
-- top-level procs that are not reachable from zero-arg entry procs via
-- direct RCall edges. If the program uses indirect calls via "__apply",
-- pruning is skipped conservatively.
codegenProgram2Prune :: Bool -> C2.Program -> String
codegenProgram2Prune prune prog =
  codegenSource $
    case codegenProgram2WithOptionsPrune defaultCodegenOptions prune prog of
      Right artifacts -> artifacts
      Left err -> error err

codegenProgram2WithOptions :: CodegenOptions -> C2.Program -> Either String CodegenArtifacts
codegenProgram2WithOptions opts = codegenProgram2WithOptionsPrune opts False

codegenProgram2WithOptionsPrune :: CodegenOptions -> Bool -> C2.Program -> Either String CodegenArtifacts
codegenProgram2WithOptionsPrune opts prune prog = do
  let prepared = pruneProgram prune prog
  genProgram2 opts prepared

pruneProgram :: Bool -> C2.Program -> C2.Program
pruneProgram False prog = prog
pruneProgram True prog@(C2.Program procs) =
  if usesApply then prog else C2.Program keptProcs
  where
    -- detect any use of the dynamic apply operator; if present, skip pruning
    usesApply = any procUsesApply procs

    procUsesApply (C2.Proc {C2.procBody = body}) = any stmtUsesApply body
    stmtUsesApply st = case st of
      C2.SAssign _ rhs -> rhsUsesApply rhs
      C2.SLoop _ body' -> any stmtUsesApply body'
      C2.SIf _ thn els -> any stmtUsesApply thn || any stmtUsesApply els
      _ -> False
    rhsUsesApply (RCall fn _) = fn == "__apply"
    rhsUsesApply _ = False

    -- build direct call graph (ignore __apply as handled above)
    callEdges = Map.fromList [(C2.procName p, calledByProc p) | p <- procs]

    calledByProc (C2.Proc {C2.procBody = body}) = foldMap stmtCalledNames body

    stmtCalledNames st = case st of
      C2.SAssign _ rhs -> rhsCalledNames rhs
      C2.SLoop _ body' -> foldMap stmtCalledNames body'
      C2.SIf _ thn els -> foldMap stmtCalledNames thn `S.union` foldMap stmtCalledNames els
      _ -> S.empty

    rhsCalledNames (RCall fn _) = if fn == "__apply" then S.empty else S.singleton fn
    rhsCalledNames _ = S.empty

    -- roots: zero-arg procs (entry points)
    rootNames = S.fromList [C2.procName p | p <- procs, null (C2.procParams p)]

    -- traverse call graph to find reachable procs
    reachable = closure callEdges rootNames

    keptProcs = [p | p <- procs, C2.procName p `S.member` reachable]

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

genProgram2 :: CodegenOptions -> C2.Program -> Either String CodegenArtifacts
genProgram2 opts prog@(C2.Program procs) = do
  let retKinds = procReturnKinds2 prog
      retTypes = inferProgramReturnTypes2 prog
      callParamTypes = buildCallParamTypes retTypes procs
      missingArrayElemVars = collectMissingArrayElemVars retTypes callParamTypes procs
      needsOmp = any (\(C2.Proc {C2.procBody = body}) -> hasParallelStmt2 body) procs
      needsMath = any (\(C2.Proc {C2.procBody = body}) -> hasMathOp body) procs
      ompInclude = if needsOmp then text "#include <omp.h>" else empty
      mathInclude = if needsMath then text "#include <math.h>" else empty
      mBench = codegenBenchmark opts
      timeInclude = case mBench of
        Nothing -> empty
        Just _ -> text "#include <time.h>"
      recoveredEnvs = map (recoverProcTypeEnv2 retTypes callParamTypes) procs
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
          zeroArgProcNames = [C2.procName p | p <- procs, null (C2.procParams p)]
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
          $$ vcat (punctuate (text "") (map (genProc2 opts retKinds retTypes callParamTypes) procs))
          $$ maybe empty (\spec -> text "" $$ genExportWrapper spec) exportSpec
          $$ if codegenEmitMain opts
            then text "" $$ genMain2 mBench retKinds retTypes procs
            else empty
      headerDoc = fmap (genHeader pairStructs recordStructs) exportSpec
  pure
    CodegenArtifacts
      { codegenSource = render sourceDoc,
        codegenHeader = render <$> headerDoc
      }

resolveExportSpec :: Map CVar VarKind -> Map CVar CType -> CallParamTypes -> [C2.Proc] -> CVar -> Either String ExportSpec
resolveExportSpec retKinds retTypes callParamTypes procs kernelName =
  case filter (\proc -> C2.procName proc == kernelName) procs of
    [] ->
      Left $ "Unknown exported kernel: " ++ BS.unpack kernelName
    proc : _ ->
      let typeEnv = recoverProcTypeEnv2 retTypes callParamTypes proc
          (arrVarsTE, tupVarsTE, pairVarsTE, recordVarsTE, floatVarsTE, _) = typeEnvToVarSets typeEnv
          resolveParamType p
            | p `S.member` floatVarsTE = "double"
            | p `S.member` arrVarsTE = "hyd_array_t*"
            | p `S.member` tupVarsTE = "hyd_tuple_t"
            | Just (ct1, ct2) <- Map.lookup p pairVarsTE = pairStructName ct1 ct2
            | Just fields <- Map.lookup p recordVarsTE = recordStructName fields
            | otherwise = "int64_t"
          params = [(sanitize p, resolveParamType p) | p <- C2.procParams proc]
       in pure
            ExportSpec
              { exportKernelName = kernelName,
                exportProcName = sanitize (C2.procName proc),
                exportWrapperName = "hyd_export_" ++ sanitizeExportName kernelName,
                exportReturnType = procReturnTypeName retKinds retTypes (C2.procName proc),
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

genMain2 :: Maybe BenchmarkConfig -> Map CVar VarKind -> Map CVar CType -> [C2.Proc] -> Doc
genMain2 mBench retKinds retTypes procs =
  text "int main(void) {"
    $$ nest 4 body
    $$ nest 4 (text "return 0;")
    $$ text "}"
  where
    zeroArgProcs = filter (\(C2.Proc {C2.procParams = params}) -> null params) procs

    body = case mBench of
      Nothing -> vcat (map callAndPrint zeroArgProcs)
      Just bc ->
        let benchName = bcKernelName bc
            (before, benchAndAfter) = break (\p -> C2.procName p == benchName) zeroArgProcs
         in case benchAndAfter of
              -- Fallback if kernel not found among zero-arg procs (should have been caught earlier).
              [] -> vcat (map callAndPrint zeroArgProcs)
              (benchProc : after) ->
                vcat (map callAndPrint before)
                  $$ benchScaffold bc benchProc
                  $$ vcat (map callAndPrint after)

    -- Generate warmup + measure loops + stats report + final call-and-print.
    benchScaffold bc proc =
      let name = C2.procName proc
          cName = sanitize name
          bsName = BS.unpack name
          warmupStr = show (bcWarmupIters bc)
          itersStr = show (bcMeasureIters bc)
          isArrayRet = case Map.lookup name retTypes of
            Just (CTArray _) -> True
            _ -> case Map.findWithDefault KScalar name retKinds of
              KArray -> True
              KFloatArray -> True
              _ -> False
          benchCacheVar = "__bench_cache_" ++ cName
       in -- Warmup loop
          text ("for (int __bench_w = 0; __bench_w < " ++ warmupStr ++ "; __bench_w++) {")
            $$ nest 4 (text ("(void) " ++ cName ++ "();"))
            $$ text "}"
            -- Measurement loop
            $$ text ("double __bench_times[" ++ itersStr ++ "];")
            $$ text ("for (int __bench_i = 0; __bench_i < " ++ itersStr ++ "; __bench_i++) {")
            $$ nest
              4
              ( text "struct timespec __ts0, __ts1;"
                  $$ text "clock_gettime(CLOCK_MONOTONIC, &__ts0);"
                  $$ text ("(void) " ++ cName ++ "();")
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

    callAndPrint (C2.Proc {C2.procName = name}) =
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

genProc2 :: CodegenOptions -> Map CVar VarKind -> Map CVar CType -> CallParamTypes -> C2.Proc -> Doc
genProc2 opts retKinds retTypes callParamTypes proc@(C2.Proc {C2.procName = name, C2.procParams = params, C2.procBody = body}) =
  let mBench = codegenBenchmark opts
      typeEnv = recoverProcTypeEnv2 retTypes callParamTypes proc
      -- Authoritative classification from the recovered type environment.
      (arrVarsTE, tupVarsTE, pairVarsTE, recordVarsTE, floatVarsTE, floatArrVarsTE) = typeEnvToVarSets typeEnv
      arrayElemTypes = inferArrayElemTypesFromStmts retTypes body (Map.fromList [(v, eltTy) | (v, CTArray eltTy) <- Map.toList typeEnv])
      -- Fallback heuristic classification for optimizer-introduced temporaries
      -- not present in the type env.
      arrVarsH = arrayVarsProc2 proc
      tupVarsH = tupleVarsProc2 proc
      pairVarsH = pairVarsProc2 proc
      (floatVarsH, floatArrVarsH) = classifyVarKinds2 retKinds body
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
      useMemo = null params && retType == "hyd_array_t*" && not isBenchKernel
      cacheVar = text "__cache_" <> text cName
      benchCacheVar = text "__bench_cache_" <> text cName
      (bodyWithoutReturn, finalReturn) = splitFinalReturn body
      localArrayVars = localArrayVarsProc2 typeEnv proc
      vecVars = classifyVecVars (codegenSimdWidth opts) body
      tupleDefs = collectTupleDefs body
      env =
        CodegenEnv
          { ceOpts = opts,
            ceRetKinds = retKinds,
            ceArrayElemTypes = arrayElemTypes,
            ceArrVars = arrVars,
            ceTupVars = tupVars,
            cePairVars = pairVars,
            ceRecordVars = recordVars,
            ceFloatVars = floatVars,
            ceFloatArrVars = floatArrVars,
            ceVecVars = vecVars,
            ceTupleDefs = tupleDefs
          }
      returnedArrayVars retAtom =
        case retAtom of
          AVar v -> case Map.lookup v typeEnv of
            Just (CTArray _) -> S.singleton v
            _ -> S.empty
          _ -> S.empty
      cleanupDoc retAtom =
        let varsToFree =
              case procRetKind of
                KArray -> localArrayVars `S.difference` returnedArrayVars retAtom
                KFloatArray -> localArrayVars `S.difference` returnedArrayVars retAtom
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
            genStmts2 env S.empty (usedVarsAtom2 retAtom) bodyWithoutReturn
              $$ cleanupDoc retAtom
              $$ cacheVar
              <+> text "="
              <+> genAtom retAtom
              <> text ";"
          Nothing ->
            genStmts2 env S.empty S.empty body
      procBodyNormal =
        case finalReturn of
          Just retAtom ->
            genStmts2 env S.empty (usedVarsAtom2 retAtom) bodyWithoutReturn
              $$ cleanupDoc retAtom
              $$ returnDoc retAtom
          Nothing ->
            genStmts2 env S.empty S.empty body
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
            else
              text retType
                <+> text cName
                <> parens cParams
                <+> text "{"
                $$ nest 4 procBodyNormal
                $$ text "}"

genStmts2 :: CodegenEnv -> Set CVar -> Set CVar -> [C2.Stmt] -> Doc
genStmts2 env declared liveAfter stmts =
  case stmts of
    [] -> empty
    stmt : rest ->
      let stmtLiveAfter = usedVarsStmts2 rest `S.union` liveAfter
          doc = genStmt2 env declared stmtLiveAfter stmt
          declared' = case stmt of
            C2.SAssign _ (RVecStore _ _ _) -> declared
            C2.SAssign v _ -> S.insert v declared
            C2.SLoop spec body ->
              let declaredWithHoists = declared `S.union` memoizedZeroArgArrayCallVars (ceRetKinds env) body
               in case C2.lsRed spec of
                    Just r -> S.insert (C2.rsAccVar r) declaredWithHoists
                    Nothing -> declaredWithHoists
            C2.SIf _ thn els ->
              let branchVars = allAssignedVars (thn ++ els) `S.intersection` stmtLiveAfter
               in declared `S.union` branchVars
            _ -> declared
       in doc $$ genStmts2 env declared' liveAfter rest

genStmt2 :: CodegenEnv -> Set CVar -> Set CVar -> C2.Stmt -> Doc
genStmt2 env declared liveAfter stmt = case stmt of
  C2.SAssign _ rhs@(RVecStore _ _ val) ->
    let mw = case val of
          AVecVar vv -> Map.lookup vv (ceVecVars env)
          _ -> Nothing
     in genRHS (ceArrayElemTypes env) (ceFloatArrVars env) mw (ceTupleDefs env) rhs <> text ";"
  C2.SAssign v rhs
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
        RProj 0 (AVar src) | Map.member src (cePairVars env) -> genAtom (AVar src) <> text ".fst"
        RProj 1 (AVar src) | Map.member src (cePairVars env) -> genAtom (AVar src) <> text ".snd"
        _ -> genRHS (ceArrayElemTypes env) (ceFloatArrVars env) (Map.lookup v (ceVecVars env)) (ceTupleDefs env) assignedRhs
  C2.SArrayWrite arr idx val
    | AVar arrV <- arr,
      Just eltTy <- Map.lookup arrV (ceArrayElemTypes env) ->
        genArrayAccess eltTy arr idx <+> text "=" <+> genAtom val <> text ";"
    | AVar arrV <- arr,
      arrV `S.member` ceFloatArrVars env ->
        text "hyd_array_set_float(" <> genAtom arr <> text "," <+> genAtom idx <> text "," <+> genAtom val <> text ");"
    | otherwise ->
        genAtom arr <> text "->data[" <> genAtom idx <> text "]" <+> text "=" <+> genAtom val <> text ";"
  C2.SLoop spec body -> genLoop2 env declared spec body
  C2.SIf cond thn els ->
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
          $$ nest 4 (genStmts2 env declared' liveAfter thn)
          $$ case els of
            [] -> text "}"
            _ ->
              text "} else {"
                $$ nest 4 (genStmts2 env declared' liveAfter els)
                $$ text "}"
  C2.SReturn a -> text "return" <+> genAtom a <> text ";"
  C2.SBreak -> text "break;"

genLoop2 :: CodegenEnv -> Set CVar -> C2.LoopSpec -> [C2.Stmt] -> Doc
genLoop2 env declared spec body =
  let iters = C2.lsIters spec
      bounds = C2.lsBounds spec
      red = C2.lsRed spec
      role = C2.lsRole spec
      (hoistedMemoCalls, loopBody) = hoistMemoizedZeroArgArrayCalls (ceRetKinds env) body
      hoistedMemoVars =
        S.fromList [v | C2.SAssign v (RCall _ []) <- hoistedMemoCalls]
      declaredWithHoisted = declared `S.union` hoistedMemoVars
      hoistedMemoDoc =
        genStmts2 env declared (usedVarsStmts2 loopBody) hoistedMemoCalls
      bodyDoc d = genStmts2 env d S.empty loopBody
      parallelPolicyClause = case C2.lsExec spec of
        C2.Parallel p ->
          let strategyComment = case C2.psStrategy p of
                C2.ParallelGeneric -> empty
                C2.ParallelScatterDirect -> text " /* scatter-direct */"
                C2.ParallelScatterAtomicAddInt -> text " /* scatter-atomic-add-int */"
                C2.ParallelScatterAtomicAddFloat -> text " /* scatter-atomic-add-float */"
                C2.ParallelScatterPrivatizedIntAdd -> text " /* scatter-privatized-int-add */"
           in strategyComment <> maybe empty (\pol -> space <> text (BS.unpack pol)) (C2.psPolicy p)
        _ -> empty
      collapseClause
        | length iters > 1 = space <> text "collapse(" <> int (length iters) <> text ")"
        | otherwise = empty
      parallelPragma extraClauses = text "#pragma omp parallel for" <> parallelPolicyClause <> collapseClause <> extraClauses
      simdPragma extraClauses = text "#pragma omp simd simdlen(" <> int defaultSimdLen <> text ")" <> extraClauses
      roleComment = case role of
        C2.LoopPlain -> text "loop"
        C2.LoopFold -> text "fold loop"
        C2.LoopMap -> text "map loop"
        C2.LoopReductionWrapper -> text "reduction wrapper loop"
        C2.LoopReduction -> text "reduction loop"
        C2.LoopMapReduction -> text "map-reduction outer loop"
      defaultSimdLen = case C2.lsExec spec of
        C2.Vector v -> C2.vsWidth v
        _ -> 1
      minSimdTripCount = 4
      constantTripCount ie = case C2.simplifyIndexExpr ie of
        C2.IConst n -> Just n
        _ -> Nothing
      shouldEmitSimd ie = maybe True (>= minSimdTripCount) (constantTripCount ie)
      atomicScatterBodyDoc elemTy declared' =
        case detectAtomicScatterAddLoop loopBody of
          Just (prefix, mGuard, arr, idx, val) ->
            let prefixLiveAfter =
                  maybe S.empty usedVarsAtom2 mGuard
                    `S.union` usedVarsAtom2 arr
                    `S.union` usedVarsAtom2 idx
                    `S.union` usedVarsAtom2 val
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
             in genStmts2 env declared' prefixLiveAfter prefix $$ guardedUpdate
          Nothing ->
            bodyDoc declared'
      privatizedScatterLoopDoc declared' iter bound =
        case detectAtomicScatterAddLoop loopBody of
          Just (prefix, mGuard, arr, idx, val) ->
            let arrStem = case arr of
                  AVar arrV -> sanitize arrV
                  _ -> "scatter"
                prefixLiveAfter =
                  maybe S.empty usedVarsAtom2 mGuard
                    `S.union` usedVarsAtom2 arr
                    `S.union` usedVarsAtom2 idx
                    `S.union` usedVarsAtom2 val
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
                        <+> text "= 0;"
                        <+> text ci
                        <+> text "<"
                        <+> genIndexExpr bound
                        <> text ";"
                        <+> text ci
                        <> text "++) {"
                        $$ nest
                          4
                          ( genStmts2 env declared' prefixLiveAfter prefix
                              $$ guardedPrivUpdate
                          )
                        $$ text "}"
                        $$ text "#pragma omp critical"
                        $$ text "{"
                        $$ nest
                          4
                          ( text "for (int64_t"
                              <+> mergeIx
                              <+> text "= 0;"
                              <+> mergeIx
                              <+> text "<"
                              <+> sizeVar
                              <> text ";"
                              <+> mergeIx
                              <> text "++) {"
                              $$ nest 4 (genArrayAccess CTInt64 arr (AVar (BS.pack mergeIxName)) <+> text "+=" <+> privVar <> text "[" <> mergeIx <> text "];")
                              $$ text "}"
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
              <+> text "= 0;"
              <+> text (sanitize iter)
              <+> text "<"
              <+> genIndexExpr bound
              <> text ";"
              <+> text (sanitize iter)
              <> text "++) {"
              $$ nest 4 (bodyDoc declared')
              $$ text "}"
   in hoistedMemoDoc $$ case (C2.lsExec spec, red, iters, bounds) of
        (C2.Serial, Just r, [i], [b]) ->
          let cacc = sanitize (C2.rsAccVar r)
              ci = sanitize i
              declared' = S.insert (C2.rsAccVar r) declaredWithHoisted
              alreadyDeclared = C2.rsAccVar r `S.member` declared
              accCType = if C2.rsAccVar r `S.member` ceFloatVars env then "double" else "int64_t"
              accInit =
                if alreadyDeclared
                  then empty
                  else text "/* acc */" $$ text accCType <+> text cacc <+> text "=" <+> genIndexExpr (C2.rsInit r) <> text ";"
           in accInit
                $$ text "for (int64_t"
                <+> text ci
                <+> text "= 0;"
                <+> text ci
                <+> text "<"
                <+> genIndexExpr b
                <> text ";"
                <+> text ci
                <> text "++) {"
                $$ nest 4 (bodyDoc declared')
                $$ text "}"
        (C2.Parallel _, Just r, [i], [b]) ->
          let cacc = sanitize (C2.rsAccVar r)
              ci = sanitize i
              declared' = S.insert (C2.rsAccVar r) declaredWithHoisted
              alreadyDeclared = C2.rsAccVar r `S.member` declared
              redClause = case C2.rsRedop r of
                RAdd -> text "reduction(+:" <> text cacc <> text ")"
                RMul -> text "reduction(*:" <> text cacc <> text ")"
              accCType = if C2.rsAccVar r `S.member` ceFloatVars env then "double" else "int64_t"
              accInit =
                if alreadyDeclared
                  then empty
                  else text "/* parallel reduction */" $$ text accCType <+> text cacc <+> text "=" <+> genIndexExpr (C2.rsInit r) <> text ";"
           in accInit
                $$ parallelPragma (space <> redClause)
                $$ text "for (int64_t"
                <+> text ci
                <+> text "= 0;"
                <+> text ci
                <+> text "<"
                <+> genIndexExpr b
                <> text ";"
                <+> text ci
                <> text "++) {"
                $$ nest 4 (bodyDoc declared')
                $$ text "}"
        (C2.Parallel _, Just r, _, _) ->
          let cacc = sanitize (C2.rsAccVar r)
              declared' = S.insert (C2.rsAccVar r) declaredWithHoisted
              alreadyDeclared = C2.rsAccVar r `S.member` declared
              redClause = case C2.rsRedop r of
                RAdd -> text "reduction(+:" <> text cacc <> text ")"
                RMul -> text "reduction(*:" <> text cacc <> text ")"
              accCType = if C2.rsAccVar r `S.member` ceFloatVars env then "double" else "int64_t"
              accInit =
                if alreadyDeclared
                  then empty
                  else text "/* parallel reduction */" $$ text accCType <+> text cacc <+> text "=" <+> genIndexExpr (C2.rsInit r) <> text ";"
           in accInit
                $$ parallelPragma (space <> redClause)
                $$ genNestedLoops iters bounds (bodyDoc declared')
        (C2.Vector v, Just r, [i], [b]) ->
          let ci = sanitize i
              cacc = sanitize (C2.rsAccVar r)
              declared' = S.insert (C2.rsAccVar r) declaredWithHoisted
              redClause = case C2.rsRedop r of
                RAdd -> text " reduction(+:" <> text cacc <> text ")"
                RMul -> text " reduction(*:" <> text cacc <> text ")"
              accCType = if C2.rsAccVar r `S.member` ceFloatVars env then "double" else "int64_t"
              accInit =
                if C2.rsAccVar r `S.member` declared
                  then empty
                  else text "/* simd reduction */" $$ text accCType <+> text cacc <+> text "=" <+> genIndexExpr (C2.rsInit r) <> text ";"
              vecComment = case C2.vsTail v of
                C2.TailNone -> text "/* vectorized reduction loop, width =" <+> int (C2.vsWidth v) <> text " */"
                _ -> text "/* vectorized reduction loop, width =" <+> int (C2.vsWidth v) <+> text "(compiler handles tail) */"
           in if shouldEmitSimd b
                then
                  accInit
                    $$ vecComment
                    $$ simdPragma redClause
                    $$ text "for (int64_t"
                    <+> text ci
                    <+> text "= 0;"
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
                    <+> text "= 0;"
                    <+> text ci
                    <+> text "<"
                    <+> genIndexExpr b
                    <> text ";"
                    <+> text ci
                    <> text "++) {"
                    $$ nest 4 (bodyDoc declared')
                    $$ text "}"
        (C2.Vector v, _, [i], [b]) ->
          let ci = sanitize i
              vecComment = case C2.vsTail v of
                C2.TailNone -> text "/* vectorized loop, width =" <+> int (C2.vsWidth v) <> text " */"
                _ -> text "/* vectorized loop, width =" <+> int (C2.vsWidth v) <+> text "(compiler handles tail) */"
           in if shouldEmitSimd b
                then
                  vecComment
                    $$ simdPragma empty
                    $$ text "for (int64_t"
                    <+> text ci
                    <+> text "= 0;"
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
                    <+> text "= 0;"
                    <+> text ci
                    <+> text "<"
                    <+> genIndexExpr b
                    <> text ";"
                    <+> text ci
                    <> text "++) {"
                    $$ nest 4 (bodyDoc declaredWithHoisted)
                    $$ text "}"
        (C2.Parallel p, _, [i], [b])
          | C2.psStrategy p == C2.ParallelScatterPrivatizedIntAdd ->
              privatizedScatterLoopDoc declaredWithHoisted i b
        (C2.Parallel _, _, [i], [b]) ->
          let ci = sanitize i
           in text "/* parallel"
                <+> roleComment
                <+> text "*/"
                $$ parallelPragma empty
                $$ text "for (int64_t"
                <+> text ci
                <+> text "= 0;"
                <+> text ci
                <+> text "<"
                <+> genIndexExpr b
                <> text ";"
                <+> text ci
                <> text "++) {"
                $$ nest
                  4
                  ( case C2.lsExec spec of
                      C2.Parallel p
                        | Just elemTy <- atomicScatterElemType p -> atomicScatterBodyDoc elemTy declaredWithHoisted
                      _ -> bodyDoc declaredWithHoisted
                  )
                $$ text "}"
        (C2.Parallel _, _, _, _) ->
          text "/* parallel"
            <+> roleComment
            <+> text "*/"
            $$ parallelPragma empty
            $$ genNestedLoops
              iters
              bounds
              ( case C2.lsExec spec of
                  C2.Parallel p
                    | Just elemTy <- atomicScatterElemType p -> atomicScatterBodyDoc elemTy declaredWithHoisted
                  _ -> bodyDoc declaredWithHoisted
              )
        (_, _, _, _) -> genNestedLoops iters bounds (bodyDoc declaredWithHoisted)

hoistMemoizedZeroArgArrayCalls :: Map CVar VarKind -> [C2.Stmt] -> ([C2.Stmt], [C2.Stmt])
hoistMemoizedZeroArgArrayCalls retKinds = foldr step ([], [])
  where
    step stmt (hoisted, kept)
      | isMemoizedZeroArgArrayCall retKinds stmt = (stmt : hoisted, kept)
      | otherwise = (hoisted, stmt : kept)

memoizedZeroArgArrayCallVars :: Map CVar VarKind -> [C2.Stmt] -> Set CVar
memoizedZeroArgArrayCallVars retKinds body =
  S.fromList [v | C2.SAssign v (RCall _ []) <- fst (hoistMemoizedZeroArgArrayCalls retKinds body)]

isMemoizedZeroArgArrayCall :: Map CVar VarKind -> C2.Stmt -> Bool
isMemoizedZeroArgArrayCall retKinds stmt = case stmt of
  C2.SAssign _ (RCall f []) ->
    case Map.findWithDefault KScalar f retKinds of
      KArray -> True
      KFloatArray -> True
      _ -> False
  _ -> False

genNestedLoops :: [CVar] -> [C2.IndexExpr] -> Doc -> Doc
genNestedLoops iters bounds body =
  foldr mk body (zip (map sanitize iters) bounds)
  where
    mk (ci, b) inner =
      text "for (int64_t"
        <+> text ci
        <+> text "= 0;"
        <+> text ci
        <+> text "<"
        <+> genIndexExpr b
        <> text ";"
        <+> text ci
        <> text "++) {"
        $$ nest 4 inner
        $$ text "}"

detectAtomicScatterAddLoop :: [C2.Stmt] -> Maybe ([C2.Stmt], Maybe Atom, Atom, Atom, Atom)
detectAtomicScatterAddLoop body =
  case reverse body of
    (C2.SIf cond thn [] : revPrefix) -> do
      (branchPrefix, arrWrite, idxWrite, val) <- detectAtomicScatterAddCore thn
      pure (reverse revPrefix ++ branchPrefix, Just cond, arrWrite, idxWrite, val)
    _ -> do
      (prefix, arrWrite, idxWrite, val) <- detectAtomicScatterAddCore body
      pure (prefix, Nothing, arrWrite, idxWrite, val)
  where
    detectAtomicScatterAddCore stmts =
      case reverse stmts of
        ( C2.SArrayWrite arrWrite idxWrite (AVar newVar)
            : C2.SAssign newVar' (RBinOp op a b)
            : C2.SAssign oldVar (RArrayLoad arrRead idxRead)
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

atomicScatterElemType :: C2.ParallelSpec -> Maybe CType
atomicScatterElemType p = case C2.psStrategy p of
  C2.ParallelScatterAtomicAddInt -> Just CTInt64
  C2.ParallelScatterAtomicAddFloat -> Just CTDouble
  _ -> Nothing

genIndexExpr :: C2.IndexExpr -> Doc
genIndexExpr expr = case C2.simplifyIndexExpr expr of
  C2.IVar v -> text (sanitize v)
  C2.IConst n -> text (show n ++ "LL")
  C2.IAdd a b -> parens (genIndexExpr a <+> text "+" <+> genIndexExpr b)
  C2.ISub a b -> parens (genIndexExpr a <+> text "-" <+> genIndexExpr b)
  C2.IMul a b -> parens (genIndexExpr a <+> text "*" <+> genIndexExpr b)
  C2.IDiv a b -> parens (genIndexExpr a <+> text "/" <+> genIndexExpr b)
  C2.INdToFlat nd shp -> text "hyd_nd_to_flat(" <> genIndexExpr nd <> text "," <+> genIndexExpr shp <> text ")"
  C2.ICall _ _ -> text "0LL"
  _ -> text "0LL"

hasParallelStmt2 :: [C2.Stmt] -> Bool
hasParallelStmt2 = any go
  where
    go st = case st of
      C2.SLoop spec body -> case C2.lsExec spec of
        C2.Parallel _ -> True
        _ -> hasParallelStmt2 body
      C2.SIf _ thn els -> hasParallelStmt2 thn || hasParallelStmt2 els
      _ -> False

-- | Returns True when any statement in the body uses a math-library UnOp
-- (CSqrt, CExpF, CLog, etc.) so we can conditionally emit @#include \<math.h\>@.
hasMathOp :: [C2.Stmt] -> Bool
hasMathOp = any go
  where
    go st = case st of
      C2.SAssign _ rhs -> rhsHasMath rhs
      C2.SArrayWrite _ _ a -> atomHasMath a
      C2.SLoop _ body -> hasMathOp body
      C2.SIf _ thn els -> hasMathOp thn || hasMathOp els
      _ -> False
    rhsHasMath (RUnOp op _) = isMathOp op
    rhsHasMath _ = False
    atomHasMath _ = False
    isMathOp op = op `elem` [CSqrt, CExpF, CLog, CSin, CCos, CAbsF, CFloorF, CCeilF, CErf]

findReturnVar2 :: [C2.Stmt] -> Maybe CVar
findReturnVar2 [] = Nothing
findReturnVar2 stmts = case last stmts of
  C2.SReturn (AVar v) -> Just v
  _ -> Nothing

splitFinalReturn :: [C2.Stmt] -> ([C2.Stmt], Maybe Atom)
splitFinalReturn stmts = case reverse stmts of
  (C2.SReturn a : rest) -> (reverse rest, Just a)
  _ -> (stmts, Nothing)

-- | True when a proc body ends with a direct float-literal return,
-- i.e. the body is just `SReturn (AFloat _)` with no intermediate variable.
-- This handles top-level float constants like `let g = 6.674e-11`.
isDirectFloatReturn :: [C2.Stmt] -> Bool
isDirectFloatReturn stmts = case stmts of
  [C2.SReturn (AFloat _)] -> True
  _ -> False

arrayVarsProc2 :: C2.Proc -> Set CVar
arrayVarsProc2 (C2.Proc {C2.procBody = body}) = arrayVarsStmts2 body

localArrayVarsProc2 :: Map CVar CType -> C2.Proc -> Set CVar
localArrayVarsProc2 typeEnv (C2.Proc {C2.procBody = body}) =
  rawLocals `S.difference` consumedByArrayPair
  where
    rawLocals =
      S.fromList
        [v | C2.SAssign v rhs <- body, isLocalArray v, ownsArrayHeader rhs]
    isLocalArray v = case Map.lookup v typeEnv of
      Just (CTArray _) -> True
      _ -> False
    ownsArrayHeader rhs = case rhs of
      RArrayAlloc {} -> True
      RCall {} -> True
      _ -> False
    -- Arrays passed as CEArray components to RPairMake are owned by the
    -- resulting pair and freed during pair-update in the enclosing loop.
    -- Excluding them here prevents a double-free at the post-return cleanup.
    consumedByArrayPair =
      S.fromList
        [ v
        | C2.SAssign _ (RPairMake ct1 ct2 a1 a2) <- body,
          (ct, a) <- [(ct1, a1), (ct2, a2)],
          ct == CEArray,
          AVar v <- [a]
        ]

-- | Emit frees for the non-returned array component(s) of any pair-of-arrays
-- variable whose one component is the returned atom.  This handles the case
-- where a foldl accumulator of type (Array, Array) has its snd (or fst)
-- extracted as the return value — the other component must be freed to avoid
-- a post-loop heap leak.
discardedArrayPairComponentDoc :: [C2.Stmt] -> Atom -> Map CVar (CElemType, CElemType) -> Doc
discardedArrayPairComponentDoc stmts retAtom pairVars =
  case retAtom of
    AVar retVar -> vcat (fstFrees ++ sndFrees)
      where
        fstFrees =
          [ text "hyd_array_free(" <> text (sanitize pairVar) <> text ".fst);"
          | C2.SAssign rv (RPairSnd _ (AVar pairVar)) <- stmts,
            rv == retVar,
            Just (CEArray, _) <- [Map.lookup pairVar pairVars]
          ]
        sndFrees =
          [ text "hyd_array_free(" <> text (sanitize pairVar) <> text ".snd);"
          | C2.SAssign rv (RPairFst _ (AVar pairVar)) <- stmts,
            rv == retVar,
            Just (_, CEArray) <- [Map.lookup pairVar pairVars]
          ]
    _ -> empty

arrayVarsStmts2 :: [C2.Stmt] -> Set CVar
arrayVarsStmts2 = foldMap arrayVarsStmt2

arrayVarsStmt2 :: C2.Stmt -> Set CVar
arrayVarsStmt2 st = case st of
  C2.SAssign v rhs -> case rhs of
    RArrayAlloc {} -> S.singleton v
    RCall fn _ | fn == "hyd_read_array_csv" -> S.singleton v
    RCall fn _ | fn == "hyd_read_float_array_csv" -> S.singleton v
    _ -> S.empty
  C2.SLoop _ body -> arrayVarsStmts2 body
  C2.SIf _ thn els -> arrayVarsStmts2 thn `S.union` arrayVarsStmts2 els
  _ -> S.empty

cleanupArrayVarsDoc :: Set CVar -> Doc
cleanupArrayVarsDoc vars =
  vcat [text "hyd_array_free(" <> text (sanitize v) <> text ");" | v <- S.toList vars]

tupleVarsProc2 :: C2.Proc -> Set CVar
tupleVarsProc2 (C2.Proc {C2.procBody = body}) = tupleVarsStmts2 body

tupleVarsStmts2 :: [C2.Stmt] -> Set CVar
tupleVarsStmts2 = foldMap tupleVarsStmt2

tupleVarsStmt2 :: C2.Stmt -> Set CVar
tupleVarsStmt2 st = case st of
  C2.SAssign v rhs -> case rhs of
    RTuple {} -> S.singleton v
    RArrayShape {} -> S.singleton v
    RShapeInit {} -> S.singleton v
    RFlatToNd {} -> S.singleton v
    _ -> S.empty
  C2.SLoop _ body -> tupleVarsStmts2 body
  C2.SIf _ thn els -> tupleVarsStmts2 thn `S.union` tupleVarsStmts2 els
  _ -> S.empty

-- | Collect the variables that hold pair values and their element types.
pairVarsProc2 :: C2.Proc -> Map CVar (CElemType, CElemType)
pairVarsProc2 (C2.Proc {C2.procBody = body}) = pairVarsStmts2 Map.empty body

-- | Forward-propagating pass: process statements in order, accumulating
-- the map of known pair-typed variables. Propagates pair type through
-- plain @RAtom (AVar src)@ copies (e.g. the foldl accumulator init).
pairVarsStmts2 :: Map CVar (CElemType, CElemType) -> [C2.Stmt] -> Map CVar (CElemType, CElemType)
pairVarsStmts2 known = foldl pairVarsStmt2 known

pairVarsStmt2 :: Map CVar (CElemType, CElemType) -> C2.Stmt -> Map CVar (CElemType, CElemType)
pairVarsStmt2 known st = case st of
  C2.SAssign v (RPairMake cty1 cty2 _ _) -> Map.insert v (cty1, cty2) known
  -- Propagate pair type through plain copies (e.g. foldl acc initialisation).
  C2.SAssign v (RAtom (AVar src))
    | Just tys <- Map.lookup src known -> Map.insert v tys known
  -- RPairFst/RPairSnd carrying a pair CElemType yield another pair variable.
  C2.SAssign v (RPairFst (CEPair ct1 ct2) _) -> Map.insert v (ct1, ct2) known
  C2.SAssign v (RPairSnd (CEPair ct1 ct2) _) -> Map.insert v (ct1, ct2) known
  C2.SLoop _ body -> pairVarsStmts2 known body
  C2.SIf _ thn els ->
    let k1 = pairVarsStmts2 known thn
        k2 = pairVarsStmts2 known els
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

classifyVarKinds2 :: Map CVar VarKind -> [C2.Stmt] -> (Set CVar, Set CVar)
classifyVarKinds2 retKinds stmts = fixpoint S.empty (initFloatArrays stmts)
  where
    isFloatArrayCall fn =
      fn == "hyd_read_float_array_csv"
        || Map.lookup fn retKinds == Just KFloatArray
    initFloatArrays = foldMap initFloatArray
    initFloatArray st = case st of
      C2.SAssign v (RCall fn _) | isFloatArrayCall fn -> S.singleton v
      C2.SLoop _ body -> foldMap initFloatArray body
      C2.SIf _ thn els -> foldMap initFloatArray thn `S.union` foldMap initFloatArray els
      _ -> S.empty

    fixpoint fv fa =
      let fa' = collectFloatArrayVars fv stmts `S.union` fa
          fv' = collectFloatVars fv fa' stmts
       in if fv' == fv && fa' == fa then (fv, fa) else fixpoint fv' fa'

    collectFloatVars fv fa = foldMap (floatVarStmt fv fa)
    floatVarStmt fv fa st = case st of
      C2.SAssign v rhs ->
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
      C2.SLoop _ body -> collectFloatVars fv fa body
      C2.SIf _ thn els -> collectFloatVars fv fa thn `S.union` collectFloatVars fv fa els
      _ -> S.empty

    collectFloatArrayVars fv = foldMap (floatArrayVarStmt fv)
    floatArrayVarStmt fv st = case st of
      C2.SAssign v (RCall fn _) | isFloatArrayCall fn -> S.singleton v
      C2.SArrayWrite (AVar arr) _ val | isFloatAtom fv val -> S.singleton arr
      C2.SLoop _ body -> collectFloatArrayVars fv body
      C2.SIf _ thn els -> collectFloatArrayVars fv thn `S.union` collectFloatArrayVars fv els
      _ -> S.empty

classifyVecVars :: Int -> [C2.Stmt] -> Map CVar Int
classifyVecVars defaultW stmts = fixpoint Map.empty stmts
  where
    fixpoint vv body =
      let vv' = collectVecVars vv defaultW body
       in if vv' == vv then vv else fixpoint vv' body

    collectVecVars known w = foldMap (goStmt known w)

    goStmt known w st = case st of
      C2.SAssign v rhs -> case rhs of
        RVecLoad {} -> Map.singleton v w
        RVecBinOp {} -> Map.singleton v w
        RVecUnOp {} -> Map.singleton v w
        RVecSplat {} -> Map.singleton v w
        RAtom (AVecVar src) | Just sw <- Map.lookup src known -> Map.singleton v sw
        _ -> Map.empty
      C2.SLoop spec body ->
        let w' = case C2.lsExec spec of
              C2.Vector vs -> C2.vsWidth vs
              _ -> w
         in collectVecVars known w' body
      C2.SIf _ thn els ->
        collectVecVars known w thn `Map.union` collectVecVars known w els
      _ -> Map.empty

procReturnKinds2 :: C2.Program -> Map CVar VarKind
-- Build return-kind map incrementally so callees are known when their callers
-- are classified. Procs are emitted in definition order (callees before callers),
-- so a left fold accumulates enough context by the time each proc is processed.
procReturnKinds2 prog@(C2.Program procs) = foldl addProc Map.empty procs
  where
    callTypes = inferProgramReturnTypes2 prog
    callParamTypes = buildCallParamTypes callTypes procs
    addProc rk proc@(C2.Proc {C2.procName = name}) = Map.insert name (retKindOf rk proc) rk
    retKindOf rk proc@(C2.Proc {C2.procBody = body}) =
      let recoveredTypeEnv = recoverProcTypeEnv2 callTypes callParamTypes proc
          (arrVarsTE, tupVarsTE, pairVarsTE, recordVarsTE, floatVarsTE, floatArrVarsTE) =
            -- Use recoveredTypeEnv directly: it corrects stale lowering-time types
            -- (e.g. RPairMake tags that defaulted to CEInt).
            typeEnvToVarSets recoveredTypeEnv
          arrVars = arrVarsTE `S.union` arrayVarsProc2 proc
          tupVars = tupVarsTE `S.union` tupleVarsProc2 proc
          pairVars = pairVarsTE `Map.union` pairVarsProc2 proc
          recordVars = recordVarsTE
          (floatVarsH, floatArrVarsH) = classifyVarKinds2 rk body
          floatVars = floatVarsTE `S.union` floatVarsH
          floatArrVars = floatArrVarsTE `S.union` floatArrVarsH
       in case findReturnVar2 body of
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
genPairStructDefs :: CodegenOptions -> C2.Program -> [TypeEnv] -> Doc
genPairStructDefs opts (C2.Program procs) recoveredEnvs =
  let allPairs = concatMap transitivePairTypes topLevelTypes
      ordered = nubBy (\a b -> fst a == fst b && snd a == snd b) allPairs
   in if null ordered
        then empty
        else text "" $$ vcat (map (uncurry (genPairStructDef opts)) ordered)
  where
    topLevelTypes = concatMap collectPairTypesProc (zip procs recoveredEnvs)
    collectPairKinds (C2.Proc {C2.procBody = body, C2.procTypeEnv = tenv}, recEnv) =
      collectPairKindsStmts body
        `S.union` collectPairKindsTypeEnv tenv
        `S.union` collectPairKindsTypeEnv recEnv
    collectPairTypesProc procEnv = map (\(ct1, ct2) -> CEPair ct1 ct2) (S.toList (collectPairKinds procEnv))
    -- Collect pairs from RPairMake in the body.
    collectPairKindsStmts = foldMap collectPairKindsStmt
    collectPairKindsStmt st = case st of
      C2.SAssign _ (RPairMake ct1 ct2 _ _) -> S.singleton (ct1, ct2)
      C2.SLoop _ body -> collectPairKindsStmts body
      C2.SIf _ thn els -> collectPairKindsStmts thn `S.union` collectPairKindsStmts els
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

genRecordStructDefs :: C2.Program -> Doc
genRecordStructDefs (C2.Program procs) =
  let allRecords = concatMap collectProcRecords procs
      ordered = nub allRecords
   in if null ordered then empty else text "" $$ vcat (map genRecordStructDef ordered)
  where
    collectProcRecords (C2.Proc {C2.procTypeEnv = tenv}) =
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
      RPairMake ct1 ct2 _ _ -> text (pairStructName ct1 ct2)
      RPairFst ct _ -> text (celemTypeCType' (ceOpts env) ct)
      RPairSnd ct _ -> text (celemTypeCType' (ceOpts env) ct)
      _ -> text "int64_t"

-- | Collect all RTuple definitions from a statement list (pre-pass).
-- Used for inlining RNdToFlat with known-arity tuples.
collectTupleDefs :: [C2.Stmt] -> Map CVar [Atom]
collectTupleDefs = foldMap go
  where
    go (C2.SAssign v (RTuple atoms)) = Map.singleton v atoms
    go (C2.SLoop _ body) = collectTupleDefs body
    go (C2.SIf _ thn els) = collectTupleDefs thn `Map.union` collectTupleDefs els
    go _ = Map.empty

genRHS :: Map CVar CType -> Set CVar -> Maybe Int -> Map CVar [Atom] -> RHS -> Doc
genRHS _ _ _ _ (RAtom a) = genAtom a
genRHS _ _ _ _ (RBinOp op a1 a2) = parens (genAtom a1 <+> genBinOp op <+> genAtom a2)
genRHS _ _ _ _ (RUnOp op a) = case op of
  CNot -> parens (text "!" <> genAtom a)
  CNeg -> parens (text "-" <> genAtom a)
  _ -> genUnOp op <> parens (genAtom a) -- math function call: f(arg)
genRHS _ _ _ _ (RTuple atoms) =
  text "hyd_tuple_make("
    <> int (length atoms)
    <> (if null atoms then empty else text "," <+> hsep (punctuate (text ",") (map (\a -> text "(int64_t)" <> genAtom a) atoms)))
    <> text ")"
genRHS _ _ _ _ (RProj i a) = genAtom a <> text ".elems[" <> integer i <> text "]"
genRHS _ _ _ _ (RRecord fields) =
  text "{"
    <> hsep (punctuate comma [text "." <> text (sanitizeFieldName field) <+> text "=" <+> genAtom atom | (field, atom) <- fields])
    <> text "}"
genRHS _ _ _ _ (RRecordProj field a) = genAtom a <> text "." <> text (sanitizeFieldName field)
genRHS _ _ _ _ (RArrayAlloc shp) = text "hyd_array_alloc(" <> genAtom shp <> text ")"
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

inferArrayElemTypesFromStmts :: Map CVar CType -> [C2.Stmt] -> Map CVar CType -> Map CVar CType
inferArrayElemTypesFromStmts retTypes stmts initial = go initial
  where
    go known =
      let known' = foldl step known stmts
       in if known' == known then known else go known'

    step known stmt = case stmt of
      C2.SAssign v (RCall fn _)
        | Just (CTArray eltTy) <- Map.lookup fn retTypes -> Map.insert v eltTy known
      C2.SAssign v (RAtom (AVar src))
        | Just eltTy <- Map.lookup src known -> Map.insert v eltTy known
      C2.SLoop _ body -> inferArrayElemTypesFromStmts retTypes body known
      C2.SIf _ thn els ->
        let knownThn = inferArrayElemTypesFromStmts retTypes thn known
            knownEls = inferArrayElemTypesFromStmts retTypes els known
         in Map.union knownThn knownEls
      _ -> known

collectMissingArrayElemVars :: Map CVar CType -> CallParamTypes -> [C2.Proc] -> [String]
collectMissingArrayElemVars retTypes callParamTypes procs =
  map BS.unpack (S.toList (foldMap missingForProc procs))
  where
    missingForProc proc =
      let typeEnv = recoverProcTypeEnv2 retTypes callParamTypes proc
          arrayElemTypes = inferArrayElemTypesFromStmts retTypes (C2.procBody proc) (Map.fromList [(v, eltTy) | (v, CTArray eltTy) <- Map.toList typeEnv])
       in missingArrayVarsInStmts arrayElemTypes (C2.procBody proc)

    missingArrayVarsInStmts known = foldMap go
      where
        go stmt = case stmt of
          C2.SAssign _ (RArrayLoad (AVar arr) _) ->
            if Map.member arr known then S.empty else S.singleton arr
          C2.SAssign _ (RVecLoad (AVar arr) _) ->
            if Map.member arr known then S.empty else S.singleton arr
          C2.SAssign _ (RVecStore (AVar arr) _ _) ->
            if Map.member arr known then S.empty else S.singleton arr
          C2.SArrayWrite (AVar arr) _ _ ->
            if Map.member arr known then S.empty else S.singleton arr
          C2.SLoop _ body -> missingArrayVarsInStmts known body
          C2.SIf _ thn els -> missingArrayVarsInStmts known thn `S.union` missingArrayVarsInStmts known els
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
allAssignedVars :: [C2.Stmt] -> Set CVar
allAssignedVars = foldMap go
  where
    go (C2.SAssign v _) = S.singleton v
    go (C2.SIf _ thn els) = allAssignedVars thn `S.union` allAssignedVars els
    go (C2.SLoop _ body) = allAssignedVars body
    go _ = S.empty

assignedRHSMap :: [C2.Stmt] -> Map CVar RHS
assignedRHSMap = foldr go Map.empty
  where
    go stmt acc = case stmt of
      C2.SAssign v rhs -> Map.insertWith (\_ existing -> existing) v rhs acc
      C2.SIf _ thn els -> assignedRHSMap thn `Map.union` assignedRHSMap els `Map.union` acc
      C2.SLoop _ body -> assignedRHSMap body `Map.union` acc
      _ -> acc
