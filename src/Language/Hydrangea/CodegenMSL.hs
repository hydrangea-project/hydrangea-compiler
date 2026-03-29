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

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List (intercalate, nub)
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
  )
import Language.Hydrangea.CodegenC
  ( CodegenOptions(..), CodegenArtifacts(..), VarKind(..)
  , defaultCodegenOptions, codegenProgram2WithOptions
  , sanitize, splitFinalReturn
  , typeEnvToVarSets, classifyVarKinds2
  , arrayVarsProc2, pairVarsProc2
  , inferArrayElemTypesFromStmts
  , procReturnKinds2
  , isFloatArithBinOp, isMathFloatOp
  , pairStructName, celemTypeLetter
  , cTypeName
  )

-- ---------------------------------------------------------------------------
-- Public types

-- | Artifacts produced by the MSL codegen.
data MSLArtifacts = MSLArtifacts
  { mslKernelSource  :: String  -- ^ Contents of the @.metal@ kernel file.
  , mslHarnessSource :: String  -- ^ Contents of the self-contained ObjC harness.
  }

-- | Options controlling which proc is selected as the GPU kernel.
data MSLOptions = MSLOptions
  { mslKernelToEmit :: Maybe CVar
    -- ^ If 'Just name', use that proc as the kernel. If 'Nothing', use the
    -- first proc with a top-level @LoopMap@ loop.
  }

defaultMSLOptions :: MSLOptions
defaultMSLOptions = MSLOptions { mslKernelToEmit = Nothing }

-- ---------------------------------------------------------------------------
-- Top-level entry point

-- | Translate a @C2.Program@ to Metal artifacts.
codegenMSL :: MSLOptions -> C2.Program -> Either String MSLArtifacts
codegenMSL opts prog@(C2.Program procs) = do
  kernelProc <- findKernelProc opts procs
  let kernelName = C2.procName kernelProc
      body       = C2.procBody kernelProc
  (preLoopStmts, mapLoopSpec, mapLoopBody, retAtom) <- analyzeKernelProc body
  () <- validateMSLLoopBody mapLoopBody
  let retTypes     = inferProgramReturnTypes2 prog
      retKinds     = procReturnKinds2 prog
      callParamTys = buildCallParamTypes retTypes procs
      typeEnv      = recoverProcTypeEnv2 retTypes callParamTys kernelProc
      arrayElemTys = inferArrayElemTypesFromStmts retTypes body
                       (Map.fromList [(v, elt) | (v, CTArray elt) <- Map.toList typeEnv])
      (inputArrays, outputArrays, scalarInputs, inBodyHoisted) =
        classifyKernelParams preLoopStmts mapLoopBody mapLoopSpec typeEnv retKinds
      -- Hoist in-body zero-arg array calls to pre-loop for the harness
      effectivePreLoop = preLoopStmts ++ inBodyHoisted
  helperC <- genHelperC procs kernelName retKinds
  let kernelSrc  = genMSLKernelSrc kernelName mapLoopSpec mapLoopBody typeEnv
                     arrayElemTys inputArrays outputArrays scalarInputs
      harnessSrc = genObjCHarnessSrc prog kernelProc kernelName effectivePreLoop
                     mapLoopSpec retAtom typeEnv arrayElemTys retKinds retTypes
                     inputArrays outputArrays scalarInputs helperC
  Right MSLArtifacts
    { mslKernelSource  = kernelSrc
    , mslHarnessSource = harnessSrc
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
      case filter (procHasMapLoop . C2.procBody) procs of
        []    -> Left "MSL backend: no proc with a map loop found in program"
        (p:_) -> Right p
  where
    procHasMapLoop = any isMapLoopStmt
    isMapLoopStmt (C2.SLoop spec _) =
      C2.lsRole spec == C2.LoopMap ||
      (case C2.lsExec spec of C2.Parallel _ -> True; _ -> False)
    isMapLoopStmt _ = False

-- | Split proc body into (preLoopStmts, loopSpec, loopBody, retAtom).
analyzeKernelProc :: [C2.Stmt] -> Either String ([C2.Stmt], C2.LoopSpec, [C2.Stmt], Maybe Atom)
analyzeKernelProc body = do
  let (bodyNoRet, retAtom) = splitFinalReturn body
  case break isMapLoopStmt bodyNoRet of
    (_, []) ->
      Left "MSL backend: no map loop found in kernel proc body"
    (preLoop, C2.SLoop spec loopBody : _) ->
      case C2.lsIters spec of
        [_] -> Right (preLoop, spec, loopBody, retAtom)
        _   -> Left "MSL backend: multi-dimensional map loops not yet supported"
    _ ->
      Left "MSL backend: unexpected statement structure in kernel proc"
  where
    isMapLoopStmt (C2.SLoop spec _) =
      C2.lsRole spec == C2.LoopMap ||
      (case C2.lsExec spec of C2.Parallel _ -> True; _ -> False)
    isMapLoopStmt _ = False

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
      RFlatToNd {} ->
        Left "MSL backend: RFlatToNd inside kernel loop not supported (use C backend)"
      RNdToFlat {} ->
        Left "MSL backend: RNdToFlat inside kernel loop not supported (use C backend)"
      RTuple {} ->
        Left "MSL backend: tuple construction inside kernel loop not supported"
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
      loopUsedVars   = usedVarsStmts2 loopBody `Set.difference` iterVars
      -- Also scan for written arrays inside nested loops
      loopWrittenAll = collectWrittenArrays loopBody
      -- Input arrays: results of zero-arg RCall in pre-loop, read in loop
      preLoopInputArrays = [ v | C2.SAssign v (RCall _ []) <- preLoopStmts
                               , v `Set.member` loopUsedVars
                               , isArrayVar v ]
      -- In-body zero-arg array calls: RCall fn [] inside loop body where fn returns an array.
      -- These are memoized calls that need to be hoisted to CPU pre-loop and passed as buffers.
      inBodyHoisted = [ stmt
                      | stmt@(C2.SAssign v (RCall fn [])) <- loopBody
                      , isArrayFn fn
                      , v `Set.notMember` Set.fromList preLoopInputArrays ]
      inBodyInputArrays = [ v | C2.SAssign v (RCall _ []) <- inBodyHoisted ]
      inputArrays    = preLoopInputArrays ++ inBodyInputArrays
      -- Output arrays: RArrayAlloc in pre-loop, written in loop
      outputArrays = [ v | C2.SAssign v (RArrayAlloc _) <- preLoopStmts
                         , v `Set.member` loopWrittenAll ]
      allArrVars   = Set.fromList inputArrays `Set.union` Set.fromList outputArrays
      -- Scalar inputs: non-array pre-loop vars used in loop body
      scalarInputs = [ v | C2.SAssign v _ <- preLoopStmts
                         , v `Set.member` loopUsedVars
                         , v `Set.notMember` allArrVars
                         , not (isArrayVar v) ]
  in (nub inputArrays, nub outputArrays, nub scalarInputs, inBodyHoisted)
  where
    isArrayVar v = case Map.lookup v typeEnv of
      Just (CTArray _) -> True
      _ -> Map.findWithDefault KScalar v retKinds `elem` [KArray, KFloatArray]
    isArrayFn fn = Map.findWithDefault KScalar fn retKinds `elem` [KArray, KFloatArray]

collectWrittenArrays :: [C2.Stmt] -> Set CVar
collectWrittenArrays = foldMap go
  where
    go (C2.SArrayWrite (AVar v) _ _) = Set.singleton v
    go (C2.SLoop _ body)             = collectWrittenArrays body
    go (C2.SIf _ thn els)            = collectWrittenArrays thn `Set.union` collectWrittenArrays els
    go _                             = Set.empty

-- ---------------------------------------------------------------------------
-- MSL type helpers

-- | Map a CType to its MSL type name.
-- CTDouble is demoted to float (MSL does not support double on most GPUs).
mslTypeName :: CType -> String
mslTypeName CTInt64   = "long"
mslTypeName CTDouble  = "float"   -- demoted
mslTypeName CTBool    = "int"
mslTypeName CTUnit    = "int"
mslTypeName CTTuple   = "uint"    -- components extracted separately
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
  -> String
genMSLKernelSrc kernelName loopSpec loopBody typeEnv arrayElemTys
                inputArrays outputArrays scalarInputs =
  unlines
    [ "#include <metal_stdlib>"
    , "using namespace metal;"
    , ""
    , mslShapeHelpers
    , ""
    , "kernel void " ++ sanitize kernelName ++ "("
    ] ++
  intercalate ",\n" (map ("    " ++) allParams) ++ ")\n" ++
  unlines
    [ "{"
    ] ++
  genMSLBody (head (C2.lsIters loopSpec)) inputArrSet outputArrSet typeEnv arrayElemTys loopBody ++
  "}\n"
  where
    inputArrSet  = Set.fromList inputArrays
    outputArrSet = Set.fromList outputArrays
    iter = head (C2.lsIters loopSpec)

    -- Buffer index assignment
    inputBufs  = zipWith (\v i -> (v, i)) inputArrays [0..]
    outputBufs = zipWith (\v i -> (v, i)) outputArrays [length inputArrays..]
    scalarBufs = zipWith (\v i -> (v, i)) scalarInputs [length inputArrays + length outputArrays..]

    allParams = inputParams ++ outputParams ++ scalarParams ++ [gidParam]

    inputParams  = [ "device const " ++ mslElemTy v ++ "* " ++ sanitize v ++ "_data [[buffer("
                     ++ show i ++ ")]]"
                   | (v, i) <- inputBufs ]
    outputParams = [ "device " ++ mslElemTy v ++ "* " ++ sanitize v ++ "_data [[buffer("
                     ++ show i ++ ")]]"
                   | (v, i) <- outputBufs ]
    scalarParams = [ "constant " ++ mslScalarTy v ++ "& " ++ sanitize v ++ " [[buffer("
                     ++ show i ++ ")]]"
                   | (v, i) <- scalarBufs ]
    gidParam     = "uint " ++ sanitize iter ++ " [[thread_position_in_grid]]"

    mslElemTy v = case Map.lookup v arrayElemTys of
      Just elt -> mslTypeName elt
      Nothing  -> case Map.lookup v typeEnv of
        Just (CTArray elt) -> mslTypeName elt
        _                  -> "long"

    mslScalarTy v = case Map.lookup v typeEnv of
      Just ct -> mslTypeName ct
      Nothing -> "long"

-- | Emit MSL body statements.
genMSLBody
  :: CVar          -- ^ Loop iterator (= gid in the kernel)
  -> Set CVar      -- ^ Input array vars
  -> Set CVar      -- ^ Output array vars
  -> TypeEnv
  -> Map CVar CType
  -> [C2.Stmt]
  -> String
genMSLBody iter inArrs outArrs typeEnv arrayElemTys stmts =
  concatMap (genMSLStmt 1 iter inArrs outArrs typeEnv arrayElemTys) stmts

genMSLStmt :: Int -> CVar -> Set CVar -> Set CVar -> TypeEnv -> Map CVar CType -> C2.Stmt -> String
genMSLStmt depth iter inArrs outArrs typeEnv arrayElemTys stmt =
  let ind = replicate (depth * 4) ' '
  in case stmt of
    C2.SAssign v _
      | v `Set.member` inArrs ->
          -- This array is a pre-fetched Metal buffer parameter; skip the call in the kernel.
          ""
    C2.SAssign v rhs ->
          ind ++ mslVarDecl v typeEnv ++ " " ++ sanitize v ++ " = " ++
          genMSLRHS iter inArrs outArrs typeEnv arrayElemTys rhs ++ ";\n"

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
                  let accCType = case Map.lookup (C2.rsAccVar r) typeEnv of
                        Just ct -> mslTypeName ct
                        Nothing -> "long"
                  in ind ++ accCType ++ " " ++ sanitize (C2.rsAccVar r) ++ " = " ++
                     genMSLIndexExpr (C2.rsInit r) ++ ";\n"
                Nothing -> ""
              loopLine = ind ++ "for (uint " ++ ci ++ " = 0; " ++ ci ++ " < " ++
                         genMSLIndexExpr b ++ "; " ++ ci ++ "++) {\n"
              bodyLines = concatMap (genMSLStmt (depth+1) iter inArrs outArrs typeEnv arrayElemTys) body
              closeLine = ind ++ "}\n"
          in initLine ++ loopLine ++ bodyLines ++ closeLine
        _ ->
          -- Multi-dim inner loops: emit nested for loops
          let bodyLines = concatMap (genMSLStmt (depth+1) iter inArrs outArrs typeEnv arrayElemTys) body
              mkLoop (ci, b) inner =
                ind ++ "for (uint " ++ sanitize ci ++ " = 0; " ++ sanitize ci ++ " < " ++
                genMSLIndexExpr b ++ "; " ++ sanitize ci ++ "++) {\n" ++ inner ++ ind ++ "}\n"
          in foldr mkLoop bodyLines (zip (C2.lsIters spec) (C2.lsBounds spec))

    C2.SIf cond thn els ->
      ind ++ "if (" ++ genMSLAtom iter cond ++ ") {\n" ++
      concatMap (genMSLStmt (depth+1) iter inArrs outArrs typeEnv arrayElemTys) thn ++
      (case els of
         [] -> ind ++ "}\n"
         _  -> ind ++ "} else {\n" ++
               concatMap (genMSLStmt (depth+1) iter inArrs outArrs typeEnv arrayElemTys) els ++
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
genMSLRHS :: CVar -> Set CVar -> Set CVar -> TypeEnv -> Map CVar CType -> RHS -> String
genMSLRHS iter inArrs outArrs typeEnv arrayElemTys rhs = case rhs of
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
  RShapeSize shp ->
    "(uint)" ++ genMSLAtom iter shp
  RShapeInit shp ->
    genMSLAtom iter shp  -- treat as pass-through for scalar shapes
  RShapeLast shp ->
    genMSLAtom iter shp
  R2DToFlat i w ->
    "((uint)(" ++ genMSLAtom iter i ++ ") * (uint)(" ++ genMSLAtom iter w ++ "))"
  RCall fn args ->
    sanitize fn ++ "(" ++ intercalate ", " (map (genMSLAtom iter) args) ++ ")"
  RPairMake _ _ a1 a2 ->
    "{.fst = " ++ genMSLAtom iter a1 ++ ", .snd = " ++ genMSLAtom iter a2 ++ "}"
  RPairFst _ a -> genMSLAtom iter a ++ ".fst"
  RPairSnd _ a -> genMSLAtom iter a ++ ".snd"
  RProj 0 (AVar src) -> genMSLAtom iter (AVar src) ++ ".fst"
  RProj 1 (AVar src) -> genMSLAtom iter (AVar src) ++ ".snd"
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
  -> String             -- ^ Helper C source (already generated)
  -> String
genObjCHarnessSrc _prog _kernelProc kernelName preLoopStmts loopSpec _retAtom
                  typeEnv arrayElemTys retKinds _retTypes
                  inputArrays outputArrays scalarInputs helperC =
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
    , "    // --- CPU pre-loop setup ---"
    ] ++
    preLoopLines ++
    [ ""
    , "    // Grid size from loop bound"
    , "    long _n = (long)" ++ genMSLIndexExpr (head (C2.lsBounds loopSpec)) ++ ";"
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
    totalBufs = length inputArrays + length outputArrays + length scalarInputs

    -- Pre-loop C statements (skip output array allocations)
    preLoopLines = concatMap (genPreLoopLine typeEnv retKinds (Set.fromList outputArrays)) preLoopStmts

    -- Input buffer creation + fill
    -- Note: CPU arrays storing CTDouble use 8-byte doubles, but Metal uses 4-byte floats.
    -- We must convert element-by-element for float arrays.
    inputBufLines = concat
      [ [ "    // Input buffer " ++ show i ++ ": " ++ sanitize v
        , "    size_t _buf" ++ show i ++ "_sz = (size_t)(_n * sizeof(" ++ elemTy ++ "));"
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
          , "      for (long _fci = 0; _fci < _n; _fci++) _fdst[_fci] = (float)_fsrc[_fci]; }"
          ]
        fillLine i v _ =
          -- Same element size on CPU and GPU (int64 / long): plain memcpy
          [ "    memcpy(_buf" ++ show i ++ ".contents, " ++ sanitize v ++ "->data, _buf" ++ show i ++ "_sz);" ]

    -- Output buffer creation
    outputBufLines = concat
      [ [ "    // Output buffer " ++ show i ++ ": " ++ sanitize v
        , "    size_t _buf" ++ show i ++ "_sz = (size_t)(_n * sizeof(" ++ elemTy ++ "));"
        , "    id<MTLBuffer> _buf" ++ show i ++ " = [_dev newBufferWithLength:_buf" ++ show i
          ++ "_sz options:MTLResourceStorageModeShared];"
        , "    memset(_buf" ++ show i ++ ".contents, 0, _buf" ++ show i ++ "_sz);"
        ]
      | (v, i) <- zip outputArrays [length inputArrays..]
      , let elemTy = mslElemTy v
      ]

    -- Scalar buffer creation (constant buffers)
    scalarBufLines = concat
      [ [ "    // Scalar buffer " ++ show i ++ ": " ++ sanitize v
        , "    " ++ scalarTy v ++ " _scalar" ++ show i ++ " = " ++ sanitize v ++ ";"
        , "    id<MTLBuffer> _buf" ++ show i ++ " = [_dev newBufferWithBytes:&_scalar" ++ show i
          ++ " length:sizeof(" ++ scalarTy v ++ ") options:MTLResourceStorageModeShared];"
        ]
      | (v, i) <- zip scalarInputs [length inputArrays + length outputArrays..]
      ]

    -- setBuffer calls
    setBufLines =
      [ "    [_enc setBuffer:_buf" ++ show i ++ " offset:0 atIndex:" ++ show i ++ "];"
      | i <- [0 .. totalBufs - 1]
      ]

    -- Output printing
    printLines = concat
      [ genOutputPrintLines v i
      | (v, i) <- zip outputArrays [length inputArrays..]
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
      in [ "    // Print output: " ++ sanitize v
         , "    " ++ eTy ++ "* _out" ++ show i ++ " = (" ++ eTy ++ "*)_buf" ++ show i ++ ".contents;"
         , "    printf(\"[\");"
         , "    for (long _pi = 0; _pi < _n; _pi++) {"
         , "        if (_pi > 0) printf(\", \");"
         , "        printf(\"" ++ fmt ++ "\", " ++ cast ++ "_out" ++ show i ++ "[_pi]);"
         , "    }"
         , "    printf(\"]\\n\");"
         ]

-- | Generate a single pre-loop C statement for the ObjC harness main.
-- Returns empty list for output array allocations (replaced by Metal buffers).
genPreLoopLine :: TypeEnv -> Map CVar VarKind -> Set CVar -> C2.Stmt -> [String]
genPreLoopLine typeEnv retKinds skipAlloc stmt = case stmt of
  C2.SAssign v (RArrayAlloc _) | v `Set.member` skipAlloc ->
    []  -- output array: skip CPU alloc, will be Metal buffer
  C2.SAssign v rhs ->
    [ "    " ++ preLoopCType typeEnv retKinds v rhs ++ " " ++ sanitize v ++ " = " ++
      genPreLoopRHS retKinds rhs ++ ";"
    ]
  _ -> []  -- skip loops, returns, etc. in pre-loop

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
      _ -> "int64_t"

-- | C expression for a pre-loop RHS (used in ObjC harness main).
genPreLoopRHS :: Map CVar VarKind -> RHS -> String
genPreLoopRHS retKinds rhs = case rhs of
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
  RProj i a         -> genCAtom a ++ ".elems[" ++ show i ++ "]"
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
  [ "// Shape helpers"
  , "static inline uint hyd_metal_shape_size(constant uint* s, int n) {"
  , "    uint r = 1;"
  , "    for (int i = 0; i < n; i++) r *= s[i];"
  , "    return r;"
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
