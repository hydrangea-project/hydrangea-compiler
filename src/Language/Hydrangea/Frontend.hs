-- |
-- Module: Language.Hydrangea.Frontend
--
-- High-level compiler entry points for parsing, type checking, lowering,
-- evaluation, and optimized C code generation.
module Language.Hydrangea.Frontend where

import Control.Monad.Except (runExceptT)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Functor.Fixedpoint (unFix)
import qualified Data.Map as Map
import qualified Data.Map.Strict as M

import Language.Hydrangea.Lexer hiding (Forall)
import Language.Hydrangea.Parser
import Language.Hydrangea.Syntax
import Language.Hydrangea.Infer
import Language.Hydrangea.Interpreter
import Language.Hydrangea.ErrorFormat (formatTypeError, formatEvalError)
import Language.Hydrangea.Fusion
import Language.Hydrangea.ShapeNormalize (normalizeShapesExp, normalizeShapesDecs)
import Language.Hydrangea.Uniquify
import Language.Hydrangea.CFG qualified as C2
import Language.Hydrangea.CFGCore (CType(..))
import Language.Hydrangea.Lowering (lowerDecs2, lowerDecs2WithTypeEnv)
import Language.Hydrangea.CFGPipeline (parallelPipeline2, parallelPipelineWithWidth, vectorizePipeline2, vectorizePipelineWithWidth, metalPipeline2)
import Language.Hydrangea.CodegenC
  ( CodegenArtifacts(..)
  , CodegenOptions(..)
  , codegenProgram2
  , codegenProgram2Prune
  , codegenProgram2WithOptionsPrune
  , defaultCodegenOptions
  )

-- | Normalize and fuse declarations before lowering or code generation.
preprocessDecs :: [Dec Range] -> [Dec Range]
preprocessDecs decs =
  let u = uniquifyDecs decs
      n = normalizeShapesDecs u
      f = fuseDecs n
  in f

formatInferResult :: Either err a -> (err -> String) -> Either String a
formatInferResult result formatErr =
  case result of
    Left err -> Left (formatErr err)
    Right value -> Right value

eraseRange :: Functor f => f Range -> f ()
eraseRange = fmap (const ())

-- | Parse a single source expression.
readExp :: ByteString -> Either String (Exp Range)
readExp str = runAlex str parseMiniML

-- | Parse a standalone polymorphic type annotation.
readPolytype :: ByteString -> Either String Polytype
readPolytype str = runAlex str parsePolytype

-- | Parse a sequence of top-level declarations.
readDecs :: ByteString -> Either String [Dec Range]
readDecs str = runAlex str parseDecs

-- | Infer top-level declaration types and format any inference errors.
inferDecsTop :: [Dec Range] -> IO (Either String [(Var, Polytype)])
inferDecsTop = inferDecsTopWithOptions defaultInferOptions

inferDecsTopWithOptions :: InferOptions -> [Dec Range] -> IO (Either String [(Var, Polytype)])
inferDecsTopWithOptions opts ds = do
  res <- runInferDecsWithOptions opts ds
  pure $ fmap fst $ formatInferResult res (formatTypeError Nothing Nothing)

-- | Like 'inferDecsTop' but also returns any accumulated warnings.
inferDecsTopWithWarnings :: [Dec Range] -> IO (Either String ([(Var, Polytype)], [String]))
inferDecsTopWithWarnings = inferDecsTopWithWarningsOptions defaultInferOptions

inferDecsTopWithWarningsOptions :: InferOptions -> [Dec Range] -> IO (Either String ([(Var, Polytype)], [String]))
inferDecsTopWithWarningsOptions opts ds = do
  res <- runInferDecsWithOptions opts ds
  pure $ formatInferResult res (formatTypeError Nothing Nothing)

-- | Convert a monomorphic 'Type' to the corresponding CFG 'CType'.
--
-- Polymorphic and function types do not have a single concrete CFG
-- representation and therefore return 'Nothing'.
typeOfType :: Type -> Maybe CType
typeOfType ty = case unFix ty of
  TyIntF               -> Just CTInt64
  TyFloatF             -> Just CTDouble
  TyBoolF              -> Just CTBool
  TyUnitF              -> Just CTUnit
  TyStringF            -> Just CTInt64
  TyVarF _             -> Nothing
  TyRefineF _ inner    -> typeOfType inner
  TyConsF _ _          -> Just CTTuple
  TyPairF t1 t2        -> CTPair <$> typeOfType t1 <*> typeOfType t2
  TyRecordF fields     -> CTRecord <$> mapM (\(field, fieldTy) -> do
                                 cty <- typeOfType fieldTy
                                 pure (field, cty)) fields
  TyArrayF _ elt       -> CTArray <$> typeOfType elt
  TyFunF _ _           -> Nothing

-- | Convert a 'Polytype' to a 'CType'. Returns 'Nothing' if the type is
-- genuinely polymorphic (has quantified variables) or is a function type.
typeOfPolytype :: Polytype -> Maybe CType
typeOfPolytype (Forall [] _ ty) = typeOfType ty
typeOfPolytype (Forall _ _ _) = Nothing

-- | Run type inference on a list of declarations and return a map from
-- declaration name to its concrete 'CType'. Polymorphic declarations
-- (those that cannot be represented as a single C type) are omitted.
inferTopLevelTypes :: [Dec Range] -> IO (Map.Map Var CType)
inferTopLevelTypes = inferTopLevelTypesWithOptions defaultInferOptions

inferTopLevelTypesWithOptions :: InferOptions -> [Dec Range] -> IO (Map.Map Var CType)
inferTopLevelTypesWithOptions opts decs = do
  res <- runInferDecsWithOptions opts decs
  case res of
    Left _            -> pure Map.empty
    Right (pairs, _) -> pure $
      M.fromList [(v, ct) | (v, poly) <- pairs, Just ct <- [typeOfPolytype poly]]

-- | Type-check a single expression and return its inferred polytype.
typeCheckExp :: Exp Range -> IO (Either String Polytype)
typeCheckExp = typeCheckExpWithOptions defaultInferOptions

typeCheckExpWithOptions :: InferOptions -> Exp Range -> IO (Either String Polytype)
typeCheckExpWithOptions opts e = do
  res <- runInferWithOptions opts (infer e)
  pure $ formatInferResult res (formatTypeError Nothing Nothing)

-- | Fuse an expression after successful type inference.
--
-- Shape normalization runs before fusion so the fusion pass sees a more
-- canonical input program.
fuseExpAfterTypeCheck :: Exp Range -> IO (Either String (Exp Range))
fuseExpAfterTypeCheck = fuseExpAfterTypeCheckWithOptions defaultInferOptions

fuseExpAfterTypeCheckWithOptions :: InferOptions -> Exp Range -> IO (Either String (Exp Range))
fuseExpAfterTypeCheckWithOptions opts e = do
  res <- runInferWithOptions opts (infer e)
  pure $
    case formatInferResult res (formatTypeError Nothing Nothing) of
      Left err -> Left err
      Right _ -> Right (fuseExp (normalizeShapesExp (uniquifyExp e)))

-- | Lower declarations directly to canonical CFG IR.
lowerToCFG2 :: [Dec Range] -> C2.Program
lowerToCFG2 = lowerDecs2

lowerToCFG2WithConcreteTypes :: Map.Map Var CType -> [Dec Range] -> C2.Program
lowerToCFG2WithConcreteTypes typeEnv = lowerDecs2WithTypeEnv typeEnv . preprocessDecs

inferAndLowerToCFG2 :: InferOptions -> [Dec Range] -> IO C2.Program
inferAndLowerToCFG2 opts decs = do
  typeEnv <- inferTopLevelTypesWithOptions opts decs
  let pre = preprocessDecs decs
  let prog = lowerDecs2WithTypeEnv typeEnv pre
  pure prog

-- | Lower declarations to CFG using inferred top-level concrete types.
lowerToCFG2WithTypes :: [Dec Range] -> IO C2.Program
lowerToCFG2WithTypes = lowerToCFG2WithTypesWithOptions defaultInferOptions

lowerToCFG2WithTypesWithOptions :: InferOptions -> [Dec Range] -> IO C2.Program
lowerToCFG2WithTypesWithOptions = inferAndLowerToCFG2

-- | Lower declarations to CFG with the optimization/vectorization pipeline.
lowerToCFG2Opt :: [Dec Range] -> C2.Program
lowerToCFG2Opt = optimizeCFG2 . lowerToCFG2

-- | Lower declarations with the same preprocessing used by optimized C emission,
-- then run the CFG optimization pipeline.
lowerToCFG2OptWithTypes :: [Dec Range] -> IO C2.Program
lowerToCFG2OptWithTypes = lowerToCFG2OptWithTypesWithOptions defaultInferOptions

lowerToCFG2OptWithTypesWithOptions :: InferOptions -> [Dec Range] -> IO C2.Program
lowerToCFG2OptWithTypesWithOptions opts decs = optimizeCFG2 <$> lowerToCFG2WithTypesWithOptions opts decs

-- | Lower declarations to CFG with the optimization + parallelization pipeline.
lowerToCFG2OptParallel :: [Dec Range] -> C2.Program
lowerToCFG2OptParallel = optimizeParallelCFG2 . lowerToCFG2

-- | Compile declarations to optimized C without parallelization.
--
-- The frontend runs uniquification, shape normalization, and fusion before
-- lowering, and can optionally prune dead procedures before code generation.
compileToCOptWithPrune :: Bool -> [Dec Range] -> String
compileToCOptWithPrune = compileSourceDecsToC compileToCFG2OptPrune

-- | Compile declarations to optimized C without parallelization.
compileToCOpt :: [Dec Range] -> String
compileToCOpt = compileToCOptWithPrune False

-- | IO variant of 'compileToCOptWithPrune' that threads inferred concrete types
-- into lowering.
compileToCOptIO :: Bool -> [Dec Range] -> IO String
compileToCOptIO = compileToCOptIOWithOptions defaultInferOptions

compileToCOptIOWithOptions :: InferOptions -> Bool -> [Dec Range] -> IO String
compileToCOptIOWithOptions = compileSourceDecsToCIO compileToCFG2OptPrune

-- | Compile declarations to optimized C with parallelization enabled.
compileToCOptParallelWithPrune :: Bool -> [Dec Range] -> String
compileToCOptParallelWithPrune = compileSourceDecsToC compileToCFG2OptParallelPrune

-- | Compile declarations to optimized C with parallelization enabled.
compileToCOptParallel :: [Dec Range] -> String
compileToCOptParallel = compileToCOptParallelWithPrune False

-- | IO variant of 'compileToCOptParallelWithPrune' that threads inferred
-- concrete types into lowering.
compileToCOptParallelIO :: Bool -> [Dec Range] -> IO String
compileToCOptParallelIO = compileToCOptParallelIOWithOptions defaultInferOptions

compileToCOptParallelIOWithOptions :: InferOptions -> Bool -> [Dec Range] -> IO String
compileToCOptParallelIOWithOptions = compileSourceDecsToCIO compileToCFG2OptParallelPrune

compileSourceDecsToC :: (Bool -> C2.Program -> String) -> Bool -> [Dec Range] -> String
compileSourceDecsToC compile prune = compile prune . lowerToCFG2 . preprocessDecs

compileSourceDecsToCIO
  :: (Bool -> C2.Program -> String)
  -> InferOptions
  -> Bool
  -> [Dec Range]
  -> IO String
compileSourceDecsToCIO compile opts prune decs =
  compile prune <$> inferAndLowerToCFG2 opts decs

-- | CFG-native optimized compilation (vectorization, no parallel)
compileToCFG2Opt :: C2.Program -> String
compileToCFG2Opt prog = codegenProgram2 (vectorizePipeline2 prog)

-- | CFG-native optimized compilation (vectorization + parallel)
compileToCFG2OptParallel :: C2.Program -> String
compileToCFG2OptParallel prog = codegenProgram2 (parallelPipeline2 prog)

-- | Compile optimized CFG to C, optionally pruning dead procedures first.
compileToCFG2OptPrune :: Bool -> C2.Program -> String
compileToCFG2OptPrune prune prog = codegenProgram2Prune prune (vectorizePipeline2 prog)

-- | Compile an optimized and parallelized CFG program to C, optionally pruning dead procedures.
compileToCFG2OptParallelPrune :: Bool -> C2.Program -> String
compileToCFG2OptParallelPrune prune prog = codegenProgram2Prune prune (parallelPipeline2 prog)

-- | Run the CFG optimization pipeline on a CFG program.
optimizeCFG2 :: C2.Program -> C2.Program
optimizeCFG2 = vectorizePipeline2

-- | Run the CFG parallelization pipeline on a CFG program.
optimizeParallelCFG2 :: C2.Program -> C2.Program
optimizeParallelCFG2 = parallelPipeline2

-- | Run the Metal-targeted pipeline: optimize + parallelize without SIMD vectorization.
optimizeMetalCFG2 :: C2.Program -> C2.Program
optimizeMetalCFG2 = metalPipeline2

-- | Compile optimized CFG to C using the given 'CodegenOptions'.
compileToCFG2OptPruneWithOpts :: CodegenOptions -> Bool -> C2.Program -> String
compileToCFG2OptPruneWithOpts opts prune prog =
  case codegenProgram2WithOptionsPrune opts prune (vectorizePipelineWithWidth (codegenSimdWidth opts) prog) of
    Right artifacts -> codegenSource artifacts
    Left err -> error err

-- | Compile optimized and parallelized CFG to C using the given 'CodegenOptions'.
compileToCFG2OptParallelPruneWithOpts :: CodegenOptions -> Bool -> C2.Program -> String
compileToCFG2OptParallelPruneWithOpts opts prune prog =
  case codegenProgram2WithOptionsPrune opts prune (parallelPipelineWithWidth (codegenSimdWidth opts) prog) of
    Right artifacts -> codegenSource artifacts
    Left err -> error err

-- | IO compile variant that accepts full 'CodegenOptions'.
compileToCOptIOWithCodegenOptions :: InferOptions -> CodegenOptions -> Bool -> [Dec Range] -> IO String
compileToCOptIOWithCodegenOptions inferOpts codegenOpts prune decs =
  compileToCFG2OptPruneWithOpts codegenOpts prune <$> inferAndLowerToCFG2 inferOpts decs

-- | IO parallel compile variant that accepts full 'CodegenOptions'.
compileToCOptParallelIOWithCodegenOptions :: InferOptions -> CodegenOptions -> Bool -> [Dec Range] -> IO String
compileToCOptParallelIOWithCodegenOptions inferOpts codegenOpts prune decs =
  compileToCFG2OptParallelPruneWithOpts codegenOpts prune <$> inferAndLowerToCFG2 inferOpts decs

-- | Erase source-range annotations from an expression.
stripRange :: Exp Range -> Either String (Exp ())
stripRange = pure . eraseRange

-- | Evaluate an expression after stripping source locations.
evalExpFrontend :: Exp Range -> IO (Either EvalError Value)
evalExpFrontend e = runExceptT $ evalExp (eraseRange e) Map.empty

-- | Evaluate declarations and return the resulting environment.
evalDecsFrontend :: [Dec Range] -> IO (Either EvalError Env)
evalDecsFrontend ds = runExceptT $ evalDecs (map eraseRange ds)

-- | Parse and evaluate an expression from source text.
readAndEval :: ByteString -> IO (Either String Value)
readAndEval str =
  case readExp str of
    Left err -> pure (Left err)
    Right e -> do
      res <- evalExpFrontend e
      pure $ case res of
        Left err -> Left (formatEvalError Nothing Nothing err)
        Right v -> Right v

-- | Parse, type-check, and evaluate declarations from source text.
readAndEvalDecs :: ByteString -> IO (Either String [(Var, Polytype, Maybe Value)])
readAndEvalDecs str =
  case readDecs str of
    Left err -> pure (Left err)
    Right ds -> typeCheckAndEvalDecs ds

-- | Type-check and evaluate declarations.
typeCheckAndEvalDecs :: [Dec Range] -> IO (Either String [(Var, Polytype, Maybe Value)])
typeCheckAndEvalDecs = typeCheckAndEvalDecsWithOptions defaultInferOptions

typeCheckAndEvalDecsWithOptions :: InferOptions -> [Dec Range] -> IO (Either String [(Var, Polytype, Maybe Value)])
typeCheckAndEvalDecsWithOptions opts decs = do
  res <- runInferDecsWithOptions opts decs
  case fmap fst $ formatInferResult res (formatTypeError Nothing Nothing) of
    Left err -> pure (Left err)
    Right typedDecs -> do
      evaled <- evalDecsFrontend decs
      pure $
        case evaled of
          Left err -> Left (formatEvalError Nothing Nothing err)
          Right env -> Right (map (\(v, ty) -> (v, ty, Map.lookup v env)) typedDecs)
