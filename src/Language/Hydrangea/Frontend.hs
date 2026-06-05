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
import qualified Data.Set as Set

import Language.Hydrangea.Lexer hiding (Forall)
import Language.Hydrangea.Parser
import Language.Hydrangea.Syntax
import Language.Hydrangea.Infer
import Language.Hydrangea.Interpreter
import Language.Hydrangea.ErrorFormat (formatTypeError, formatEvalError)
import Language.Hydrangea.Fusion
import Language.Hydrangea.ShapeNormalize (normalizeShapesExp, normalizeShapesDecs)
import Language.Hydrangea.Uniquify
import Language.Hydrangea.Specialize (monomorphizeDecs, polymorphicFnNames)
import Language.Hydrangea.CFG qualified as CFG
import Language.Hydrangea.CFGCore (CType(..))
import Language.Hydrangea.Lowering (lowerDecs, lowerDecsWithTypeEnv, lowerDecsWithTypeEnvAndRanks, lowerDecsWithEnvs)
import Language.Hydrangea.CFGPipeline
  ( PipelineOptions(..)
  , metalPipeline
  , metalPipelineWithTiling
  , parallelPipeline
  , parallelPipelineWithWidth
  , pipelineWithOptions
  , vectorizePipeline
  , vectorizePipelineWithWidth
  )
import Language.Hydrangea.CodegenC
  ( CodegenArtifacts(..)
  , CodegenOptions(..)
  , codegenProgram
  , codegenProgramPrune
  , codegenProgramWithOptionsPrune
  )

-- | Options controlling the high-level frontend preprocessing pipeline.
data FrontendOptions = FrontendOptions
  { frontendSkipFusion :: Bool
    -- ^ When 'True', skip the array-fusion pass entirely.  Intermediate
    --   arrays that would normally be fused away are materialised; useful
    --   for benchmarking the impact of fusion.
  } deriving (Eq, Show)

defaultFrontendOptions :: FrontendOptions
defaultFrontendOptions = FrontendOptions { frontendSkipFusion = False }

-- | Normalize and fuse declarations before lowering or code generation.
preprocessDecs :: [Dec Range] -> [Dec Range]
preprocessDecs = preprocessDecsWithOptions defaultFrontendOptions

-- | Like 'preprocessDecs' but respects 'FrontendOptions'.
preprocessDecsWithOptions :: FrontendOptions -> [Dec Range] -> [Dec Range]
preprocessDecsWithOptions opts decs =
  let u = uniquifyDecs decs
      n = normalizeShapesDecs u
  in if frontendSkipFusion opts then n else fuseDecs n

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

-- | Count the rank (number of dimensions) of an array type.
rankOfType :: Type -> Maybe Int
rankOfType ty = case unFix ty of
  TyArrayF shape _ -> Just (countCons shape)
  TyRefineF _ inner -> rankOfType inner
  _ -> Nothing
  where
    countCons t = case unFix t of
      TyConsF _ rest -> 1 + countCons rest
      _ -> 0

-- | Extract the rank from a polytype, or 'Nothing' if polymorphic / not an array.
rankOfPolytype :: Polytype -> Maybe Int
rankOfPolytype (Forall [] _ ty) = rankOfType ty
rankOfPolytype (Forall _ _ _)   = Nothing

-- | Compute top-level type and rank maps from type inference.
inferTopLevelTypesAndRanksWithOptions
  :: InferOptions -> [Dec Range] -> IO (Map.Map Var CType, Map.Map Var Int)
inferTopLevelTypesAndRanksWithOptions opts decs = do
  res <- runInferDecsWithOptions opts decs
  case res of
    Left _ -> pure (Map.empty, Map.empty)
    Right (pairs, _) -> pure
      ( M.fromList [(v, ct) | (v, poly) <- pairs, Just ct <- [typeOfPolytype poly]]
      , M.fromList [(v, r)  | (v, poly) <- pairs, Just r  <- [rankOfPolytype poly]]
      )


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
lowerToCFG :: [Dec Range] -> CFG.Program
lowerToCFG = lowerDecs

lowerToCFGWithConcreteTypes :: Map.Map Var CType -> [Dec Range] -> CFG.Program
lowerToCFGWithConcreteTypes typeEnv = lowerDecsWithTypeEnv typeEnv . preprocessDecs

inferAndLowerToCFG :: InferOptions -> [Dec Range] -> IO CFG.Program
inferAndLowerToCFG = inferAndLowerToCFGWithFrontendOptions defaultFrontendOptions

inferAndLowerToCFGWithFrontendOptions :: FrontendOptions -> InferOptions -> [Dec Range] -> IO CFG.Program
inferAndLowerToCFGWithFrontendOptions frontOpts opts decs = do
  -- Normalize, fuse, and uniquify the source program.
  let pre = preprocessDecsWithOptions frontOpts decs
  -- Whole-program monomorphization: specialize polymorphic top-level functions
  -- so every binder sits in a concrete (ground) context before lowering.
  mono <- monomorphizeProgram opts pre
  -- Recover concrete per-binder types and top-level types/ranks from the
  -- monomorphic program that we actually lower.
  binderEnv <- inferBinderTypeEnvWithOptions opts mono
  (typeEnv, rankEnv) <- inferTopLevelTypesAndRanksWithOptions opts mono
  let prog = lowerDecsWithEnvs typeEnv rankEnv binderEnv mono
  pure prog

-- | Run the whole-program monomorphization pass: discover polymorphic
-- top-level functions, specialize them per call site, and restore globally
-- unique binder names.  On inference failure (or when there is nothing
-- polymorphic to specialize) this is the identity, preserving the exact
-- declarations that were previously lowered.
monomorphizeProgram :: InferOptions -> [Dec Range] -> IO [Dec Range]
monomorphizeProgram opts pre = do
  res <- runInferDecsWithOptions opts pre
  case res of
    Right (schemes, _warnings) ->
      let polyNames = polymorphicFnNames schemes pre
      in if Set.null polyNames
           then pure pre
           else pure (uniquifyDecsForce (monomorphizeDecs polyNames pre))
    Left _ -> pure pre

-- | Infer the monomorphic program and project each ground value-binder type to
-- its CFG 'CType'.  Polymorphic or non-representable binders are omitted.
inferBinderTypeEnvWithOptions :: InferOptions -> [Dec Range] -> IO (Map.Map Var CType)
inferBinderTypeEnvWithOptions opts decs = do
  res <- runInferDecsBinderTypes opts decs
  case res of
    Left _      -> pure Map.empty
    Right binds -> pure $ M.fromList [(v, ct) | (v, ty) <- M.toList binds, Just ct <- [typeOfType ty]]

-- | Lower declarations to CFG using inferred top-level concrete types.
lowerToCFGWithTypes :: [Dec Range] -> IO CFG.Program
lowerToCFGWithTypes = lowerToCFGWithTypesWithOptions defaultInferOptions

lowerToCFGWithTypesWithOptions :: InferOptions -> [Dec Range] -> IO CFG.Program
lowerToCFGWithTypesWithOptions = inferAndLowerToCFG

-- | Lower declarations to CFG with the optimization/vectorization pipeline.
lowerToCFGOpt :: [Dec Range] -> CFG.Program
lowerToCFGOpt = optimizeCFG . lowerToCFG

-- | Lower declarations with the same preprocessing used by optimized C emission,
-- then run the CFG optimization pipeline.
lowerToCFGOptWithTypes :: [Dec Range] -> IO CFG.Program
lowerToCFGOptWithTypes = lowerToCFGOptWithTypesWithOptions defaultInferOptions

lowerToCFGOptWithTypesWithOptions :: InferOptions -> [Dec Range] -> IO CFG.Program
lowerToCFGOptWithTypesWithOptions opts decs = optimizeCFG <$> lowerToCFGWithTypesWithOptions opts decs

-- | Lower declarations to CFG with the optimization + parallelization pipeline.
lowerToCFGOptParallel :: [Dec Range] -> CFG.Program
lowerToCFGOptParallel = optimizeParallelCFG . lowerToCFG

-- | Compile declarations to optimized C without parallelization.
--
-- The frontend runs uniquification, shape normalization, and fusion before
-- lowering, and can optionally prune dead procedures before code generation.
compileToCOptWithPrune :: Bool -> [Dec Range] -> String
compileToCOptWithPrune = compileSourceDecsToC compileToCFGOptPrune

-- | Compile declarations to optimized C without parallelization.
compileToCOpt :: [Dec Range] -> String
compileToCOpt = compileToCOptWithPrune False

-- | IO variant of 'compileToCOptWithPrune' that threads inferred concrete types
-- into lowering.
compileToCOptIO :: Bool -> [Dec Range] -> IO String
compileToCOptIO = compileToCOptIOWithOptions defaultInferOptions

compileToCOptIOWithOptions :: InferOptions -> Bool -> [Dec Range] -> IO String
compileToCOptIOWithOptions = compileSourceDecsToCIO compileToCFGOptPrune

-- | Compile declarations to optimized C with parallelization enabled.
compileToCOptParallelWithPrune :: Bool -> [Dec Range] -> String
compileToCOptParallelWithPrune = compileSourceDecsToC compileToCFGOptParallelPrune

-- | Compile declarations to optimized C with parallelization enabled.
compileToCOptParallel :: [Dec Range] -> String
compileToCOptParallel = compileToCOptParallelWithPrune False

-- | IO variant of 'compileToCOptParallelWithPrune' that threads inferred
-- concrete types into lowering.
compileToCOptParallelIO :: Bool -> [Dec Range] -> IO String
compileToCOptParallelIO = compileToCOptParallelIOWithOptions defaultInferOptions

compileToCOptParallelIOWithOptions :: InferOptions -> Bool -> [Dec Range] -> IO String
compileToCOptParallelIOWithOptions = compileSourceDecsToCIO compileToCFGOptParallelPrune

compileSourceDecsToC :: (Bool -> CFG.Program -> String) -> Bool -> [Dec Range] -> String
compileSourceDecsToC compile prune = compile prune . lowerToCFG . preprocessDecs

compileSourceDecsToCIO
  :: (Bool -> CFG.Program -> String)
  -> InferOptions
  -> Bool
  -> [Dec Range]
  -> IO String
compileSourceDecsToCIO compile opts prune decs =
  compile prune <$> inferAndLowerToCFG opts decs

-- | CFG-native optimized compilation (vectorization, no parallel)
compileToCFGOpt :: CFG.Program -> String
compileToCFGOpt prog = codegenProgram (vectorizePipeline prog)

-- | CFG-native optimized compilation (vectorization + parallel)
compileToCFGOptParallel :: CFG.Program -> String
compileToCFGOptParallel prog = codegenProgram (parallelPipeline prog)

-- | Compile optimized CFG to C, optionally pruning dead procedures first.
compileToCFGOptPrune :: Bool -> CFG.Program -> String
compileToCFGOptPrune prune prog = codegenProgramPrune prune (vectorizePipeline prog)

-- | Compile an optimized and parallelized CFG program to C, optionally pruning dead procedures.
compileToCFGOptParallelPrune :: Bool -> CFG.Program -> String
compileToCFGOptParallelPrune prune prog = codegenProgramPrune prune (parallelPipeline prog)

-- | Run the CFG optimization pipeline on a CFG program.
optimizeCFG :: CFG.Program -> CFG.Program
optimizeCFG = vectorizePipeline

-- | Run the configurable CFG optimization pipeline.
optimizeCFGWithPipelineOptions :: PipelineOptions -> CFG.Program -> CFG.Program
optimizeCFGWithPipelineOptions = pipelineWithOptions

-- | Run the CFG parallelization pipeline on a CFG program.
optimizeParallelCFG :: CFG.Program -> CFG.Program
optimizeParallelCFG = parallelPipeline

-- | Run the Metal-targeted pipeline: optimize + parallelize without SIMD vectorization.
optimizeMetalCFG :: CFG.Program -> CFG.Program
optimizeMetalCFG = metalPipeline

-- | Run the Metal-targeted pipeline with configurable tiling.
optimizeMetalCFGWithTiling :: Bool -> CFG.Program -> CFG.Program
optimizeMetalCFGWithTiling = metalPipelineWithTiling

-- | Compile optimized CFG to C using the given 'CodegenOptions'.
compileToCFGOptPruneWithOpts :: CodegenOptions -> Bool -> CFG.Program -> String
compileToCFGOptPruneWithOpts opts prune prog =
  case codegenProgramWithOptionsPrune opts prune (vectorizePipelineWithWidth (codegenSimdWidth opts) prog) of
    Right artifacts -> codegenSource artifacts
    Left err -> error err

-- | Compile optimized and parallelized CFG to C using the given 'CodegenOptions'.
compileToCFGOptParallelPruneWithOpts :: CodegenOptions -> Bool -> CFG.Program -> String
compileToCFGOptParallelPruneWithOpts opts prune prog =
  case codegenProgramWithOptionsPrune opts prune (parallelPipelineWithWidth (codegenSimdWidth opts) prog) of
    Right artifacts -> codegenSource artifacts
    Left err -> error err

-- | Compile CFG to C using explicit pipeline options and codegen options.
compileToCFGPruneWithPipelineAndOpts :: PipelineOptions -> CodegenOptions -> Bool -> CFG.Program -> String
compileToCFGPruneWithPipelineAndOpts pipelineOpts codegenOpts prune prog =
  let opts = pipelineOpts { poVectorWidth = codegenSimdWidth codegenOpts }
  in case codegenProgramWithOptionsPrune codegenOpts prune (pipelineWithOptions opts prog) of
       Right artifacts -> codegenSource artifacts
       Left err -> error err

-- | IO compile variant that accepts full 'CodegenOptions'.
compileToCOptIOWithCodegenOptions :: InferOptions -> CodegenOptions -> Bool -> [Dec Range] -> IO String
compileToCOptIOWithCodegenOptions inferOpts codegenOpts prune decs =
  compileToCFGOptPruneWithOpts codegenOpts prune <$> inferAndLowerToCFG inferOpts decs

-- | IO parallel compile variant that accepts full 'CodegenOptions'.
compileToCOptParallelIOWithCodegenOptions :: InferOptions -> CodegenOptions -> Bool -> [Dec Range] -> IO String
compileToCOptParallelIOWithCodegenOptions inferOpts codegenOpts prune decs =
  compileToCFGOptParallelPruneWithOpts codegenOpts prune <$> inferAndLowerToCFG inferOpts decs

-- | IO compile variant with explicit pipeline options and codegen options.
compileToCOptIOWithPipelineOptionsAndCodegenOptions
  :: InferOptions -> PipelineOptions -> CodegenOptions -> Bool -> [Dec Range] -> IO String
compileToCOptIOWithPipelineOptionsAndCodegenOptions inferOpts pipelineOpts codegenOpts prune decs =
  compileToCFGPruneWithPipelineAndOpts pipelineOpts codegenOpts prune <$> inferAndLowerToCFG inferOpts decs

-- | Like 'compileToCOptIOWithPipelineOptionsAndCodegenOptions' but also
-- accepts 'FrontendOptions' to e.g. disable the fusion pass.
compileToCOptIOWithAllOptions
  :: FrontendOptions -> InferOptions -> PipelineOptions -> CodegenOptions -> Bool -> [Dec Range] -> IO String
compileToCOptIOWithAllOptions frontOpts inferOpts pipelineOpts codegenOpts prune decs =
  compileToCFGPruneWithPipelineAndOpts pipelineOpts codegenOpts prune
    <$> inferAndLowerToCFGWithFrontendOptions frontOpts inferOpts decs

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
