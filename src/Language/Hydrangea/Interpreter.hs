{-# LANGUAGE OverloadedStrings, LambdaCase #-}

-- |
-- Module: Language.Hydrangea.Interpreter
--
-- A reference interpreter for Hydrangea that evaluates expressions and declarations
-- to concrete values. This interpreter prioritizes clarity and correctness over
-- performance.
--
-- The value representation includes:
-- - Primitive values (Int, Bool, String, Unit)
-- - Tuples/vectors as inductive cons lists
-- - Immutable arrays with explicit shape and row-major flattened storage
-- - Closures capturing the environment and pattern-matched parameters
-- - Operators as first-class values (curried binary operators)
--
-- Array semantics use row-major indexing where a shape is represented as a tuple
-- of integers (Int * Int * ... * ()) and indices are similarly structured.
module Language.Hydrangea.Interpreter where

-- Note: runnable examples are intentionally restricted to core modules.

import Control.Monad (foldM, forM, when, zipWithM)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy.Char8 (unpack)
import Data.List (intercalate, isInfixOf, sortOn)
import qualified Data.Map as Map
import Data.Map (Map)
import Language.Hydrangea.Syntax
import Language.Hydrangea.Shape (generateIndicesRowMajor, computeOffsetRowMajor)
import Text.PrettyPrint.HughesPJClass
import Prelude hiding ((<>))
import System.Environment (getEnv)
import Text.Read (readMaybe)

-- | A runtime value.
data Value
  = -- | Integer value
    VInt Integer
  | -- | Float value (double precision)
    VFloat Double
  | -- | Boolean value
    VBool Bool
  | -- | String value
    VString ByteString
  | -- | Unit value
    VUnit
  | -- | Tuple/vector as inductive cons: VTuple [v1, v2, ..., vn] represents (v1, v2, ..., vn)
    -- An empty tuple VTuple [] represents ()
    VTuple [Value]
  | -- | Pair value: a product of exactly two values of arbitrary types.
    -- Distinct from VTuple (which is used for shapes) and compiles to a C struct.
    VPair Value Value
  | -- | Structural record value with named fields stored in canonical field-name order.
    VRecord [(Var, Value)]
  | -- | Closure: captured environment, patterns to match against one argument,
    -- and the body expression to evaluate.
    VClosure Env [Pat ()] (Exp ())
  | -- | First-class operator value (curried for binary operators)
    -- Nothing means waiting for 2 args, Just v means waiting for 1 more arg with v already applied
    VPrimOp (Maybe Value) (Operator ())
  | -- | Stencil accessor value: applied one integer offset at a time.
    -- Carries the boundary condition, source array shape and elements,
    -- the current ND index being processed, and the offsets collected so far.
    VStencilAcc (BoundaryCondition ()) [Integer] [Value] [Integer] [Integer]
    -- bnd shape elems ndIdx offsets_so_far
  | -- | Immutable array with shape (as a list of integers in row-major order)
    -- and flattened element values stored in row-major order
    VArray [Integer] [Value]
  deriving (Eq, Show)

-- | Environment mapping variables to values
type Env = Map Var Value

-- | Evaluation monad with IO for file reading support
type EvalM = ExceptT EvalError IO

-- | Evaluation errors
data EvalError
  = UnboundVariable Var
  | TypeError String
  | IndexOutOfBounds String
  | DivisionByZero
  | MismatchedPatterns String
  | InvalidArrayOperation String
  | ArityMismatch String
  | RuntimeError String
  deriving (Eq, Show)

-- | Evaluate an expression with an empty environment
eval :: Exp () -> EvalM Value
eval e = evalExp e Map.empty

-- | Evaluate an expression with a given environment
evalExp :: Exp () -> Env -> EvalM Value
evalExp expr env = case expr of
  EInt _ n -> pure $ VInt n
  EFloat _ f -> pure $ VFloat f
  EBool _ b -> pure $ VBool b
  EString _ s -> pure $ VString s
  EUnit _ -> pure VUnit
  EVec _ es -> VTuple <$> mapM (\e -> evalExp e env) es
  EVar _ v -> case Map.lookup v env of
    Just val -> case val of
      VClosure cEnv [] body -> evalExp body cEnv  -- Zero-argument closure is evaluated immediately
      _ -> pure val
    Nothing -> throwError $ UnboundVariable v
  ENeg _ e -> do
    v <- evalExp e env
    case v of
      VInt n -> pure $ VInt (-n)
      VFloat f -> pure $ VFloat (-f)
      _ -> throwError $ TypeError "Cannot negate non-numeric value"
  EBinOp _ e1 op e2 -> do
    v1 <- evalExp e1 env
    v2 <- evalExp e2 env
    res <- liftEither $ evalBinOp v1 op v2
    pure res
  EUnOp _ op e -> do
    v <- evalExp e env
    liftEither $ evalUnOp op v
  EOp _ op -> pure $ VPrimOp Nothing op
  EApp _ ef ea -> do
    vf <- evalExp ef env
    va <- evalExp ea env
    evalApp vf va env
  EIfThen _ cond e -> do
    vcond <- evalExp cond env
    case vcond of
      VBool True -> evalExp e env
      VBool False -> pure VUnit
      _ -> throwError $ TypeError "If condition must be boolean"
  EIfThenElse _ cond e1 e2 -> do
    vcond <- evalExp cond env
    case vcond of
      VBool True -> evalExp e1 env
      VBool False -> evalExp e2 env
      _ -> throwError $ TypeError "If condition must be boolean"
  EProj _ i e -> do
    v <- evalExp e env
    case v of
      VTuple vals -> if i < 0 || i >= fromIntegral (length vals)
        then throwError $ IndexOutOfBounds $ "Projection index " ++ show i ++ " out of bounds"
        else pure $ vals !! fromIntegral i
      _ -> throwError $ TypeError "Projection only works on tuples"
  EPair _ e1 e2 -> VPair <$> evalExp e1 env <*> evalExp e2 env
  ERecord _ fields -> do
    values <- mapM (\(field, expr') -> do
      value <- evalExp expr' env
      pure (field, value)) fields
    pure $ VRecord (sortOn fst values)
  ERecordProj _ e field -> do
    v <- evalExp e env
    case v of
      VRecord fields ->
        case lookup field fields of
          Just fieldVal -> pure fieldVal
          Nothing -> throwError $ TypeError ("Record field not found: " ++ unpack field)
      _ -> throwError $ TypeError "Record projection only works on records"
  ELetIn _ (Dec _ var pats _ body) e -> do
    -- Create a closure for the declaration and bind it in the environment
    let closure = VClosure env pats body
    let newEnv = Map.insert var closure env
    evalExp e newEnv
  -- Array operations
  EGenerate _ szExpr fnExpr -> do
    vSz <- evalExp szExpr env
    shape <- liftEither $ shapeFromValue vSz
    vFn <- evalExp fnExpr env
    let indices = generateIndicesRowMajor shape
    vals <- mapM (\idx -> do
      let arg = valueFromShape idx
      evalApp vFn arg env
      ) indices
    pure $ VArray shape vals
  EIndex _ idxExpr arrExpr -> do
    vidx <- evalExp idxExpr env
    vidx_shape <- liftEither $ shapeFromValue vidx
    varr <- evalExp arrExpr env
    case varr of
      VArray shape vals -> do
        offRes <- pure $ computeOffsetRowMajor shape vidx_shape
        offset <- case offRes of
          Right o -> pure o
          Left msg -> if "out of bounds" `isInfixOf` msg then throwError $ IndexOutOfBounds msg else throwError $ InvalidArrayOperation msg
        if offset < 0 || offset >= length vals
          then throwError $ IndexOutOfBounds "Array index out of bounds"
          else pure $ vals !! offset
      _ -> throwError $ TypeError "Index operation requires an array"
  ECheckIndex _ idxExpr defExpr arrExpr -> do
    vidx <- evalExp idxExpr env
    vidx_shape <- liftEither $ shapeFromValue vidx
    vdef <- evalExp defExpr env
    varr <- evalExp arrExpr env
    case varr of
      VArray shape vals -> do
        offRes <- pure $ computeOffsetRowMajor shape vidx_shape
        offset <- case offRes of
          Right o -> pure o
          Left msg -> if "out of bounds" `isInfixOf` msg then pure (-1) else throwError $ InvalidArrayOperation msg
        if offset < 0 || offset >= length vals
          then pure vdef
          else pure $ vals !! offset
      _ -> throwError $ TypeError "CheckIndex operation requires an array"
  EShapeOf _ arrExpr -> do
    varr <- evalExp arrExpr env
    case varr of
      VArray shape _ -> pure $ valueFromShape shape
      _ -> throwError $ TypeError "shape_of requires an array"
  EFill _ shapeExpr valExpr -> do
    vShape <- evalExp shapeExpr env
    shape <- liftEither $ shapeFromValue vShape
    vVal <- evalExp valExpr env
    let size = product shape
    pure $ VArray shape (replicate (fromInteger size) vVal)
  EMap _ fnExpr arrExpr -> do
    vFn <- evalExp fnExpr env
    varr <- evalExp arrExpr env
    case varr of
      VArray shape vals -> do
        newVals <- mapM (\v -> evalApp vFn v env) vals
        pure $ VArray shape newVals
      _ -> throwError $ TypeError "Map requires an array"
  EZipWith _ fnExpr arr1Expr arr2Expr -> do
    vFn <- evalExp fnExpr env
    varr1 <- evalExp arr1Expr env
    varr2 <- evalExp arr2Expr env
    case (varr1, varr2) of
      (VArray shape1 vals1, VArray shape2 vals2) ->
        if shape1 /= shape2
          then throwError $ InvalidArrayOperation "ZipWith: arrays must have the same shape"
          else do
            newVals <- sequence $ zipWith3 (\a b _ -> evalApp vFn a env >>= \fnA -> evalApp fnA b env) vals1 vals2 (repeat ())
            pure $ VArray shape1 newVals
      _ -> throwError $ TypeError "ZipWith requires two arrays"
  EReduce _ fnExpr zeroExpr arrExpr -> do
    vFn <- evalExp fnExpr env
    vZero <- evalExp zeroExpr env
    varr <- evalExp arrExpr env
    case varr of
      VArray shape vals ->
        case reverse shape of
          [] -> throwError $ InvalidArrayOperation "Reduce: 0D arrays are not supported"
          redDim : revPrefix -> do
            let outShape = reverse revPrefix
            let prefixIndices = generateIndicesRowMajor outShape
                reduceValuesAtPrefix prefix =
                  foldM
                    (\acc k -> do
                        let idx = prefix ++ [k]
                        offRes <- pure $ computeOffsetRowMajor shape idx
                        offset <- case offRes of
                          Right o -> pure o
                          Left msg -> if "out of bounds" `isInfixOf` msg then throwError $ IndexOutOfBounds msg else throwError $ InvalidArrayOperation msg
                        let v = vals !! offset
                        fnAcc <- evalApp vFn acc env
                        res <- evalApp fnAcc v env
                        pure res
                    )
                    vZero
                    [0 .. redDim - 1]
            outVals <- mapM reduceValuesAtPrefix prefixIndices
            pure $ VArray outShape outVals
      _ -> throwError $ TypeError "Reduce requires an array"
  EReduceGenerate _ fnExpr zeroExpr shapeExpr genExpr -> do
    vFn <- evalExp fnExpr env
    vZero <- evalExp zeroExpr env
    vShape <- evalExp shapeExpr env
    shape <- liftEither $ shapeFromValue vShape
    vGen <- evalExp genExpr env
    case reverse shape of
      [] -> throwError $ InvalidArrayOperation "ReduceGenerate: 0D arrays are not supported"
      redDim : revPrefix -> do
        let outShape = reverse revPrefix
            prefixIndices = generateIndicesRowMajor outShape
            reduceGeneratedAtPrefix prefix = do
              vals <- mapM (\k -> evalApp vGen (valueFromShape (prefix ++ [k])) env) [0 .. redDim - 1]
              foldM
                (\acc v -> do
                  fnAcc <- evalApp vFn acc env
                  res <- evalApp fnAcc v env
                  pure res
                )
                vZero
                vals
        outVals <- mapM reduceGeneratedAtPrefix prefixIndices
        pure $ VArray outShape outVals
  -- foldl step init arr: strict left fold returning the final accumulator (scalar)
  EFoldl _ fnExpr initExpr arrExpr -> do
    vFn   <- evalExp fnExpr env
    vInit <- evalExp initExpr env
    varr  <- evalExp arrExpr env
    case varr of
      VArray shape vals ->
        case shape of
          [n] -> foldM
                   (\acc k -> do
                       fnAcc <- evalApp vFn acc env
                       res   <- evalApp fnAcc (vals !! k) env
                       pure res)
                   vInit
                    [0 .. fromIntegral n - 1]
          _ -> throwError $ InvalidArrayOperation "foldl: array must be 1-dimensional"
      _ -> throwError $ TypeError "foldl: third argument must be an array"
  EScan _ fnExpr initExpr arrExpr -> do
    vFn <- evalExp fnExpr env
    vInit <- evalExp initExpr env
    varr <- evalExp arrExpr env
    case varr of
      VArray shape vals ->
        case shape of
          [_] -> do
            (_accFinal, revOut) <- foldM
              (\(acc, outs) v -> do
                  fnAcc <- evalApp vFn acc env
                  acc' <- evalApp fnAcc v env
                  pure (acc', acc : outs)
              )
              (vInit, [])
              vals
            pure $ VArray shape (reverse revOut)
          _ -> throwError $ InvalidArrayOperation "scan: array must be 1-dimensional"
      _ -> throwError $ TypeError "scan: third argument must be an array"
  ESegmentedReduce _ fnExpr initExpr offsetsExpr valsExpr -> do
    vFn <- evalExp fnExpr env
    vInit <- evalExp initExpr env
    voffsets <- evalExp offsetsExpr env
    vvals <- evalExp valsExpr env
    case (voffsets, vvals) of
      (VArray offsetsShape offsetsVals, VArray valsShape vals)
        | offsetsShape == [0] ->
            throwError $ InvalidArrayOperation "segmented_reduce: offsets must have at least one element"
        | otherwise ->
            case (offsetsShape, valsShape) of
              ([_], [_]) ->
                case traverse valueAsInt offsetsVals of
                  Just offsets -> do
                    let segmentBounds = zip offsets (drop 1 offsets)
                    outVals <- forM segmentBounds $ \(start, stop) ->
                      foldM
                        (\acc ix -> do
                            fnAcc <- evalApp vFn acc env
                            res <- evalApp fnAcc (vals !! fromIntegral ix) env
                            pure res
                        )
                        vInit
                        [start .. stop - 1]
                    pure $ VArray [fromIntegral (length segmentBounds)] outVals
                  Nothing -> throwError $ TypeError "segmented_reduce: offsets must contain integers"
              _ -> throwError $ InvalidArrayOperation "segmented_reduce: offsets and values must be 1-dimensional"
      _ -> throwError $ TypeError "segmented_reduce: expected offsets and values arrays"
  ESortIndices _ arrExpr -> do
    varr <- evalExp arrExpr env
    case varr of
      VArray shape vals ->
        case shape of
          [_] ->
            case traverse valueAsInt vals of
              Just keys ->
                let indexed = zip [0 :: Integer ..] keys
                    sorted = sortOn snd indexed
                in pure $ VArray shape (map (VInt . fst) sorted)
              Nothing -> throwError $ TypeError "sort_indices: input array must contain integers"
          _ -> throwError $ InvalidArrayOperation "sort_indices: array must be 1-dimensional"
      _ -> throwError $ TypeError "sort_indices: argument must be an array"
  ECOOSumDuplicates _ nrowsExpr ncolsExpr nnzExpr rowsExpr colsExpr valsExpr -> do
    vnrows <- evalExp nrowsExpr env
    vncols <- evalExp ncolsExpr env
    vnnz <- evalExp nnzExpr env
    vrows <- evalExp rowsExpr env
    vcols <- evalExp colsExpr env
    vvals <- evalExp valsExpr env
    case (valueAsInt vnrows, valueAsInt vncols, valueAsInt vnnz, vrows, vcols, vvals) of
      (Just nrows, Just ncols, Just nnz, VArray rowShape rowVals, VArray colShape colVals, VArray valShape valVals)
        | rowShape == colShape && rowShape == valShape ->
            case rowShape of
              [_] ->
                case (traverse valueAsInt rowVals, traverse valueAsInt colVals, traverse valueAsInt valVals) of
                  (Just rows, Just cols, Just vals) -> do
                    let cap = length rows
                        live = min cap (max 0 (fromIntegral nnz))
                        triples = take live (zip3 rows cols vals)
                        dedup [] = []
                        dedup ((r, c, v):xs) = go r c v xs
                          where
                            go cr cc acc [] = [(cr, cc, acc)]
                            go cr cc acc ((r', c', v'):rest)
                              | r' == cr && c' == cc = go cr cc (acc + v') rest
                              | otherwise = (cr, cc, acc) : go r' c' v' rest
                        out = dedup triples
                        outNnz = length out
                        outRows = map (\(r, _, _) -> VInt r) out ++ replicate (cap - outNnz) (VInt 0)
                        outCols = map (\(_, c, _) -> VInt c) out ++ replicate (cap - outNnz) (VInt 0)
                        outVals = map (\(_, _, v) -> VInt v) out ++ replicate (cap - outNnz) (VInt 0)
                    pure $ VRecord $ sortOn fst
                      [ ("nrows", VInt nrows)
                      , ("ncols", VInt ncols)
                      , ("nnz", VInt (fromIntegral outNnz))
                      , ("rows", VArray rowShape outRows)
                      , ("cols", VArray colShape outCols)
                      , ("vals", VArray valShape outVals)
                      ]
                  _ -> throwError $ TypeError "coo_sum_duplicates: rows, cols, and vals must contain integers"
              _ -> throwError $ InvalidArrayOperation "coo_sum_duplicates: rows, cols, and vals must be 1-dimensional"
      _ -> throwError $ TypeError "coo_sum_duplicates: expected integer scalars and integer arrays"
  ECSRFromSortedCOO _ nrowsExpr ncolsExpr nnzExpr rowsExpr colsExpr valsExpr -> do
    vnrows <- evalExp nrowsExpr env
    vncols <- evalExp ncolsExpr env
    vnnz <- evalExp nnzExpr env
    vrows <- evalExp rowsExpr env
    vcols <- evalExp colsExpr env
    vvals <- evalExp valsExpr env
    case (valueAsInt vnrows, valueAsInt vncols, valueAsInt vnnz, vrows, vcols, vvals) of
      (Just nrows, Just ncols, Just nnz, VArray rowShape rowVals, VArray colShape colVals, VArray valShape valVals)
        | rowShape == colShape && rowShape == valShape ->
            case rowShape of
              [_] ->
                case (traverse valueAsInt rowVals, traverse valueAsInt colVals, traverse valueAsInt valVals) of
                  (Just rows, Just cols, Just vals) -> do
                    let cap = length rows
                        live = min cap (max 0 (fromIntegral nnz))
                        liveRows = take live rows
                        liveCols = take live cols
                        liveVals = take live vals
                        rowCounts =
                          [ fromIntegral (length (filter (== r) liveRows))
                          | r <- [0 .. nrows - 1]
                          ]
                        rowPtr = scanl (+) 0 rowCounts
                        outCols = map VInt liveCols ++ replicate (cap - live) (VInt 0)
                        outVals = map VInt liveVals ++ replicate (cap - live) (VInt 0)
                    pure $ VRecord $ sortOn fst
                      [ ("nrows", VInt nrows)
                      , ("ncols", VInt ncols)
                      , ("nnz", VInt (fromIntegral live))
                      , ("row_ptr", VArray [nrows + 1] (map VInt rowPtr))
                      , ("col_idx", VArray colShape outCols)
                      , ("vals", VArray valShape outVals)
                      ]
                  _ -> throwError $ TypeError "csr_from_sorted_coo: rows, cols, and vals must contain integers"
              _ -> throwError $ InvalidArrayOperation "csr_from_sorted_coo: rows, cols, and vals must be 1-dimensional"
      _ -> throwError $ TypeError "csr_from_sorted_coo: expected integer scalars and integer arrays"
  EPermute _ combExpr defaultsExpr permFnExpr arrExpr -> do
    vComb <- evalExp combExpr env
    vDefaults <- evalExp defaultsExpr env
    vPermFn <- evalExp permFnExpr env
    varr <- evalExp arrExpr env
    case (vDefaults, varr) of
      (VArray dstShape dstVals, VArray srcShape srcVals) ->
        if length srcShape /= length dstShape
          then throwError $ InvalidArrayOperation "Permute: source and destination must have same rank"
          else do
            let dstIndices = generateIndicesRowMajor dstShape
            -- For each destination index, find if any source element maps to it
            newVals <- forM (zip dstIndices [0..]) $ \(dstIdx, dstOffset) -> do
              let dstVal = dstVals !! dstOffset
              -- Search through source indices to find one that maps to this destination
              let srcIndices = generateIndicesRowMajor srcShape
              mbResult <- foldM (\acc (srcIdx, srcOffset) -> case acc of
                Just _ -> pure acc  -- Already found a mapping
                Nothing -> do
                  permIdx <- evalApp vPermFn (valueFromShape srcIdx) env
                  permIdxShape <- liftEither $ shapeFromValue permIdx
                  case computeOffsetRowMajor dstShape permIdxShape of
                    Right permOffset | permOffset == dstOffset -> do
                      -- This source element maps to this destination
                      let srcVal = srcVals !! srcOffset
                      result <- evalApp vComb srcVal env >>= \fnSrc -> evalApp fnSrc dstVal env
                      pure (Just result)
                    _ -> pure Nothing  -- Out of bounds or doesn't match
                ) Nothing (zip srcIndices [0..])
              case mbResult of
                Just result -> pure result
                Nothing -> pure dstVal  -- No source element maps here, use default
            pure $ VArray dstShape newVals
      _ -> throwError $ TypeError "Permute requires two arrays"
  EScatter _ combExpr defaultsExpr idxArrExpr valsExpr -> do
    vComb <- evalExp combExpr env
    vDefaults <- evalExp defaultsExpr env
    vIdxArr <- evalExp idxArrExpr env
    vValsArr <- evalExp valsExpr env
    case (vDefaults, vIdxArr, vValsArr) of
      (VArray dstShape dstVals, VArray idxShape idxVals, VArray valsShape vals) ->
        if idxShape /= valsShape
          then throwError $ InvalidArrayOperation "Scatter: index and values arrays must have same shape"
          else do
            let applyScatter dstVals' (idxVal, val) = do
                  idxShape' <- liftEither $ shapeFromValue idxVal
                  offRes <- pure $ computeOffsetRowMajor dstShape idxShape'
                  dstOffset <- case offRes of
                    Right o -> pure o
                    Left msg -> if "out of bounds" `isInfixOf` msg then throwError $ IndexOutOfBounds msg else throwError $ InvalidArrayOperation msg
                  if dstOffset < 0 || dstOffset >= length dstVals'
                    then throwError $ IndexOutOfBounds "Scatter: index out of bounds"
                    else do
                      let oldVal = dstVals' !! fromIntegral dstOffset
                      combined <- evalApp vComb val env >>= \fnVal -> evalApp fnVal oldVal env
                      pure $ take (fromIntegral dstOffset) dstVals' ++ [combined] ++ drop (fromIntegral dstOffset + 1) dstVals'
            result <- foldM applyScatter dstVals (zip idxVals vals)
            pure $ VArray dstShape result
      _ -> throwError $ TypeError "Scatter requires index array and values array"
  EScatterGuarded _ combExpr defaultsExpr idxArrExpr valsExpr guardExpr -> do
    vComb <- evalExp combExpr env
    vDefaults <- evalExp defaultsExpr env
    vIdxArr <- evalExp idxArrExpr env
    vValsArr <- evalExp valsExpr env
    vGuardArr <- evalExp guardExpr env
    case (vDefaults, vIdxArr, vValsArr, vGuardArr) of
      (VArray dstShape dstVals, VArray idxShape idxVals, VArray valsShape vals, VArray guardShape guards) ->
        if idxShape /= valsShape || idxShape /= guardShape
          then throwError $ InvalidArrayOperation "ScatterGuarded: index, values, and guard arrays must have same shape"
          else do
            let applyScatter dstVals' (idxVal, val, guardVal) =
                  case guardVal of
                    VBool False -> pure dstVals'
                    VBool True -> do
                      idxShape' <- liftEither $ shapeFromValue idxVal
                      offRes <- pure $ computeOffsetRowMajor dstShape idxShape'
                      dstOffset <- case offRes of
                        Right o -> pure o
                        Left msg -> if "out of bounds" `isInfixOf` msg then throwError $ IndexOutOfBounds msg else throwError $ InvalidArrayOperation msg
                      if dstOffset < 0 || dstOffset >= length dstVals'
                        then throwError $ IndexOutOfBounds "ScatterGuarded: index out of bounds"
                        else do
                          let oldVal = dstVals' !! fromIntegral dstOffset
                          combined <- evalApp vComb val env >>= \fnVal -> evalApp fnVal oldVal env
                          pure $ take (fromIntegral dstOffset) dstVals' ++ [combined] ++ drop (fromIntegral dstOffset + 1) dstVals'
                    _ -> throwError $ TypeError "ScatterGuarded: guard array must contain booleans"
            result <- foldM applyScatter dstVals (zip3 idxVals vals guards)
            pure $ VArray dstShape result
      _ -> throwError $ TypeError "ScatterGuarded requires index, values, and guard arrays"
  EScatterGenerate _ combExpr defaultsExpr idxArrExpr valFnExpr ->
    -- Evaluate as scatter c d idx (generate (shape_of idx) valFn).
    let a = firstParam defaultsExpr
        ai = firstParam idxArrExpr
     in evalExp (EScatter a combExpr defaultsExpr idxArrExpr
                   (EGenerate ai (EShapeOf ai idxArrExpr) valFnExpr)) env
  EGather _ idxArrExpr arrExpr -> do
    vIdxArr <- evalExp idxArrExpr env
    varr <- evalExp arrExpr env
    case (vIdxArr, varr) of
      (VArray idxShape idxVals, VArray srcShape srcVals) ->
        do
          gatheredVals <- forM idxVals $ \idxVal -> do
            idxShape' <- liftEither $ shapeFromValue idxVal
            offRes <- pure $ computeOffsetRowMajor srcShape idxShape'
            srcOffset <- case offRes of
              Right o -> pure o
              Left msg -> if "out of bounds" `isInfixOf` msg then throwError $ IndexOutOfBounds msg else throwError $ InvalidArrayOperation msg
            if srcOffset < 0 || srcOffset >= length srcVals
              then throwError $ IndexOutOfBounds "Gather: index out of bounds"
              else pure $ srcVals !! srcOffset
          pure $ VArray idxShape gatheredVals
      _ -> throwError $ TypeError "Gather requires index array and source array"
  EReplicate _ shapeDims arrExpr -> do
    varr <- evalExp arrExpr env
    case varr of
      VArray srcShape srcVals -> do
        (outShape, consumeMap) <- liftEither $ buildReplicateShape shapeDims srcShape
        outVals <- liftEither $ replicateValues consumeMap srcShape srcVals outShape
        pure $ VArray outShape outVals
      _ -> throwError $ TypeError "Replicate requires an array"
    where
      buildReplicateShape dims shape = go dims 0
        where
          go [] srcIdx =
            if srcIdx == length shape
              then pure ([], [])
              else Left $ InvalidArrayOperation "Replicate: shape spec does not match source rank"
          go (dim : rest) srcIdx =
            case dim of
              ShapeAll _ ->
                if srcIdx < length shape
                  then do
                    (os, cm) <- go rest (srcIdx + 1)
                    pure (shape !! srcIdx : os, True : cm)
                  else Left $ InvalidArrayOperation "Replicate: too many ShapeAll dimensions"
              ShapeAny _ e -> do
                v <- evalExpPure e env
                case v of
                  VInt n -> do
                    (os, cm) <- go rest srcIdx
                    pure (n : os, False : cm)
                  _ -> Left $ TypeError "Replicate dimension must be an integer"
              ShapeDim _ e -> do
                v <- evalExpPure e env
                case v of
                  VInt n -> do
                    (os, cm) <- go rest srcIdx
                    pure (n : os, False : cm)
                  _ -> Left $ TypeError "Replicate dimension must be an integer"
  ESlice _ sliceDims arrExpr -> do
    varr <- evalExp arrExpr env
    case varr of
      VArray srcShape srcVals -> do
        sliceSpecs <- forM sliceDims $ \dim ->
          case dim of
            SliceAll _ -> pure Nothing
            SliceRange _ startExpr lenExpr -> do
              startV <- evalExp startExpr env
              lenV <- evalExp lenExpr env
              case (startV, lenV) of
                (VInt start, VInt len) -> pure (Just (start, len))
                _ -> throwError $ TypeError "Slice indices must be integers"
        (outShape, sliceIndices) <- liftEither $ buildSliceShape srcShape sliceSpecs
        slicedVals <- forM sliceIndices $ \idx -> do
          let offRes = computeOffsetRowMajor srcShape idx
          offset <- case offRes of
            Right o -> pure o
            Left msg -> if "out of bounds" `isInfixOf` msg then throwError $ IndexOutOfBounds msg else throwError $ InvalidArrayOperation msg
          if offset < 0 || offset >= length srcVals
            then throwError $ IndexOutOfBounds "Slice: index out of bounds"
            else pure $ srcVals !! offset
        pure $ VArray outShape slicedVals
      _ -> throwError $ TypeError "Slice requires an array"
  EReshape _ newShapeExpr arrExpr -> do
    vnewShape <- evalExp newShapeExpr env
    newShape <- liftEither $ shapeFromValue vnewShape
    varr <- evalExp arrExpr env
    case varr of
      VArray srcShape vals -> do
        let srcSize = product srcShape
        let dstSize = product newShape
        if srcSize /= dstSize
          then throwError $ InvalidArrayOperation $ "Reshape: size mismatch: " ++ show srcSize ++ " vs " ++ show dstSize
          else pure $ VArray newShape vals
      _ -> throwError $ TypeError "Reshape requires an array"
  EReadArray _ shapeExpr fileExpr -> do
    vShape <- evalExp shapeExpr env
    shape <- liftEither $ shapeFromValue vShape
    vFile <- evalExp fileExpr env
    case vFile of
      VString s -> do
        let filename = BS.unpack (stripQuotes s)
        contents <- liftIO $ readFile filename
        let values = parseCSVInts contents
            expected = product shape
        when (fromIntegral (length values) /= expected) $
          throwError $ RuntimeError $ "read_array: expected " ++ show expected
            ++ " values but got " ++ show (length values)
        pure $ VArray shape (map VInt values)
      _ -> throwError $ TypeError "read_array: filename must be a string"

  EReadArrayFloat _ shapeExpr fileExpr -> do
    vShape <- evalExp shapeExpr env
    shape <- liftEither $ shapeFromValue vShape
    vFile <- evalExp fileExpr env
    case vFile of
      VString s -> do
        let filename = BS.unpack (stripQuotes s)
        contents <- liftIO $ readFile filename
        let values = parseCSVFloats contents
            expected = product shape
        when (fromIntegral (length values) /= expected) $
          throwError $ RuntimeError $ "read_array_float: expected " ++ show expected
            ++ " values but got " ++ show (length values)
        pure $ VArray shape (map VFloat values)
      _ -> throwError $ TypeError "read_array_float: filename must be a string"

  EWriteArray _ arrExpr fileExpr -> do
    vArr <- evalExp arrExpr env
    vFile <- evalExp fileExpr env
    case (vArr, vFile) of
      (VArray _ vals, VString s) -> do
        let filename = BS.unpack (stripQuotes s)
            values = map (\case VInt i -> show i; _ -> "0") vals
            content = intercalate "," values ++ "\n"
        liftIO $ writeFile filename content
        pure VUnit
      _ -> throwError $ TypeError "write_array: expected array and string"

  EWriteArrayFloat _ arrExpr fileExpr -> do
    vArr <- evalExp arrExpr env
    vFile <- evalExp fileExpr env
    case (vArr, vFile) of
      (VArray _ vals, VString s) -> do
        let filename = BS.unpack (stripQuotes s)
            values = map (\case VFloat f -> show f; _ -> "0") vals
            content = intercalate "," values ++ "\n"
        liftIO $ writeFile filename content
        pure VUnit
      _ -> throwError $ TypeError "write_array_float: expected array and string"

  EGetEnvInt _ varExpr -> do
    vVar <- evalExp varExpr env
    case vVar of
      VString s -> do
        let varName = BS.unpack (stripQuotes s)
        val <- liftIO $ getEnv varName
        case readMaybe val of
          Just i -> pure $ VInt i
          Nothing -> throwError $ RuntimeError $ "get_env_int: cannot parse '" ++ val ++ "' as int"
      _ -> throwError $ TypeError "get_env_int: argument must be a string"

  EGetEnvString _ varExpr -> do
    vVar <- evalExp varExpr env
    case vVar of
      VString s -> do
        let varName = BS.unpack (stripQuotes s)
        val <- liftIO $ getEnv varName
        pure $ VString (BS.pack val)
      _ -> throwError $ TypeError "get_env_string: argument must be a string"

  EStencil _ bnd fnExp arrExp -> do
    vArr <- evalExp arrExp env
    vFn  <- evalExp fnExp env
    case vArr of
      VArray shape vals -> do
        let indices = generateIndicesRowMajor shape
        outVals <- forM indices $ \ndIdx -> do
          -- Build an accessor closure that takes integer offsets and returns
          -- the element at ndIdx + offsets after applying the boundary condition.
          let accessorVal = buildStencilAccessor bnd shape vals ndIdx
          evalApp vFn accessorVal env
        pure $ VArray shape outVals
      _ -> throwError $ TypeError "stencil: source must be an array"

-- | Strip surrounding quotes from a ByteString
stripQuotes :: ByteString -> ByteString
stripQuotes s
  | BS.length s >= 2 && BS.head s == '"' && BS.last s == '"' = BS.tail (BS.init s)
  | otherwise = s

valueAsInt :: Value -> Maybe Integer
valueAsInt (VInt n) = Just n
valueAsInt _ = Nothing

-- | Parse comma/newline-separated integers from a string
parseCSVInts :: String -> [Integer]
parseCSVInts = map read . filter (not . null) . map strip . splitDelims ",\n\r"
  where
    strip = filter (\c -> c /= ' ' && c /= '\n' && c /= '\r' && c /= '\t')
    splitDelims :: String -> String -> [String]
    splitDelims _ [] = [""]
    splitDelims delims (x:xs)
      | x `elem` delims = "" : splitDelims delims xs
      | otherwise = let (w:ws) = splitDelims delims xs in (x:w) : ws

-- | Parse comma/newline-separated doubles from a string
parseCSVFloats :: String -> [Double]
parseCSVFloats = map read . filter (not . null) . map strip . splitDelims ",\n\r"
  where
    strip = filter (\c -> c /= ' ' && c /= '\n' && c /= '\r' && c /= '\t')
    splitDelims :: String -> String -> [String]
    splitDelims _ [] = [""]
    splitDelims delims (x:xs)
      | x `elem` delims = "" : splitDelims delims xs
      | otherwise = let (w:ws) = splitDelims delims xs in (x:w) : ws

-- | Pure evaluation helper for contexts that don't support IO (e.g. replicate shape dims).
-- This fails if the expression requires IO (e.g. read_array).
evalExpPure :: Exp () -> Env -> Either EvalError Value
evalExpPure expr env = case expr of
  EInt _ n -> pure $ VInt n
  EVar _ v -> case Map.lookup v env of
    Just val -> pure val
    Nothing -> Left $ UnboundVariable v
  _ -> Left $ TypeError "Complex expression in shape dimension"

-- | Replicate a source array into a larger shape by projecting each output index back to the consumed source axes.
replicateValues :: [Bool] -> [Integer] -> [Value] -> [Integer] -> Either EvalError [Value]
replicateValues consumeMap srcShape srcVals outShape =
  forM (generateIndicesRowMajor outShape) $ \outIdx -> do
    let srcIdx = [i | (i, keep) <- zip outIdx consumeMap, keep]
    let offRes = computeOffsetRowMajor srcShape srcIdx
    offset <- case offRes of
      Right o -> pure o
      Left msg -> if "out of bounds" `isInfixOf` msg then Left $ IndexOutOfBounds msg else Left $ InvalidArrayOperation msg
    pure $ srcVals !! offset
  

-- | Helper function to build slice shape and compute indices
buildSliceShape :: [Integer] -> [Maybe (Integer, Integer)] -> Either EvalError ([Integer], [[Integer]])
buildSliceShape srcShape sliceSpecs = do
  when (length srcShape /= length sliceSpecs) $
    Left $ InvalidArrayOperation "Slice: shape spec does not match source rank"
  dims <- zipWithM buildDim srcShape sliceSpecs
  let outShape = map snd dims
  let sliceIndices = cartesian (map fst dims)
  pure (outShape, sliceIndices)
  where
    buildDim s Nothing = pure ([0 .. s - 1], s)
    buildDim s (Just (start, len))
      | start < 0 || len < 0 = Left $ InvalidArrayOperation "Slice: negative bounds"
      | start + len > s = Left $ IndexOutOfBounds "Slice: index out of bounds"
      | otherwise = pure ([start .. start + len - 1], len)
    cartesian [] = [[]]
    cartesian (xs : xss) = [x : rest | x <- xs, rest <- cartesian xss]

-- | Apply a function value to an argument
evalApp :: Value -> Value -> Env -> EvalM Value
evalApp (VClosure env pats body) arg _env = do
  -- Match the argument against the first pattern
  case pats of
    [] -> throwError $ ArityMismatch "Closure has no patterns to match"
    (pat : restPats) -> do
      newEnv <- liftEither $ matchPattern pat arg env
      if null restPats
        then do
          res <- evalExp body newEnv
          pure res
        else pure $ VClosure newEnv restPats body
evalApp (VPrimOp Nothing op) arg _env = do
  -- Binary operator waiting for first argument
  pure $ VPrimOp (Just arg) op
evalApp (VPrimOp (Just arg1) op) arg2 _env = do
  -- Binary operator with both arguments provided
  liftEither $ evalBinOp arg1 op arg2
evalApp (VStencilAcc bnd shape vals ndIdx offsets) arg _env = do
  -- Stencil accessor: collect one integer offset.
  offset <- case arg of
    VInt n -> pure n
    _      -> throwError $ TypeError "stencil accessor: offset must be an integer"
  let offsets' = offsets ++ [offset]
  if length offsets' == length shape
    then loadStencilElem bnd shape vals ndIdx offsets'
    else pure $ VStencilAcc bnd shape vals ndIdx offsets'
evalApp other _arg _env =
  throwError $ TypeError $ "Cannot apply non-function value: " ++ show other

-- | Match a pattern against a value, extending the environment
matchPattern :: Pat () -> Value -> Env -> Either EvalError Env
matchPattern (PVar _ v) val env = pure $ Map.insert v val env
matchPattern (PVec _ pats) (VTuple vals) env =
  if length pats /= length vals
    then Left $ MismatchedPatterns "Pattern length mismatch in tuple"
    else foldM (\e (p, v) -> matchPattern p v e) env (zip pats vals)
matchPattern _ _ _ = Left $ MismatchedPatterns "Pattern match failure"

-- | Evaluate a binary operator
evalBinOp :: Value -> Operator () -> Value -> Either EvalError Value
evalBinOp (VInt a) (Plus _) (VInt b) = pure $ VInt (a + b)
evalBinOp (VInt a) (Minus _) (VInt b) = pure $ VInt (a - b)
evalBinOp (VInt a) (Times _) (VInt b) = pure $ VInt (a * b)
evalBinOp (VInt a) (Divide _) (VInt b) =
  if b == 0 then Left DivisionByZero else pure $ VInt (a `div` b)
evalBinOp (VInt a) (Eq _) (VInt b) = pure $ VBool (a == b)
evalBinOp (VInt a) (Neq _) (VInt b) = pure $ VBool (a /= b)
evalBinOp (VInt a) (Lt _) (VInt b) = pure $ VBool (a < b)
evalBinOp (VInt a) (Le _) (VInt b) = pure $ VBool (a <= b)
evalBinOp (VInt a) (Gt _) (VInt b) = pure $ VBool (a > b)
evalBinOp (VInt a) (Ge _) (VInt b) = pure $ VBool (a >= b)
evalBinOp (VBool a) (And _) (VBool b) = pure $ VBool (a && b)
evalBinOp (VBool a) (Or _) (VBool b) = pure $ VBool (a || b)
evalBinOp (VBool a) (Eq _) (VBool b) = pure $ VBool (a == b)
evalBinOp (VBool a) (Neq _) (VBool b) = pure $ VBool (a /= b)
evalBinOp (VFloat a) (PlusF _)  (VFloat b) = pure $ VFloat (a + b)
evalBinOp (VFloat a) (MinusF _) (VFloat b) = pure $ VFloat (a - b)
evalBinOp (VFloat a) (TimesF _) (VFloat b) = pure $ VFloat (a * b)
evalBinOp (VFloat a) (DivideF _) (VFloat b) = pure $ VFloat (a / b)
evalBinOp (VFloat a) (EqF _)  (VFloat b) = pure $ VBool (a == b)
evalBinOp (VFloat a) (NeqF _) (VFloat b) = pure $ VBool (a /= b)
evalBinOp (VFloat a) (LtF _)  (VFloat b) = pure $ VBool (a < b)
evalBinOp (VFloat a) (LeF _)  (VFloat b) = pure $ VBool (a <= b)
evalBinOp (VFloat a) (GtF _)  (VFloat b) = pure $ VBool (a > b)
evalBinOp (VFloat a) (GeF _)  (VFloat b) = pure $ VBool (a >= b)
evalBinOp a op b =
  Left $ TypeError $ "Cannot apply " ++ show op ++ " to " ++ show a ++ " and " ++ show b

-- | Evaluate a unary operator
evalUnOp :: UnOperator () -> Value -> Either EvalError Value
evalUnOp (Not _) (VBool b) = pure $ VBool (not b)
evalUnOp (Fst _) (VTuple (v : _)) = pure v
evalUnOp (Fst _) (VPair v _) = pure v
evalUnOp (Fst _) _ = Left $ TypeError "fst requires a non-empty tuple or a pair"
evalUnOp (Snd _) (VTuple (_ : v : _)) = pure v
evalUnOp (Snd _) (VPair _ v) = pure v
evalUnOp (Snd _) _ = Left $ TypeError "snd requires a tuple with at least 2 elements or a pair"
evalUnOp (Sqrt  _) (VFloat f) = pure $ VFloat (sqrt f)
evalUnOp (ExpF  _) (VFloat f) = pure $ VFloat (exp f)
evalUnOp (Log   _) (VFloat f) = pure $ VFloat (log f)
evalUnOp (Sin   _) (VFloat f) = pure $ VFloat (sin f)
evalUnOp (Cos   _) (VFloat f) = pure $ VFloat (cos f)
evalUnOp (AbsF  _) (VFloat f) = pure $ VFloat (abs f)
evalUnOp (FloorF _) (VFloat f) = pure $ VFloat (fromIntegral (floor f :: Integer))
evalUnOp (CeilF  _) (VFloat f) = pure $ VFloat (fromIntegral (ceiling f :: Integer))
evalUnOp (Erf   _) (VFloat f) = pure $ VFloat (erf' f)
  where
    -- Abramowitz and Stegun approximation (max error 1.5e-7)
    erf' x
      | x < 0    = -(erf' (-x))
      | otherwise =
          let t  = 1.0 / (1.0 + 0.3275911 * x)
              p  = 0.254829592
              q  = -0.284496736
              r  = 1.421413741
              s  = -1.453152027
              u  = 1.061405429
              poly = t * (p + t * (q + t * (r + t * (s + t * u))))
          in 1.0 - poly * exp (-(x * x))
evalUnOp (FloatOf _) (VInt n) = pure $ VFloat (fromIntegral n)
evalUnOp op val =
  Left $ TypeError $ "Cannot apply " ++ show op ++ " to " ++ show val

-- | Convert a value to a shape (list of integers)
shapeFromValue :: Value -> Either EvalError [Integer]
shapeFromValue (VInt n) = pure [n]
shapeFromValue (VTuple vals) = concat <$> mapM shapeFromValue vals
shapeFromValue VUnit = pure []
shapeFromValue _ = Left $ TypeError "Shape must be an integer or tuple of integers"

-- | Convert a shape (list of integers) to a value (nested tuple)
valueFromShape :: [Integer] -> Value
valueFromShape [] = VUnit
valueFromShape ns = VTuple (map VInt ns)

-- Note: index/offset helpers are provided by `Language.Hydrangea.Shape`.

-- | Build a stencil accessor value for a given source array and current ND index.
--
-- The returned value is a curried closure that accepts one integer offset per
-- dimension. Each integer is consumed left-to-right (outermost dimension first),
-- and once all offsets are supplied the boundary-conditioned element is returned.
--
-- For a 1D array the accessor has type `Int -> a`.
-- For a 2D array the accessor has type `Int -> Int -> a`.
buildStencilAccessor :: BoundaryCondition () -> [Integer] -> [Value] -> [Integer] -> Value
buildStencilAccessor bnd shape vals ndIdx =
  VStencilAcc bnd shape vals ndIdx []

-- | Mirror reflection of an index at boundaries.
-- Uses period-2N fold-back: indices beyond [0, N-1] are reflected.
mirrorIndex :: Integer -> Integer -> Integer
mirrorIndex dim raw
  | dim <= 1  = 0
  | otherwise =
      let period = 2 * (dim - 1)
          r      = raw `mod` period
          r'     = if r < 0 then r + period else r
      in if r' < dim then r' else period - r'

-- | Apply a boundary condition along one dimension.
applyBndDim :: BoundaryCondition () -> Integer -> Integer -> Integer -> Integer
applyBndDim bnd dim center offset =
  case bnd of
    BClamp   -> max 0 (min (dim - 1) (center + offset))
    BWrap    -> ((center + offset) `mod` dim + dim) `mod` dim
    BMirror  -> mirrorIndex dim (center + offset)
    BConst _ -> center + offset  -- OOB check performed separately

-- | Load a stencil element with boundary handling once all offsets are known.
loadStencilElem :: BoundaryCondition () -> [Integer] -> [Value] -> [Integer] -> [Integer] -> EvalM Value
loadStencilElem bnd shape vals ndIdx offsets = do
  let rawIdx = zipWith (+) ndIdx offsets
      isOOB  = any (\(dim, raw) -> raw < 0 || raw >= dim) (zip shape rawIdx)
  case (bnd, isOOB) of
    (BConst constExp, True) -> evalExp constExp Map.empty
    _ ->
      let boundedIdx = zipWith3 (applyBndDim bnd) shape ndIdx offsets
          strides    = tail (scanr (*) 1 shape)
          flat       = sum (zipWith (*) boundedIdx strides)
      in if flat < 0 || fromIntegral flat >= length vals
           then throwError $ InvalidArrayOperation "stencil: index out of bounds"
           else pure $ vals !! fromIntegral flat

-- | Evaluate a list of declarations, returning bindings
evalDecs :: [Dec ()] -> EvalM Env
evalDecs decs = go decs Map.empty
  where
    go [] env = pure env
    go (Dec _ var pats _ body : rest) env = do
      val <-
        if null pats
          then evalExp body env
          else pure (VClosure env pats body)
      let newEnv = Map.insert var val env
      go rest newEnv

-- | Pretty printing for runtime values
instance Pretty Value where
  pPrint (VInt n) = integer n
  pPrint (VFloat f) = text (show f)
  pPrint (VBool b) = text $ if b then "true" else "false"
  pPrint (VString s) = doubleQuotes (text (unpack s))
  pPrint VUnit = text "()"
  pPrint (VTuple vals) = parens (sep (punctuate (text ",") (map pPrint vals)))
  pPrint (VPair v1 v2) = parens (pPrint v1 <> text ", " <> pPrint v2)
  pPrint (VRecord fields) =
    braces (sep (punctuate comma [text (unpack field) <+> text "=" <+> pPrint fieldVal | (field, fieldVal) <- fields]))
  pPrint (VClosure _ _ _) = text "<closure>"
  pPrint (VPrimOp (Just v) op) = parens (pPrint v <+> text (operatorToString op))
  pPrint (VPrimOp Nothing op) = text (operatorToString op)
  pPrint (VStencilAcc _ _ _ _ _) = text "<stencil-acc>"
  pPrint (VArray shape vals) =
    text "array" <+> brackets (sep (punctuate (text ",") (map integer shape)))
      <+> brackets (sep (punctuate (text ",") (map pPrint vals)))

-- | Convert an operator to a string representation
operatorToString :: Operator () -> String
operatorToString (Plus _) = "+"
operatorToString (Minus _) = "-"
operatorToString (Times _) = "*"
operatorToString (Divide _) = "/"
operatorToString (Eq _) = "=="
operatorToString (Neq _) = "<>"
operatorToString (Lt _) = "<"
operatorToString (Le _) = "<="
operatorToString (Gt _) = ">"
operatorToString (Ge _) = ">="
operatorToString (And _) = "&&"
operatorToString (Or _) = "||"
operatorToString (PlusF _) = "+."
operatorToString (MinusF _) = "-."
operatorToString (TimesF _) = "*."
operatorToString (DivideF _) = "/."
operatorToString (EqF _) = "=."
operatorToString (NeqF _) = "<>."
operatorToString (LtF _) = "<."
operatorToString (LeF _) = "<=."
operatorToString (GtF _) = ">."
operatorToString (GeF _) = ">=."

-- | Pretty printing for evaluation errors
instance Pretty EvalError where
  pPrint (UnboundVariable v) = text "Unbound variable:" <+> text (unpack v)
  pPrint (TypeError msg) = text "Type error:" <+> text msg
  pPrint (IndexOutOfBounds msg) = text "Index out of bounds:" <+> text msg
  pPrint DivisionByZero = text "Division by zero"
  pPrint (MismatchedPatterns msg) = text "Pattern match failed:" <+> text msg
  pPrint (InvalidArrayOperation msg) = text "Invalid array operation:" <+> text msg
  pPrint (ArityMismatch msg) = text "Arity mismatch:" <+> text msg
  pPrint (RuntimeError msg) = text "Runtime error:" <+> text msg
