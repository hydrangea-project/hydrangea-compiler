{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.Lowering
--
-- Translation from the high-level surface AST to the imperative CFG IR.
--
-- Key responsibilities:
--
-- * Inline small user-defined functions to expose array structure for
--   later passes.
-- * Introduce fresh temporaries (e.g. @t0@, @t1@, ...) and rename
--   formals when inlining to prevent name collisions.
-- * Lower array operations (map, generate, zipWith, reduce, scatter, …)
--   into explicit 'SLoop' and 'RHS' forms.
module Language.Hydrangea.Lowering
  ( lowerDecs2
  , lowerDecs2WithTypeEnv
  , lowerDecs2WithTypeEnvAndRanks
  ) where

import Control.Applicative ((<|>))
import Control.Monad (when, zipWithM)
import Control.Monad.State
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as S
import Language.Hydrangea.CFGCore
import Language.Hydrangea.CFG hiding (CVar)
import Language.Hydrangea.Lexer (Range)
import Language.Hydrangea.Syntax
import Language.Hydrangea.Util (stripStringQuotes)

data LowerState = LowerState
  { lsFresh :: !Int
  , lsFnEnv :: Map Var ([Pat Range], Exp Range)
  , lsValueProcs :: Set Var
  , lsTypeEnv :: Map CVar CType
    -- ^ Concrete type for every CFG variable, populated during lowering.
  , lsArrayFacts :: Map CVar ArrayFact
    -- ^ Buffer-level facts for the procedure being lowered; reset per proc.
  , lsVectorAccessFacts :: Map CVar VectorAccessFact
    -- ^ Access and layout facts for the vectorizer; reset per proc.
  , lsTopTypeEnv :: Map Var CType
    -- ^ Top-level declaration types from inference (@Var → CType@).
  , lsArrayRankEnv :: Map Var Int
    -- ^ Array rank (number of dimensions) for each top-level declaration.
  }

type LowerM = State LowerState

freshCVar :: ByteString -> LowerM CVar
freshCVar prefix = do
  n <- gets lsFresh
  modify' $ \s -> s { lsFresh = n + 1 }
  pure $ BS.append prefix (BS.pack (show n))

-- | Allocate a fresh loop-iterator variable and register it as @CTInt64@.
freshIterVar :: ByteString -> LowerM CVar
freshIterVar prefix = do
  v <- freshCVar prefix
  registerCType v CTInt64
  pure v

registerFn :: Var -> [Pat Range] -> Exp Range -> LowerM ()
registerFn name pats body =
  modify' $ \s -> s { lsFnEnv = M.insert name (pats, body) (lsFnEnv s) }

lookupFn :: Var -> LowerM (Maybe ([Pat Range], Exp Range))
lookupFn name = gets (M.lookup name . lsFnEnv)

initState :: LowerState
initState = LowerState
  { lsFresh = 0
  , lsFnEnv = M.empty
  , lsValueProcs = S.empty
  , lsTypeEnv = M.empty
  , lsArrayFacts = M.empty
  , lsVectorAccessFacts = M.empty
  , lsTopTypeEnv = M.empty
  , lsArrayRankEnv = M.empty
  }

initStateWithTypeEnv :: Map Var CType -> LowerState
initStateWithTypeEnv topEnv = initState { lsTopTypeEnv = topEnv }

initStateWithTypeEnvAndRanks :: Map Var CType -> Map Var Int -> LowerState
initStateWithTypeEnvAndRanks topEnv rankEnv =
  initState { lsTopTypeEnv = topEnv, lsArrayRankEnv = rankEnv }

-- | Lower a list of top-level declarations into a 'Program'.
--
-- Multi-argument function declarations are pre-registered so they can be
-- inlined at call sites; each remaining declaration becomes a 'Proc'.
lowerDecs2 :: [Dec Range] -> Program
lowerDecs2 decs = evalState go initState
  where
    go = do
      mapM_ preRegister decs
      Program <$> mapM lowerDec decs
    preRegister (Dec _ name pats _ _ body)
      | not (null pats) = registerFn name pats body
      | otherwise       = pure ()

-- | Like 'lowerDecs2' but initialised with a top-level type environment
-- from inference, enabling accurate 'CType' annotation for generated variables.
lowerDecs2WithTypeEnv :: Map Var CType -> [Dec Range] -> Program
lowerDecs2WithTypeEnv topEnv decs = evalState go (initStateWithTypeEnv topEnv)
  where
    go = do
      mapM_ preRegister decs
      Program <$> mapM lowerDec decs
    preRegister (Dec _ name pats _ _ body)
      | not (null pats) = registerFn name pats body
      | otherwise       = pure ()

-- | Like 'lowerDecs2WithTypeEnv' but also supplies an array-rank map that
-- enables rank-aware optimisations during lowering (e.g. emitting a scalar
-- accumulator instead of a 0-D output array for 1-D reductions).
lowerDecs2WithTypeEnvAndRanks :: Map Var CType -> Map Var Int -> [Dec Range] -> Program
lowerDecs2WithTypeEnvAndRanks topEnv rankEnv decs =
  evalState go (initStateWithTypeEnvAndRanks topEnv rankEnv)
  where
    go = do
      mapM_ preRegister decs
      Program <$> mapM lowerDec decs
    preRegister (Dec _ name pats _ _ body)
      | not (null pats) = registerFn name pats body
      | otherwise       = pure ()

lowerDec :: Dec Range -> LowerM Proc
lowerDec (Dec _ name pats _ _ body) = do
  let params = concatMap patVarNames pats
  when (null pats) $
    modify' $ \s -> s { lsValueProcs = S.insert name (lsValueProcs s) }
  savedFacts <- gets lsArrayFacts
  savedVectorFacts <- gets lsVectorAccessFacts
  modify' $ \s -> s { lsArrayFacts = M.empty
                    , lsVectorAccessFacts = M.empty
                    }
  (stmts, result) <- lowerExp body
  typeEnv <- gets lsTypeEnv
  arrayFacts <- gets lsArrayFacts
  vectorAccessFacts <- gets lsVectorAccessFacts
  modify' $ \s -> s { lsArrayFacts = savedFacts
                    , lsVectorAccessFacts = savedVectorFacts
                    }
  pure Proc
    { procName    = name
    , procParams  = params
    , procBody    = stmts ++ [SReturn result]
    , procTypeEnv = typeEnv
    , procArrayFacts = arrayFacts
    , procVectorAccessFacts = vectorAccessFacts
    }

patVarNames :: Pat a -> [CVar]
patVarNames (PVar _ v) = [v]
patVarNames (PBound _ v _) = [v]
patVarNames (PVec _ ps) = concatMap patVarNames ps
patVarNames (PPair _ p1 p2) = patVarNames p1 ++ patVarNames p2

shapeRankExp :: Exp a -> Maybe Int
shapeRankExp expr = case expr of
  EVec _ es -> Just (length es)
  _ -> Nothing

shapeLength1DExp :: Exp a -> Maybe (Exp a)
shapeLength1DExp expr = case expr of
  EVec _ [lenExp] -> Just lenExp
  _ -> Nothing

-- | Determine the rank of a shape expression.  For 'EVec' the rank is the
-- number of elements; for @'EShapeOf' (EVar v)@ the rank is looked up from
-- the array-rank environment populated by type inference.
shapeRankM :: Exp a -> LowerM (Maybe Int)
shapeRankM expr = case expr of
  EVec _ es         -> pure (Just (length es))
  EShapeOf _ (EVar _ v) -> gets (M.lookup v . lsArrayRankEnv)
  _                 -> pure Nothing

-- | Lower an expression, returning the emitted statements and the atom
-- that holds the result.
lowerExp :: Exp Range -> LowerM ([Stmt], Atom)
lowerExp expr = case expr of
  EInt _ n -> pure ([], AInt n)
  EFloat _ f -> pure ([], AFloat f)
  EBool _ b -> pure ([], ABool b)
  EString _ s -> pure ([], AString (stripStringQuotes s))
  EUnit _ -> pure ([], AUnit)

  EVar _ v -> do
    isValueProc <- gets (S.member v . lsValueProcs)
    if isValueProc
      then do
        t <- freshCVar "t"
        mTopTy <- gets (M.lookup v . lsTopTypeEnv)
        maybe (pure ()) (registerCType t) mTopTy
        pure ([SAssign t (RCall v [])], AVar t)
      else pure ([], AVar v)

  EVec _ es -> do
    (stmtss, atoms) <- unzip <$> mapM lowerExp es
    t <- freshCVar "t"
    registerCType t CTTuple
    propagateTupleDenseLinearIndex t atoms
    pure (concat stmtss ++ [SAssign t (RTuple atoms)], AVar t)

  ENeg _ e -> do
    (s, a) <- lowerExp e
    t <- freshCVar "t"
    pure (s ++ [SAssign t (RUnOp CNeg a)], AVar t)

  EBinOp _ e1 op e2 -> do
    (s1, a1) <- lowerExp e1
    (s2, a2) <- lowerExp e2
    t <- freshCVar "t"
    let bop = lowerBinOp op
    mapM_ (registerCType t) (binopResultCType bop)
    propagateDenseLinearIndexBinOp t bop a1 a2
    pure (s1 ++ s2 ++ [SAssign t (RBinOp bop a1 a2)], AVar t)

  EUnOp _ (Not _) e -> do
    (s, a) <- lowerExp e
    t <- freshCVar "t"
    registerCType t CTBool
    pure (s ++ [SAssign t (RUnOp CNot a)], AVar t)

  EUnOp _ (Fst _) e -> do
    (s, a) <- lowerExp e
    t <- freshCVar "t"
    mPairTys <- pairElemTypes a
    case mPairTys of
      Just (ct1, _ct2) -> do
        maybe (pure ()) (registerCType t) (elemTypeToCTypeMaybe ct1)
        pure (s ++ [SAssign t (RPairFst ct1 a)], AVar t)
      Nothing -> pure (s ++ [SAssign t (RProj 0 a)], AVar t)

  EUnOp _ (Snd _) e -> do
    (s, a) <- lowerExp e
    t <- freshCVar "t"
    mPairTys <- pairElemTypes a
    case mPairTys of
      Just (_ct1, ct2) -> do
        maybe (pure ()) (registerCType t) (elemTypeToCTypeMaybe ct2)
        pure (s ++ [SAssign t (RPairSnd ct2 a)], AVar t)
      Nothing -> pure (s ++ [SAssign t (RProj 1 a)], AVar t)

  EUnOp _ (Sqrt  _) e -> lowerMathUnOp CSqrt  e
  EUnOp _ (ExpF  _) e -> lowerMathUnOp CExpF  e
  EUnOp _ (Log   _) e -> lowerMathUnOp CLog   e
  EUnOp _ (Sin   _) e -> lowerMathUnOp CSin   e
  EUnOp _ (Cos   _) e -> lowerMathUnOp CCos   e
  EUnOp _ (AbsF  _) e -> lowerMathUnOp CAbsF  e
  EUnOp _ (FloorF _) e -> lowerMathUnOp CFloorF e
  EUnOp _ (CeilF  _) e -> lowerMathUnOp CCeilF  e
  EUnOp _ (Erf   _) e -> lowerMathUnOp CErf   e
  EUnOp _ (FloatOf _) e -> lowerMathUnOp CFloatOf e
  EUnOp _ (IntOf _) e -> do
    (s, a) <- lowerExp e
    t <- freshCVar "t"
    registerCType t CTInt64
    pure (s ++ [SAssign t (RUnOp CIntOf a)], AVar t)

  EOp _ op -> do
    t <- freshCVar "op"
    let name = case op of
          Plus _ -> "__prim_add"
          Minus _ -> "__prim_sub"
          Times _ -> "__prim_mul"
          Divide _ -> "__prim_div"
          Mod _    -> "__prim_mod"
          Eq _ -> "__prim_eq"
          Neq _ -> "__prim_neq"
          Lt _ -> "__prim_lt"
          Le _ -> "__prim_le"
          Gt _ -> "__prim_gt"
          Ge _ -> "__prim_ge"
          And _ -> "__prim_and"
          Or _ -> "__prim_or"
          PlusF _ -> "__prim_addf"
          MinusF _ -> "__prim_subf"
          TimesF _ -> "__prim_mulf"
          DivideF _ -> "__prim_divf"
          EqF _ -> "__prim_eqf"
          NeqF _ -> "__prim_neqf"
          LtF _ -> "__prim_ltf"
          LeF _ -> "__prim_lef"
          GtF _ -> "__prim_gtf"
          GeF _ -> "__prim_gef"
    pure ([SAssign t (RAtom (AVar name))], AVar t)

  EProj _ i e -> do
    (s, a) <- lowerExp e
    t <- freshCVar "t"
    aTy <- ctypeOfAtom a
    when (i == 0) $ propagateDenseLinearIndex t a
    -- Check if the atom is *positively confirmed* to be a scalar int.
    -- We must not skip the projection for variables whose type is unknown
    -- (defaulting to CTInt64) since they may actually be tuples (e.g.
    -- fused generate index parameters).
    isConfirmedScalar <- isAtomConfirmedScalar s a
    let singletonProjAtom =
          case reverse s of
            (SAssign v (RTuple [inner]) : _) | a == AVar v -> Just inner
            _ -> Nothing
    let rhs
          | i == 0, Just inner <- singletonProjAtom = RAtom inner
          | i == 0 && isConfirmedScalar = RAtom a
          | otherwise = RProj i a
    pure (s ++ [SAssign t rhs], AVar t)

  ERecord _ fields -> do
    loweredFields <- mapM (\(field, expr') -> do
      (stmts, atom) <- lowerExp expr'
      cty <- ctypeOfAtom atom
      pure (stmts, (field, atom), (field, cty))) fields
    t <- freshCVar "record"
    let stmts = concatMap (\(ss, _, _) -> ss) loweredFields
        fieldAtoms = normalizeRecordFields (map (\(_, fieldAtom, _) -> fieldAtom) loweredFields)
        fieldTypes = map (\(_, _, fieldTy) -> fieldTy) loweredFields
        recordTy = CTRecord (normalizeRecordFields fieldTypes)
    registerCType t recordTy
    pure (stmts ++ [SAssign t (RRecord fieldAtoms)], AVar t)

  ERecordProj _ e field -> do
    (s, a) <- lowerExp e
    t <- freshCVar "record_field"
    mFieldTy <- recordFieldType a field
    maybe (pure ()) (registerCType t) mFieldTy
    pure (s ++ [SAssign t (RRecordProj field a)], AVar t)

  EPair _ e1 e2 -> do
    (s1, a1) <- lowerExp e1
    (s2, a2) <- lowerExp e2
    met1 <- elemTypeOfAtom a1
    met2 <- elemTypeOfAtom a2
    t <- freshCVar "pair"
    -- Use CEInt as a placeholder only when the real type is unknown; CFGTyping
    -- will recover the correct type via its fixpoint.  Crucially, do NOT
    -- register a pair type whose components default to CTInt64 — that would
    -- poison procTypeEnv and prevent CFGTyping from converging to the correct type.
    let et1 = fromMaybe CEInt met1
        et2 = fromMaybe CEInt met2
    case (met1, met2) of
      (Just _, Just _) -> registerCType t (CTPair (elemTypeToCType et1) (elemTypeToCType et2))
      _                -> pure ()  -- leave unregistered; CFGTyping recovers
    pure (s1 ++ s2 ++ [SAssign t (RPairMake et1 et2 a1 a2)], AVar t)

  EIfThen _ cond thn -> do
    (sc, ac) <- lowerExp cond
    (st, at') <- lowerExp thn
    r <- freshCVar "r"
    thnTy <- ctypeOfAtom at'
    when (thnTy /= CTUnknown) $ registerCType r thnTy
    pure (sc ++ [SIf ac (st ++ [SAssign r (RAtom at')]) [SAssign r (RAtom AUnit)]], AVar r)

  EIfThenElse _ cond thn els -> do
    (sc, ac) <- lowerExp cond
    (st, at') <- lowerExp thn
    (se, ae) <- lowerExp els
    r <- freshCVar "r"
    -- Propagate the result type from either branch so downstream code does not
    -- see CTUnknown when this if-else is used as a record field or array body.
    thnTy <- ctypeOfAtom at'
    elsTy <- ctypeOfAtom ae
    let rTy = case (thnTy, elsTy) of
                (CTUnknown, t) -> t
                (t, _)        -> t
    when (rTy /= CTUnknown) $ registerCType r rTy
    pure (sc ++ [SIf ac (st ++ [SAssign r (RAtom at')]) (se ++ [SAssign r (RAtom ae)])], AVar r)

  ELetIn _ (Dec _ name pats _ _ body) rest
    | not (null pats) -> do
        registerFn name pats body
        lowerExp rest
    | otherwise -> do
        (sb, ab) <- lowerExp body
        let bindStmt = SAssign name (RAtom ab)
        propagatePairInfo name ab
        (sr, ar) <- lowerExp rest
        pure (sb ++ [bindStmt] ++ sr, ar)

  EBoundLetIn _ x _ rhs body -> do
    (sr, ar) <- lowerExp rhs
    let bindStmt = SAssign x (RAtom ar)
    propagatePairInfo x ar
    (sb, ab) <- lowerExp body
    pure (sr ++ [bindStmt] ++ sb, ab)

  EApp _ fn arg -> handleApp fn arg

  -- === Array operations ===

  EGenerate _ shape fnExp -> do
    (ss, as') <- lowerExp shape
    arr  <- freshCVar "arr"
    idx  <- freshCVar "idx"
    _val <- freshCVar "val"
    (setupStmts, loopStmt) <- case shapeRankExp shape of
      -- 1D: flat loop, pass flat index directly to the body function.
      Just 1 -> do
        i <- freshIterVar "i"
        n <- freshCVar "n"
        bodyStmts <- inlineArrayFn1D fnExp i _val
        valTy <- ctypeOfAtom (AVar _val)
        registerCType arr (CTArray valTy)
        markArrayFreshWriteOnce arr
        markContiguousWriteArray arr
        pure ( [ SAssign arr (RArrayAlloc as')
               , SAssign n   (RShapeSize as')
               ]
             , SLoop (LoopSpec [i] [atomToIndexExpr (AVar n)] Serial Nothing LoopMap)
                 (bodyStmts ++ [SArrayWrite (AVar arr) (AVar i) (AVar _val)])
             )
      -- 2D: nested (i, j) loops; avoids the flat→ND→flat roundtrip.
      Just 2 -> do
        i    <- freshIterVar "i"
        j    <- freshIterVar "j"
        dim0 <- freshCVar "dim0"
        dim1 <- freshCVar "dim1"
        mul  <- freshCVar "mul"
        flat <- freshCVar "flat"
        registerCType dim0 CTInt64
        registerCType dim1 CTInt64
        registerCType mul  CTInt64
        registerCType flat CTInt64
        registerCType idx  CTTuple
        bodyStmts <- inlineArrayFn fnExp idx _val
        valTy <- ctypeOfAtom (AVar _val)
        registerCType arr (CTArray valTy)
        markArrayFreshWriteOnce arr
        markContiguousWriteArray arr
        pure ( [ SAssign arr  (RArrayAlloc as')
               , SAssign dim0 (RProj 0 as')
               , SAssign dim1 (RProj 1 as')
               ]
             , SLoop (LoopSpec [i, j]
                               [atomToIndexExpr (AVar dim0), atomToIndexExpr (AVar dim1)]
                               Serial Nothing LoopMap)
                 ( SAssign idx (RTuple [AVar i, AVar j])
                   : bodyStmts
                   ++ [ SAssign mul  (RBinOp CMul (AVar i) (AVar dim1))
                      , SAssign flat (RBinOp CAdd (AVar mul) (AVar j))
                      , SArrayWrite (AVar arr) (AVar flat) (AVar _val)
                      ]
                 )
             )
      -- N-D (n>2 or unknown rank): flat loop with flat→ND decomposition.
      _ -> do
        i <- freshIterVar "i"
        n <- freshCVar "n"
        registerCType idx CTTuple
        noteDenseLinearIndex idx i
        bodyStmts <- inlineArrayFn fnExp idx _val
        valTy <- ctypeOfAtom (AVar _val)
        registerCType arr (CTArray valTy)
        markArrayFreshWriteOnce arr
        markContiguousWriteArray arr
        pure ( [ SAssign arr (RArrayAlloc as')
               , SAssign n   (RShapeSize as')
               ]
             , SLoop (LoopSpec [i] [atomToIndexExpr (AVar n)] Serial Nothing LoopMap)
                 ( SAssign idx (RFlatToNd (AVar i) as')
                   : bodyStmts
                   ++ [SArrayWrite (AVar arr) (AVar i) (AVar _val)]
                 )
             )
    pure (ss ++ setupStmts ++ [loopStmt], AVar arr)

  EFill _ shape valExp -> do
    (sv, av) <- lowerExp valExp
    arr <- freshCVar "arr"
    i <- freshIterVar "i"
    valTy <- ctypeOfAtom av
    registerCType arr (CTArray valTy)
    markArrayFreshWriteOnce arr
    markContiguousWriteArray arr
    case shapeLength1DExp shape of
      Just lenExp -> do
        (sl, alen) <- lowerExp lenExp
        shp <- freshCVar "fill_shp"
        registerCType shp CTTuple
        pure ( sl ++ sv
             ++ [ SAssign shp (RTuple [alen])
                , SAssign arr (RArrayAlloc (AVar shp))
                , SLoop (LoopSpec [i] [atomToIndexExpr alen] Serial Nothing LoopMap)
                    [SArrayWrite (AVar arr) (AVar i) av]
                 ]
              , AVar arr
              )
      Nothing -> do
        (ss, as') <- lowerExp shape
        n <- freshCVar "n"
        pure ( ss ++ sv
             ++ [ SAssign arr (RArrayAlloc as')
                , SAssign n (RShapeSize as')
                , SLoop (LoopSpec [i] [atomToIndexExpr (AVar n)] Serial Nothing LoopMap)
                    [SArrayWrite (AVar arr) (AVar i) av]
                 ]
              , AVar arr
              )

  EMap _ fnExp arrExp -> do
    (sa, aa) <- lowerExp arrExp
    shp <- freshCVar "shp"
    arr <- freshCVar "arr"
    n <- freshCVar "n"
    i <- freshIterVar "i"
    elem' <- freshCVar "elem"
    val <- freshCVar "val"
    -- Register the element type of elem' from the source array so that
    -- inlineScalarFn can correctly handle index-pattern functions applied
    -- to scalar-element arrays (e.g. map over sort_indices output).
    arrTy <- ctypeOfAtom aa
    case arrTy of
      CTArray et -> registerCType elem' et
      _          -> pure ()
    bodyStmts <- inlineScalarFn fnExp elem' val
    valTy <- ctypeOfAtom (AVar val)
    registerCType arr (CTArray valTy)
    noteReadOnlyAtom aa
    markArrayFreshWriteOnce arr
    noteDenseReadAtom aa
    markContiguousWriteArray arr
    pure ( sa
         ++ [ SAssign shp (RArrayShape aa)
            , SAssign arr (RArrayAlloc (AVar shp))
            , SAssign n (RShapeSize (AVar shp))
            , SLoop (LoopSpec [i] [atomToIndexExpr (AVar n)] Serial Nothing LoopMap)
                ( [SAssign elem' (RArrayLoad aa (AVar i))]
                   ++ bodyStmts
                   ++ [SArrayWrite (AVar arr) (AVar i) (AVar val)]
                )
            ]
         , AVar arr
         )

  EZipWith _ fnExp arr1Exp arr2Exp -> do
    (s1, a1) <- lowerExp arr1Exp
    (s2, a2) <- lowerExp arr2Exp
    shp <- freshCVar "shp"
    arr <- freshCVar "arr"
    n <- freshCVar "n"
    i <- freshIterVar "i"
    e1 <- freshCVar "e"
    e2 <- freshCVar "e"
    val <- freshCVar "val"
    bodyStmts <- inlineBinaryFn fnExp e1 e2 val
    valTy <- ctypeOfAtom (AVar val)
    registerCType arr (CTArray valTy)
    noteReadOnlyAtom a1
    noteReadOnlyAtom a2
    markArrayFreshWriteOnce arr
    noteDenseReadAtom a1
    noteDenseReadAtom a2
    markContiguousWriteArray arr
    pure ( s1 ++ s2
         ++ [ SAssign shp (RArrayShape a1)
            , SAssign arr (RArrayAlloc (AVar shp))
            , SAssign n (RShapeSize (AVar shp))
            , SLoop (LoopSpec [i] [atomToIndexExpr (AVar n)] Serial Nothing LoopMap)
                ( [ SAssign e1 (RArrayLoad a1 (AVar i))
                  , SAssign e2 (RArrayLoad a2 (AVar i))
                  ]
                  ++ bodyStmts
                  ++ [SArrayWrite (AVar arr) (AVar i) (AVar val)]
                )
            ]
         , AVar arr
         )

  EReduce _ fnExp initExp arrExp -> do
    (si, ai) <- lowerExp initExp
    (sa, aa) <- lowerExp arrExp
    shp <- freshCVar "shp"
    outShp <- freshCVar "out_shp"
    redDim <- freshCVar "red_dim"
    outN <- freshCVar "out_n"
    outArr <- freshCVar "out_arr"
    j <- freshIterVar "j"
    acc <- freshCVar "acc"
    k <- freshCVar "k"
    base <- freshCVar "base"
    flatIn <- freshCVar "flat_in"
    elem' <- freshCVar "elem"
    (bodyStmts, mRedop) <- lowerReductionStep fnExp acc elem'
    let mReductionSpec = fmap (ReductionSpec acc (atomToIndexExpr ai)) mRedop
    pure ( si ++ sa
         ++ [ SAssign shp (RArrayShape aa)
            , SAssign outShp (RShapeInit (AVar shp))
            , SAssign redDim (RShapeLast (AVar shp))
            , SAssign outN (RShapeSize (AVar outShp))
             , SAssign outArr (RArrayAlloc (AVar outShp))
             , SLoop (LoopSpec [j] [atomToIndexExpr (AVar outN)] Serial Nothing LoopMapReduction)
                  ( [ SAssign base (RBinOp CMul (AVar j) (AVar redDim))
                      , SAssign acc (RAtom ai)
                      , SLoop (LoopSpec [k] [atomToIndexExpr (AVar redDim)] Serial mReductionSpec LoopReduction)
                          ( [ SAssign flatIn (RBinOp CAdd (AVar base) (AVar k))
                           , SAssign elem' (RArrayLoad aa (AVar flatIn))
                           ]
                           ++ bodyStmts
                         )
                    , SArrayWrite (AVar outArr) (AVar j) (AVar acc)
                    ]
                 )
            ]
         , AVar outArr
         )

  EReduceGenerate _ fnExp initExp shapeExp genExp -> do
    (si, ai) <- lowerExp initExp
    (ss, as') <- lowerExp shapeExp
    mRank <- shapeRankM shapeExp
    case mRank of
      Just 1 -> do
        -- 1-D input: skip the outer LoopMapReduction (which iterates exactly
        -- once) and accumulate directly in a scalar.  Store the result into a
        -- freshly allocated 0-D array so the proc return type is unchanged.
        -- Use inlineArrayFn1D with the raw loop counter k (CTInt64) so that
        -- array element accesses inside the body use k directly as a flat
        -- index, avoiding the flat_to_nd / nd_to_flat round-trip entirely.
        outShp <- freshCVar "out_shp"
        redDim <- freshCVar "red_dim"
        outArr <- freshCVar "out_arr"
        acc    <- freshCVar "acc"
        k      <- freshIterVar "k"
        elem'  <- freshCVar "elem"
        genStmts <- inlineArrayFn1D genExp k elem'
        (bodyStmts, mRedop) <- lowerReductionStep fnExp acc elem'
        let mReductionSpec = fmap (ReductionSpec acc (atomToIndexExpr ai)) mRedop
        pure ( si ++ ss
             ++ [ SAssign outShp (RShapeInit as')
                , SAssign redDim (RShapeLast as')
                , SAssign outArr (RArrayAlloc (AVar outShp))
                , SAssign acc (RAtom ai)
                , SLoop (LoopSpec [k] [atomToIndexExpr (AVar redDim)] Serial mReductionSpec LoopReduction)
                    ( genStmts
                      ++ bodyStmts
                    )
                , SArrayWrite (AVar outArr) (AInt 0) (AVar acc)
                ]
             , AVar outArr
             )
      _ -> do
        -- General case: output is a multi-element array.
        outShp <- freshCVar "out_shp"
        redDim <- freshCVar "red_dim"
        outN <- freshCVar "out_n"
        outArr <- freshCVar "out_arr"
        j <- freshIterVar "j"
        acc <- freshCVar "acc"
        k <- freshCVar "k"
        base <- freshCVar "base"
        flatIn <- freshCVar "flat_in"
        idx <- freshCVar "idx"
        elem' <- freshCVar "elem"
        _val <- freshCVar "val"
        registerCType idx CTTuple
        noteDenseLinearIndex idx k
        genStmts <- inlineArrayFn genExp idx elem'
        (bodyStmts, mRedop) <- lowerReductionStep fnExp acc elem'
        let mReductionSpec = fmap (ReductionSpec acc (atomToIndexExpr ai)) mRedop
        pure ( si ++ ss
             ++ [ SAssign outShp (RShapeInit as')
                , SAssign redDim (RShapeLast as')
                , SAssign outN (RShapeSize (AVar outShp))
                , SAssign outArr (RArrayAlloc (AVar outShp))
                , SLoop (LoopSpec [j] [atomToIndexExpr (AVar outN)] Serial Nothing LoopMapReduction)
                    ( [ SAssign base (RBinOp CMul (AVar j) (AVar redDim))
                      -- Initialize the accumulator for this output element.
                      , SAssign acc (RAtom ai)
                      , SLoop (LoopSpec [k] [atomToIndexExpr (AVar redDim)] Serial mReductionSpec LoopReduction)
                          ( [ SAssign flatIn (RBinOp CAdd (AVar base) (AVar k))
                            , SAssign idx (RFlatToNd (AVar flatIn) as')
                            ]
                            ++ genStmts
                            ++ bodyStmts
                          )
                      , SArrayWrite (AVar outArr) (AVar j) (AVar acc)
                      ]
                    )
                ]
             , AVar outArr
             )

  -- Optimized: foldl over fill avoids allocating a temporary array.
  -- The fill element is loop-invariant; assign it once before the loop.
  EFoldl _ fnExp initExp (EFill _ shapeExp valExp) -> do
    (si, ai)  <- lowerExp initExp
    (sv, av)  <- lowerExp valExp
    acc   <- freshCVar "foldl_acc"
    k     <- freshCVar "foldl_k"
    elem' <- freshCVar "foldl_elem"
    propagatePairInfo acc ai
    bodyStmts <- inlineBinaryFn fnExp acc elem' acc
    case shapeLength1DExp shapeExp of
      Just lenExp -> do
        (sl, alen) <- lowerExp lenExp
        pure ( si ++ sl ++ sv
             ++ [ SAssign acc   (RAtom ai)
                , SAssign elem' (RAtom av)
                , SLoop (LoopSpec [k] [atomToIndexExpr alen] Serial Nothing LoopFold)
                    bodyStmts
                ]
             , AVar acc
             )
      Nothing -> do
        (ss, as') <- lowerExp shapeExp
        n <- freshCVar "foldl_n"
        pure ( si ++ ss ++ sv
             ++ [ SAssign n     (RShapeSize as')
                , SAssign acc   (RAtom ai)
                , SAssign elem' (RAtom av)
                , SLoop (LoopSpec [k] [atomToIndexExpr (AVar n)] Serial Nothing LoopFold)
                    bodyStmts
                ]
             , AVar acc
             )

  -- Optimized: foldl over generate avoids allocating a temporary array.
  -- The generator body is inlined directly into the loop.
  EFoldl _ fnExp initExp (EGenerate _ shapeExp genFnExp) -> do
    (si, ai)  <- lowerExp initExp
    (ss, as') <- lowerExp shapeExp
    n     <- freshCVar "foldl_n"
    acc   <- freshCVar "foldl_acc"
    k     <- freshCVar "foldl_k"
    elem' <- freshCVar "foldl_elem"
    propagatePairInfo acc ai
    genStmts  <- inlineArrayFn1D genFnExp k elem'
    bodyStmts <- inlineBinaryFn fnExp acc elem' acc
    pure ( si ++ ss
         ++ [ SAssign n   (RShapeSize as')
            , SAssign acc (RAtom ai)
            , SLoop (LoopSpec [k] [atomToIndexExpr (AVar n)] Serial Nothing LoopFold)
                ( genStmts ++ bodyStmts )
            ]
         , AVar acc
         )

  EFoldl _ fnExp initExp arrExp -> do
    (si, ai) <- lowerExp initExp
    (sa, aa) <- lowerExp arrExp
    shp  <- freshCVar "foldl_shp"
    n    <- freshCVar "foldl_n"
    acc  <- freshCVar "foldl_acc"
    k    <- freshCVar "foldl_k"
    elem' <- freshCVar "foldl_elem"
    propagatePairInfo acc ai
    bodyStmts <- inlineBinaryFn fnExp acc elem' acc
    pure ( si ++ sa
         ++ [ SAssign shp (RArrayShape aa)
            , SAssign n (RShapeLast (AVar shp))
            , SAssign acc (RAtom ai)
            , SLoop (LoopSpec [k] [atomToIndexExpr (AVar n)] Serial Nothing LoopFold)
                ( [ SAssign elem' (RArrayLoad aa (AVar k))
                  ]
                  ++ bodyStmts
                )
             ]
         , AVar acc
         )
  -- foldl_while: early-exit fold. Emits a break when the predicate on the
  -- accumulator is false before processing the next element.
  EFoldlWhile _ predExp fnExp initExp (EFill _ shapeExp valExp) -> do
    (si, ai)  <- lowerExp initExp
    (sv, av)  <- lowerExp valExp
    acc    <- freshCVar "foldl_acc"
    k      <- freshCVar "foldl_k"
    elem'  <- freshCVar "foldl_elem"
    predV  <- freshCVar "foldl_pred"
    propagatePairInfo acc ai
    bodyStmts <- inlineBinaryFn fnExp acc elem' acc
    predStmts <- inlineScalarFn predExp acc predV
    case shapeLength1DExp shapeExp of
      Just lenExp -> do
        (sl, alen) <- lowerExp lenExp
        pure ( si ++ sl ++ sv
             ++ [ SAssign acc   (RAtom ai)
                , SAssign elem' (RAtom av)
                , SLoop (LoopSpec [k] [atomToIndexExpr alen] Serial Nothing LoopFold)
                    ( predStmts
                      ++ [SIf (AVar predV) [] [SBreak]]
                      ++ bodyStmts
                    )
                ]
             , AVar acc
             )
      Nothing -> do
        (ss, as') <- lowerExp shapeExp
        n <- freshCVar "foldl_n"
        pure ( si ++ ss ++ sv
             ++ [ SAssign n     (RShapeSize as')
                , SAssign acc   (RAtom ai)
                , SAssign elem' (RAtom av)
                , SLoop (LoopSpec [k] [atomToIndexExpr (AVar n)] Serial Nothing LoopFold)
                    ( predStmts
                      ++ [SIf (AVar predV) [] [SBreak]]
                      ++ bodyStmts
                    )
                ]
             , AVar acc
             )

  EFoldlWhile _ predExp fnExp initExp (EGenerate _ shapeExp genFnExp) -> do
    (si, ai)  <- lowerExp initExp
    (ss, as') <- lowerExp shapeExp
    n      <- freshCVar "foldl_n"
    acc    <- freshCVar "foldl_acc"
    k      <- freshCVar "foldl_k"
    elem'  <- freshCVar "foldl_elem"
    predV  <- freshCVar "foldl_pred"
    propagatePairInfo acc ai
    genStmts  <- inlineArrayFn1D genFnExp k elem'
    bodyStmts <- inlineBinaryFn fnExp acc elem' acc
    predStmts <- inlineScalarFn predExp acc predV
    pure ( si ++ ss
         ++ [ SAssign n   (RShapeSize as')
            , SAssign acc (RAtom ai)
            , SLoop (LoopSpec [k] [atomToIndexExpr (AVar n)] Serial Nothing LoopFold)
                ( predStmts
                  ++ [SIf (AVar predV) [] [SBreak]]
                  ++ genStmts
                  ++ bodyStmts
                )
            ]
         , AVar acc
         )

  EFoldlWhile _ predExp fnExp initExp arrExp -> do
    (si, ai) <- lowerExp initExp
    (sa, aa) <- lowerExp arrExp
    shp   <- freshCVar "foldl_shp"
    n     <- freshCVar "foldl_n"
    acc   <- freshCVar "foldl_acc"
    k     <- freshCVar "foldl_k"
    elem' <- freshCVar "foldl_elem"
    predV <- freshCVar "foldl_pred"
    propagatePairInfo acc ai
    bodyStmts <- inlineBinaryFn fnExp acc elem' acc
    predStmts <- inlineScalarFn predExp acc predV
    pure ( si ++ sa
         ++ [ SAssign shp (RArrayShape aa)
            , SAssign n (RShapeLast (AVar shp))
            , SAssign acc (RAtom ai)
            , SLoop (LoopSpec [k] [atomToIndexExpr (AVar n)] Serial Nothing LoopFold)
                ( predStmts
                  ++ [SIf (AVar predV) [] [SBreak]]
                  ++ [SAssign elem' (RArrayLoad aa (AVar k))]
                  ++ bodyStmts
                )
            ]
         , AVar acc
         )

  EScan _ fnExp initExp arrExp -> do
    (si, ai) <- lowerExp initExp
    (sa, aa) <- lowerExp arrExp
    shp <- freshCVar "scan_shp"
    n <- freshCVar "scan_n"
    outArr <- freshCVar "scan_arr"
    acc <- freshCVar "scan_acc"
    k <- freshCVar "scan_k"
    elem' <- freshCVar "scan_elem"
    accTy <- ctypeOfAtom ai
    -- Only register concrete types; CTUnknown must not enter procTypeEnv.
    when (accTy /= CTUnknown) $ do
      registerCType acc accTy
      registerCType outArr (CTArray accTy)
    propagatePairInfo acc ai
    bodyStmts <- inlineBinaryFn fnExp acc elem' acc
    pure ( si ++ sa
         ++ [ SAssign shp (RArrayShape aa)
            , SAssign n (RShapeLast (AVar shp))
            , SAssign outArr (RArrayAlloc (AVar shp))
            , SAssign acc (RAtom ai)
            , SLoop (LoopSpec [k] [atomToIndexExpr (AVar n)] Serial Nothing LoopFold)
                ( [ SAssign elem' (RArrayLoad aa (AVar k))
                  , SArrayWrite (AVar outArr) (AVar k) (AVar acc)
                  ]
                  ++ bodyStmts
                )
             ]
          , AVar outArr
          )
  EScanInclusive _ fnExp initExp arrExp -> do
    (si, ai) <- lowerExp initExp
    (sa, aa) <- lowerExp arrExp
    shp <- freshCVar "scan_incl_shp"
    n <- freshCVar "scan_incl_n"
    outArr <- freshCVar "scan_incl_arr"
    acc <- freshCVar "scan_incl_acc"
    k <- freshCVar "scan_incl_k"
    elem' <- freshCVar "scan_incl_elem"
    accTy <- ctypeOfAtom ai
    -- Only register concrete types; CTUnknown must not enter procTypeEnv.
    when (accTy /= CTUnknown) $ do
      registerCType acc accTy
      registerCType outArr (CTArray accTy)
    propagatePairInfo acc ai
    bodyStmts <- inlineBinaryFn fnExp acc elem' acc
    pure ( si ++ sa
         ++ [ SAssign shp (RArrayShape aa)
            , SAssign n (RShapeLast (AVar shp))
            , SAssign outArr (RArrayAlloc (AVar shp))
            , SAssign acc (RAtom ai)
            , SLoop (LoopSpec [k] [atomToIndexExpr (AVar n)] Serial Nothing LoopFold)
                ( [ SAssign elem' (RArrayLoad aa (AVar k)) ]
                  ++ bodyStmts
                  ++ [SArrayWrite (AVar outArr) (AVar k) (AVar acc)]
                )
             ]
         , AVar outArr
         )
  EScanR _ fnExp initExp arrExp -> do
    (si, ai) <- lowerExp initExp
    (sa, aa) <- lowerExp arrExp
    shp <- freshCVar "scanr_shp"
    n <- freshCVar "scanr_n"
    n1 <- freshCVar "scanr_n1"
    outArr <- freshCVar "scanr_arr"
    acc <- freshCVar "scanr_acc"
    k <- freshCVar "scanr_k"
    ix <- freshCVar "scanr_ix"
    elem' <- freshCVar "scanr_elem"
    accTy <- ctypeOfAtom ai
    -- Only register concrete types; CTUnknown must not enter procTypeEnv.
    when (accTy /= CTUnknown) $ do
      registerCType acc accTy
      registerCType outArr (CTArray accTy)
    propagatePairInfo acc ai
    bodyStmts <- inlineBinaryFn fnExp acc elem' acc
    pure ( si ++ sa
         ++ [ SAssign shp (RArrayShape aa)
            , SAssign n (RShapeLast (AVar shp))
            , SAssign n1 (RBinOp CSub (AVar n) (AInt 1))
            , SAssign outArr (RArrayAlloc (AVar shp))
            , SAssign acc (RAtom ai)
            , SLoop (LoopSpec [k] [atomToIndexExpr (AVar n)] Serial Nothing LoopFold)
                ( [ SAssign ix (RBinOp CSub (AVar n1) (AVar k))
                  , SAssign elem' (RArrayLoad aa (AVar ix))
                  , SArrayWrite (AVar outArr) (AVar ix) (AVar acc)
                  ]
                  ++ bodyStmts
                )
             ]
         , AVar outArr
         )
  EScanRInclusive _ fnExp initExp arrExp -> do
    (si, ai) <- lowerExp initExp
    (sa, aa) <- lowerExp arrExp
    shp <- freshCVar "scanr_incl_shp"
    n <- freshCVar "scanr_incl_n"
    n1 <- freshCVar "scanr_incl_n1"
    outArr <- freshCVar "scanr_incl_arr"
    acc <- freshCVar "scanr_incl_acc"
    k <- freshCVar "scanr_incl_k"
    ix <- freshCVar "scanr_incl_ix"
    elem' <- freshCVar "scanr_incl_elem"
    accTy <- ctypeOfAtom ai
    -- Only register concrete types; CTUnknown must not enter procTypeEnv.
    when (accTy /= CTUnknown) $ do
      registerCType acc accTy
      registerCType outArr (CTArray accTy)
    propagatePairInfo acc ai
    bodyStmts <- inlineBinaryFn fnExp acc elem' acc
    pure ( si ++ sa
         ++ [ SAssign shp (RArrayShape aa)
            , SAssign n (RShapeLast (AVar shp))
            , SAssign n1 (RBinOp CSub (AVar n) (AInt 1))
            , SAssign outArr (RArrayAlloc (AVar shp))
            , SAssign acc (RAtom ai)
            , SLoop (LoopSpec [k] [atomToIndexExpr (AVar n)] Serial Nothing LoopFold)
                ( [ SAssign ix (RBinOp CSub (AVar n1) (AVar k))
                  , SAssign elem' (RArrayLoad aa (AVar ix))
                  ]
                  ++ bodyStmts
                  ++ [SArrayWrite (AVar outArr) (AVar ix) (AVar acc)]
                )
             ]
         , AVar outArr
         )
  ESegmentedReduce _ fnExp initExp offsetsExp valsExp -> do
    (si, ai) <- lowerExp initExp
    (so, ao) <- lowerExp offsetsExp
    (sv, av) <- lowerExp valsExp
    offsetsShp <- freshCVar "segred_offsets_shp"
    offsetsN <- freshCVar "segred_offsets_n"
    outN <- freshCVar "segred_out_n"
    outShp <- freshCVar "segred_out_shp"
    outArr <- freshCVar "segred_out"
    seg <- freshCVar "segred_seg"
    start <- freshCVar "segred_start"
    nextSeg <- freshCVar "segred_next_seg"
    stop <- freshCVar "segred_stop"
    segLen <- freshCVar "segred_len"
    acc <- freshCVar "segred_acc"
    k <- freshCVar "segred_k"
    elemIx <- freshCVar "segred_elem_ix"
    elem' <- freshCVar "segred_elem"
    accTy <- ctypeOfAtom ai
    -- Only register concrete types; CTUnknown must not enter procTypeEnv.
    when (accTy /= CTUnknown) $ do
      registerCType acc accTy
      registerCType outArr (CTArray accTy)
    propagatePairInfo acc ai
    bodyStmts <- inlineBinaryFn fnExp acc elem' acc
    pure
      ( si ++ so ++ sv
          ++ [ SAssign offsetsShp (RArrayShape ao)
             , SAssign offsetsN (RShapeLast (AVar offsetsShp))
             , SAssign outN (RBinOp CSub (AVar offsetsN) (AInt 1))
             , SAssign outShp (RTuple [AVar outN])
             , SAssign outArr (RArrayAlloc (AVar outShp))
             , SLoop (LoopSpec [seg] [atomToIndexExpr (AVar outN)] Serial Nothing LoopPlain)
                 [ SAssign start (RArrayLoad ao (AVar seg))
                 , SAssign nextSeg (RBinOp CAdd (AVar seg) (AInt 1))
                 , SAssign stop (RArrayLoad ao (AVar nextSeg))
                 , SAssign segLen (RBinOp CSub (AVar stop) (AVar start))
                 , SAssign acc (RAtom ai)
                 , SLoop (LoopSpec [k] [atomToIndexExpr (AVar segLen)] Serial Nothing LoopReduction)
                     ( [ SAssign elemIx (RBinOp CAdd (AVar start) (AVar k))
                       , SAssign elem' (RArrayLoad av (AVar elemIx))
                       ]
                       ++ bodyStmts
                     )
                 , SArrayWrite (AVar outArr) (AVar seg) (AVar acc)
                 ]
             ]
      , AVar outArr
      )
  EIota _ nExp -> do
    -- Lower iota n as: allocate [n] int array, fill with loop index i
    (sn, an) <- lowerExp nExp
    shp <- freshCVar "iota_shp"
    arr <- freshCVar "iota_arr"
    n   <- freshCVar "iota_n"
    i   <- freshCVar "iota_i"
    registerCType arr (CTArray CTInt64)
    markArrayFreshWriteOnce arr
    markContiguousWriteArray arr
    pure ( sn
         ++ [ SAssign shp (RTuple [an])
            , SAssign arr (RArrayAlloc (AVar shp))
            , SAssign n   (RShapeSize  (AVar shp))
            , SLoop (LoopSpec [i] [atomToIndexExpr (AVar n)] Serial Nothing LoopMap)
                [SArrayWrite (AVar arr) (AVar i) (AVar i)]
            ]
         , AVar arr
         )
  EMakeIndex _ _ arrExp ->
    -- make_index is a type-level annotation; lower to the inner array unchanged
    lowerExp arrExp
  ESortIndices _ arrExp -> do
    (sa, aa) <- lowerExp arrExp
    outArr <- freshCVar "sort_idx"
    registerCType outArr (CTArray CTInt64)
    pure (sa ++ [SAssign outArr (RCall "hyd_sort_indices" [aa])], AVar outArr)
  ECOOSumDuplicates _ nrowsExp ncolsExp nnzExp rowsExp colsExp valsExp -> do
    (srNRows, arNRows) <- lowerExp nrowsExp
    (srNCols, arNCols) <- lowerExp ncolsExp
    (srNnz, arNnz) <- lowerExp nnzExp
    -- Create loop variables first so they can be passed to lowerForSeqConsume,
    -- which may use them inside the inline per-iteration statements it builds.
    shp <- freshCVar "coo_shp"
    outRows <- freshCVar "coo_rows"
    outCols <- freshCVar "coo_cols"
    outVals <- freshCVar "coo_vals"
    outNnz <- freshCVar "coo_nnz"
    i <- freshCVar "coo_i"
    row <- freshCVar "coo_row"
    col <- freshCVar "coo_col"
    val <- freshCVar "coo_val"
    -- Inline EMap inputs directly into the dedup loop; avoids materialising
    -- sorted_rows / sorted_cols / sorted_vals as separate intermediate arrays.
    (srRows, rowSrcAtom, rowIterStmts) <- lowerForSeqConsume rowsExp i row
    (srCols, _,          colIterStmts) <- lowerForSeqConsume colsExp i col
    (srVals, _,          valIterStmts) <- lowerForSeqConsume valsExp i val
    lastIdx <- freshCVar "coo_last"
    prevRow <- freshCVar "coo_prev_row"
    prevCol <- freshCVar "coo_prev_col"
    prevVal <- freshCVar "coo_prev_val"
    mergedVal <- freshCVar "coo_merged_val"
    hasPrev <- freshCVar "coo_has_prev"
    sameRow <- freshCVar "coo_same_row"
    sameCol <- freshCVar "coo_same_col"
    sameEntry <- freshCVar "coo_same_entry"
    result <- freshCVar "coo_result"
    let cooTy = CTRecord
          [ ("cols", CTArray CTInt64)
          , ("ncols", CTInt64)
          , ("nnz", CTInt64)
          , ("nrows", CTInt64)
          , ("rows", CTArray CTInt64)
          , ("vals", CTArray CTInt64)
          ]
    registerCType outRows (CTArray CTInt64)
    registerCType outCols (CTArray CTInt64)
    registerCType outVals (CTArray CTInt64)
    registerCType outNnz CTInt64
    registerCType prevRow CTInt64
    registerCType prevCol CTInt64
    registerCType prevVal CTInt64
    registerCType result cooTy
    pure
      ( srNRows ++ srNCols ++ srNnz ++ srRows ++ srCols ++ srVals
          ++ [ SAssign shp (RArrayShape rowSrcAtom)
             , SAssign outRows (RArrayAlloc (AVar shp))
             , SAssign outCols (RArrayAlloc (AVar shp))
             , SAssign outVals (RArrayAlloc (AVar shp))
             , SAssign outNnz (RAtom (AInt 0))
             -- Loop-carried scalars cache the last written row/col/val, eliminating
             -- array reads per iteration from the dedup loop.
             , SAssign prevRow (RAtom (AInt 0))
             , SAssign prevCol (RAtom (AInt 0))
             , SAssign prevVal (RAtom (AInt 0))
             , SLoop (LoopSpec [i] [atomToIndexExpr arNnz] Serial Nothing LoopPlain)
                 ( rowIterStmts ++ colIterStmts ++ valIterStmts ++
                 [ SAssign hasPrev (RBinOp CGt (AVar outNnz) (AInt 0))
                 , SIf (AVar hasPrev)
                     [ SAssign sameRow (RBinOp CEq (AVar row) (AVar prevRow))
                     , SAssign sameCol (RBinOp CEq (AVar col) (AVar prevCol))
                     , SAssign sameEntry (RBinOp CAnd (AVar sameRow) (AVar sameCol))
                     , SIf (AVar sameEntry)
                         -- Merge: update prevVal in-register, write back once
                         [ SAssign mergedVal (RBinOp CAdd (AVar prevVal) (AVar val))
                         , SAssign prevVal (RAtom (AVar mergedVal))
                         , SAssign lastIdx (RBinOp CSub (AVar outNnz) (AInt 1))
                         , SArrayWrite (AVar outVals) (AVar lastIdx) (AVar mergedVal)
                         ]
                         [ SArrayWrite (AVar outRows) (AVar outNnz) (AVar row)
                         , SArrayWrite (AVar outCols) (AVar outNnz) (AVar col)
                         , SArrayWrite (AVar outVals) (AVar outNnz) (AVar val)
                         , SAssign prevRow (RAtom (AVar row))
                         , SAssign prevCol (RAtom (AVar col))
                         , SAssign prevVal (RAtom (AVar val))
                         , SAssign outNnz (RBinOp CAdd (AVar outNnz) (AInt 1))
                         ]
                     ]
                     [ SArrayWrite (AVar outRows) (AVar outNnz) (AVar row)
                     , SArrayWrite (AVar outCols) (AVar outNnz) (AVar col)
                     , SArrayWrite (AVar outVals) (AVar outNnz) (AVar val)
                     , SAssign prevRow (RAtom (AVar row))
                     , SAssign prevCol (RAtom (AVar col))
                     , SAssign prevVal (RAtom (AVar val))
                     , SAssign outNnz (RBinOp CAdd (AVar outNnz) (AInt 1))
                     ]
                 ])
             , SAssign result
                 (RRecord
                   [ ("cols", AVar outCols)
                   , ("ncols", arNCols)
                   , ("nnz", AVar outNnz)
                   , ("nrows", arNRows)
                   , ("rows", AVar outRows)
                   , ("vals", AVar outVals)
                   ])
             ]
      , AVar result
      )
  ECSRFromSortedCOO _ nrowsExp ncolsExp nnzExp rowsExp colsExp valsExp -> do
    (srNRows, arNRows) <- lowerExp nrowsExp
    (srNCols, arNCols) <- lowerExp ncolsExp
    (srNnz, arNnz) <- lowerExp nnzExp
    (srRows, arRows) <- lowerExp rowsExp
    (srCols, arCols) <- lowerExp colsExp
    (srVals, arVals) <- lowerExp valsExp
    shp <- freshCVar "csr_shp"
    countsShp <- freshCVar "csr_counts_shp"
    rowPtrShp <- freshCVar "csr_rowptr_shp"
    counts <- freshCVar "csr_counts"
    rowPtr <- freshCVar "csr_row_ptr"
    outCols <- freshCVar "csr_col_idx"
    outVals <- freshCVar "csr_vals"
    nrowsPlus1 <- freshCVar "csr_nrows1"
    i <- freshCVar "csr_i"
    row <- freshCVar "csr_row"
    oldCount <- freshCVar "csr_old_count"
    newCount <- freshCVar "csr_new_count"
    acc <- freshCVar "csr_acc"
    cnt <- freshCVar "csr_cnt"
    col <- freshCVar "csr_col"
    val <- freshCVar "csr_val"
    result <- freshCVar "csr_result"
    let csrTy = CTRecord
          [ ("col_idx", CTArray CTInt64)
          , ("ncols", CTInt64)
          , ("nnz", CTInt64)
          , ("nrows", CTInt64)
          , ("row_ptr", CTArray CTInt64)
          , ("vals", CTArray CTInt64)
          ]
    registerCType counts (CTArray CTInt64)
    registerCType rowPtr (CTArray CTInt64)
    registerCType outCols (CTArray CTInt64)
    registerCType outVals (CTArray CTInt64)
    registerCType acc CTInt64
    registerCType result csrTy
    pure
      ( srNRows ++ srNCols ++ srNnz ++ srRows ++ srCols ++ srVals
          ++ [ SAssign shp (RArrayShape arCols)
             , SAssign countsShp (RTuple [arNRows])
             , SAssign counts (RArrayAlloc (AVar countsShp))
             , SAssign nrowsPlus1 (RBinOp CAdd arNRows (AInt 1))
             , SAssign rowPtrShp (RTuple [AVar nrowsPlus1])
             , SAssign rowPtr (RArrayAlloc (AVar rowPtrShp))
             , SAssign outCols (RArrayAlloc (AVar shp))
             , SAssign outVals (RArrayAlloc (AVar shp))
             , SLoop (LoopSpec [i] [atomToIndexExpr arNRows] Serial Nothing LoopPlain)
                 [ SArrayWrite (AVar counts) (AVar i) (AInt 0)
                 ]
             , SLoop (LoopSpec [i] [atomToIndexExpr arNnz] Serial Nothing LoopPlain)
                 [ SAssign row (RArrayLoad arRows (AVar i))
                 , SAssign oldCount (RArrayLoad (AVar counts) (AVar row))
                 , SAssign newCount (RBinOp CAdd (AVar oldCount) (AInt 1))
                 , SArrayWrite (AVar counts) (AVar row) (AVar newCount)
                 , SAssign col (RArrayLoad arCols (AVar i))
                 , SAssign val (RArrayLoad arVals (AVar i))
                 , SArrayWrite (AVar outCols) (AVar i) (AVar col)
                 , SArrayWrite (AVar outVals) (AVar i) (AVar val)
                 ]
             , SAssign acc (RAtom (AInt 0))
             , SLoop (LoopSpec [i] [atomToIndexExpr arNRows] Serial Nothing LoopPlain)
                 [ SArrayWrite (AVar rowPtr) (AVar i) (AVar acc)
                 , SAssign cnt (RArrayLoad (AVar counts) (AVar i))
                 , SAssign acc (RBinOp CAdd (AVar acc) (AVar cnt))
                 ]
             , SArrayWrite (AVar rowPtr) arNRows (AVar acc)
             , SAssign result
                 (RRecord
                   [ ("col_idx", AVar outCols)
                   , ("ncols", arNCols)
                   , ("nnz", arNnz)
                   , ("nrows", arNRows)
                   , ("row_ptr", AVar rowPtr)
                   , ("vals", AVar outVals)
                   ])
             ]
      , AVar result
      )
  -- Index into a scalar (0-D) reduce_generate: keeps the reduction scalar
  -- in the CFG rather than materialising a temporary 0-D array.
  EIndex _ idxExp arrExp@(EReduceGenerate _ fnExp initExp shapeExp genExp) -> do
    case idxExp of
      EUnit{} -> do
        (si, ai) <- lowerExp initExp
        (ss, as') <- lowerExp shapeExp
        mRank <- shapeRankM shapeExp
        case mRank of
          Just 1 -> do
            -- 1-D reduction into a scalar: preserve the LoopReductionWrapper (so
            -- downstream polyhedral analysis keeps finding the standard structure)
            -- but use the loop counter k directly as the array index, skipping
            -- the flat_to_nd / base / flat_in overhead that would otherwise live
            -- inside the hot inner loop.
            redDim <- freshCVar "red_dim"
            j      <- freshIterVar "j"
            acc    <- freshCVar "acc"
            k      <- freshIterVar "k"
            elem'  <- freshCVar "elem"
            val    <- freshCVar "val"
            genStmts <- inlineArrayFn1D genExp k elem'
            (bodyStmts, mRedop) <- lowerReductionStep fnExp acc elem'
            let mReductionSpec = fmap (ReductionSpec acc (atomToIndexExpr ai)) mRedop
            pure ( si ++ ss
                 ++ [ SAssign redDim (RShapeLast as')
                    , SAssign acc (RAtom ai)
                    , SLoop (LoopSpec [j] [IConst 1] Serial Nothing LoopReductionWrapper)
                        [ SLoop (LoopSpec [k] [atomToIndexExpr (AVar redDim)] Serial mReductionSpec LoopReduction)
                            ( genStmts ++ bodyStmts )
                        ]
                    , SAssign val (RAtom (AVar acc))
                    ]
                 , AVar val
                 )
          _ -> do
            -- General N-D case: materialise the shape, iterate over the outer
            -- dimension with j, compute flat index, convert to nd index.
            redDim <- freshCVar "red_dim"
            outN <- freshCVar "out_n"
            j <- freshIterVar "j"
            acc <- freshCVar "acc"
            k <- freshCVar "k"
            base <- freshCVar "base"
            flatIn <- freshCVar "flat_in"
            idx <- freshCVar "idx"
            elem' <- freshCVar "elem"
            val <- freshCVar "val"
            registerCType idx CTTuple
            noteDenseLinearIndex idx k
            genStmts <- inlineArrayFn genExp idx elem'
            (bodyStmts, mRedop) <- lowerReductionStep fnExp acc elem'
            let mReductionSpec = fmap (ReductionSpec acc (atomToIndexExpr ai)) mRedop
            outShp <- freshCVar "out_shp"
            pure ( si ++ ss
                 ++ [ SAssign outShp (RShapeInit as')
                    , SAssign redDim (RShapeLast as')
                    , SAssign outN (RShapeSize (AVar outShp))
                    , SAssign acc (RAtom ai)
                    , SLoop (LoopSpec [j] [atomToIndexExpr (AVar outN)] Serial Nothing LoopReductionWrapper)
                        ( [ SAssign base (RBinOp CMul (AVar j) (AVar redDim))
                          , SLoop (LoopSpec [k] [atomToIndexExpr (AVar redDim)] Serial mReductionSpec LoopReduction)
                              ( [ SAssign flatIn (RBinOp CAdd (AVar base) (AVar k))
                                , SAssign idx (RFlatToNd (AVar flatIn) as')
                                ]
                                ++ genStmts
                                ++ bodyStmts
                              )
                          ]
                        )
                    , SAssign val (RAtom (AVar acc))
                    ]
                 , AVar val
                 )
      _ -> do
        (si, ai) <- lowerExp idxExp
        (sa, aa) <- lowerExp arrExp
        shp <- freshCVar "shp"
        off <- freshCVar "off"
        val <- freshCVar "val"
        pure ( si ++ sa
             ++ [ SAssign shp (RArrayShape aa)
                , SAssign off (RNdToFlat ai (AVar shp))
                , SAssign val (RArrayLoad aa (AVar off))
                ]
             , AVar val
             )

  EIndex _ idxExp arrExp -> do
    val <- freshCVar "val"
    case idxExp of
      EVec _ [idx1Exp] -> do
        (si, ai) <- lowerExp idx1Exp
        (sa, aa) <- lowerExp arrExp
        whenM (atomHasDenseLinearOrigin ai) (noteDenseReadAtom aa)
        pure (si ++ sa ++ [SAssign val (RArrayLoad aa ai)], AVar val)
      _ -> do
        (si, ai) <- lowerExp idxExp
        (sa, aa) <- lowerExp arrExp
        aiTy <- ctypeOfAtom ai
        -- Fast path: if the index is already a flat int64 (e.g. because we
        -- are inside the rank-1 reduction loop and the parameter was bound as
        -- the loop iteration variable), skip the nd_to_flat round-trip.
        if aiTy == CTInt64
          then do
            whenM (atomHasDenseLinearOrigin ai) (noteDenseReadAtom aa)
            pure (si ++ sa ++ [SAssign val (RArrayLoad aa ai)], AVar val)
          else do
            shp <- freshCVar "shp"
            off <- freshCVar "off"
            propagateDenseLinearIndex off ai
            whenM (atomHasDenseLinearOrigin ai) (noteDenseReadAtom aa)
            pure ( si ++ sa
                 ++ [ SAssign shp (RArrayShape aa)
                    , SAssign off (RNdToFlat ai (AVar shp))
                    , SAssign val (RArrayLoad aa (AVar off))
                    ]
                 , AVar val
                 )

  ECheckIndex _ idxExp defExp arrExp -> do
    (si, ai) <- lowerExp idxExp
    (sd, ad) <- lowerExp defExp
    (sa, aa) <- lowerExp arrExp
    shp <- freshCVar "shp"
    off <- freshCVar "off"
    n <- freshCVar "n"
    inBounds <- freshCVar "ib"
    val <- freshCVar "val"
    loaded <- freshCVar "ld"
    propagateDenseLinearIndex off ai
    whenM (atomHasDenseLinearOrigin ai) (noteDenseReadAtom aa)
    pure ( si ++ sd ++ sa
         ++ [ SAssign shp (RArrayShape aa)
            , SAssign off (RNdToFlat ai (AVar shp))
            , SAssign n (RShapeSize (AVar shp))
            , SAssign inBounds (RBinOp CLt (AVar off) (AVar n))
            , SIf (AVar inBounds)
                [SAssign loaded (RArrayLoad aa (AVar off)), SAssign val (RAtom (AVar loaded))]
                [SAssign val (RAtom ad)]
            ]
         , AVar val
         )

  EShapeOf _ arrExp -> do
    (sa, aa) <- lowerExp arrExp
    t <- freshCVar "t"
    pure (sa ++ [SAssign t (RArrayShape aa)], AVar t)

  ESlice _ dims arrExp -> do
    (sa, aa) <- lowerExp arrExp
    outArr <- freshCVar "outArr"
    outShp <- freshCVar "outShp"
    outN <- freshCVar "outN"
    i <- freshIterVar "i"
    outIdx <- freshCVar "outIdx"
    srcTuple <- freshCVar "srcTuple"
    flatSrc <- freshCVar "flatSrc"
    val <- freshCVar "val"
    let dimCount = length dims
    dimVars <- if dimCount == 0
                 then pure []
                 else mapM (\d -> freshCVar ("dim" <> BS.pack (show (d :: Int)))) [0 .. dimCount - 1]
    let dimStmts = concat $ zipWith (\d v -> [SAssign v (RProj d (AVar srcTuple))]) [0..] dimVars
    pure ( sa
         ++ [ SAssign outShp (RArrayShape aa)
            , SAssign outN (RShapeSize (AVar outShp))
            , SAssign outArr (RArrayAlloc (AVar outShp))
            , SAssign outN (RShapeSize (AVar outShp))
            , SLoop (LoopSpec [i] [atomToIndexExpr (AVar outN)] Serial Nothing LoopPlain)
                ( [SAssign outIdx (RNdToFlat (AVar i) (AVar outShp))]
                  ++ dimStmts
                  ++ [ SAssign srcTuple (RTuple (map AVar dimVars))
                     , SAssign flatSrc (RNdToFlat (AVar srcTuple) aa)
                     , SAssign val (RArrayLoad aa (AVar flatSrc))
                     , SArrayWrite (AVar outArr) (AVar i) (AVar val)
                     ]
                )
            ]
         , AVar outArr
         )

  EReshape _ newShapeExp arrExp -> do
    (ss, as') <- lowerExp newShapeExp
    (sa, aa) <- lowerExp arrExp
    arr <- freshCVar "arr"
    arrTy <- ctypeOfAtom aa
    registerCType arr arrTy
    noteReadOnlyAtom aa
    pure ( ss ++ sa
         ++ [SAssign arr (RCall "hyd_array_reshape_view" [aa, as'])]
         , AVar arr
         )

  EReplicate _ dims arrExp -> do
    (sa, aa) <- lowerExp arrExp
    shpSrc <- freshCVar "shp"
    outShp <- freshCVar "oshp"
    arr <- freshCVar "arr"
    n <- freshCVar "n"
    i <- freshIterVar "i"
    outIdx <- freshCVar "oidx"
    flatSrc <- freshCVar "fs"
    val <- freshCVar "val"
    let srcPositions = [j | (j, d) <- zip [0 :: Integer ..] dims, isShapeAllDim d]
    projStmts <- mapM (\(_, j) -> do
        p <- freshCVar "p"
        pure (p, [SAssign p (RProj (fromInteger j) (AVar outIdx))])) (zip [0 :: Integer ..] srcPositions)
    let projVars = map fst projStmts
        projAssigns = concatMap snd projStmts
    srcTuple <- freshCVar "st"
    pure ( sa
         ++ [ SAssign shpSrc (RArrayShape aa)
            , SAssign outShp (RCall "__replicate_shape" [AVar shpSrc])
            , SAssign arr (RArrayAlloc (AVar outShp))
            , SAssign n (RShapeSize (AVar outShp))
            , SLoop (LoopSpec [i] [atomToIndexExpr (AVar n)] Serial Nothing LoopPlain)
                ( projAssigns
                  ++ [ SAssign srcTuple (RTuple (map AVar projVars))
                     , SAssign flatSrc (RNdToFlat (AVar srcTuple) aa)
                     , SAssign val (RArrayLoad aa (AVar flatSrc))
                     , SArrayWrite (AVar arr) (AVar i) (AVar val)
                     ]
                )
            ]
         , AVar arr
         )

  EPermute _ _combExp defaultsExp _permFnExp srcExp -> do
    (sd, ad) <- lowerExp defaultsExp
    (ss, as') <- lowerExp srcExp
    shpSrc <- freshCVar "shp"
    shpDst <- freshCVar "shp"
    out <- freshCVar "out"
    nDst <- freshCVar "n"
    nSrc <- freshCVar "n"
    j <- freshIterVar "j"
    i <- freshIterVar "i"
    elemD <- freshCVar "elem"
    idx <- freshCVar "idx"
    permIdx <- freshCVar "pidx"
    _flatPerm <- freshCVar "fp"
    srcElem <- freshCVar "se"
    registerCType idx CTTuple
    noteDenseLinearIndex idx i
    pure ( sd ++ ss
         ++ [ SAssign shpDst (RArrayShape ad)
            , SAssign out (RArrayAlloc (AVar shpDst))
            , SAssign nDst (RShapeSize (AVar shpDst))
            , SLoop (LoopSpec [j] [atomToIndexExpr (AVar nDst)] Serial Nothing LoopPlain)
                [SAssign elemD (RArrayLoad ad (AVar j)), SArrayWrite (AVar out) (AVar j) (AVar elemD)]
            , SAssign shpSrc (RArrayShape as')
            , SAssign nSrc (RShapeSize (AVar shpSrc))
            , SLoop (LoopSpec [i] [atomToIndexExpr (AVar nSrc)] Serial Nothing LoopPlain)
                ( [ SAssign srcElem (RArrayLoad as' (AVar i))
                  , SAssign idx (RFlatToNd (AVar i) (AVar shpSrc))
                  , SArrayWrite (AVar out) (AVar permIdx) (AVar srcElem)
                  ]
                )
            ]
         , AVar out
         )

  EScatter _ combExp defaultsExp idxArrExp valsExp -> do
    (sd, ad) <- lowerExp defaultsExp
    (si, ai) <- lowerExp idxArrExp
    (sv, av) <- lowerExp valsExp
    shp <- freshCVar "shp"
    n <- freshCVar "n"
    i <- freshIterVar "i"
    val <- freshCVar "val"
    oldVal <- freshCVar "old"
    newVal <- freshCVar "new"
    idx <- freshCVar "idx"
    combStmts <- inlineBinaryFn combExp val oldVal newVal
    pure ( sd ++ si ++ sv
         ++ [ SAssign shp (RArrayShape ai)
            , SAssign n (RShapeSize (AVar shp))
            , SLoop (LoopSpec [i] [atomToIndexExpr (AVar n)] Serial Nothing LoopPlain)
                ( [ SAssign idx (RArrayLoad ai (AVar i))
                  , SAssign val (RArrayLoad av (AVar i))
                  , SAssign oldVal (RArrayLoad ad (AVar idx))
                  ] ++ combStmts ++
                  [ SArrayWrite ad (AVar idx) (AVar newVal)
                  ]
                )
            ]
         , ad
         )

  EScatterGuarded _ combExp defaultsExp idxArrExp valsExp guardExp -> do
    case (idxArrExp, valsExp, guardExp) of
      (EGenerate _ shpExp routeFn, EGenerate _ _ valFnExp, EGenerate _ _ guardFnExp) -> do
        (sd, ad) <- lowerExp defaultsExp
        (sshp, ashp) <- lowerExp shpExp
        n <- freshCVar "n"
        i <- freshIterVar "i"
        guardVal <- freshCVar "guard"
        val <- freshCVar "val"
        oldVal <- freshCVar "old"
        newVal <- freshCVar "new"
        idx <- freshCVar "idx"
        combStmts <- inlineBinaryFn combExp val oldVal newVal
        if is1DShapeExp shpExp
          then do
            routeStmts <- inlineArrayFn1D routeFn i idx
            valStmts <- inlineArrayFn1D valFnExp i val
            guardStmts <- inlineArrayFn1D guardFnExp i guardVal
            (hoistedCalls, kernelStmts) <-
              hoistZeroArgValueProcCalls (routeStmts ++ valStmts ++ guardStmts)
            pure ( sd ++ sshp
                 ++ hoistedCalls
                 ++ [ SAssign n (RShapeSize ashp)
                    , SLoop (LoopSpec [i] [atomToIndexExpr (AVar n)] Serial Nothing LoopPlain)
                        ( kernelStmts ++
                          [ SIf (AVar guardVal)
                              ( [ SAssign oldVal (RArrayLoad ad (AVar idx))
                                ] ++ combStmts ++
                                [ SArrayWrite ad (AVar idx) (AVar newVal)
                                ]
                              )
                              []
                          ]
                        )
                    ]
                 , ad
                 )
          else do
            ndIdx <- freshCVar "nd"
            registerCType ndIdx CTTuple
            noteDenseLinearIndex ndIdx i
            routeStmts <- inlineArrayFn routeFn ndIdx idx
            valStmts <- inlineArrayFn valFnExp ndIdx val
            guardStmts <- inlineArrayFn guardFnExp ndIdx guardVal
            (hoistedCalls, kernelStmts) <-
              hoistZeroArgValueProcCalls (routeStmts ++ valStmts ++ guardStmts)
            pure ( sd ++ sshp
                 ++ hoistedCalls
                 ++ [ SAssign n (RShapeSize ashp)
                    , SLoop (LoopSpec [i] [atomToIndexExpr (AVar n)] Serial Nothing LoopPlain)
                        ( [ SAssign ndIdx (RFlatToNd (AVar i) ashp)
                          ] ++ kernelStmts ++
                          [ SIf (AVar guardVal)
                              ( [ SAssign oldVal (RArrayLoad ad (AVar idx))
                                ] ++ combStmts ++
                                [ SArrayWrite ad (AVar idx) (AVar newVal)
                                ]
                              )
                              []
                          ]
                        )
                    ]
                 , ad
                 )
      _ -> do
        (sd, ad) <- lowerExp defaultsExp
        (si, ai) <- lowerExp idxArrExp
        (sv, av) <- lowerExp valsExp
        (sg, ag) <- lowerExp guardExp
        shp <- freshCVar "shp"
        n <- freshCVar "n"
        i <- freshIterVar "i"
        guardVal <- freshCVar "guard"
        val <- freshCVar "val"
        oldVal <- freshCVar "old"
        newVal <- freshCVar "new"
        idx <- freshCVar "idx"
        combStmts <- inlineBinaryFn combExp val oldVal newVal
        pure ( sd ++ si ++ sv ++ sg
             ++ [ SAssign shp (RArrayShape ai)
                , SAssign n (RShapeSize (AVar shp))
                , SLoop (LoopSpec [i] [atomToIndexExpr (AVar n)] Serial Nothing LoopPlain)
                    ( [ SAssign guardVal (RArrayLoad ag (AVar i)) ]
                   ++ [ SIf (AVar guardVal)
                        ( [ SAssign idx (RArrayLoad ai (AVar i))
                          , SAssign val (RArrayLoad av (AVar i))
                          , SAssign oldVal (RArrayLoad ad (AVar idx))
                          ]
                          ++ combStmts
                          ++ [ SArrayWrite ad (AVar idx) (AVar newVal) ]
                        )
                        []
                      ]
                    )
                ]
             , ad
             )

  EScatterGenerate _ combExp defaultsExp idxArrExp valFnExp -> do
    case idxArrExp of
      EGenerate _ shpExp routeFn -> do
        (sd, ad) <- lowerExp defaultsExp
        (sshp, ashp) <- lowerExp shpExp
        n <- freshCVar "n"
        i <- freshIterVar "i"
        val <- freshCVar "val"
        oldVal <- freshCVar "old"
        newVal <- freshCVar "new"
        idx <- freshCVar "idx"
        combStmts <- inlineBinaryFn combExp val oldVal newVal
        if is1DShapeExp shpExp
          then do
            routeStmts <- inlineArrayFn1D routeFn i idx
            valStmts <- inlineArrayFn1D valFnExp i val
            (hoistedCalls, kernelStmts) <- hoistZeroArgValueProcCalls (routeStmts ++ valStmts)
            pure ( sd ++ sshp
                 ++ hoistedCalls
                 ++ [ SAssign n (RShapeSize ashp)
                    , SLoop (LoopSpec [i] [atomToIndexExpr (AVar n)] Serial Nothing LoopPlain)
                        ( kernelStmts ++
                          [ SAssign oldVal (RArrayLoad ad (AVar idx))
                          ] ++ combStmts ++
                          [ SArrayWrite ad (AVar idx) (AVar newVal)
                          ]
                        )
                    ]
                 , ad
                 )
          else do
            ndIdx <- freshCVar "nd"
            registerCType ndIdx CTTuple
            noteDenseLinearIndex ndIdx i
            routeStmts <- inlineArrayFn routeFn ndIdx idx
            valStmts <- inlineArrayFn valFnExp ndIdx val
            (hoistedCalls, kernelStmts) <- hoistZeroArgValueProcCalls (routeStmts ++ valStmts)
            pure ( sd ++ sshp
                 ++ hoistedCalls
                 ++ [ SAssign n (RShapeSize ashp)
                    , SLoop (LoopSpec [i] [atomToIndexExpr (AVar n)] Serial Nothing LoopPlain)
                        ( [ SAssign ndIdx (RFlatToNd (AVar i) ashp)
                          ] ++ kernelStmts ++
                          [ SAssign oldVal (RArrayLoad ad (AVar idx))
                          ] ++ combStmts ++
                          [ SArrayWrite ad (AVar idx) (AVar newVal)
                          ]
                        )
                    ]
                 , ad
                 )
      EMap _ routeFn srcArrExp -> do
        (sd, ad) <- lowerExp defaultsExp
        (ssrc, asrc) <- lowerExp srcArrExp
        shp <- freshCVar "shp"
        n <- freshCVar "n"
        i <- freshIterVar "i"
        ndIdx <- freshCVar "nd"
        registerCType ndIdx CTTuple
        noteDenseLinearIndex ndIdx i
        elem' <- freshCVar "elem"
        val <- freshCVar "val"
        oldVal <- freshCVar "old"
        newVal <- freshCVar "new"
        idx <- freshCVar "idx"
        routeStmts <- inlineScalarFn routeFn elem' idx
        valStmts <- inlineArrayFn valFnExp ndIdx val
        (hoistedCalls, kernelStmts) <- hoistZeroArgValueProcCalls (routeStmts ++ valStmts)
        combStmts <- inlineBinaryFn combExp val oldVal newVal
        noteReadOnlyAtom asrc
        noteDenseReadAtom asrc
        pure ( sd ++ ssrc
             ++ hoistedCalls
             ++ [ SAssign shp (RArrayShape asrc)
                , SAssign n (RShapeSize (AVar shp))
                , SLoop (LoopSpec [i] [atomToIndexExpr (AVar n)] Serial Nothing LoopPlain)
                    ( [ SAssign ndIdx (RFlatToNd (AVar i) (AVar shp))
                      , SAssign elem' (RArrayLoad asrc (AVar i))
                      ] ++ kernelStmts ++
                      [ SAssign oldVal (RArrayLoad ad (AVar idx))
                      ] ++ combStmts ++
                      [ SArrayWrite ad (AVar idx) (AVar newVal)
                      ]
                    )
                ]
             , ad
             )
      _ -> do
        (sd, ad) <- lowerExp defaultsExp
        (si, ai) <- lowerExp idxArrExp
        shp <- freshCVar "shp"
        n <- freshCVar "n"
        i <- freshIterVar "i"
        ndIdx <- freshCVar "nd"
        registerCType ndIdx CTTuple
        noteDenseLinearIndex ndIdx i
        val <- freshCVar "val"
        oldVal <- freshCVar "old"
        newVal <- freshCVar "new"
        idx <- freshCVar "idx"
        valStmts <- inlineArrayFn valFnExp ndIdx val
        (hoistedCalls, kernelStmts) <- hoistZeroArgValueProcCalls valStmts
        combStmts <- inlineBinaryFn combExp val oldVal newVal
        pure ( sd ++ si
             ++ hoistedCalls
             ++ [ SAssign shp (RArrayShape ai)
                , SAssign n (RShapeSize (AVar shp))
                , SLoop (LoopSpec [i] [atomToIndexExpr (AVar n)] Serial Nothing LoopPlain)
                    ( [ SAssign ndIdx (RFlatToNd (AVar i) (AVar shp))
                      , SAssign idx (RArrayLoad ai (AVar i))
                      ] ++ kernelStmts ++
                      [ SAssign oldVal (RArrayLoad ad (AVar idx))
                      ] ++ combStmts ++
                      [ SArrayWrite ad (AVar idx) (AVar newVal)
                      ]
                    )
                ]
             , ad
             )

  EGather _ idxArrExp srcArrExp -> do
    (sidx, aidx) <- lowerExp idxArrExp
    (ssrc, asrc) <- lowerExp srcArrExp
    idxTy <- ctypeOfAtom aidx
    idxShp <- freshCVar "shp"
    srcShp <- freshCVar "srcShp"
    arr    <- freshCVar "arr"
    n      <- freshCVar "n"
    i      <- freshIterVar "i"
    ndIdx  <- freshCVar "ndIdx"
    flat   <- freshCVar "flat"
    val    <- freshCVar "val"
    noteReadOnlyAtom aidx
    noteReadOnlyAtom asrc
    markArrayFreshWriteOnce arr
    noteDenseReadAtom aidx
    noteIndirectReadAtom asrc
    markContiguousWriteArray arr
    let scalarRoutes =
          case idxTy of
            CTArray CTInt64 -> True
            _ -> False
    pure ( sidx ++ ssrc
         ++ [ SAssign idxShp (RArrayShape aidx)
            , SAssign srcShp (RArrayShape asrc)
            , SAssign arr    (RArrayAlloc (AVar idxShp))
            , SAssign n      (RShapeSize (AVar idxShp))
            , SLoop (LoopSpec [i] [atomToIndexExpr (AVar n)] Serial Nothing LoopMap)
                ( [ SAssign ndIdx (RArrayLoad aidx (AVar i)) ]
                  ++ (if scalarRoutes
                        then [SAssign flat (RAtom (AVar ndIdx))]
                        else [SAssign flat (RNdToFlat (AVar ndIdx) (AVar srcShp))])
                  ++ [ SAssign val (RArrayLoad asrc (AVar flat))
                     , SArrayWrite (AVar arr) (AVar i) (AVar val)
                     ]
                )
            ]
         , AVar arr
         )

  EReadArray _ shapeExp fileExp -> do
    (ss, as') <- lowerExp shapeExp
    (sf, af) <- lowerExp fileExp
    t <- freshCVar "t"
    pure (ss ++ sf ++ [SAssign t (RCall "hyd_read_array_csv" [af, as'])], AVar t)

  EReadArrayFloat _ shapeExp fileExp -> do
    (ss, as') <- lowerExp shapeExp
    (sf, af) <- lowerExp fileExp
    t <- freshCVar "t"
    pure (ss ++ sf ++ [SAssign t (RCall "hyd_read_float_array_csv" [af, as'])], AVar t)

  EWriteArray _ arrExp pathExp -> do
    (sa, aa) <- lowerExp arrExp
    (sp, ap) <- lowerExp pathExp
    pure (sa ++ sp ++ [SAssign (BS.pack "_discarded") (RCall "hyd_write_array_csv" [aa, ap])], AUnit)

  EWriteArrayFloat _ arrExp pathExp -> do
    (sa, aa) <- lowerExp arrExp
    (sp, ap) <- lowerExp pathExp
    pure (sa ++ sp ++ [SAssign (BS.pack "_discarded") (RCall "hyd_write_array_csv_float" [aa, ap])], AUnit)

  EGetEnvInt _ v -> do
    (sv, av) <- lowerExp v
    t <- freshCVar "t"
    pure (sv ++ [SAssign t (RCall "getenv_int" [av])], AVar t)

  EGetEnvString _ v -> do
    (sv, av) <- lowerExp v
    t <- freshCVar "t"
    pure (sv ++ [SAssign t (RCall "getenv_string" [av])], AVar t)

  EStencil _ bnd fnExp arrExp -> do
    (sa, aa) <- lowerExp arrExp
    (sDef, maDef) <- case bnd of
      BConst defExp -> do (s, a) <- lowerExp defExp; pure (s, Just a)
      _             -> pure ([], Nothing)
    mLambda <- extractStencilFn fnExp
    let (accVar, bodyExp) = case mLambda of
          Just r  -> r
          Nothing -> error "EStencil: expected a single-parameter lambda function"
    let rank = detectStencilRank accVar bodyExp
    (bodyExp', accCalls) <- substituteAccCalls accVar rank bodyExp
    shp    <- freshCVar "shp"
    outArr <- freshCVar "arr"
    n      <- freshCVar "n"
    i      <- freshIterVar "i"
    (preLoopStmts, idxVars, dimVars) <- case rank of
      2 -> do
        n0   <- freshCVar "n0"
        n1   <- freshCVar "n1"
        i0   <- freshCVar "i0"
        i1   <- freshCVar "i1"
        itmp <- freshCVar "itmp"
        let stmts = [ SAssign n0   (RProj 0 (AVar shp))
                    , SAssign n1   (RProj 1 (AVar shp))
                    , SAssign i0   (RBinOp CDiv (AVar i) (AVar n1))
                    , SAssign itmp (RBinOp CMul (AVar i0) (AVar n1))
                    , SAssign i1   (RBinOp CSub (AVar i) (AVar itmp))
                    ]
        pure (stmts, [i0, i1], [n0, n1])
      _ -> pure ([], [i], [n])
    bndStmtss <- mapM
      (genBndStmts aa (map AVar idxVars) (map AVar dimVars) bnd maDef)
      accCalls
    (sb, ab) <- lowerExp bodyExp'
    let loopBody = preLoopStmts
                ++ concat bndStmtss
                ++ sb
                ++ [SArrayWrite (AVar outArr) (AVar i) ab]
    pure ( sa ++ sDef
        ++ [ SAssign shp    (RArrayShape aa)
           , SAssign outArr (RArrayAlloc (AVar shp))
           , SAssign n      (RShapeSize (AVar shp))
           , SLoop (LoopSpec [i] [atomToIndexExpr (AVar n)] Serial Nothing LoopPlain) loopBody
           ]
         , AVar outArr
         )

-- | Extract the accessor variable name and body from a stencil function expression.
-- Handles both inline lambdas and named functions registered in the environment.
extractStencilFn :: Exp Range -> LowerM (Maybe (Var, Exp Range))
extractStencilFn (ELetIn _ (Dec _ _name [PVar _ accVar] _ _ body) _) =
  pure $ Just (accVar, body)
extractStencilFn (EVar _ name) = do
  mFn <- lookupFn name
  case mFn of
    Just ([PVar _ accVar], body) -> pure $ Just (accVar, body)
    _                            -> pure Nothing
extractStencilFn _ = pure Nothing

-- | Detect stencil rank (1 or 2) by scanning body for accessor call patterns.
detectStencilRank :: Var -> Exp Range -> Int
detectStencilRank accVar body = fromMaybe 1 (go body)
  where
    go (EApp _ (EApp _ (EVar _ v) _) _) | v == accVar = Just 2
    go (EApp _ (EVar _ v) _)             | v == accVar = Just 1
    go (EApp _ f x)                   = go f <|> go x
    go (EBinOp _ l _ r)               = go l <|> go r
    go (ENeg _ e)                      = go e
    go (EUnOp _ _ e)                   = go e
    go (ELetIn _ (Dec _ _ _ _ _ b) e)    = go b <|> go e
    go (EIfThenElse _ c t el)          = go c <|> go t <|> go el
    go (EIfThen _ c t)                 = go c <|> go t
    go (EPair _ e1 e2)                 = go e1 <|> go e2
    go (ERecord _ fields)              = foldr (<|>) Nothing (map (go . snd) fields)
    go (ERecordProj _ e _)             = go e
    go (EProj _ _ e)                   = go e
    go _                               = Nothing

-- | Substitute all accessor call sites in the body with fresh CVars.
-- Returns the transformed body and a list of (offsetExprs, resultVar) pairs.
substituteAccCalls :: Var -> Int -> Exp Range
                   -> LowerM (Exp Range, [([Exp Range], CVar)])
substituteAccCalls accVar rank = go
  where
    go expr = case expr of
      EApp r (EApp _ (EVar _ v) d0) d1 | v == accVar && rank == 2 -> do
        rv <- freshCVar "sten"
        pure (EVar r rv, [([d0, d1], rv)])
      EApp r (EVar _ v) d | v == accVar && rank == 1 -> do
        rv <- freshCVar "sten"
        pure (EVar r rv, [([d], rv)])
      EApp r f x -> do
        (f', ps1) <- go f
        (x', ps2) <- go x
        pure (EApp r f' x', ps1 ++ ps2)
      EBinOp r e1 op e2 -> do
        (e1', ps1) <- go e1
        (e2', ps2) <- go e2
        pure (EBinOp r e1' op e2', ps1 ++ ps2)
      ENeg r e -> do (e', ps) <- go e; pure (ENeg r e', ps)
      EUnOp r op e -> do (e', ps) <- go e; pure (EUnOp r op e', ps)
      ELetIn r (Dec da dn dp dw dt db) e -> do
        (db', ps1) <- go db
        (e',  ps2) <- go e
        pure (ELetIn r (Dec da dn dp dw dt db') e', ps1 ++ ps2)
      EIfThenElse r c t el -> do
        (c', ps1) <- go c; (t', ps2) <- go t; (el', ps3) <- go el
        pure (EIfThenElse r c' t' el', ps1 ++ ps2 ++ ps3)
      EIfThen r c t -> do
        (c', ps1) <- go c; (t', ps2) <- go t
        pure (EIfThen r c' t', ps1 ++ ps2)
      EPair r e1 e2 -> do
        (e1', ps1) <- go e1; (e2', ps2) <- go e2
        pure (EPair r e1' e2', ps1 ++ ps2)
      ERecord r fields -> do
        fields' <- mapM
          (\(field, fieldExp) -> do
            (fieldExp', ps) <- go fieldExp
            pure ((field, fieldExp'), ps))
          fields
        pure (ERecord r (map fst fields'), concatMap snd fields')
      ERecordProj r e field -> do
        (e', ps) <- go e
        pure (ERecordProj r e' field, ps)
      EProj r ix e -> do (e', ps) <- go e; pure (EProj r ix e', ps)
      _ -> pure (expr, [])

-- | Generate boundary-conditioned load statements for one accessor call site.
genBndStmts :: Atom              -- source array
            -> [Atom]            -- current ND index atoms
            -> [Atom]            -- dimension size atoms
            -> BoundaryCondition Range
            -> Maybe Atom        -- default value atom for BConst
            -> ([Exp Range], CVar)
            -> LowerM [Stmt]
genBndStmts aa idxAtoms dimAtoms bnd maDef (offsetExprs, resultVar) = do
  offsetPairs <- mapM lowerExp offsetExprs
  let offsetStmtss = map fst offsetPairs
      offsetAtoms  = map snd offsetPairs
  bndStmts <- case (idxAtoms, dimAtoms, offsetAtoms) of
    ([iAtom], [nAtom], [dAtom]) ->
      genBnd1D aa iAtom nAtom dAtom bnd maDef resultVar
    ([i0, i1], [n0, n1], [d0, d1]) ->
      genBnd2D aa i0 i1 n0 n1 d0 d1 bnd maDef resultVar
    _ -> error "genBndStmts: unsupported stencil rank"
  pure (concat offsetStmtss ++ bndStmts)

-- | Compute the bounded 1D index into a fresh variable (no array load).
-- For BConst, stores the raw (unchecked) index — caller handles OOB.
genBnd1DVar :: Atom -> Atom -> Atom -> BoundaryCondition Range
            -> LowerM (CVar, [Stmt])
genBnd1DVar nAtom iAtom dAtom bnd = do
  raw <- freshCVar "raw"
  bi  <- freshCVar "bi"
  stmts <- case bnd of
    BClamp -> do
      lt0 <- freshCVar "lt0"
      geN <- freshCVar "geN"
      nm1 <- freshCVar "nm1"
      pure [ SAssign raw (RBinOp CAdd iAtom dAtom)
           , SAssign bi  (RAtom (AVar raw))
           , SAssign lt0 (RBinOp CLt (AVar raw) (AInt 0))
           , SIf (AVar lt0) [SAssign bi (RAtom (AInt 0))] []
           , SAssign nm1 (RBinOp CSub nAtom (AInt 1))
           , SAssign geN (RBinOp CGe (AVar bi) nAtom)
           , SIf (AVar geN) [SAssign bi (RAtom (AVar nm1))] []
           ]
    BWrap -> do
      q   <- freshCVar "q"
      tmp <- freshCVar "tmp"
      lt0 <- freshCVar "lt0"
      pure [ SAssign raw (RBinOp CAdd iAtom dAtom)
           , SAssign q   (RBinOp CDiv (AVar raw) nAtom)
           , SAssign tmp (RBinOp CMul (AVar q) nAtom)
           , SAssign bi  (RBinOp CSub (AVar raw) (AVar tmp))
           , SAssign lt0 (RBinOp CLt (AVar bi) (AInt 0))
           , SIf (AVar lt0) [SAssign bi (RBinOp CAdd (AVar bi) nAtom)] []
           ]
    BMirror -> do
      nm1    <- freshCVar "nm1"
      period <- freshCVar "period"
      q      <- freshCVar "q"
      tmp    <- freshCVar "tmp"
      r      <- freshCVar "r"
      lt0    <- freshCVar "lt0"
      geN    <- freshCVar "geN"
      pure [ SAssign raw    (RBinOp CAdd iAtom dAtom)
           , SAssign nm1    (RBinOp CSub nAtom (AInt 1))
           , SAssign period (RBinOp CMul (AInt 2) (AVar nm1))
           , SAssign q      (RBinOp CDiv (AVar raw) (AVar period))
           , SAssign tmp    (RBinOp CMul (AVar q) (AVar period))
           , SAssign r      (RBinOp CSub (AVar raw) (AVar tmp))
           , SAssign lt0    (RBinOp CLt (AVar r) (AInt 0))
           , SIf (AVar lt0) [SAssign r (RBinOp CAdd (AVar r) (AVar period))] []
           , SAssign geN    (RBinOp CGe (AVar r) nAtom)
           , SIf (AVar geN) [SAssign r (RBinOp CSub (AVar period) (AVar r))] []
           , SAssign bi (RAtom (AVar r))
           ]
    BConst _ ->
      -- Raw (unchecked) index; OOB check happens at flat-index level in genBnd2D.
      pure [ SAssign raw (RBinOp CAdd iAtom dAtom)
           , SAssign bi  (RAtom (AVar raw))
           ]
  pure (bi, stmts)

-- | Generate 1D boundary arithmetic and array load into resultVar.
genBnd1D :: Atom -> Atom -> Atom -> Atom
         -> BoundaryCondition Range -> Maybe Atom -> CVar -> LowerM [Stmt]
genBnd1D aa iAtom nAtom dAtom bnd maDef resultVar = case bnd of
  BConst _ -> do
    raw <- freshCVar "raw"
    ge0 <- freshCVar "ge0"
    ltN <- freshCVar "ltN"
    ib  <- freshCVar "ib"
    let defAtom = fromMaybe (AInt 0) maDef
    pure [ SAssign raw (RBinOp CAdd iAtom dAtom)
         , SAssign ge0 (RBinOp CGe (AVar raw) (AInt 0))
         , SAssign ltN (RBinOp CLt (AVar raw) nAtom)
         , SAssign ib  (RBinOp CAnd (AVar ge0) (AVar ltN))
         , SIf (AVar ib)
             [SAssign resultVar (RArrayLoad aa (AVar raw))]
             [SAssign resultVar (RAtom defAtom)]
         ]
  _ -> do
    (bi, bndStmts) <- genBnd1DVar nAtom iAtom dAtom bnd
    pure $ bndStmts ++ [SAssign resultVar (RArrayLoad aa (AVar bi))]

-- | Generate 2D boundary arithmetic and array load into resultVar.
-- Dimensions are bounded independently (except BConst which checks both).
genBnd2D :: Atom -> Atom -> Atom -> Atom -> Atom -> Atom -> Atom
         -> BoundaryCondition Range -> Maybe Atom -> CVar -> LowerM [Stmt]
genBnd2D aa i0 i1 n0 n1 d0 d1 bnd maDef resultVar = case bnd of
  BConst _ -> do
    raw0 <- freshCVar "raw0"
    raw1 <- freshCVar "raw1"
    ge0a <- freshCVar "ge0a"; ltNa <- freshCVar "ltNa"
    ge0b <- freshCVar "ge0b"; ltNb <- freshCVar "ltNb"
    ib0  <- freshCVar "ib0";  ib1  <- freshCVar "ib1"
    ib   <- freshCVar "ib"
    n1r0 <- freshCVar "n1r0"; flatBi <- freshCVar "fbi"
    let defAtom = fromMaybe (AInt 0) maDef
    pure [ SAssign raw0  (RBinOp CAdd i0 d0)
         , SAssign raw1  (RBinOp CAdd i1 d1)
         , SAssign ge0a  (RBinOp CGe (AVar raw0) (AInt 0))
         , SAssign ltNa  (RBinOp CLt (AVar raw0) n0)
         , SAssign ge0b  (RBinOp CGe (AVar raw1) (AInt 0))
         , SAssign ltNb  (RBinOp CLt (AVar raw1) n1)
         , SAssign ib0   (RBinOp CAnd (AVar ge0a) (AVar ltNa))
         , SAssign ib1   (RBinOp CAnd (AVar ge0b) (AVar ltNb))
         , SAssign ib    (RBinOp CAnd (AVar ib0)  (AVar ib1))
         , SAssign n1r0  (RBinOp CMul (AVar raw0) n1)
         , SAssign flatBi (RBinOp CAdd (AVar n1r0) (AVar raw1))
         , SIf (AVar ib)
             [SAssign resultVar (RArrayLoad aa (AVar flatBi))]
             [SAssign resultVar (RAtom defAtom)]
         ]
  _ -> do
    (bi0, stmts0) <- genBnd1DVar n0 i0 d0 bnd
    (bi1, stmts1) <- genBnd1DVar n1 i1 d1 bnd
    n1bi0  <- freshCVar "n1bi0"
    flatBi <- freshCVar "fbi"
    pure ( stmts0 ++ stmts1
        ++ [ SAssign n1bi0  (RBinOp CMul (AVar bi0) n1)
           , SAssign flatBi (RBinOp CAdd (AVar n1bi0) (AVar bi1))
           , SAssign resultVar (RArrayLoad aa (AVar flatBi))
           ]
         )

handleApp :: Exp Range -> Exp Range -> LowerM ([Stmt], Atom)
handleApp fn arg = case fn of
  EVar _ fName -> do
    mFn <- lookupFn fName
    case mFn of
      Just (pats, body) -> do
        (sa, aa) <- lowerExp arg
        case pats of
          (pat1:restPats) -> do
            -- Use bindAppliedPat (not bindPatAtom) so that a PVec [p]
            -- pattern applied to a CTInt64 atom correctly wraps the scalar
            -- in a 1-element tuple before projecting.  This matches how
            -- multi-arg application sites (lines ~2046, ~2066) work, and
            -- is necessary when a map over sort_indices output calls a
            -- function with an index-pattern ([i]) parameter.
            bindStmts <- bindAppliedPat pat1 arg aa
            if null restPats
              then do
                (sb, ab) <- lowerExp body
                pure (sa ++ bindStmts ++ sb, ab)
              else do
                let newName = BS.append fName "__partial"
                registerFn newName restPats body
                pure (sa ++ bindStmts, AVar newName)
          [] -> do
            t <- freshCVar "t"
            pure (sa ++ [SAssign t (RCall fName [aa])], AVar t)
      Nothing -> do
        (sa, aa) <- lowerExp arg
        t <- freshCVar "t"
        pure (sa ++ [SAssign t (RCall fName [aa])], AVar t)
  -- Saturated binary operator: ((op) lhs) rhs  →  lhs op rhs
  -- This pattern appears when fusion builds EApp (EApp (EOp op) lhs) rhs
  -- instead of EBinOp, e.g. the fused scatter combinator.
  EApp _ (EOp _ op) innerArg -> do
    (s1, a1) <- lowerExp innerArg
    (s2, a2) <- lowerExp arg
    t <- freshCVar "t"
    let bop = lowerBinOp op
    mapM_ (registerCType t) (binopResultCType bop)
    pure (s1 ++ s2 ++ [SAssign t (RBinOp bop a1 a2)], AVar t)

  EApp _ (EVar _ fName) firstArg -> do
    mFn <- lookupFn fName
    case mFn of
      Just (pats, body) | length pats >= 2 -> do
        (s1, a1) <- lowerExp firstArg
        (s2, a2) <- lowerExp arg
        b1 <- bindAppliedPat (pats !! 0) firstArg a1
        b2 <- bindAppliedPat (pats !! 1) arg a2
        let restPats = drop 2 pats
        if null restPats
          then do
            (sb, ab) <- lowerExp body
            pure (s1 ++ s2 ++ b1 ++ b2 ++ sb, ab)
          else do
            let newName = BS.append fName "__partial2"
            registerFn newName restPats body
            pure (s1 ++ s2 ++ b1 ++ b2, AVar newName)
      _ -> do
        (sf, af) <- lowerExp fn
        (sa, aa) <- lowerExp arg
        case af of
          AVar pName -> do
            mPartialFn <- lookupFn pName
            case mPartialFn of
              Just (pats, body) -> case pats of
                (pat1:restPats) -> do
                  bindStmts <- bindAppliedPat pat1 arg aa
                  if null restPats
                    then do
                      (sb, ab) <- lowerExp body
                      pure (sf ++ sa ++ bindStmts ++ sb, ab)
                    else do
                      let newName = BS.append pName "__partial"
                      registerFn newName restPats body
                      pure (sf ++ sa ++ bindStmts, AVar newName)
                [] -> do
                  t <- freshCVar "t"
                  pure (sf ++ sa ++ [SAssign t (RCall pName [aa])], AVar t)
              Nothing -> do
                t <- freshCVar "t"
                pure (sf ++ sa ++ [SAssign t (RCall "__apply" [af, aa])], AVar t)
          _ -> do
            t <- freshCVar "t"
            pure (sf ++ sa ++ [SAssign t (RCall "__apply" [af, aa])], AVar t)
  _ -> do
    (sf, af) <- lowerExp fn
    (sa, aa) <- lowerExp arg
    case af of
      AVar pName -> do
        mFn <- lookupFn pName
        case mFn of
          Just (pats, body) -> case pats of
            (pat1:restPats) -> do
              bindStmts <- bindAppliedPat pat1 arg aa
              if null restPats
                then do
                  (sb, ab) <- lowerExp body
                  pure (sf ++ sa ++ bindStmts ++ sb, ab)
                else do
                  let newName = BS.append pName "__partial"
                  registerFn newName restPats body
                  pure (sf ++ sa ++ bindStmts, AVar newName)
            [] -> do
              t <- freshCVar "t"
              pure (sf ++ sa ++ [SAssign t (RCall pName [aa])], AVar t)
          Nothing -> do
            t <- freshCVar "t"
            pure (sf ++ sa ++ [SAssign t (RCall "__apply" [af, aa])], AVar t)
      _ -> do
        t <- freshCVar "t"
        pure (sf ++ sa ++ [SAssign t (RCall "__apply" [af, aa])], AVar t)

isShapeAllDim :: ShapeDim a -> Bool
isShapeAllDim (ShapeAll _) = True
isShapeAllDim _ = False

ctypeOfAtom :: Atom -> LowerM CType
ctypeOfAtom (AFloat _)  = pure CTDouble
ctypeOfAtom (ABool _)   = pure CTBool
ctypeOfAtom AUnit       = pure CTUnit
ctypeOfAtom (AString _) = pure CTInt64
ctypeOfAtom (AInt _)    = pure CTInt64
ctypeOfAtom (AVar v)    = gets $ fromMaybe CTUnknown . M.lookup v . lsTypeEnv
ctypeOfAtom (AVecVar _) = pure CTUnknown

-- | Determine the @CElemType@ of an atom for use in @RPairMake@.
-- Returns 'Nothing' when the atom's type is unknown or not representable
-- as a 'CElemType'.  Callers must not fall back to 'CEInt' silently;
-- unregistered pair types are left for CFGTyping to recover correctly.
elemTypeOfAtom :: Atom -> LowerM (Maybe CElemType)
elemTypeOfAtom (AFloat _) = pure (Just CEFloat)
elemTypeOfAtom (ABool _)  = pure (Just CEBool)
elemTypeOfAtom (AInt _)   = pure (Just CEInt)
elemTypeOfAtom (AVar v)   = gets $ \st ->
  case M.lookup v (lsTypeEnv st) of
    Just ct -> ctypeToElemType ct
    Nothing -> Nothing
elemTypeOfAtom _          = pure Nothing

-- | Check if an atom is positively confirmed to be a scalar integer.
-- Returns True only when we have evidence the atom is not a tuple.
-- Used by EProj lowering to decide whether @proj 0 x@ can be elided.
isAtomConfirmedScalar :: [Stmt] -> Atom -> LowerM Bool
isAtomConfirmedScalar _ (AInt {})  = pure True
isAtomConfirmedScalar _ (AFloat {}) = pure True
isAtomConfirmedScalar _ (ABool {}) = pure True
isAtomConfirmedScalar stmts (AVar v) = do
  -- Check (a): was the variable produced by a definitively scalar RHS?
  let byRHS = case reverse stmts of
        (SAssign v' rhs' : _) | v == v' -> case rhs' of
          RAtom (AInt {})  -> True
          RAtom (AFloat {}) -> True
          RBinOp {}        -> True
          RUnOp {}         -> True
          RArrayLoad {}    -> True
          _                -> False
        _ -> False
  if byRHS then pure True else do
    -- Check (b): is the variable explicitly registered in the type env?
    mTy <- gets (M.lookup v . lsTypeEnv)
    pure $ case mTy of
      Just CTInt64  -> True
      Just CTDouble -> True
      Just CTBool   -> True
      _             -> False
isAtomConfirmedScalar _ _ = pure False

-- | Register a concrete @CType@ for a variable in @lsTypeEnv@.
registerCType :: CVar -> CType -> LowerM ()
registerCType v ct = modify' $ \st ->
  st { lsTypeEnv = M.insert v ct (lsTypeEnv st) }

emptyArrayFact :: ArrayFact
emptyArrayFact = ArrayFact
  { afFreshAlloc = False
  , afWriteOnce = False
  , afReadOnly = False
  }

recordArrayFact :: CVar -> (ArrayFact -> ArrayFact) -> LowerM ()
recordArrayFact v updateFact = modify' $ \st ->
  let fact = updateFact (M.findWithDefault emptyArrayFact v (lsArrayFacts st))
  in st { lsArrayFacts = M.insert v fact (lsArrayFacts st) }

markArrayFreshWriteOnce :: CVar -> LowerM ()
markArrayFreshWriteOnce v =
  recordArrayFact v $ \fact ->
    fact { afFreshAlloc = True, afWriteOnce = True }

noteReadOnlyAtom :: Atom -> LowerM ()
noteReadOnlyAtom (AVar v) =
  recordArrayFact v $ \fact -> fact { afReadOnly = True }
noteReadOnlyAtom _ = pure ()

emptyVectorAccessFact :: VectorAccessFact
emptyVectorAccessFact = VectorAccessFact
  { vxfDenseLinearIndexOf = Nothing
  , vxfDenseRead = False
  , vxfIndirectRead = False
  , vxfContiguousWrite = False
  }

recordVectorAccessFact :: CVar -> (VectorAccessFact -> VectorAccessFact) -> LowerM ()
recordVectorAccessFact v updateFact = modify' $ \st ->
  let fact = updateFact (M.findWithDefault emptyVectorAccessFact v (lsVectorAccessFacts st))
  in st { lsVectorAccessFacts = M.insert v fact (lsVectorAccessFacts st) }

noteDenseLinearIndex :: CVar -> CVar -> LowerM ()
noteDenseLinearIndex v iter =
  recordVectorAccessFact v $ \fact -> fact { vxfDenseLinearIndexOf = Just iter }

propagateDenseLinearIndex :: CVar -> Atom -> LowerM ()
propagateDenseLinearIndex dst (AVar src) = do
  mOrigin <- gets $ fmap vxfDenseLinearIndexOf . M.lookup src . lsVectorAccessFacts
  case mOrigin of
    Just (Just iter) -> noteDenseLinearIndex dst iter
    _ -> pure ()
propagateDenseLinearIndex _ _ = pure ()

atomHasDenseLinearOrigin :: Atom -> LowerM Bool
atomHasDenseLinearOrigin (AVar v) =
  gets $ maybe False ((/= Nothing) . vxfDenseLinearIndexOf) . M.lookup v . lsVectorAccessFacts
atomHasDenseLinearOrigin _ = pure False

atomDenseLinearOrigin :: Atom -> LowerM (Maybe CVar)
atomDenseLinearOrigin (AVar v) =
  gets $ maybe Nothing vxfDenseLinearIndexOf . M.lookup v . lsVectorAccessFacts
atomDenseLinearOrigin _ = pure Nothing

propagateDenseLinearIndexBinOp :: CVar -> BinOp -> Atom -> Atom -> LowerM ()
propagateDenseLinearIndexBinOp dst bop a b = do
  originA <- atomDenseLinearOrigin a
  originB <- atomDenseLinearOrigin b
  case bop of
    CAdd -> case (originA, originB) of
      (Just iter, Nothing) -> noteDenseLinearIndex dst iter
      (Nothing, Just iter) -> noteDenseLinearIndex dst iter
      _ -> pure ()
    CSub -> case (originA, originB) of
      (Just iter, Nothing) -> noteDenseLinearIndex dst iter
      _ -> pure ()
    _ ->
      pure ()

propagateTupleDenseLinearIndex :: CVar -> [Atom] -> LowerM ()
propagateTupleDenseLinearIndex dst atoms =
  case unsnoc atoms of
    Just (prefix, lastAtom) -> do
      prefixOrigins <- mapM atomDenseLinearOrigin prefix
      lastOrigin <- atomDenseLinearOrigin lastAtom
      case (all (== Nothing) prefixOrigins, lastOrigin) of
        (True, Just iter) -> noteDenseLinearIndex dst iter
        _ -> pure ()
    Nothing ->
      pure ()
  where
    unsnoc xs = case reverse xs of
      [] -> Nothing
      y : ys -> Just (reverse ys, y)

noteDenseReadAtom :: Atom -> LowerM ()
noteDenseReadAtom (AVar v) =
  recordVectorAccessFact v $ \fact -> fact { vxfDenseRead = True }
noteDenseReadAtom _ = pure ()

noteIndirectReadAtom :: Atom -> LowerM ()
noteIndirectReadAtom (AVar v) =
  recordVectorAccessFact v $ \fact -> fact { vxfIndirectRead = True }
noteIndirectReadAtom _ = pure ()

markContiguousWriteArray :: CVar -> LowerM ()
markContiguousWriteArray v =
  recordVectorAccessFact v $ \fact -> fact { vxfContiguousWrite = True }

whenM :: Monad m => m Bool -> m () -> m ()
whenM cond action = do
  ok <- cond
  when ok action

-- | Return Just (ct1, ct2) if the atom names a pair variable, Nothing otherwise.
pairElemTypes :: Atom -> LowerM (Maybe (CElemType, CElemType))
pairElemTypes (AVar v) = gets $ \st ->
  case M.lookup v (lsTypeEnv st) of
    Just (CTPair t1 t2) | Just et1 <- ctypeToElemType t1
                        , Just et2 <- ctypeToElemType t2 -> Just (et1, et2)
    _ -> Nothing
pairElemTypes _        = pure Nothing

recordFieldType :: Atom -> Var -> LowerM (Maybe CType)
recordFieldType (AVar v) field = gets $ \st ->
  case M.lookup v (lsTypeEnv st) of
    Just (CTRecord fields) -> lookup field fields
    _ -> Nothing
recordFieldType _ _ = pure Nothing

-- | Propagate concrete type info from a source atom to a destination
-- variable. Call this whenever a plain copy of the form
-- @SAssign dst (RAtom srcAtom)@
-- is emitted so that subsequent uses of the destination can correctly determine
-- its type.
propagatePairInfo :: CVar -> Atom -> LowerM ()
propagatePairInfo dst (AVar src) = do
  st <- get
  case M.lookup src (lsTypeEnv st) of
    Just ct -> registerCType dst ct
    Nothing -> pure ()
propagatePairInfo _ _ = pure ()

elemTypeToCTypeMaybe :: CElemType -> Maybe CType
elemTypeToCTypeMaybe ceTy = case ceTy of
  CEInt -> Just CTInt64
  CEFloat -> Just CTDouble
  CEBool -> Just CTBool
  CEPair ct1 ct2 -> Just (CTPair (elemTypeToCType ct1) (elemTypeToCType ct2))
  CEArray -> Just (CTArray CTDouble)

lowerBinOp :: Operator a -> BinOp
lowerBinOp (Plus _) = CAdd
lowerBinOp (Minus _) = CSub
lowerBinOp (Times _) = CMul
lowerBinOp (Divide _) = CDiv
lowerBinOp (Mod _) = CMod
lowerBinOp (Eq _) = CEq
lowerBinOp (Neq _) = CNeq
lowerBinOp (Lt _) = CLt
lowerBinOp (Le _) = CLe
lowerBinOp (Gt _) = CGt
lowerBinOp (Ge _) = CGe
lowerBinOp (And _) = CAnd
lowerBinOp (Or _) = COr
lowerBinOp (PlusF _) = CAddF
lowerBinOp (MinusF _) = CSubF
lowerBinOp (TimesF _) = CMulF
lowerBinOp (DivideF _) = CDivF
lowerBinOp (EqF _) = CEqF
lowerBinOp (NeqF _) = CNeqF
lowerBinOp (LtF _) = CLtF
lowerBinOp (LeF _) = CLeF
lowerBinOp (GtF _) = CGtF
lowerBinOp (GeF _) = CGeF

-- | True for binary operators whose result type is Float.
isFloatBinOp :: BinOp -> Bool
isFloatBinOp CAddF = True
isFloatBinOp CSubF = True
isFloatBinOp CMulF = True
isFloatBinOp CDivF = True
isFloatBinOp _     = False

-- | The result 'CType' of a binary operator, if statically known.
binopResultCType :: BinOp -> Maybe CType
binopResultCType op = case op of
  CAdd  -> Just CTInt64;  CSub -> Just CTInt64;  CMul -> Just CTInt64
  CDiv  -> Just CTInt64;  CMod -> Just CTInt64
  CEq   -> Just CTBool;   CNeq -> Just CTBool
  CLt   -> Just CTBool;   CLe  -> Just CTBool
  CGt   -> Just CTBool;   CGe  -> Just CTBool
  CAnd  -> Just CTBool;   COr  -> Just CTBool
  CAddF -> Just CTDouble; CSubF -> Just CTDouble
  CMulF -> Just CTDouble; CDivF -> Just CTDouble
  CEqF  -> Just CTBool;   CNeqF -> Just CTBool
  CLtF  -> Just CTBool;   CLeF  -> Just CTBool
  CGtF  -> Just CTBool;   CGeF  -> Just CTBool

-- | Lower a unary math function (Float -> Float) to a CFG RUnOp.
lowerMathUnOp :: UnOp -> Exp Range -> LowerM ([Stmt], Atom)
lowerMathUnOp op e = do
  (s, a) <- lowerExp e
  t <- freshCVar "t"
  registerCType t CTDouble
  pure (s ++ [SAssign t (RUnOp op a)], AVar t)

atomToIndexExpr :: Atom -> IndexExpr
atomToIndexExpr (AVar v) = IVar v
atomToIndexExpr (AInt n) = IConst n
atomToIndexExpr _ = IConst 0

bindCopyVar :: CVar -> Atom -> LowerM [Stmt]
bindCopyVar dst src = do
  -- Only propagate a concrete type when the source atom has one registered.
  -- For AVar atoms not yet in lsTypeEnv (e.g. array parameters inlined from
  -- a callee), ctypeOfAtom would return the CTInt64 default, incorrectly
  -- poisoning the destination's type in procTypeEnv and blocking later
  -- backward-propagation through RArrayLoad / RArrayShape.
  mSrcTy <- case src of
    AVar v  -> gets (M.lookup v . lsTypeEnv)
    _       -> Just <$> ctypeOfAtom src
  mapM_ (registerCType dst) mSrcTy
  propagatePairInfo dst src
  pure [SAssign dst (RAtom src)]

bindPatAtom :: Pat Range -> Atom -> LowerM [Stmt]
bindPatAtom (PVar _ v) src = bindCopyVar v src
bindPatAtom (PBound _ v _) src = bindCopyVar v src
bindPatAtom (PVec _ ps) src = bindPVecPats ps src
bindPatAtom (PPair _ p1 p2) src = do
  fstVar <- freshCVar "fst"
  sndVar <- freshCVar "snd"
  mPairTys <- pairElemTypes src
  let (fstRhs, sndRhs) = case mPairTys of
        Just (ct1, ct2) -> (RPairFst ct1 src, RPairSnd ct2 src)
        Nothing         -> (RProj 0 src,       RProj 1 src)
  fstStmts <- bindPatAtom p1 (AVar fstVar)
  sndStmts <- bindPatAtom p2 (AVar sndVar)
  pure $ [SAssign fstVar fstRhs, SAssign sndVar sndRhs] ++ fstStmts ++ sndStmts

bindAppliedPat :: Pat Range -> Exp Range -> Atom -> LowerM [Stmt]
bindAppliedPat pat arg src =
  case (pat, arg) of
    (PVec _ ps, EVec _ _) -> bindPVecPats ps src
    (PVec _ [p], _) -> do
      srcTy <- ctypeOfAtom src
      if srcTy == CTInt64
        then do
          tupleSrc <- freshCVar "idx1"
          bindStmts <- bindPVecPats [p] (AVar tupleSrc)
          pure $ [SAssign tupleSrc (RTuple [src])] ++ bindStmts
        else bindPVecPats [p] src
    _ -> bindPatAtom pat src

bindPVecPats :: [Pat Range] -> Atom -> LowerM [Stmt]
bindPVecPats pats src = concat <$> zipWithM bindElement [0..] pats
  where
    bindElement idx pat = do
      t <- freshCVar "p"
      let names = patVarNames pat
      when (idx == 0) $ propagateDenseLinearIndex t src
      scalar1D <- case idx of
        0 -> do
          hasDenseOrigin <- atomHasDenseLinearOrigin src
          explicitIntTy <- case src of
            AVar v -> gets $ (== Just CTInt64) . M.lookup v . lsTypeEnv
            _ -> pure False
          pure $ hasDenseOrigin && explicitIntTy
        _ -> pure False
      let projStmt
            | scalar1D = SAssign t (RAtom src)
            | otherwise = SAssign t (RProj idx src)
      case names of
        (pv:_) -> do
          propagateDenseLinearIndex pv (AVar t)
          pure [projStmt, SAssign pv (RAtom (AVar t))]
        [] -> pure [projStmt]

-- | Rename occurrences of a variable inside atoms/rhs/statements.
-- Used to avoid accidental collisions between lowered temporaries and
-- parameter names when inlining user-defined functions.
renameVarInAtom :: CVar -> CVar -> Atom -> Atom
renameVarInAtom old new at =
  case at of
    AVar v | v == old -> AVar new
    AVecVar v | v == old -> AVecVar new
    _ -> at

renameVarInIndexExpr :: CVar -> CVar -> IndexExpr -> IndexExpr
renameVarInIndexExpr old new ie = case ie of
  IVar v | v == old -> IVar new
  IConst{} -> ie
  IAdd a b -> IAdd (renameVarInIndexExpr old new a) (renameVarInIndexExpr old new b)
  ISub a b -> ISub (renameVarInIndexExpr old new a) (renameVarInIndexExpr old new b)
  IMul a b -> IMul (renameVarInIndexExpr old new a) (renameVarInIndexExpr old new b)
  IDiv a b -> IDiv (renameVarInIndexExpr old new a) (renameVarInIndexExpr old new b)
  ITuple es -> ITuple (map (renameVarInIndexExpr old new) es)
  IProj i e -> IProj i (renameVarInIndexExpr old new e)
  IFlatToNd a b -> IFlatToNd (renameVarInIndexExpr old new a) (renameVarInIndexExpr old new b)
  INdToFlat a b -> INdToFlat (renameVarInIndexExpr old new a) (renameVarInIndexExpr old new b)
  ICall f args -> ICall f (map (renameVarInIndexExpr old new) args)
  _ -> ie

renameVarInRHS :: CVar -> CVar -> RHS -> RHS
renameVarInRHS old new rhs =
  case rhs of
    RAtom a -> RAtom (renameVarInAtom old new a)
    RBinOp op a b -> RBinOp op (renameVarInAtom old new a) (renameVarInAtom old new b)
    RUnOp op a -> RUnOp op (renameVarInAtom old new a)
    RTuple as -> RTuple (map (renameVarInAtom old new) as)
    RProj i a -> RProj i (renameVarInAtom old new a)
    RRecord fields -> RRecord [(field, renameVarInAtom old new atom) | (field, atom) <- fields]
    RRecordProj field a -> RRecordProj field (renameVarInAtom old new a)
    RArrayAlloc a -> RArrayAlloc (renameVarInAtom old new a)
    RArrayLoad a b -> RArrayLoad (renameVarInAtom old new a) (renameVarInAtom old new b)
    RArrayShape a -> RArrayShape (renameVarInAtom old new a)
    RShapeSize a -> RShapeSize (renameVarInAtom old new a)
    RShapeInit a -> RShapeInit (renameVarInAtom old new a)
    RShapeLast a -> RShapeLast (renameVarInAtom old new a)
    RFlatToNd a b -> RFlatToNd (renameVarInAtom old new a) (renameVarInAtom old new b)
    RNdToFlat a b -> RNdToFlat (renameVarInAtom old new a) (renameVarInAtom old new b)
    R2DToFlat a b -> R2DToFlat (renameVarInAtom old new a) (renameVarInAtom old new b)
    RCall f args -> RCall f (map (renameVarInAtom old new) args)
    RVecLoad a b -> RVecLoad (renameVarInAtom old new a) (renameVarInAtom old new b)
    RVecStore a b c -> RVecStore (renameVarInAtom old new a) (renameVarInAtom old new b) (renameVarInAtom old new c)
    RVecBinOp op a b -> RVecBinOp op (renameVarInAtom old new a) (renameVarInAtom old new b)
    RVecUnOp op a -> RVecUnOp op (renameVarInAtom old new a)
    RVecSplat a -> RVecSplat (renameVarInAtom old new a)
    RVecReduce op a -> RVecReduce op (renameVarInAtom old new a)
    RPairMake ct1 ct2 a b -> RPairMake ct1 ct2 (renameVarInAtom old new a) (renameVarInAtom old new b)
    RPairFst ct a -> RPairFst ct (renameVarInAtom old new a)
    RPairSnd ct a -> RPairSnd ct (renameVarInAtom old new a)

renameVarInReductionSpec :: CVar -> CVar -> ReductionSpec -> ReductionSpec
renameVarInReductionSpec old new (ReductionSpec acc initE redop) =
  let acc' = if acc == old then new else acc
      initE' = renameVarInIndexExpr old new initE
  in ReductionSpec acc' initE' redop

renameLoopSpec :: CVar -> CVar -> LoopSpec -> LoopSpec
renameLoopSpec old new (LoopSpec its bounds exec red role) =
  let its' = map (\t -> if t == old then new else t) its
      bounds' = map (renameVarInIndexExpr old new) bounds
      red' = fmap (renameVarInReductionSpec old new) red
  in LoopSpec its' bounds' exec red' role

renameVarInStmt :: CVar -> CVar -> Stmt -> Stmt
renameVarInStmt old new stmt =
  case stmt of
    SAssign v rhs -> SAssign (if v == old then new else v) (renameVarInRHS old new rhs)
    SArrayWrite a b c -> SArrayWrite (renameVarInAtom old new a) (renameVarInAtom old new b) (renameVarInAtom old new c)
    SLoop spec body -> SLoop (renameLoopSpec old new spec) (map (renameVarInStmt old new) body)
    SIf cond tBranch eBranch -> SIf (renameVarInAtom old new cond) (map (renameVarInStmt old new) tBranch) (map (renameVarInStmt old new) eBranch)
    SReturn a -> SReturn (renameVarInAtom old new a)
    SBreak -> SBreak

renameVarInStmts :: CVar -> CVar -> [Stmt] -> [Stmt]
renameVarInStmts old new = map (renameVarInStmt old new)

hoistZeroArgValueProcCalls :: [Stmt] -> LowerM ([Stmt], [Stmt])
hoistZeroArgValueProcCalls stmts = go M.empty M.empty [] [] stmts
  where
    go _ _ hoisted acc [] = pure (reverse hoisted, reverse acc)
    go procCache subst hoisted acc (stmt:rest) = do
      let stmt' = foldl' (\s (old, new) -> renameVarInStmt old new s) stmt (M.toList subst)
      case stmt' of
        SAssign v (RCall f []) -> do
          isValueProc <- gets (S.member f . lsValueProcs)
          if isValueProc
            then case M.lookup f procCache of
              Just cachedVar ->
                go procCache (M.insert v cachedVar subst) hoisted acc rest
              Nothing -> do
                cachedVar <- freshCVar (f <> "_cache")
                let callStmt = SAssign cachedVar (RCall f [])
                    procCache' = M.insert f cachedVar procCache
                    subst' = M.insert v cachedVar subst
                go procCache' subst' (callStmt : hoisted) acc rest
            else
              go procCache subst hoisted (stmt' : acc) rest
        _ ->
          go procCache subst hoisted (stmt' : acc) rest

reductionRedopFromBinOp :: BinOp -> Maybe Redop
reductionRedopFromBinOp bop = case bop of
  CAdd  -> Just RAdd
  CAddF -> Just RAdd
  CMul  -> Just RMul
  CMulF -> Just RMul
  _     -> Nothing

reductionRedopFromStmts :: [Stmt] -> Maybe Redop
reductionRedopFromStmts [] = Nothing
reductionRedopFromStmts (SAssign v (RBinOp bop (AVar v') _) : rest)
  | v == v' = reductionRedopFromBinOp bop <|> reductionRedopFromStmts rest
reductionRedopFromStmts (_ : rest) = reductionRedopFromStmts rest

lowerReductionStep :: Exp Range -> CVar -> CVar -> LowerM ([Stmt], Maybe Redop)
lowerReductionStep fnExp acc elem' = case fnExp of
  EOp _ op ->
    let bop = lowerBinOp op
    in pure ([SAssign acc (RBinOp bop (AVar acc) (AVar elem'))], reductionRedopFromBinOp bop)
  EVar _ name -> do
    mFn <- lookupFn name
    case mFn of
      Just ([PVar _ x, PVar _ y], EBinOp _ (EVar _ bx) op' (EVar _ by)) | bx == x, by == y ->
        let bop = lowerBinOp op'
        in pure ([SAssign acc (RBinOp bop (AVar acc) (AVar elem'))], reductionRedopFromBinOp bop)
      _ -> do
        stmts <- inlineBinaryFn fnExp acc elem' acc
        pure (stmts, reductionRedopFromStmts stmts)
  _ -> do
    stmts <- inlineBinaryFn fnExp acc elem' acc
    pure (stmts, reductionRedopFromStmts stmts)

-- | Returns True when the given shape expression is a 1-element vector,
-- meaning the array is 1-dimensional. Used to skip hyd_flat_to_nd in scatter
-- loops, replacing it with the raw loop counter.
is1DShapeExp :: Exp a -> Bool
is1DShapeExp (EVec _ [_]) = True
is1DShapeExp _            = False

inlineArrayFn :: Exp Range -> CVar -> CVar -> LowerM [Stmt]
inlineArrayFn fnExp paramVar resultVar = case fnExp of
  EVar _ name -> do
    mFn <- lookupFn name
    case mFn of
      Just ([pat], body) -> do
        (stmts, atom) <- lowerExp body
        case pat of
          PVar _ x -> do
            px <- freshCVar "p"
            let stmts' = renameVarInStmts x px stmts
                atom' = renameVarInAtom x px atom
            atomTy <- ctypeOfAtom atom'
            registerCType resultVar atomTy
            propagatePairInfo resultVar atom'
            bindParam <- bindCopyVar px (AVar paramVar)
            pure $ bindParam ++ stmts' ++ [SAssign resultVar (RAtom atom')]
          PBound _ x _ -> do
            px <- freshCVar "p"
            let stmts' = renameVarInStmts x px stmts
                atom' = renameVarInAtom x px atom
            atomTy <- ctypeOfAtom atom'
            registerCType resultVar atomTy
            propagatePairInfo resultVar atom'
            bindParam <- bindCopyVar px (AVar paramVar)
            pure $ bindParam ++ stmts' ++ [SAssign resultVar (RAtom atom')]
          PVec _ ps -> do
            paramTy <- ctypeOfAtom (AVar paramVar)
            binds <-
              case (ps, paramTy) of
                ([_], CTInt64) -> do
                  tupleParam <- freshCVar "idx1"
                  bindStmts <- bindPVecPats ps (AVar tupleParam)
                  pure $ [SAssign tupleParam (RTuple [AVar paramVar])] ++ bindStmts
                _ -> bindPVecPats ps (AVar paramVar)
            (stmts', atom') <- lowerExp body
            atomTy <- ctypeOfAtom atom'
            registerCType resultVar atomTy
            propagatePairInfo resultVar atom'
            pure $ binds ++ stmts' ++ [SAssign resultVar (RAtom atom')]
          PPair _ _ _ -> do
            binds <- bindPatAtom pat (AVar paramVar)
            (stmts', atom') <- lowerExp body
            atomTy <- ctypeOfAtom atom'
            registerCType resultVar atomTy
            propagatePairInfo resultVar atom'
            pure $ binds ++ stmts' ++ [SAssign resultVar (RAtom atom')]
      _ -> pure [SAssign resultVar (RCall name [AVar paramVar])]
  ELetIn _ (Dec _ name pats _ _ body) rest -> do
    registerFn name pats body
    inlineArrayFn rest paramVar resultVar
  _ -> do
    (sf, af) <- lowerExp fnExp
    case af of
      AVar partialName -> do
        mPart <- lookupFn partialName
        case mPart of
          Just _ -> do
            partStmts <- inlineArrayFn (EVar undefined partialName) paramVar resultVar
            pure $ sf ++ partStmts
          Nothing -> pure $ sf ++ [SAssign resultVar (RCall "__apply" [af, AVar paramVar])]
      _ -> pure $ sf ++ [SAssign resultVar (RCall "__apply" [af, AVar paramVar])]

inlineArrayFn1D :: Exp Range -> CVar -> CVar -> LowerM [Stmt]
inlineArrayFn1D fnExp paramVar resultVar = case fnExp of
  EVar _ name -> do
    mFn <- lookupFn name
    case mFn of
      Just ([pat], body) -> do
        bindStmts <- case pat of
          PVar _ x -> do
            propagateDenseLinearIndex x (AVar paramVar)
            bindCopyVar x (AVar paramVar)
          PBound _ x _ -> do
            propagateDenseLinearIndex x (AVar paramVar)
            bindCopyVar x (AVar paramVar)
          PVec _ [PVar _ x] -> do
            propagateDenseLinearIndex x (AVar paramVar)
            bindCopyVar x (AVar paramVar)
          PVec _ [PBound _ x _] -> do
            propagateDenseLinearIndex x (AVar paramVar)
            bindCopyVar x (AVar paramVar)
          PVec _ [p] -> do
            paramTy <- ctypeOfAtom (AVar paramVar)
            if paramTy == CTInt64
              then do
                tupleParam <- freshCVar "idx1"
                propagateDenseLinearIndex tupleParam (AVar paramVar)
                bindVecStmts <- bindPVecPats [p] (AVar tupleParam)
                pure $ [SAssign tupleParam (RTuple [AVar paramVar])] ++ bindVecStmts
              else bindPVecPats [p] (AVar paramVar)
          _ -> pure [SAssign resultVar (RCall name [AVar paramVar])]
        case bindStmts of
          [SAssign v (RCall f [AVar p])] | v == resultVar && f == name && p == paramVar ->
            pure bindStmts
          _ -> do
            (stmts, atom) <- lowerExp body
            atomTy <- ctypeOfAtom atom
            registerCType resultVar atomTy
            propagatePairInfo resultVar atom
            pure $ bindStmts ++ stmts ++ [SAssign resultVar (RAtom atom)]
      _ -> pure [SAssign resultVar (RCall name [AVar paramVar])]
  ELetIn _ (Dec _ name pats _ _ body) rest -> do
    registerFn name pats body
    inlineArrayFn1D rest paramVar resultVar
  _ -> do
    (sf, af) <- lowerExp fnExp
    case af of
      AVar partialName -> do
        mPart <- lookupFn partialName
        case mPart of
          Just _ -> do
            partStmts <- inlineArrayFn1D (EVar undefined partialName) paramVar resultVar
            pure $ sf ++ partStmts
          Nothing -> pure $ sf ++ [SAssign resultVar (RCall "__apply" [af, AVar paramVar])]
      _ -> pure $ sf ++ [SAssign resultVar (RCall "__apply" [af, AVar paramVar])]

inlineScalarFn :: Exp Range -> CVar -> CVar -> LowerM [Stmt]
inlineScalarFn fnExp paramVar resultVar = case fnExp of
  EVar _ name -> do
    mFn <- lookupFn name
    case mFn of
      Just ([PVar _ x], body) -> do
        -- Propagate paramVar's type to x before lowering the body so that
        -- index-pattern functions applied to x inside (e.g. `row_of x` where
        -- row_of expects [i]) can correctly determine whether to wrap the
        -- argument in a tuple.
        paramTy <- ctypeOfAtom (AVar paramVar)
        when (paramTy /= CTUnknown) $ registerCType x paramTy
        (stmts, atom) <- lowerExp body
        px <- freshCVar "p"
        let stmts' = renameVarInStmts x px stmts
            atom' = renameVarInAtom x px atom
        atomTy <- ctypeOfAtom atom'
        registerCType resultVar atomTy
        propagatePairInfo resultVar atom'
        bindParam <- bindCopyVar px (AVar paramVar)
        pure $ bindParam ++ stmts' ++ [SAssign resultVar (RAtom atom')]
      Just ([PBound _ x _], body) -> do
        paramTy <- ctypeOfAtom (AVar paramVar)
        when (paramTy /= CTUnknown) $ registerCType x paramTy
        (stmts, atom) <- lowerExp body
        px <- freshCVar "p"
        let stmts' = renameVarInStmts x px stmts
            atom' = renameVarInAtom x px atom
        atomTy <- ctypeOfAtom atom'
        registerCType resultVar atomTy
        propagatePairInfo resultVar atom'
        bindParam <- bindCopyVar px (AVar paramVar)
        pure $ bindParam ++ stmts' ++ [SAssign resultVar (RAtom atom')]
      Just ([PVec _ ps], body) -> do
        binds <-
          case ps of
            [_] -> do
              paramTy <- ctypeOfAtom (AVar paramVar)
              if paramTy == CTInt64
                then do
                  tupleParam <- freshCVar "idx1"
                  bindStmts <- bindPVecPats ps (AVar tupleParam)
                  pure $ [SAssign tupleParam (RTuple [AVar paramVar])] ++ bindStmts
                else bindPVecPats ps (AVar paramVar)
            _ -> bindPVecPats ps (AVar paramVar)
        (stmts, atom) <- lowerExp body
        atomTy <- ctypeOfAtom atom
        registerCType resultVar atomTy
        propagatePairInfo resultVar atom
        pure $ binds ++ stmts ++ [SAssign resultVar (RAtom atom)]
      _ -> pure [SAssign resultVar (RCall name [AVar paramVar])]
  ELetIn _ (Dec _ name pats _ _ body) rest -> do
    registerFn name pats body
    inlineScalarFn rest paramVar resultVar
  _ -> do
    (sf, af) <- lowerExp fnExp
    pure $ sf ++ [SAssign resultVar (RCall "__apply" [af, AVar paramVar])]

-- | Lower an array expression for use as a sequential input to a consumer loop.
--
-- When the expression is @EMap fn src@ (possibly wrapped in @ELetIn@
-- function-binding wrappers), the map result is *not* materialised.  Instead
-- we lower @src@ once (as setup) and emit per-iteration statements that load
-- @src[loopVar]@ and then inline @fn@ to produce @resultVar@.  This avoids
-- allocating a temporary array and makes the dedup/scatter loops process
-- their inputs without an extra pass.
--
-- For any other expression the array is materialised normally and each
-- iteration does a plain @RArrayLoad@.
--
-- Returns @(setup_stmts, src_atom, per_iter_stmts)@ where
--   * @setup_stmts@ run once before the consumer loop,
--   * @src_atom@ is the underlying source array atom (useful for shape queries),
--   * @per_iter_stmts@ produce @resultVar = expr[loopVar]@ each iteration.
lowerForSeqConsume :: Exp Range -> CVar -> CVar -> LowerM ([Stmt], Atom, [Stmt])
lowerForSeqConsume arrExp loopVar resultVar = case arrExp of
  EMap _ fn src -> do
    (ssrc, asrc) <- lowerExp src
    elem' <- freshCVar "elem"
    srcTy <- ctypeOfAtom asrc
    case srcTy of
      CTArray et -> registerCType elem' et
      _          -> pure ()
    bodyStmts <- inlineScalarFn fn elem' resultVar
    let perIter = SAssign elem' (RArrayLoad asrc (AVar loopVar)) : bodyStmts
    pure (ssrc, asrc, perIter)
  ELetIn _ (Dec _ name pats _ _ body) rest
    | not (null pats) -> do
        registerFn name pats body
        lowerForSeqConsume rest loopVar resultVar
    | otherwise -> materialize
  _ -> materialize
  where
    materialize = do
      (sarr, aarr) <- lowerExp arrExp
      pure (sarr, aarr, [SAssign resultVar (RArrayLoad aarr (AVar loopVar))])

inlineBinaryFn :: Exp Range -> CVar -> CVar -> CVar -> LowerM [Stmt]
inlineBinaryFn fnExp param1 param2 resultVar = case fnExp of
  EOp _ op ->
    pure [ SAssign resultVar (RBinOp (lowerBinOp op) (AVar param1) (AVar param2)) ]
  EVar _ name -> do
    mFn <- lookupFn name
    case mFn of
      Just ([PVar _ x, PVar _ y], body) ->
        case body of
          EBinOp _ (EVar _ bx) op' (EVar _ by) | bx == x, by == y ->
            pure [ SAssign resultVar (RBinOp (lowerBinOp op') (AVar param1) (AVar param2)) ]
          _ -> do
            propagatePairInfo x (AVar param1)
            (stmts, atom) <- lowerExp body
            px <- freshCVar "p"
            py <- freshCVar "p"
            let stmts' = renameVarInStmts x px $ renameVarInStmts y py stmts
                atom' = renameVarInAtom x px $ renameVarInAtom y py atom
            propagatePairInfo px (AVar param1)
            propagatePairInfo resultVar atom'
            pure $ [ SAssign px (RAtom (AVar param1))
                   , SAssign py (RAtom (AVar param2))
                   ] ++ stmts' ++ [SAssign resultVar (RAtom atom')]
      _ -> do
        t <- freshCVar "t"
        pure [ SAssign t (RCall name [AVar param1])
             , SAssign resultVar (RCall "__apply" [AVar t, AVar param2])
             ]
  ELetIn _ (Dec _ name pats _ _ body) rest -> do
    registerFn name pats body
    inlineBinaryFn rest param1 param2 resultVar
  _ -> do
    (sf, af) <- lowerExp fnExp
    case af of
      AVar partialName -> do
        mPart <- lookupFn partialName
        case mPart of
          Just _ -> do
            partStmts <- inlineBinaryFn (EVar undefined partialName) param1 param2 resultVar
            pure $ sf ++ partStmts
          Nothing -> do
            t <- freshCVar "t"
            pure $ sf ++ [ SAssign t (RCall "__apply" [af, AVar param1])
                         , SAssign resultVar (RCall "__apply" [AVar t, AVar param2])
                         ]
      _ -> do
        t <- freshCVar "t"
        pure $ sf ++ [ SAssign t (RCall "__apply" [af, AVar param1])
                     , SAssign resultVar (RCall "__apply" [AVar t, AVar param2])
                     ]
