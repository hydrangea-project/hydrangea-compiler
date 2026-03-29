{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.CFGTyping
--
-- Conservative type recovery for CFG procedures.
--
-- The authoritative source of CFG types is 'procTypeEnv', but some downstream
-- passes need a slightly richer environment after optimization introduces new
-- temporaries or when direct-call result types are only known at the
-- program level. This module centralizes that recovery logic so passes do not
-- each grow their own local type reconstruction.
--
-- The analysis is intentionally conservative:
--
-- * it trusts 'procTypeEnv' when present,
-- * it propagates only simple RHS result types,
-- * it recovers array element types from writes and already-typed loads, and
-- * it uses direct-call return types discovered at the program level.
--
-- Notably, it does /not/ infer array element types backwards from floating
-- arithmetic alone.
module Language.Hydrangea.CFGTyping
  ( TypeEnv
  , inferAtomType2
  , lookupArrayElemType2
  , recoverProcTypeEnv2
  , inferProgramReturnTypes2
  , buildCallParamTypes
  , CallParamTypes
  ) where

import Control.Applicative ((<|>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Language.Hydrangea.CFG
import Language.Hydrangea.CFGCore (Atom(..), BinOp(..), CType(..), UnOp(..))
import Language.Hydrangea.CFGCore qualified as C

type TypeEnv = Map CVar CType

-- | Maps each procedure name to the ordered types of its formal parameters.
-- 'Nothing' entries indicate parameters whose types could not be determined.
type CallParamTypes = Map CVar [Maybe CType]

bindVarType :: CVar -> CType -> TypeEnv -> TypeEnv
bindVarType v ct typeEnv = case M.lookup v typeEnv of
  Nothing -> M.insert v ct typeEnv
  Just ct' | ct' == ct -> typeEnv
  _ -> typeEnv

bindAtomType :: Atom -> CType -> TypeEnv -> TypeEnv
bindAtomType atom ct typeEnv = case atom of
  AVar v -> bindVarType v ct typeEnv
  _ -> typeEnv

inferAtomType2 :: TypeEnv -> Atom -> Maybe CType
inferAtomType2 typeEnv atom = case atom of
  AVar v -> M.lookup v typeEnv
  AInt {} -> Just CTInt64
  AFloat {} -> Just CTDouble
  ABool {} -> Just CTBool
  AUnit -> Just CTUnit
  AString {} -> Just CTInt64
  AVecVar {} -> Nothing

lookupArrayElemType2 :: TypeEnv -> Atom -> Maybe CType
lookupArrayElemType2 typeEnv atom = case atom of
  AVar v -> case M.lookup v typeEnv of
    Just (CTArray eltTy) -> Just eltTy
    _ -> Nothing
  _ -> Nothing

inferBinOpResultType :: BinOp -> Maybe CType
inferBinOpResultType op = case op of
  CAdd -> Just CTInt64
  CSub -> Just CTInt64
  CMul -> Just CTInt64
  CDiv -> Just CTInt64
  CMod -> Just CTInt64
  CEq -> Just CTBool
  CNeq -> Just CTBool
  CLt -> Just CTBool
  CLe -> Just CTBool
  CGt -> Just CTBool
  CGe -> Just CTBool
  CAnd -> Just CTBool
  COr -> Just CTBool
  CAddF -> Just CTDouble
  CSubF -> Just CTDouble
  CMulF -> Just CTDouble
  CDivF -> Just CTDouble
  CEqF -> Just CTBool
  CNeqF -> Just CTBool
  CLtF -> Just CTBool
  CLeF -> Just CTBool
  CGtF -> Just CTBool
  CGeF -> Just CTBool

inferBinOpOperandType :: BinOp -> Maybe CType
inferBinOpOperandType op = case op of
  CAdd -> Just CTInt64
  CSub -> Just CTInt64
  CMul -> Just CTInt64
  CDiv -> Just CTInt64
  CMod -> Just CTInt64
  CEq -> Just CTInt64
  CNeq -> Just CTInt64
  CLt -> Just CTInt64
  CLe -> Just CTInt64
  CGt -> Just CTInt64
  CGe -> Just CTInt64
  CAnd -> Just CTBool
  COr -> Just CTBool
  CAddF -> Just CTDouble
  CSubF -> Just CTDouble
  CMulF -> Just CTDouble
  CDivF -> Just CTDouble
  CEqF -> Just CTDouble
  CNeqF -> Just CTDouble
  CLtF -> Just CTDouble
  CLeF -> Just CTDouble
  CGtF -> Just CTDouble
  CGeF -> Just CTDouble

inferUnOpResultType :: UnOp -> Maybe CType
inferUnOpResultType op = case op of
  CNot -> Just CTBool
  CNeg -> Nothing
  CSqrt -> Just CTDouble
  CExpF -> Just CTDouble
  CLog -> Just CTDouble
  CSin -> Just CTDouble
  CCos -> Just CTDouble
  CAbsF -> Just CTDouble
  CFloorF -> Just CTDouble
  CCeilF -> Just CTDouble
  CErf -> Just CTDouble
  CFloatOf -> Just CTDouble
  CIntOf -> Just CTInt64

inferUnOpOperandType :: UnOp -> Maybe CType
inferUnOpOperandType op = case op of
  CNot -> Just CTBool
  CNeg -> Nothing
  CSqrt -> Just CTDouble
  CExpF -> Just CTDouble
  CLog -> Just CTDouble
  CSin -> Just CTDouble
  CCos -> Just CTDouble
  CAbsF -> Just CTDouble
  CFloorF -> Just CTDouble
  CCeilF -> Just CTDouble
  CErf -> Just CTDouble
  CFloatOf -> Just CTInt64
  CIntOf -> Just CTDouble

inferCallType :: Map CVar CType -> CVar -> Maybe CType
inferCallType callTypes fn =
  M.lookup fn callTypes <|> case fn of
    "hyd_read_array_csv" -> Just (CTArray CTInt64)
    "hyd_read_float_array_csv" -> Just (CTArray CTDouble)
    "hyd_flat_to_nd" -> Just CTTuple
    "getenv_int" -> Just CTInt64
    "getenv_string" -> Just CTInt64
    _ -> Nothing

inferStmtTypes :: Map CVar CType -> CallParamTypes -> TypeEnv -> Stmt -> TypeEnv
inferStmtTypes callTypes callParamTypes typeEnv stmt = case stmt of
  SAssign v rhs -> inferAssignTypes callTypes callParamTypes typeEnv v rhs
  SArrayWrite arr _ val ->
    -- Derive the array element type from the written value.
    case (arr, inferAtomType2 typeEnv val) of
      (AVar arrV, Just eltTy) -> M.insert arrV (CTArray eltTy) typeEnv
      _ -> typeEnv
  SLoop _ body ->
    inferStmtListTypes callTypes callParamTypes typeEnv body
  SIf _ thn els ->
    let thnEnv = inferStmtListTypes callTypes callParamTypes typeEnv thn
        elsEnv = inferStmtListTypes callTypes callParamTypes thnEnv els
    in elsEnv
  SReturn _ ->
    typeEnv
  SBreak ->
    typeEnv

inferAssignTypes :: Map CVar CType -> CallParamTypes -> TypeEnv -> CVar -> C.RHS -> TypeEnv
inferAssignTypes callTypes callParamTypes typeEnv v rhs = case rhs of
  C.RAtom atom ->
    -- Pair types propagate from source to destination; other types use the
    -- conservative binding to preserve already-inferred information.
    case inferAtomType2 typeEnv atom of
      Just ct@(CTPair _ _) ->
        bindAtomType atom ct (M.insert v ct typeEnv)
      Just ct ->
        bindAtomType atom ct (bindVarType v ct typeEnv)
      Nothing ->
        case M.lookup v typeEnv of
          Just ct -> bindAtomType atom ct typeEnv
          Nothing -> typeEnv
  C.RBinOp op a1 a2 ->
    let typeEnv1 = maybe typeEnv (\ct -> bindVarType v ct typeEnv) (inferBinOpResultType op)
    in case inferBinOpOperandType op of
         Just argTy -> bindAtomType a1 argTy (bindAtomType a2 argTy typeEnv1)
         Nothing -> typeEnv1
  C.RUnOp op a ->
    case op of
      CNeg ->
        case inferAtomType2 typeEnv a <|> M.lookup v typeEnv of
          Just ct@CTDouble -> bindAtomType a ct (bindVarType v ct typeEnv)
          Just ct@CTInt64  -> bindAtomType a ct (bindVarType v ct typeEnv)
          _ -> typeEnv
      _ ->
        let typeEnv1 = maybe typeEnv (\ct -> bindVarType v ct typeEnv) (inferUnOpResultType op)
        in case inferUnOpOperandType op of
             Just argTy -> bindAtomType a argTy typeEnv1
             Nothing -> typeEnv1
  C.RArrayAlloc {} ->
    typeEnv
  C.RArrayLoad arr _ ->
    case lookupArrayElemType2 typeEnv arr of
      Just eltTy ->
        bindVarType v eltTy typeEnv
      Nothing ->
        case M.lookup v typeEnv of
          Just eltTy -> bindAtomType arr (CTArray eltTy) typeEnv
          Nothing -> typeEnv
  C.RArrayShape _ ->
    bindVarType v CTTuple typeEnv
  C.RProj i src ->
    -- Tuple projections yield CTInt64 (tuples hold shape/index integers).
    -- Pair projections are re-derived from the current pair element types.
    case inferAtomType2 typeEnv src of
      Just CTTuple             -> bindVarType v CTInt64 typeEnv
      Just (CTPair ct1 ct2)    -> M.insert v (if i == 0 then ct1 else ct2) typeEnv
      _ -> typeEnv
  C.RPairMake _ _ a1 a2 ->
    -- Re-derive the pair type from the current element types.
    case (inferAtomType2 typeEnv a1, inferAtomType2 typeEnv a2) of
      (Just ct1, Just ct2) -> M.insert v (CTPair ct1 ct2) typeEnv
      _ -> typeEnv
  C.RPairFst _ct a ->
    case inferAtomType2 typeEnv a of
      Just (CTPair ct1 _) -> M.insert v ct1 typeEnv
      _ -> typeEnv
  C.RPairSnd _ct a ->
    case inferAtomType2 typeEnv a of
      Just (CTPair _ ct2) -> M.insert v ct2 typeEnv
      _ -> typeEnv
  C.RShapeSize _ ->
    bindVarType v CTInt64 typeEnv
  C.RShapeInit _ ->
    bindVarType v CTTuple typeEnv
  C.RShapeLast _ ->
    bindVarType v CTInt64 typeEnv
  C.RCall fn args ->
    let typeEnv1 = maybe typeEnv (\ct -> bindVarType v ct typeEnv) (inferCallType callTypes fn)
        mParamTypes = M.findWithDefault [] fn callParamTypes
    in foldl (\env (arg, mpt) -> case mpt of
                 Just pt -> bindAtomType arg pt env
                 Nothing -> env
              ) typeEnv1 (zip args mParamTypes)
  _ ->
    typeEnv

-- | Collect all (srcVar, projIdx, resultVar) triples from RProj assignments
-- in a statement list (recursively into loops/conditionals).
collectProjAssigns :: [Stmt] -> [(CVar, Integer, CVar)]
collectProjAssigns = concatMap go
  where
    go stmt = case stmt of
      SAssign v (C.RProj i (AVar src)) -> [(src, i, v)]
      SLoop _ body -> collectProjAssigns body
      SIf _ thn els -> collectProjAssigns thn ++ collectProjAssigns els
      _ -> []

-- | After a fixpoint pass, infer source pair types from known RProj result types.
-- For each source variable that appears as both @RProj 0 src@ and @RProj 1 src@
-- and whose result variables have known types, bind src to @CTPair ct0 ct1@.
inferProjSourceTypes :: TypeEnv -> [(CVar, Integer, CVar)] -> TypeEnv
inferProjSourceTypes typeEnv projAssigns =
  foldl bindIfKnown typeEnv (M.toList projBySource)
  where
    projBySource :: Map CVar (Map Integer CVar)
    projBySource = foldl (\m (src, i, v) ->
                            M.insertWith M.union src (M.singleton i v) m)
                         M.empty projAssigns
    bindIfKnown env (src, iToV) =
      -- Only infer a pair type when the source has no existing type;
      -- an existing CTTuple or CTArray must not be overridden.
      case M.lookup src env of
        Just _ -> env   -- already typed; leave it alone
        Nothing ->
          case (M.lookup 0 iToV >>= (`M.lookup` env),
                M.lookup 1 iToV >>= (`M.lookup` env)) of
            (Just ct0, Just ct1) -> M.insert src (CTPair ct0 ct1) env
            _ -> env

inferStmtListTypes :: Map CVar CType -> CallParamTypes -> TypeEnv -> [Stmt] -> TypeEnv
inferStmtListTypes callTypes callParamTypes initialEnv stmts = outerGo initialEnv
  where
    projAssigns = collectProjAssigns stmts
    -- Inner fixpoint: propagate types through assignments until stable.
    innerGo typeEnv =
      let typeEnv' = foldl (inferStmtTypes callTypes callParamTypes) typeEnv stmts
      in if typeEnv' == typeEnv then typeEnv else innerGo typeEnv'
    -- Outer fixpoint: after propagating types, infer source pair types from
    -- RProj uses. Repeat if any new pair types are discovered.
    outerGo typeEnv =
      let after = innerGo typeEnv
          withPairs = inferProjSourceTypes after projAssigns
      in if withPairs == after then after else outerGo withPairs

collectReturnAtoms :: [Stmt] -> [Atom]
collectReturnAtoms = concatMap go
  where
    go stmt = case stmt of
      SReturn atom -> [atom]
      SLoop _ body -> collectReturnAtoms body
      SIf _ thn els -> collectReturnAtoms thn ++ collectReturnAtoms els
      _ -> []

inferProcReturnType :: Map CVar CType -> Proc -> Maybe CType
inferProcReturnType callTypes proc = do
  atom <- case collectReturnAtoms (procBody proc) of
    [retAtom] -> Just retAtom
    _ -> Nothing
  inferAtomType2 typeEnv atom
  where
    typeEnv = inferStmtListTypes callTypes M.empty (procTypeEnv proc) (procBody proc)

inferProgramReturnTypes2 :: Program -> Map CVar CType
inferProgramReturnTypes2 (Program procs) = foldl addProc M.empty procs
  where
    addProc callTypes proc =
      case inferProcReturnType callTypes proc of
        Just retTy -> M.insert (procName proc) retTy callTypes
        Nothing -> callTypes

-- | Build a map from procedure name to its ordered parameter types.
-- Runs a single-pass type recovery (without inter-proc argument propagation)
-- to discover parameter types that are not recorded in 'procTypeEnv' at
-- lowering time (e.g. array parameters typed only via backward propagation
-- through 'RArrayLoad').
buildCallParamTypes :: Map CVar CType -> [Proc] -> CallParamTypes
buildCallParamTypes retTypes procs = M.fromList
  [ (procName proc, map (\p -> M.lookup p recoveredEnv) (procParams proc))
  | proc <- procs
  , let recoveredEnv = inferStmtListTypes retTypes M.empty (procTypeEnv proc) (procBody proc)
  ]

-- | Recover a richer per-procedure type environment from CFG structure,
-- a map of direct-call return types, and a map of callee parameter types.
recoverProcTypeEnv2 :: Map CVar CType -> CallParamTypes -> Proc -> TypeEnv
recoverProcTypeEnv2 callTypes callParamTypes proc =
  inferStmtListTypes callTypes callParamTypes (procTypeEnv proc) (procBody proc)
