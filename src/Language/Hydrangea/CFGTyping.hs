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
  ) where

import Control.Applicative ((<|>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Language.Hydrangea.CFG
import Language.Hydrangea.CFGCore (Atom(..), BinOp(..), CType(..), UnOp(..))
import Language.Hydrangea.CFGCore qualified as C

type TypeEnv = Map CVar CType

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

inferCallType :: Map CVar CType -> CVar -> Maybe CType
inferCallType callTypes fn =
  M.lookup fn callTypes <|> case fn of
    "hyd_read_array_csv" -> Just (CTArray CTInt64)
    "hyd_read_float_array_csv" -> Just (CTArray CTDouble)
    "getenv_int" -> Just CTInt64
    "getenv_string" -> Just CTInt64
    _ -> Nothing

inferStmtTypes :: Map CVar CType -> TypeEnv -> Stmt -> TypeEnv
inferStmtTypes callTypes typeEnv stmt = case stmt of
  SAssign v rhs -> inferAssignTypes callTypes typeEnv v rhs
  SArrayWrite arr _ val ->
    case inferAtomType2 typeEnv val of
      Just eltTy -> bindAtomType arr (CTArray eltTy) typeEnv
      Nothing -> typeEnv
  SLoop _ body ->
    inferStmtListTypes callTypes typeEnv body
  SIf _ thn els ->
    let thnEnv = inferStmtListTypes callTypes typeEnv thn
        elsEnv = inferStmtListTypes callTypes thnEnv els
    in elsEnv
  SReturn _ ->
    typeEnv

inferAssignTypes :: Map CVar CType -> TypeEnv -> CVar -> C.RHS -> TypeEnv
inferAssignTypes callTypes typeEnv v rhs = case rhs of
  C.RAtom atom ->
    case inferAtomType2 typeEnv atom <|> M.lookup v typeEnv of
      Just ct ->
        bindAtomType atom ct (bindVarType v ct typeEnv)
      Nothing ->
        typeEnv
  C.RBinOp op a1 a2 ->
    let typeEnv1 = maybe typeEnv (\ct -> bindVarType v ct typeEnv) (inferBinOpResultType op)
    in case inferBinOpOperandType op of
         Just argTy -> bindAtomType a1 argTy (bindAtomType a2 argTy typeEnv1)
         Nothing -> typeEnv1
  C.RUnOp op a ->
    case op of
      CNeg ->
        case inferAtomType2 typeEnv a <|> M.lookup v typeEnv of
          Just ct@(CTDouble) -> bindAtomType a ct (bindVarType v ct typeEnv)
          Just ct@(CTInt64) -> bindAtomType a ct (bindVarType v ct typeEnv)
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
  C.RProj _ _ ->
    bindVarType v CTInt64 typeEnv
  C.RShapeSize _ ->
    bindVarType v CTInt64 typeEnv
  C.RShapeInit _ ->
    bindVarType v CTTuple typeEnv
  C.RShapeLast _ ->
    bindVarType v CTInt64 typeEnv
  C.RCall fn _ ->
    maybe typeEnv (\ct -> bindVarType v ct typeEnv) (inferCallType callTypes fn)
  _ ->
    typeEnv

inferStmtListTypes :: Map CVar CType -> TypeEnv -> [Stmt] -> TypeEnv
inferStmtListTypes callTypes initialEnv stmts = go initialEnv
  where
    go typeEnv =
      let typeEnv' = foldl (inferStmtTypes callTypes) typeEnv stmts
      in if typeEnv' == typeEnv then typeEnv else go typeEnv'

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
    typeEnv = inferStmtListTypes callTypes (procTypeEnv proc) (procBody proc)

inferProgramReturnTypes2 :: Program -> Map CVar CType
inferProgramReturnTypes2 (Program procs) = foldl addProc M.empty procs
  where
    addProc callTypes proc =
      case inferProcReturnType callTypes proc of
        Just retTy -> M.insert (procName proc) retTy callTypes
        Nothing -> callTypes

-- | Recover a richer per-procedure type environment from CFG structure and a
-- map of direct-call return types.
recoverProcTypeEnv2 :: Map CVar CType -> Proc -> TypeEnv
recoverProcTypeEnv2 callTypes proc =
  inferStmtListTypes callTypes (procTypeEnv proc) (procBody proc)
