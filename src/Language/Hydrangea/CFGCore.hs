{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.CFGCore
--
-- Shared core IR atoms and RHS nodes used by CFG analysis/transforms/codegen.
module Language.Hydrangea.CFGCore
  ( CVar
  , BinOp(..)
  , UnOp(..)
  , Atom(..)
  , RHS(..)
  , Redop(..)
  , CElemType(..)
  , CType(..)
  , elemTypeToCType
  , ctypeToElemType
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)

-- | Canonical variable name used across CFG representations.
type CVar = ByteString

-- | Binary operators used in @RBinOp@ and vector/bin-op RHS forms.
data BinOp = CAdd | CSub | CMul | CDiv | CMod | CEq | CNeq | CLt | CLe | CGt | CGe | CAnd | COr
           | CAddF | CSubF | CMulF | CDivF | CEqF | CNeqF | CLtF | CLeF | CGtF | CGeF
  deriving (Eq, Show)

-- | Unary operators.
data UnOp = CNot | CNeg
  -- Math functions (Float -> Float); lowered to <math.h> calls.
  | CSqrt | CExpF | CLog | CSin | CCos | CAbsF | CFloorF | CCeilF | CErf | CFloatOf | CIntOf
  deriving (Eq, Show)

-- | Evaluatable, side-effect-free atoms. @AVecVar@ names a vector/array
-- variable reference.
data Atom = AVar CVar | AInt Integer | AFloat Double | ABool Bool | AUnit | AString ByteString | AVecVar CVar
  deriving (Eq, Show)

-- | Element type descriptor for pair construction, used to carry type
-- information through the CFG IR so the code generator can emit the
-- correct C struct type without a separate type environment.
data CElemType = CEInt | CEFloat | CEBool | CEPair CElemType CElemType
  deriving (Eq, Show, Ord)

-- | Comprehensive concrete C type for all values that appear in generated code.
-- This is the authoritative type representation threaded from type inference
-- through lowering into the CFG and code generator.
--
-- 'CTRecord' is a forward-compatible placeholder for named struct types
-- (records); it carries an ordered list of (field name, field type) pairs.
data CType
  = CTInt64                           -- ^ int64_t
  | CTDouble                          -- ^ double
  | CTBool                            -- ^ bool (int)
  | CTUnit                            -- ^ unit (void / unused)
  | CTTuple                           -- ^ shape tuple (hyd_tuple_t; components are all int)
  | CTArray CType                     -- ^ hyd_array_t* of element type
  | CTPair CType CType                -- ^ hyd_pair_XY_t struct
  | CTRecord [(ByteString, CType)]    -- ^ named struct (for future records)
  deriving (Eq, Ord, Show)

-- | Lift a 'CElemType' (pair-component descriptor) to the full 'CType'.
elemTypeToCType :: CElemType -> CType
elemTypeToCType CEInt           = CTInt64
elemTypeToCType CEFloat         = CTDouble
elemTypeToCType CEBool          = CTBool
elemTypeToCType (CEPair ct1 ct2) = CTPair (elemTypeToCType ct1) (elemTypeToCType ct2)

-- | Project a 'CType' back to 'CElemType', if it is representable as one.
-- Returns 'Nothing' for types that cannot appear as pair components
-- (arrays, tuples, records).
ctypeToElemType :: CType -> Maybe CElemType
ctypeToElemType CTInt64           = Just CEInt
ctypeToElemType CTDouble          = Just CEFloat
ctypeToElemType CTBool            = Just CEBool
ctypeToElemType (CTPair ct1 ct2)  = CEPair <$> ctypeToElemType ct1 <*> ctypeToElemType ct2
ctypeToElemType _                 = Nothing

-- | Right-hand-side expressions for @SAssign@ in the CFG core. These are
-- intentionally low-level to map directly to imperative operations and
-- code generation templates.
data RHS
  = RAtom Atom
  | RBinOp BinOp Atom Atom
  | RUnOp UnOp Atom
  | RTuple [Atom]
  | RProj Integer Atom
  | RRecord [(ByteString, Atom)]
  | RRecordProj ByteString Atom
  | RPairMake CElemType CElemType Atom Atom
  | RPairFst CElemType Atom    -- ^ CElemType is the type of the extracted fst field
  | RPairSnd CElemType Atom    -- ^ CElemType is the type of the extracted snd field
  | RArrayAlloc Atom
  | RArrayLoad Atom Atom
  | RArrayShape Atom
  | RShapeSize Atom
  | RShapeInit Atom
  | RShapeLast Atom
  | RFlatToNd Atom Atom
  | RNdToFlat Atom Atom
  | R2DToFlat Atom Atom
  | RCall CVar [Atom]
  | RVecLoad Atom Atom
  | RVecStore Atom Atom Atom
  | RVecBinOp BinOp Atom Atom
  | RVecUnOp UnOp Atom
  | RVecSplat Atom
  | RVecReduce BinOp Atom
  deriving (Eq, Show)

-- | Reduction operators used by @ReductionSpec@ in CFG.
data Redop = RAdd | RMul
  deriving (Eq, Show)
