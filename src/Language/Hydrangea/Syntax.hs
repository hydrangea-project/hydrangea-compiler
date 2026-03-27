{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Module: Language.Hydrangea.Syntax

Abstract syntax for the Hydrangea source language: types, expressions,
declarations, and patterns.

Types use a functor @'TypeF'@ with an outer @'Fix'@ (aliased as @'Type'@).
The same functor is reused for unification-backed types (@'UType'@), which
are @UTerm TypeF IntVar@.  Refinement wrappers (@'TyRefineF'@) bind a
symbolic name to a value for use in refinement predicates stored on
@'Polytype'@ schemes.

Expression nodes carry a generic annotation @a@ (typically a source
@'Range'@) so that parsers, error reporters, and later passes can attach
position information.
-}
module Language.Hydrangea.Syntax where

import Control.Unification hiding (applyBindings, (=:=))
import Control.Unification.IntVar
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Eq.Deriving
import Data.Functor.Fixedpoint
import Data.List (sortOn)
import Data.Ord.Deriving
import GHC.Generics (Generic1)
import Language.Hydrangea.Predicate (TaggedPred)
import Text.Show.Deriving

-- | Variables in our syntax are just bytestrings
type Var = ByteString

deriving instance Unifiable []
deriving instance Unifiable ((,) Var)

-- | Pattern synonym for a type variable node.
pattern TyVar :: Var -> Type
pattern TyVar v = Fix (TyVarF v)

-- | Pattern synonym for the boolean type.
pattern TyBool :: Type
pattern TyBool = Fix TyBoolF

-- | Pattern synonym for the integer type.
pattern TyInt :: Type
pattern TyInt = Fix TyIntF

-- | Pattern synonym for the floating-point type.
pattern TyFloat :: Type
pattern TyFloat = Fix TyFloatF

-- | Pattern synonym for the unit type.
pattern TyUnit :: Type
pattern TyUnit = Fix TyUnitF

-- | Pattern synonym for the string type.
pattern TyString :: Type
pattern TyString = Fix TyStringF

-- | Pattern synonym for a pair type.
pattern TyPair :: Type -> Type -> Type
pattern TyPair t1 t2 = Fix (TyPairF t1 t2)

-- | Pattern synonym for a structural record type.
pattern TyRecord :: [(Var, Type)] -> Type
pattern TyRecord fields = Fix (TyRecordF fields)

-- | Pattern synonym for the inductive tuple form used for shapes.
pattern TyCons :: Type -> Type -> Type
pattern TyCons t1 t2 = Fix (TyConsF t1 t2)

-- | Pattern synonym for a function type.
pattern TyFun :: Type -> Type -> Type
pattern TyFun t1 t2 = Fix (TyFunF t1 t2)

-- | Pattern synonym for an array type carrying shape and element type.
pattern TyArray :: Type -> Type -> Type
pattern TyArray shape elt = Fix (TyArrayF shape elt)

-- | Pattern synonym for a refinement wrapper around a base type.
pattern TyRefine :: Var -> Type -> Type
pattern TyRefine v t = Fix (TyRefineF v t)

-- | Shape functor for Hydrangea types.  A concrete 'Type' is @Fix TypeF@;
-- a unification type is @UTerm TypeF IntVar@.
data TypeF f
  = TyVarF Var           -- ^ Type variable.
  | TyRefineF Var f      -- ^ Refinement wrapper binding a symbolic name to a type.
  | TyUnitF              -- ^ Unit type @()@.
  | TyIntF               -- ^ Integer type.
  | TyFloatF             -- ^ Double-precision floating-point type.
  | TyBoolF              -- ^ Boolean type.
  | TyStringF            -- ^ String type.
  | TyConsF f f          -- ^ Inductive tuple cons used for shapes: @Int * Int * ()@.
  | TyPairF f f          -- ^ Heterogeneous product of two types; lowers to a C struct.
  | TyRecordF [(Var, f)] -- ^ Structural record with named fields in canonical order.
  | TyArrayF f f         -- ^ Array with a shape (inductive tuple) and an element type.
  | TyFunF f f           -- ^ Function type.
  deriving (Functor, Foldable, Traversable, Generic1, Unifiable)

-- | Create a type variable node
tyVar :: Var -> Fix TypeF
tyVar v = Fix $ TyVarF v

-- | Create a unit type node
tyUnit :: Fix TypeF
tyUnit = Fix TyUnitF

-- | Create an int type node
tyInt :: Fix TypeF
tyInt = Fix TyIntF

-- | Create a float type node
tyFloat :: Fix TypeF
tyFloat = Fix TyFloatF

-- | Create a bool type node
tyBool :: Fix TypeF
tyBool = Fix TyBoolF

-- | Create a string type node
tyString :: Fix TypeF
tyString = Fix TyStringF

-- | Create a pair type node
tyPair :: Fix TypeF -> Fix TypeF -> Fix TypeF
tyPair t1 t2 = Fix $ TyPairF t1 t2

-- | Sort record fields into canonical name order.
normalizeRecordFields :: [(Var, a)] -> [(Var, a)]
normalizeRecordFields = sortOn fst

-- | Create a record type node with fields canonicalized by name.
tyRecord :: [(Var, Fix TypeF)] -> Fix TypeF
tyRecord = Fix . TyRecordF . normalizeRecordFields

-- | Create an inductive tuple node
tyCons :: Fix TypeF -> Fix TypeF -> Fix TypeF
tyCons t1 t2 = Fix $ TyConsF t1 t2

-- | Create an array type node from a shape type and an element type.
tyArray :: Fix TypeF -> Fix TypeF -> Fix TypeF
tyArray shape elt = Fix $ TyArrayF shape elt

-- | Create a refinement wrapper with the given binder name.
tyRefine :: Var -> Fix TypeF -> Fix TypeF
tyRefine v t = Fix $ TyRefineF v t

-- | Create a function type node
tyFun :: Fix TypeF -> Fix TypeF -> Fix TypeF
tyFun t1 t2 = Fix $ TyFunF t1 t2

deriving instance (Show f) => Show (TypeF f)

deriving instance (Eq f) => Eq (TypeF f)

deriving instance (Ord f) => Ord (TypeF f)

-- | Concrete Hydrangea type: the fixed point of 'TypeF'.
type Type = Fix TypeF

-- | Hydrangea type with unification variables: @UTerm TypeF IntVar@.
type UType = UTerm TypeF IntVar

-- | Universally quantified type scheme with tagged refinement predicates.
--
-- Predicates are split into hypotheses (@Hyp@, known facts) and obligations
-- (@Obl@, safety conditions that must be entailed).  The @Pred@ alias is kept
-- for backward compatibility in patterns that peel @untagPred@.
data Poly t = Forall [Var] [TaggedPred] t
  deriving (Functor, Eq, Show, Ord)

-- | Fully elaborated polymorphic types over concrete syntax types.
type Polytype = Poly Type

$(deriveShow1 ''TypeF)
$(deriveEq1 ''TypeF)
$(deriveOrd1 ''TypeF)

-- | Polymorphic types whose bodies may still contain unification variables.
type UPolytype = Poly UType

-- | An annotated name (distinct from 'Var', which carries no annotation).
data Name a = Name a ByteString
  deriving (Functor, Foldable, Eq, Show)

-- | Convert a @Name@ to a @Var@ by stripping the annotation.
unName :: Name a -> Var
unName (Name _ bs) = bs

-- | Top-level or local value/function declaration.
data Dec a
  = Dec a Var [Pat a] (Maybe Polytype) (Exp a)
  deriving (Functor, Foldable)

deriving instance (Show a) => Show (Dec a)

deriving instance (Eq a) => Eq (Dec a)

deriving instance (Ord a) => Ord (Dec a)

-- | Extract the binding name from a declaration.
decName :: Dec a -> Var
decName (Dec _ name _ _ _) = name

-- | Extract an optional user-supplied polytype annotation from a declaration.
decPolyTy :: Dec a -> Maybe Polytype
decPolyTy (Dec _ _ _ polyty _) = polyty

-- | Expression AST parameterised by an annotation @a@ (typically a source 'Range').
data Exp a
  = -- | Integer literal with its annotation and numeric value
    EInt a Integer
  | -- | Float literal with its annotation and numeric value (double precision)
    EFloat a Double
  | -- | Variable reference (annotation, variable name)
    EVar a Var
  | -- | String literal (annotation, contents)
    EString a ByteString
  | -- | Unit value @()@ (annotation)
    EUnit a
  | -- | Boolean literal (annotation, value)
    EBool a Bool
  | -- | Vector/tuple construction; a list of element expressions
    EVec a [Exp a]
  | -- | Function/application: @f x@
    EApp a (Exp a) (Exp a)
  | -- | If-then expression without an @else@ branch (treated as unit/partial)
    EIfThen a (Exp a) (Exp a)
  | -- | If-then-else expression (condition, then-branch, else-branch)
    EIfThenElse a (Exp a) (Exp a) (Exp a)
  | -- | Unary numeric negation: @-e@
    ENeg a (Exp a)
  | -- | Binary operator application (lhs, operator, rhs)
    EBinOp a (Exp a) (Operator a) (Exp a)
  | -- | Unary operator application (e.g. @fst@, @snd@, @not@)
    EUnOp a (UnOperator a) (Exp a)
  | -- | Operator value used as a first-class operator
    EOp a (Operator a)
  | -- | Let-binding: @let ... in ...@ (annotation, declaration, body)
    ELetIn a (Dec a) (Exp a)
  | -- | Projection from an inductive tuple by index (annotation, index, tuple)
    EProj a Integer (Exp a)
  | -- | Pair construction: (e1, e2) — a product of two values of arbitrary types.
    -- Distinct from shape vec literals ([e1, e2]) and compiles to a C struct.
    EPair a (Exp a) (Exp a)
  | -- | Record construction with named fields, canonicalized by field name.
    ERecord a [(Var, Exp a)]
  | -- | Named field projection from a record.
    ERecordProj a (Exp a) Var
  | -- | Generate an immutable array: size (int) and a function (index -> value)
    EGenerate a (Exp a) (Exp a)
  | -- | Map a function over an array: function, array
    EMap a (Exp a) (Exp a)
  | -- | Zip two arrays with a binary function
    EZipWith a (Exp a) (Exp a) (Exp a)
  | -- | Reduce an array along the trailing/rightmost axis (rank-lowering): function, initial value, array
    EReduce a (Exp a) (Exp a) (Exp a)
  | -- | Reduce a generated array along the trailing axis without materializing it: function, init, shape, generator
    EReduceGenerate a (Exp a) (Exp a) (Exp a) (Exp a)
  | -- | Strict left fold over a 1-D array; the accumulator type may differ from the element type.
    -- @foldl : (s -> a -> s) -> s -> Array[n, a] -> s@
    EFoldl a (Exp a) (Exp a) (Exp a)
  | -- | Exclusive prefix scan over a 1-D array.
    -- @scan : (s -> a -> s) -> s -> Array[n, a] -> Array[n, s]@
    -- The output at position @i@ is the state /before/ consuming element @i@.
    EScan a (Exp a) (Exp a) (Exp a)
  | -- | Segmented reduction: reduces each @vals[offsets[i] : offsets[i+1]]@ from left to right.
    -- @segmented_reduce : (s -> a -> s) -> s -> offsets -> vals -> Array[nsegs, s]@
    ESegmentedReduce a (Exp a) (Exp a) (Exp a) (Exp a)
  | -- | Stable sort returning the permutation of indices that sorts the input keys in ascending order.
    -- @sort_indices : Array[n, Int] -> Array[n, Int]@
    ESortIndices a (Exp a)
  | -- | Generate the integer sequence [0, 1, …, n-1].
    -- Elements are guaranteed to be valid indices into any array of size n.
    -- @iota : Int -> Array[n, Int]@
    EIota a (Exp a)
  | -- | Assert that every element of an integer array is a valid index into an array of size N.
    -- This is a type-level annotation (no runtime check); the user is responsible for
    -- correctness.  Gather-safety checking will accept the result as an index array for
    -- any source array whose first dimension is at least N.
    -- @make_index : Int -> Array[s, Int] -> Array[s, Int]@
    EMakeIndex a (Exp a) (Exp a)
  | -- | Sum duplicate entries of a COO sparse matrix in sorted row-major order.
    -- @coo_sum_duplicates : nrows -> ncols -> nnz -> rows -> cols -> vals -> COO@
    ECOOSumDuplicates a (Exp a) (Exp a) (Exp a) (Exp a) (Exp a) (Exp a)
  | -- | Convert a sorted COO sparse matrix to CSR format.
    -- @csr_from_sorted_coo : nrows -> ncols -> nnz -> rows -> cols -> vals -> CSR@
    ECSRFromSortedCOO a (Exp a) (Exp a) (Exp a) (Exp a) (Exp a) (Exp a)
  | -- | Permute elements into a default array using an index mapping
    EPermute a (Exp a) (Exp a) (Exp a) (Exp a)
  | -- | Scatter values into a default array using an index array
    EScatter a (Exp a) (Exp a) (Exp a) (Exp a)
  | -- | Scatter values into a default array using an index array,
    --   but only contribute source positions whose guard array entry is true.
    --   Fields: combine, defaults, index array, values array, guard array.
    EScatterGuarded a (Exp a) (Exp a) (Exp a) (Exp a) (Exp a)
  | -- | Scatter generated values without materialising an intermediate values array.
    --   Equivalent to @EScatter c d idx (EGenerate (EShapeOf idx) valFn)@.
    --   Fields: combine, defaults, index array, value generator function.
    EScatterGenerate a (Exp a) (Exp a) (Exp a) (Exp a)
  | -- | Gather values from an array using an index array
    EGather a (Exp a) (Exp a)
  | -- | Index into an array: index, array
    EIndex a (Exp a) (Exp a)
  | -- | Check index into an array with default value: index, default value, array
    ECheckIndex a (Exp a) (Exp a) (Exp a)
  | -- | Fill an array of the given shape with a constant value
    EFill a (Exp a) (Exp a)
  | -- | Extract the shape of an array as a tuple of integers
    EShapeOf a (Exp a)
  | -- | Replicate an array according to a shape specification
    EReplicate a [ShapeDim a] (Exp a)
  | -- | Slice an array using per-dimension slice specifications
    ESlice a [SliceDim a] (Exp a)
  | -- | Reshape an array to a new shape
    EReshape a (Exp a) (Exp a)
  | -- | Read an array from a CSV file: read_array shape "file.csv"
    EReadArray a (Exp a) (Exp a)
  | -- | Read a float array from a CSV file: read_array_float shape "file.csv"
    EReadArrayFloat a (Exp a) (Exp a)
  | -- | Write an array to a CSV file: write_array arr "file.csv"
    EWriteArray a (Exp a) (Exp a)
  | -- | Write a float array to a CSV file: write_array_float arr "file.csv"
    EWriteArrayFloat a (Exp a) (Exp a)
  | -- | Read an integer from an environment variable: get_env_int "VAR"
    EGetEnvInt a (Exp a)
  | -- | Read a string from an environment variable: get_env_string "VAR"
    EGetEnvString a (Exp a)
  | -- | Stencil computation: boundary condition, accessor function, source array.
    -- The accessor function receives a curried offset function @acc@ such that
    -- @acc d@ (1D) or @acc d0 d1@ (2D) returns the element at
    -- @current_index + offset@ subject to the boundary condition.
    -- stencil : Boundary -> ((Int -> a) -> b) -> Array[sh, a] -> Array[sh, b]
    EStencil a (BoundaryCondition a) (Exp a) (Exp a)
  | -- | Bound-annotated let-in: @let x bound E = rhs in body@.
    -- Asserts (and if possible verifies statically) that @rhs@ evaluates to a
    -- value in @[0, E)@, then binds @x@ with that value-bound annotation for
    -- propagation through @body@.  Identity at runtime.
    EBoundLetIn a Var (Exp a) (Exp a) (Exp a)
  deriving (Functor, Foldable)

-- | Boundary condition for stencil operations.
-- Determines what value is returned when an offset index falls outside the
-- array's valid range.
data BoundaryCondition a
  = -- | Clamp: repeat the nearest edge element.
    BClamp
  | -- | Wrap: treat the array as periodic (modular indexing).
    BWrap
  | -- | Mirror: reflect at the boundary (fold-back indexing).
    BMirror
  | -- | Constant: return a user-supplied default value for out-of-bounds accesses.
    BConst (Exp a)
  deriving (Functor, Foldable, Show, Eq, Ord)

-- | Shape dimensions for replicate specifications.
data ShapeDim a
  = -- | Preserve a dimension from the source array.
    ShapeAll a
  | -- | Introduce a new dimension with an explicit size.
    ShapeAny a (Exp a)
  | -- | Introduce a new dimension sized by an expression.
    ShapeDim a (Exp a)
  deriving (Functor, Foldable, Show, Eq, Ord)

-- | Slice dimensions for array slicing.
data SliceDim a
  = -- | Preserve the full dimension.
    SliceAll a
  | -- | Slice a dimension by (start, length).
    SliceRange a (Exp a) (Exp a)
  deriving (Functor, Foldable, Show, Eq, Ord)

-- | Extract the outer annotation carried by an expression node.
firstParam :: Exp a -> a
firstParam e = case e of
  EInt a _ -> a
  EFloat a _ -> a
  EVar a _ -> a
  EString a _ -> a
  EUnit a -> a
  EBool a _ -> a
  EVec a _ -> a
  EApp a _ _ -> a
  EIfThen a _ _ -> a
  EIfThenElse a _ _ _ -> a
  ENeg a _ -> a
  EBinOp a _ _ _ -> a
  EUnOp a _ _ -> a
  EOp a _ -> a
  ELetIn a _ _ -> a
  EProj a _ _ -> a
  EPair a _ _ -> a
  ERecord a _ -> a
  ERecordProj a _ _ -> a
  EGenerate a _ _ -> a
  EMap a _ _ -> a
  EZipWith a _ _ _ -> a
  EReduce a _ _ _ -> a
  EReduceGenerate a _ _ _ _ -> a
  EFoldl a _ _ _ -> a
  EScan a _ _ _ -> a
  ESegmentedReduce a _ _ _ _ -> a
  ESortIndices a _ -> a
  EIota a _ -> a
  EMakeIndex a _ _ -> a
  ECOOSumDuplicates a _ _ _ _ _ _ -> a
  ECSRFromSortedCOO a _ _ _ _ _ _ -> a
  EPermute a _ _ _ _ -> a
  EScatter a _ _ _ _ -> a
  EScatterGuarded a _ _ _ _ _ -> a
  EScatterGenerate a _ _ _ _ -> a
  EGather a _ _ -> a
  EIndex a _ _ -> a
  ECheckIndex a _ _ _ -> a
  EFill a _ _ -> a
  EShapeOf a _ -> a
  EReplicate a _ _ -> a
  ESlice a _ _ -> a
  EReshape a _ _ -> a
  EReadArray a _ _ -> a
  EReadArrayFloat a _ _ -> a
  EWriteArray a _ _ -> a
  EWriteArrayFloat a _ _ -> a
  EGetEnvInt a _ -> a
  EGetEnvString a _ -> a
  EStencil a _ _ _ -> a
  EBoundLetIn a _ _ _ _ -> a

deriving instance (Show a) => Show (Exp a)

deriving instance (Eq a) => Eq (Exp a)

deriving instance (Ord a) => Ord (Exp a)

-- | Unary operators in surface syntax.
data UnOperator a
  = Not a     -- ^ Logical negation.
  | Fst a     -- ^ First projection.
  | Snd a     -- ^ Second projection.
  | Sqrt a    -- ^ Square root (@Float -> Float@).
  | ExpF a    -- ^ Natural exponential (@Float -> Float@).
  | Log a     -- ^ Natural logarithm (@Float -> Float@).
  | Sin a     -- ^ Sine (@Float -> Float@).
  | Cos a     -- ^ Cosine (@Float -> Float@).
  | AbsF a    -- ^ Absolute value (@Float -> Float@).
  | FloorF a  -- ^ Floor (@Float -> Float@).
  | CeilF a   -- ^ Ceiling (@Float -> Float@).
  | Erf a     -- ^ Error function (@Float -> Float@).
  | FloatOf a -- ^ Integer-to-float cast (@Int -> Float@).
  deriving (Functor, Foldable, Show, Eq, Ord)

-- | Binary operators in surface syntax.
data Operator a
  = Plus a    -- ^ Integer addition.
  | Minus a   -- ^ Integer subtraction.
  | Times a   -- ^ Integer multiplication.
  | Divide a  -- ^ Integer division.
  | Mod a     -- ^ Integer modulo (non-negative result; `x % m` is in [0, m) for m > 0).
  | Eq a      -- ^ Integer equality.
  | Neq a     -- ^ Integer inequality.
  | Lt a      -- ^ Integer less-than.
  | Le a      -- ^ Integer less-than-or-equal.
  | Gt a      -- ^ Integer greater-than.
  | Ge a      -- ^ Integer greater-than-or-equal.
  | And a     -- ^ Boolean conjunction.
  | Or a      -- ^ Boolean disjunction.
  | PlusF a   -- ^ Float addition.
  | MinusF a  -- ^ Float subtraction.
  | TimesF a  -- ^ Float multiplication.
  | DivideF a -- ^ Float division.
  | EqF a     -- ^ Float equality.
  | NeqF a    -- ^ Float inequality.
  | LtF a     -- ^ Float less-than.
  | LeF a     -- ^ Float less-than-or-equal.
  | GtF a     -- ^ Float greater-than.
  | GeF a     -- ^ Float greater-than-or-equal.
  deriving (Functor, Foldable, Show, Eq, Ord)

-- | Patterns used for function parameters and destructuring binds.
data Pat a
  = PVar a Var
  | PVec a [Pat a]
  | -- | Bound-annotated index pattern: @[i bound E]@.
    -- Declares that the index variable @i@ is in @[0, E)@.  Used inside
    -- @generate@ to enable value-bound propagation to the output array.
    PBound a Var (Exp a)
  deriving (Functor, Foldable)

instance (Show a) => Show (Pat a) where
  show (PVar a v) = "PVar (" ++ show a ++ ") " ++ show v
  show (PVec a ps) = "PVec (" ++ show a ++ ") " ++ show ps
  show (PBound a v _) = "PBound (" ++ show a ++ ") " ++ show v ++ " <expr>"

instance (Eq a) => Eq (Pat a) where
  PVar a1 v1       == PVar a2 v2       = a1 == a2 && v1 == v2
  PVec a1 ps1      == PVec a2 ps2      = a1 == a2 && ps1 == ps2
  PBound a1 v1 _   == PBound a2 v2 _   = a1 == a2 && v1 == v2
  _                == _                = False

instance (Ord a) => Ord (Pat a) where
  compare (PVar a1 v1)     (PVar a2 v2)     = compare a1 a2 <> compare v1 v2
  compare (PVec a1 ps1)    (PVec a2 ps2)    = compare a1 a2 <> compare ps1 ps2
  compare (PBound a1 v1 _) (PBound a2 v2 _) = compare a1 a2 <> compare v1 v2
  compare (PVar _ _)       _                = LT
  compare _                (PVar _ _)       = GT
  compare (PVec _ _)       _                = LT
  compare _                (PVec _ _)       = GT
