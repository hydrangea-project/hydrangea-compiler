{-# OPTIONS_GHC -Wno-orphans #-}
-- |
-- Module: Language.Hydrangea.Pretty
--
-- Pretty-printing utilities for Hydrangea syntax, types, predicates, and
-- declarations.
module Language.Hydrangea.Pretty where
import Language.Hydrangea.Predicate
import Language.Hydrangea.Syntax
import Language.Hydrangea.Util (stripStringQuotes)
import Text.PrettyPrint.HughesPJClass
import Prelude hiding ((<>))
import Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Functor.Fixedpoint

instance Pretty (Name a) where
  pPrint (Name _ bs) = text $ unpack bs

instance Pretty (Dec a) where
  pPrint dec = text "let" <+> prettyDecNoLet dec

-- | Render a declaration body without the leading @let@ keyword.
prettyDecNoLet :: Dec a -> Doc
prettyDecNoLet (Dec _ nam pats mwhere mty body) =
  (text . unpack) nam
    <+> sep (map pPrint pats)
    <+> prettyWhere mwhere
    <+> prettyTy mty
    <+> text "="
    <+> pPrint body

-- | Render an optional where clause.
prettyWhere :: Maybe RefinePred -> Doc
prettyWhere Nothing     = empty
prettyWhere (Just rpred) = text "where" <+> pPrintRefinePred rpred

-- | Pretty-print a refinement term (used in where clauses).
pPrintRefineTerm :: RefineTerm -> Doc
pPrintRefineTerm (RTVar v)      = text (unpack v)
pPrintRefineTerm (RTConst n)    = int (fromIntegral n)
pPrintRefineTerm (RTDim v i)    = text "dim" <+> text (unpack v) <+> int i
pPrintRefineTerm (RTElem v)     = text "elem" <+> text (unpack v)
pPrintRefineTerm (RTAdd l r)    = parens (pPrintRefineTerm l <+> text "+" <+> pPrintRefineTerm r)
pPrintRefineTerm (RTSub l r)    = parens (pPrintRefineTerm l <+> text "-" <+> pPrintRefineTerm r)
pPrintRefineTerm (RTMul n t)    = parens (int (fromIntegral n) <+> text "*" <+> pPrintRefineTerm t)

-- | Pretty-print a refinement predicate (used in where clauses).
pPrintRefinePred :: RefinePred -> Doc
pPrintRefinePred (RPBound v e)    = text "bound" <+> text (unpack v) <+> parens (pPrintRefineTerm e)
pPrintRefinePred (RPRel op l r)   = pPrintRefineTerm l <+> pPrintRelOp op <+> pPrintRefineTerm r
pPrintRefinePred (RPAnd p q)      = pPrintRefinePred p <+> text "&" <+> pPrintRefinePred q

pPrintRelOp :: RelOp -> Doc
pPrintRelOp RLt  = text "<"
pPrintRelOp RLe  = text "<="
pPrintRelOp RGt  = text ">"
pPrintRelOp RGe  = text ">="
pPrintRelOp REq  = text "="
pPrintRelOp RNeq = text "<>"

-- | Render an optional type annotation prefix.
prettyTy :: Maybe Polytype -> Doc
prettyTy ty = case ty of
                Nothing -> empty
                Just annTy -> text ":" <+> pPrint annTy

instance (Pretty f) => Pretty (TypeF f) where
  pPrint (TyVarF v) = text (unpack v)
  pPrint (TyRefineF v t) = pPrint t <> text "{" <> text (unpack v) <> text "}"
  pPrint TyUnitF = text "()"
  pPrint TyIntF = text "Int"
  pPrint TyFloatF = text "Float"
  pPrint TyBoolF = text "Bool"
  pPrint TyStringF = text "String"
  pPrint (TyConsF t1 t2) = parens (pPrint t1 <> text ", " <> pPrint t2)
  pPrint (TyPairF t1 t2) = parens (pPrint t1 <> text ", " <> pPrint t2)
  pPrint (TyRecordF fields) =
    braces (sep (punctuate comma [text (unpack field) <+> text ":" <+> pPrint fieldTy | (field, fieldTy) <- fields]))
  pPrint (TyArrayF shape elt) = text "Array" <+> parens (pPrint shape) <+> pPrint elt
  pPrint (TyFunF t1 t2) = pPrint t1 <+> text "->" <+> pPrint t2

instance Pretty Type where
  pPrint (Fix t) = pPrint t

instance (Pretty t) => Pretty (Poly t) where
  pPrint (Forall [] [] t) = pPrint t
  pPrint (Forall vars preds t) =
    let headDoc = text "forall" <+> hsep (map (text . unpack) vars) <+> text "." <+> pPrint t
     in if null preds
          then headDoc
          else headDoc <+> text "where" <+> braces (sep (punctuate comma (map (pPrint . untagPred) preds)))

instance Pretty Term where
  pPrint term =
    case term of
      TVar v -> text (unpack v)
      TConst n -> int (fromIntegral n)
      TAdd l r -> parens (pPrint l <+> text "+" <+> pPrint r)
      TSub l r -> parens (pPrint l <+> text "-" <+> pPrint r)
      TMul l r -> parens (pPrint l <+> text "*" <+> pPrint r)
      TNeg t -> text "-" <> pPrint t
      TDim v i -> text "dim" <> parens (text (unpack v) <> text "," <+> int i)
      TValBoundDim v i -> text "vbound" <> parens (text (unpack v) <> text "," <+> int i)
      TMax l r -> text "max" <> parens (pPrint l <> text "," <+> pPrint r)

instance Pretty Pred where
  pPrint pred' =
    case pred' of
      PLt l r -> pPrint l <+> text "<" <+> pPrint r
      PLe l r -> pPrint l <+> text "<=" <+> pPrint r
      PEq l r -> pPrint l <+> text "=" <+> pPrint r
      PGe l r -> pPrint l <+> text ">=" <+> pPrint r
      PGt l r -> pPrint l <+> text ">" <+> pPrint r

instance Pretty (Exp a) where
  pPrint (EInt _ n) = int $ fromIntegral n
  pPrint (EFloat _ f) = text (show f)
  pPrint (EBool _ b) = text $ if b then "true" else "false"
  pPrint (EVar _ name) = text $ unpack name
  pPrint (EString _ bs) = doubleQuotes (text (unpack (stripStringQuotes bs)))
  pPrint (EUnit _) = text "()"
  pPrint (EVec _ exps) = brackets (sep (punctuate comma (map pPrint exps)))
  pPrint (EApp _ e1 e2) = pPrint e1 <+> pPrint e2
  pPrint (EIfThen _ cond e) = text "if" <+> pPrint cond <+> text "then" <+> pPrint e
  pPrint (EIfThenElse _ cond e1 e2) =
    text "if" <+> pPrint cond <+> text "then" <+> pPrint e1 <+> text "else" <+> pPrint e2
  pPrint (ENeg _ e) = text "-" <> pPrint e
  pPrint (EBinOp _ e1 op e2) = pPrint e1 <+> pPrint op <+> pPrint e2
  pPrint (EUnOp _ op e) = pPrint op <> pPrint e
  pPrint (EOp _ op) = pPrint op
  pPrint (ELetIn _ dec body) =
    parens (text "let" <+> prettyDecNoLet dec <+> text "in" <+> pPrint body)
  pPrint (EProj _ i e) = text "proj" <+> int (fromIntegral i) <+> pPrint e
  pPrint (EPair _ e1 e2) = parens (pPrint e1 <> text ", " <> pPrint e2)
  pPrint (ERecord _ fields) =
    braces (sep (punctuate comma [text (unpack field) <+> text "=" <+> pPrint fieldExp | (field, fieldExp) <- fields]))
  pPrint (ERecordProj _ e field) = pPrint e <> text "." <> text (unpack field)
  pPrint (EGenerate _ sz f) = text "generate" <+> pPrint sz <+> pPrint f
  pPrint (EMap _ f arr) = text "map" <+> pPrint f <+> pPrint arr
  pPrint (EZipWith _ f arr1 arr2) = text "zipwith" <+> pPrint f <+> pPrint arr1 <+> pPrint arr2
  pPrint (EReduce _ f z arr) = text "reduce" <+> pPrint f <+> pPrint z <+> pPrint arr
  pPrint (EReduceGenerate _ f z shape gen) =
    text "reduce_generate" <+> pPrint f <+> pPrint z <+> pPrint shape <+> pPrint gen
  pPrint (EFoldl _ f z arr) = text "foldl" <+> pPrint f <+> pPrint z <+> pPrint arr
  pPrint (EFoldlWhile _ p f z arr) = text "foldl_while" <+> pPrint p <+> pPrint f <+> pPrint z <+> pPrint arr
  pPrint (EScan _ f z arr) = text "scan" <+> pPrint f <+> pPrint z <+> pPrint arr
  pPrint (EScanInclusive _ f z arr) = text "scan_inclusive" <+> pPrint f <+> pPrint z <+> pPrint arr
  pPrint (EScanR _ f z arr) = text "scanr" <+> pPrint f <+> pPrint z <+> pPrint arr
  pPrint (EScanRInclusive _ f z arr) = text "scanr_inclusive" <+> pPrint f <+> pPrint z <+> pPrint arr
  pPrint (ESegmentedReduce _ f z offsets vals) =
    text "segmented_reduce" <+> pPrint f <+> pPrint z <+> pPrint offsets <+> pPrint vals
  pPrint (ESortIndices _ arr) = text "sort_indices" <+> pPrint arr
  pPrint (EIota _ n) = text "iota" <+> pPrint n
  pPrint (EMakeIndex _ n arr) = text "make_index" <+> pPrint n <+> pPrint arr
  pPrint (ECOOSumDuplicates _ nrows ncols nnz rows cols vals) =
    text "coo_sum_duplicates" <+> pPrint nrows <+> pPrint ncols <+> pPrint nnz
      <+> pPrint rows <+> pPrint cols <+> pPrint vals
  pPrint (ECSRFromSortedCOO _ nrows ncols nnz rows cols vals) =
    text "csr_from_sorted_coo" <+> pPrint nrows <+> pPrint ncols <+> pPrint nnz
      <+> pPrint rows <+> pPrint cols <+> pPrint vals
  pPrint (EPermute _ comb defaults permFn arr) =
    text "permute" <+> pPrint comb <+> pPrint defaults <+> pPrint permFn <+> pPrint arr
  pPrint (EScatter _ comb defaults idxArr vals) =
    text "scatter" <+> pPrint comb <+> pPrint defaults <+> pPrint idxArr <+> pPrint vals
  pPrint (EScatterGuarded _ comb defaults idxArr vals guardArr) =
    text "scatter_guarded" <+> pPrint comb <+> pPrint defaults <+> pPrint idxArr <+> pPrint vals <+> pPrint guardArr
  pPrint (EScatterGenerate _ comb defaults idxArr valFn) =
    text "scatter_generate" <+> pPrint comb <+> pPrint defaults <+> pPrint idxArr <+> pPrint valFn
  pPrint (EGather _ idxArr arr) = text "gather" <+> pPrint idxArr <+> pPrint arr
  pPrint (EIndex _ idx arr) = text "index" <+> pPrint idx <+> pPrint arr
  pPrint (ECheckIndex _ idx defVal arr) = text "check_index" <+> pPrint idx <+> pPrint defVal <+> pPrint arr
  pPrint (EFill _ shape val) = text "fill" <+> pPrint shape <+> pPrint val
  pPrint (EShapeOf _ arr) = text "shape_of" <+> pPrint arr
  pPrint (EReplicate _ dims arr) = text "replicate" <+> brackets (sep (punctuate comma (map pPrint dims))) <+> pPrint arr
  pPrint (ESlice _ dims arr) = text "slice" <+> brackets (sep (punctuate comma (map pPrint dims))) <+> pPrint arr
  pPrint (EReshape _ shape arr) = text "reshape" <+> pPrint shape <+> pPrint arr
  pPrint (EReadArray _ shape file) = text "read_array" <+> pPrint shape <+> pPrint file
  pPrint (EReadArrayFloat _ shape file) = text "read_array_float" <+> pPrint shape <+> pPrint file
  pPrint (EWriteArray _ arr file) = text "write_array" <+> pPrint arr <+> pPrint file
  pPrint (EWriteArrayFloat _ arr file) = text "write_array_float" <+> pPrint arr <+> pPrint file
  pPrint (EGetEnvInt _ var) = text "get_env_int" <+> pPrint var
  pPrint (EGetEnvString _ var) = text "get_env_string" <+> pPrint var
  pPrint (EStencil _ bnd f arr) =
    text "stencil" <+> pPrint bnd <+> pPrint f <+> pPrint arr
  pPrint (EBoundLetIn _ x boundE rhs body) =
    parens (text "let" <+> text (unpack x) <+> text "bound" <+> pPrint boundE
            <+> text "=" <+> pPrint rhs <+> text "in" <+> pPrint body)

instance Pretty (BoundaryCondition a) where
  pPrint BClamp        = text "clamp"
  pPrint BWrap         = text "wrap"
  pPrint BMirror       = text "mirror"
  pPrint (BConst v)    = parens (text "constant" <+> pPrint v)

instance Pretty (UnOperator a) where
  pPrint (Not _)    = text "not"
  pPrint (Fst _)    = text "fst"
  pPrint (Snd _)    = text "snd"
  pPrint (Sqrt   _) = text "sqrt"
  pPrint (ExpF   _) = text "expf"
  pPrint (Log    _) = text "log"
  pPrint (Sin    _) = text "sin"
  pPrint (Cos    _) = text "cos"
  pPrint (AbsF   _) = text "abs_f"
  pPrint (FloorF _) = text "floor_f"
  pPrint (CeilF  _) = text "ceil_f"
  pPrint (Erf    _) = text "erf"
  pPrint (FloatOf _) = text "float_of"


instance Pretty (Operator a) where
  pPrint (Plus _) = text "+"
  pPrint (Minus _) = text "-"
  pPrint (Times _) = text "*"
  pPrint (Divide _) = text "/"
  pPrint (Mod _) = text "%"
  pPrint (Eq _) = text "=="
  pPrint (Neq _) = text "<>"
  pPrint (Lt _) = text "<"
  pPrint (Le _) = text "<="
  pPrint (Gt _) = text ">"
  pPrint (Ge _) = text ">="
  pPrint (And _) = text "&&"
  pPrint (Or _) = text "||"
  pPrint (PlusF _) = text "+."
  pPrint (MinusF _) = text "-."
  pPrint (TimesF _) = text "*."
  pPrint (DivideF _) = text "/."
  pPrint (EqF _) = text "=."
  pPrint (NeqF _) = text "<>."
  pPrint (LtF _) = text "<."
  pPrint (LeF _) = text "<=."
  pPrint (GtF _) = text ">."
  pPrint (GeF _) = text ">=."

instance Pretty (Pat a) where
  pPrint (PVar _ nam) = parens (text $ unpack nam)
  pPrint (PVec _ pats) =
    brackets (sep (punctuate (text ",") (map pPrint pats)))
  pPrint (PBound _ nam e) = text (unpack nam) <+> text "bound" <+> pPrint e
  pPrint (PPair _ p1 p2) = parens (pPrint p1 <> text "," <+> pPrint p2)

instance Pretty (ShapeDim a) where
  pPrint dim =
    case dim of
      ShapeAll _ -> text "All"
      ShapeAny _ e -> text "Any" <+> pPrint e
      ShapeDim _ e -> pPrint e

instance Pretty (SliceDim a) where
  pPrint dim =
    case dim of
      SliceAll _ -> text "All"
      SliceRange _ start len -> brackets (pPrint start <> text ", " <> pPrint len)
