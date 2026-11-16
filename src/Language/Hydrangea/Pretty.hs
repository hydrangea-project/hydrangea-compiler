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
prettyDecNoLet (Dec _ nam pats mwhere ann body) =
  (if decAnnRegroup ann then text "regroup" else empty)
    <+> (text . unpack) nam
    <+> hsep (map pPrint pats)
    <+> prettyWhere mwhere
    <+> prettyTy (decAnnType ann)
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
      TDiv l r -> parens (pPrint l <+> text "/" <+> pPrint r)
      TMod l r -> parens (pPrint l <+> text "%" <+> pPrint r)

instance Pretty Pred where
  pPrint pred' =
    case pred' of
      PLt l r -> pPrint l <+> text "<" <+> pPrint r
      PLe l r -> pPrint l <+> text "<=" <+> pPrint r
      PEq l r -> pPrint l <+> text "=" <+> pPrint r
      PNeq l r -> pPrint l <+> text "<>" <+> pPrint r
      PGe l r -> pPrint l <+> text ">=" <+> pPrint r
      PGt l r -> pPrint l <+> text ">" <+> pPrint r

-- | Expressions print so that the parser reads them back as the same tree:
-- 'pprExp' takes the precedence of the surrounding context and parenthesises
-- only where the grammar requires it.  The levels mirror the operator table
-- in @Parser.y@ (higher binds tighter):
--
-- * 0: @let@, @if@ and anything else that extends as far right as it can
-- * 1–8: binary operators, from @|@ up to @*.@ @/.@
-- * 'specialPrec': builtin forms such as @map f xs@ (their arguments are atoms)
-- * 'appPrec': function application
-- * 'atomPrec': literals, variables, brackets
instance Pretty (Exp a) where
  pPrint = pprExp 0

specialPrec, appPrec, atomPrec :: Int
specialPrec = 9
appPrec = 10
atomPrec = 11

data Assoc = AssocLeft | AssocNone

-- | Precedence level and associativity of a binary operator.
opFixity :: Operator a -> (Int, Assoc)
opFixity op = case op of
  Or _      -> (1, AssocLeft)
  And _     -> (2, AssocLeft)
  Eq _      -> (3, AssocNone)
  Neq _     -> (3, AssocNone)
  Lt _      -> (3, AssocNone)
  Le _      -> (3, AssocNone)
  Gt _      -> (3, AssocNone)
  Ge _      -> (3, AssocNone)
  Plus _    -> (4, AssocLeft)
  Minus _   -> (4, AssocLeft)
  Times _   -> (5, AssocLeft)
  Divide _  -> (5, AssocLeft)
  Mod _     -> (5, AssocLeft)
  EqF _     -> (6, AssocNone)
  NeqF _    -> (6, AssocNone)
  LtF _     -> (6, AssocNone)
  LeF _     -> (6, AssocNone)
  GtF _     -> (6, AssocNone)
  GeF _     -> (6, AssocNone)
  PlusF _   -> (7, AssocLeft)
  MinusF _  -> (7, AssocLeft)
  TimesF _  -> (8, AssocLeft)
  DivideF _ -> (8, AssocLeft)

-- | Prefix minus parses at the level of binary @-@.
negPrec :: Int
negPrec = 4

-- | Parenthesise when the context binds tighter than the expression.
wrap :: Int -> Int -> Doc -> Doc
wrap ctx own d = if ctx > own then parens d else d

-- | An argument of a builtin or of an application.
atom :: Exp a -> Doc
atom = pprExp atomPrec

-- | A builtin form: keyword followed by atomic arguments.
special :: Int -> String -> [Doc] -> Doc
special ctx kw args = wrap ctx specialPrec (text kw <+> hsep args)

-- | A bracketed, comma-separated list.
commaList :: [Doc] -> Doc
commaList = brackets . sep . punctuate comma

pprExp :: Int -> Exp a -> Doc
pprExp ctx expr = case expr of
  -- Literals.  The lexer has no negative literals (a leading '-' is prefix
  -- minus), so a negative constant is bracketed to stay a single argument.
  EInt _ n -> negLit (n < 0) (integer n)
  EFloat _ f -> negLit (f < 0) (text (show f))
  EBool _ b -> text (if b then "true" else "false")
  EVar _ name -> text (unpack name)
  EString _ bs -> doubleQuotes (text (unpack (stripStringQuotes bs)))
  EUnit _ -> text "()"
  EOp _ op -> parens (pPrint op)
  EVec _ es -> commaList (map (pprExp 0) es)
  EPair _ a b -> parens (pprExp 0 a <> comma <+> pprExp 0 b)
  ERecord _ fields ->
    braces (sep (punctuate comma [text (unpack f) <+> text "=" <+> pprExp 0 e | (f, e) <- fields]))
  ERecordProj _ e field -> atom e <> text "." <> text (unpack field)
  EProj _ i e -> text "proj" <+> integer i <+> atom e

  EApp _ f x -> wrap ctx appPrec (pprExp appPrec f <+> atom x)
  ENeg _ e -> wrap ctx negPrec (text "-" <> pprExp (negPrec + 1) e)
  EBinOp _ l op r ->
    let (lvl, assoc) = opFixity op
        lctx = case assoc of { AssocLeft -> lvl; AssocNone -> lvl + 1 }
     in wrap ctx lvl (pprExp lctx l <+> pPrint op <+> pprExp (lvl + 1) r)
  EUnOp _ op e -> wrap ctx specialPrec (pPrint op <+> atom e)

  -- These extend as far right as they can, so they are bracketed anywhere
  -- but the top of an expression.
  EIfThen _ c t -> wrap ctx 0 (text "if" <+> pprExp 0 c <+> text "then" <+> pprExp 0 t)
  EIfThenElse _ c t e ->
    -- A one-armed if in the then branch would capture this else.
    let thenDoc = case t of
          EIfThen {} -> parens (pprExp 0 t)
          _          -> pprExp 0 t
     in wrap ctx 0 (text "if" <+> pprExp 0 c <+> text "then" <+> thenDoc <+> text "else" <+> pprExp 0 e)
  ELetIn _ dec body ->
    wrap ctx 0 (text "let" <+> prettyDecNoLet dec <+> text "in" <+> pprExp 0 body)
  EBoundLetIn _ x boundE rhs body ->
    wrap ctx 0 (text "let" <+> text (unpack x) <+> text "bound" <+> atom boundE
                  <+> text "=" <+> pprExp 0 rhs <+> text "in" <+> pprExp 0 body)

  EGenerate _ sz f -> sp "generate" [atom sz, atom f]
  EMap _ f arr -> sp "map" [atom f, atom arr]
  EZipWith _ f a b -> sp "zipwith" [atom f, atom a, atom b]
  EAppend _ a b -> sp "append" [atom a, atom b]
  EReduce _ f z arr -> sp "reduce" [atom f, atom z, atom arr]
  EReduceGenerate _ f z shape gen -> sp "reduce_generate" [atom f, atom z, atom shape, atom gen]
  EIterate _ n initArr f -> sp "iterate" [atom n, atom initArr, atom f]
  EFoldl _ f z arr -> sp "foldl" [atom f, atom z, atom arr]
  EFoldlWhile _ p f z arr -> sp "foldl_while" [atom p, atom f, atom z, atom arr]
  EScan _ f z arr -> sp "scan" [atom f, atom z, atom arr]
  EScanInclusive _ f z arr -> sp "scan_inclusive" [atom f, atom z, atom arr]
  EScanR _ f z arr -> sp "scanr" [atom f, atom z, atom arr]
  EScanRInclusive _ f z arr -> sp "scanr_inclusive" [atom f, atom z, atom arr]
  ESegmentedReduce _ f z offsets vals -> sp "segmented_reduce" [atom f, atom z, atom offsets, atom vals]
  ESortIndices _ arr -> sp "sort_indices" [atom arr]
  EIota _ n -> sp "iota" [atom n]
  EMakeIndex _ n arr -> sp "make_index" [atom n, atom arr]
  ECOOSumDuplicates _ nrows ncols nnz rows cols vals ->
    sp "coo_sum_duplicates" (map atom [nrows, ncols, nnz, rows, cols, vals])
  ECSRFromSortedCOO _ nrows ncols nnz rows cols vals ->
    sp "csr_from_sorted_coo" (map atom [nrows, ncols, nnz, rows, cols, vals])
  EPermute _ comb defaults permFn arr -> sp "permute" (map atom [comb, defaults, permFn, arr])
  EScatter _ comb defaults idxArr vals -> sp "scatter" (map atom [comb, defaults, idxArr, vals])
  EScatterGuarded _ comb defaults idxArr vals guardArr ->
    sp "scatter_guarded" (map atom [comb, defaults, idxArr, vals, guardArr])
  -- The remaining scatter forms are produced by fusion and have no surface
  -- syntax; they print in the same style for reading, not for re-parsing.
  EScatterGenerate _ comb defaults idxArr valFn ->
    sp "scatter_generate" (map atom [comb, defaults, idxArr, valFn])
  EScatterChain _ comb defaults phases ->
    sp "scatter_chain" [atom comb, atom defaults, commaList (map ppPhase phases)]
    where
      ppPhase p = parens (sep (punctuate comma (map (pprExp 0) ([spIndex p, spValues p] ++ maybe [] pure (spGuard p)))))
  EScatterGen _ comb defaults phases ->
    sp "scatter_gen" [atom comb, atom defaults, commaList (map ppGenPhase phases)]
    where
      ppGenPhase p = parens (sep (punctuate comma (map (pprExp 0)
        ([sgpShape p, sgpIndexFn p, sgpValueFn p] ++ maybe [] pure (sgpGuardFn p)))))
  EGather _ idxArr arr -> sp "gather" [atom idxArr, atom arr]
  EIndex _ idx arr -> sp "index" [atom idx, atom arr]
  ECheckIndex _ idx defVal arr -> sp "check_index" [atom idx, atom defVal, atom arr]
  ECompact _ m z g xs ->
    sp (case m of { CompactStable -> "compact_stable"; CompactUnordered -> "compact_unordered" })
       [atom z, atom g, atom xs]
  EFill _ shape val -> sp "fill" [atom shape, atom val]
  EShapeOf _ arr -> sp "shape_of" [atom arr]
  EReplicate _ dims arr -> sp "replicate" [commaList (map pPrint dims), atom arr]
  ESlice _ dims arr -> sp "slice" [commaList (map pPrint dims), atom arr]
  EReshape _ shape arr -> sp "reshape" [atom shape, atom arr]
  EReadArray _ shape file -> sp "read_array" [atom shape, atom file]
  EReadArrayFloat _ shape file -> sp "read_array_float" [atom shape, atom file]
  EWriteArray _ arr file -> sp "write_array" [atom arr, atom file]
  EWriteArrayFloat _ arr file -> sp "write_array_float" [atom arr, atom file]
  EGetEnvInt _ var -> sp "get_env_int" [atom var]
  EGetEnvString _ var -> sp "get_env_string" [atom var]
  EStencil _ bnd f arr -> sp "stencil" [pPrint bnd, atom f, atom arr]
  EReify _ e -> sp "reify" [atom e]
  where
    sp = special ctx
    negLit isNeg d = if isNeg then parens d else d

instance Pretty (BoundaryCondition a) where
  pPrint BClamp        = text "clamp"
  pPrint BWrap         = text "wrap"
  pPrint BMirror       = text "mirror"
  pPrint (BConst v)    = parens (text "constant" <+> atom v)

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
  pPrint (IntOf _) = text "int_of_float"


instance Pretty (Operator a) where
  pPrint (Plus _) = text "+"
  pPrint (Minus _) = text "-"
  pPrint (Times _) = text "*"
  pPrint (Divide _) = text "/"
  pPrint (Mod _) = text "%"
  pPrint (Eq _) = text "="
  pPrint (Neq _) = text "<>"
  pPrint (Lt _) = text "<"
  pPrint (Le _) = text "<="
  pPrint (Gt _) = text ">"
  pPrint (Ge _) = text ">="
  pPrint (And _) = text "&"
  pPrint (Or _) = text "|"
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
  pPrint (PVar _ nam) = text (unpack nam)
  pPrint (PVec _ pats) =
    brackets (sep (punctuate (text ",") (map pPrint pats)))
  pPrint (PBound _ nam e) = text (unpack nam) <+> text "bound" <+> atom e
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
