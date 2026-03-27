{
-- |
-- Module: Language.Hydrangea.Parser
--
-- Happy-generated parser entry points for expressions, polytypes, and
-- declaration lists.
module Language.Hydrangea.Parser
  ( parseMiniML, parsePolytype, parseDecs
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (fromJust)
import Data.Monoid (First (..))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Language.Hydrangea.Lexer as L
import Language.Hydrangea.Syntax
}

-- | Parse a single expression from lexer tokens.
%name parseMiniML exp
-- | Parse a standalone polytype.
%name parsePolytype polytype
-- | Parse a list of top-level declarations.
%name parseDecs decs
%tokentype { L.RangedToken }
%error { parseError }
%expect 1
%monad { L.Alex } { >>= } { pure }
%lexer { lexer } { L.RangedToken L.EOF _ }

%token
  -- Identifiers
  identifier { L.RangedToken (L.Identifier _) _ }
  -- Constants
  string     { L.RangedToken (L.String _) _ }
  integer    { L.RangedToken (L.Integer _) _ }
  float_lit  { L.RangedToken (L.Float _) _ }
  -- Keywords
  let        { L.RangedToken L.Let _ }
  in         { L.RangedToken L.In _ }
  if         { L.RangedToken L.If _ }
  then       { L.RangedToken L.Then _ }
  else       { L.RangedToken L.Else _ }
  fn         { L.RangedToken L.Fn _ }
  '=>'       { L.RangedToken L.FatArrow _ }
  forall     { L.RangedToken L.Forall _ }
  generate   { L.RangedToken L.Generate _ }
  map        { L.RangedToken L.Map _ }
  zipwith    { L.RangedToken L.ZipWith _ }
  reduce     { L.RangedToken L.Reduce _ }
  reduce_generate { L.RangedToken L.ReduceGenerate _ }
  foldl      { L.RangedToken L.Foldl _ }
  scan       { L.RangedToken L.Scan _ }
  segmented_reduce { L.RangedToken L.SegmentedReduce _ }
  sort_indices { L.RangedToken L.SortIndices _ }
  iota       { L.RangedToken L.Iota _ }
  make_index { L.RangedToken L.MakeIndex _ }
  bound      { L.RangedToken L.Bound _ }
  coo_sum_duplicates { L.RangedToken L.COOSumDuplicates _ }
  csr_from_sorted_coo { L.RangedToken L.CSRFromSortedCOO _ }
  index      { L.RangedToken L.Index _ }
  shape_of   { L.RangedToken L.ShapeOf _ }
  check_index { L.RangedToken L.CheckIndex _ }
  replicate  { L.RangedToken L.Replicate _ }
  slice      { L.RangedToken L.Slice _ }
  reshape    { L.RangedToken L.Reshape _ }
  fill       { L.RangedToken L.Fill _ }
  permute    { L.RangedToken L.Permute _ }
  scatter    { L.RangedToken L.Scatter _ }
  scatter_guarded { L.RangedToken L.ScatterGuarded _ }
  gather     { L.RangedToken L.Gather _ }
  stencil    { L.RangedToken L.Stencil _ }
  clamp      { L.RangedToken L.Clamp _ }
  wrap       { L.RangedToken L.Wrap _ }
  mirror     { L.RangedToken L.Mirror _ }
  constant   { L.RangedToken L.Constant _ }
  sqrt       { L.RangedToken L.Sqrt _ }
  fst        { L.RangedToken L.TFst _ }
  snd        { L.RangedToken L.TSnd _ }
  math_exp   { L.RangedToken L.ExpF _ }
  log        { L.RangedToken L.Log _ }
  sin        { L.RangedToken L.Sin _ }
  cos        { L.RangedToken L.Cos _ }
  abs_f      { L.RangedToken L.AbsF _ }
  floor_f    { L.RangedToken L.FloorF _ }
  ceil_f     { L.RangedToken L.CeilF _ }
  erf        { L.RangedToken L.Erf _ }
  float_of   { L.RangedToken L.FloatOf _ }
  read_array       { L.RangedToken L.ReadArray _ }
  read_array_float { L.RangedToken L.ReadArrayFloat _ }
  write_array       { L.RangedToken L.WriteArray _ }
  write_array_float { L.RangedToken L.WriteArrayFloat _ }
  get_env_int      { L.RangedToken L.GetEnvInt _ }
  get_env_string   { L.RangedToken L.GetEnvString _ }
  int        { L.RangedToken L.TInt _ }
  bool       { L.RangedToken L.TBool _ }
  float      { L.RangedToken L.TFloat _ }
  true       { L.RangedToken L.BTrue _ }
  false      { L.RangedToken L.BFalse _ }
  all        { L.RangedToken L.All _ }
  any        { L.RangedToken L.Any _ }
  -- Arithmetic operators
  '+'        { L.RangedToken L.Plus _ }
  '-'        { L.RangedToken L.Minus _ }
  '*'        { L.RangedToken L.Times _ }
  '/'        { L.RangedToken L.Divide _ }
  '%'        { L.RangedToken L.Percent _ }
  -- Float arithmetic operators
  '+.'       { L.RangedToken L.TPlusF _ }
  '-.'       { L.RangedToken L.TMinusF _ }
  '*.'       { L.RangedToken L.TTimesF _ }
  '/.'       { L.RangedToken L.TDivideF _ }
  -- Comparison operators
  '='        { L.RangedToken L.Eq _ }
  '<>'       { L.RangedToken L.Neq _ }
  '<'        { L.RangedToken L.Lt _ }
  '<='       { L.RangedToken L.Le _ }
  '>'        { L.RangedToken L.Gt _ }
  '>='       { L.RangedToken L.Ge _ }
  -- Float comparison operators
  '=.'       { L.RangedToken L.TEqF _ }
  '<>.'      { L.RangedToken L.TNeqF _ }
  '<.'       { L.RangedToken L.TLtF _ }
  '<=.'      { L.RangedToken L.TLeF _ }
  '>.'       { L.RangedToken L.TGtF _ }
  '>=.'      { L.RangedToken L.TGeF _ }
  -- Logical operators
  '&'        { L.RangedToken L.And _ }
  '|'        { L.RangedToken L.Or _ }
   -- Parenthesis
   '('        { L.RangedToken L.LPar _ }
   ')'        { L.RangedToken L.RPar _ }
   '{'        { L.RangedToken L.LBrace _ }
   '}'        { L.RangedToken L.RBrace _ }
   -- Vectors
   '['        { L.RangedToken L.LBrack _ }
   ']'        { L.RangedToken L.RBrack _ }
  ','        { L.RangedToken L.Comma _ }
  proj       { L.RangedToken L.Proj _ }
  -- Types
  ':'        { L.RangedToken L.Colon _ }
  '->'       { L.RangedToken L.Arrow _ }
  '.'        { L.RangedToken L.Period _ }

%right fn '=>'
%right else in
%right '->'
%left '|'
%left '&'
%nonassoc '=' '<>' '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/' '%'
%nonassoc '=.' '<>.' '<.' '>.' '<=.' '>=.'
%left '+.' '-.'
%left '*.' '/.'

%%

name :: { Name L.Range }
  : identifier { unTok $1 (\range (L.Identifier name) -> Name range name) }

type :: { Type }
  : int            { tyInt }
  | bool           { tyBool }
  | float          { tyFloat }
  | name           { (tyVar (unName $1)) }
  | '(' ')'        { tyUnit }
  | '(' type ')'   { $2 }
  | '(' type ',' type ')' { tyPair $2 $4 }
  | '{' typeRecordFields '}' { tyRecord $2 }
  | type '*' type  { (mkTyVec $1 $3) }
  | type '->' type { (tyFun $1 $3) }

polytype :: { Polytype }
  : forall many(name) '.' type { Forall (map unName $2) [] $4 }
  | type { Forall [] [] $1 }

typeAnnotation :: { Polytype }
  : ':' polytype { $2 }

dec :: { Dec L.Range }
  : let name many(pat) optional(typeAnnotation) '=' exp { Dec (L.rtRange $1 <-> info $6) (unName $2) $3 $4 $6 }

decs :: { [Dec L.Range] }
  : many(dec) { $1 }

exp :: { Exp L.Range }
  : expapp                   { $1 }
  | expcond                  { $1 }
  -- Arithmetic operators
  | '-' exp                  { ENeg (L.rtRange $1 <-> info $2) $2 }
  | exp '+'  exp             { EBinOp (info $1 <-> info $3) $1 (Plus (L.rtRange $2)) $3 }
  | exp '-'  exp             { EBinOp (info $1 <-> info $3) $1 (Minus (L.rtRange $2)) $3 }
  | exp '*'  exp             { EBinOp (info $1 <-> info $3) $1 (Times (L.rtRange $2)) $3 }
  | exp '/'  exp             { EBinOp (info $1 <-> info $3) $1 (Divide (L.rtRange $2)) $3 }
  | exp '%'  exp             { EBinOp (info $1 <-> info $3) $1 (Mod (L.rtRange $2)) $3 }
  -- Comparison operators
  | exp '='  exp             { EBinOp (info $1 <-> info $3) $1 (Eq (L.rtRange $2)) $3 }
  | exp '<>' exp             { EBinOp (info $1 <-> info $3) $1 (Neq (L.rtRange $2)) $3 }
  | exp '<'  exp             { EBinOp (info $1 <-> info $3) $1 (Lt (L.rtRange $2)) $3 }
  | exp '<=' exp             { EBinOp (info $1 <-> info $3) $1 (Le (L.rtRange $2)) $3 }
  | exp '>'  exp             { EBinOp (info $1 <-> info $3) $1 (Gt (L.rtRange $2)) $3 }
  | exp '>=' exp             { EBinOp (info $1 <-> info $3) $1 (Ge (L.rtRange $2)) $3 }
  -- Logical operators
  | exp '&'  exp             { EBinOp (info $1 <-> info $3) $1 (And (L.rtRange $2)) $3 }
  | exp '|'  exp             { EBinOp (info $1 <-> info $3) $1 (Or (L.rtRange $2)) $3 }
  -- Float arithmetic operators
  | exp '+.' exp             { EBinOp (info $1 <-> info $3) $1 (PlusF (L.rtRange $2)) $3 }
  | exp '-.' exp             { EBinOp (info $1 <-> info $3) $1 (MinusF (L.rtRange $2)) $3 }
  | exp '*.' exp             { EBinOp (info $1 <-> info $3) $1 (TimesF (L.rtRange $2)) $3 }
  | exp '/.' exp             { EBinOp (info $1 <-> info $3) $1 (DivideF (L.rtRange $2)) $3 }
  -- Float comparison operators
  | exp '=.'  exp            { EBinOp (info $1 <-> info $3) $1 (EqF (L.rtRange $2)) $3 }
  | exp '<>.' exp            { EBinOp (info $1 <-> info $3) $1 (NeqF (L.rtRange $2)) $3 }
  | exp '<.'  exp            { EBinOp (info $1 <-> info $3) $1 (LtF (L.rtRange $2)) $3 }
  | exp '<=.' exp            { EBinOp (info $1 <-> info $3) $1 (LeF (L.rtRange $2)) $3 }
  | exp '>.'  exp            { EBinOp (info $1 <-> info $3) $1 (GtF (L.rtRange $2)) $3 }
  | exp '>=.' exp            { EBinOp (info $1 <-> info $3) $1 (GeF (L.rtRange $2)) $3 }
  -- Lambda: fn pat1 pat2 ... => body  desugars to  let __lambda_L_C pat1 pat2 ... = body in __lambda_L_C
  | fn pat many(pat) '=>' exp
      { ELetIn (L.rtRange $1 <-> info $5) (Dec (L.rtRange $1 <-> info $5) (mkLambdaName $1) ($2 : $3) Nothing $5) (EVar (L.rtRange $1 <-> info $5) (mkLambdaName $1)) }
  -- Binding
  | dec in exp               { ELetIn (info $1 <-> info $3) $1 $3 }
  -- Bound-annotated binding: let x bound E = rhs in body
  | let name bound atom '=' exp in exp
      { EBoundLetIn (L.rtRange $1 <-> info $8) (unName $2) $4 $6 $8 }

expapp :: { Exp L.Range }
  : expapp atom              { EApp (info $1 <-> info $2) $1 $2 }
  | atom                     { $1 }

expcond :: { Exp L.Range }
  : if exp then exp %shift   { EIfThen (L.rtRange $1 <-> info $4) $2 $4 }
  | if exp then exp else exp { EIfThenElse (L.rtRange $1 <-> info $6) $2 $4 $6 }
  | expspecial               { $1 }

expspecial :: { Exp L.Range }
  : generate atom atom         { EGenerate (L.rtRange $1 <-> info $3) $2 $3 }
  | fill atom atom             { EFill (L.rtRange $1 <-> info $3) $2 $3 }
  | replicate shapespec atom    { EReplicate (L.rtRange $1 <-> info $3) $2 $3 }
  | slice slicespec atom         { ESlice (L.rtRange $1 <-> info $3) $2 $3 }
  | reshape atom atom           { EReshape (L.rtRange $1 <-> info $3) $2 $3 }
  | map atom atom              { EMap (L.rtRange $1 <-> info $3) $2 $3 }
  | zipwith atom atom atom      { EZipWith (L.rtRange $1 <-> info $4) $2 $3 $4 }
  | reduce atom atom atom      { EReduce (L.rtRange $1 <-> info $4) $2 $3 $4 }
  | reduce_generate atom atom atom atom { EReduceGenerate (L.rtRange $1 <-> info $5) $2 $3 $4 $5 }
  | foldl atom atom atom       { EFoldl (L.rtRange $1 <-> info $4) $2 $3 $4 }
  | scan atom atom atom        { EScan (L.rtRange $1 <-> info $4) $2 $3 $4 }
  | segmented_reduce atom atom atom atom
      { ESegmentedReduce (L.rtRange $1 <-> info $5) $2 $3 $4 $5 }
  | sort_indices atom          { ESortIndices (L.rtRange $1 <-> info $2) $2 }
  | iota atom                  { EIota (L.rtRange $1 <-> info $2) $2 }
  | make_index atom atom       { EMakeIndex (L.rtRange $1 <-> info $3) $2 $3 }
  | coo_sum_duplicates atom atom atom atom atom atom
      { ECOOSumDuplicates (L.rtRange $1 <-> info $7) $2 $3 $4 $5 $6 $7 }
  | csr_from_sorted_coo atom atom atom atom atom atom
      { ECSRFromSortedCOO (L.rtRange $1 <-> info $7) $2 $3 $4 $5 $6 $7 }
  | permute atom atom atom atom { EPermute (L.rtRange $1 <-> info $5) $2 $3 $4 $5 }
  | scatter atom atom atom atom { EScatter (L.rtRange $1 <-> info $5) $2 $3 $4 $5 }
  | scatter_guarded atom atom atom atom atom { EScatterGuarded (L.rtRange $1 <-> info $6) $2 $3 $4 $5 $6 }
  | gather atom atom            { EGather (L.rtRange $1 <-> info $3) $2 $3 }
  | sqrt atom      { EUnOp (L.rtRange $1 <-> info $2) (Sqrt (L.rtRange $1)) $2 }
  | fst atom       { EUnOp (L.rtRange $1 <-> info $2) (Fst (L.rtRange $1)) $2 }
  | snd atom       { EUnOp (L.rtRange $1 <-> info $2) (Snd (L.rtRange $1)) $2 }
  | math_exp atom  { EUnOp (L.rtRange $1 <-> info $2) (ExpF (L.rtRange $1)) $2 }
  | log atom       { EUnOp (L.rtRange $1 <-> info $2) (Log (L.rtRange $1)) $2 }
  | sin atom       { EUnOp (L.rtRange $1 <-> info $2) (Sin (L.rtRange $1)) $2 }
  | cos atom       { EUnOp (L.rtRange $1 <-> info $2) (Cos (L.rtRange $1)) $2 }
  | abs_f atom     { EUnOp (L.rtRange $1 <-> info $2) (AbsF (L.rtRange $1)) $2 }
  | floor_f atom   { EUnOp (L.rtRange $1 <-> info $2) (FloorF (L.rtRange $1)) $2 }
  | ceil_f atom    { EUnOp (L.rtRange $1 <-> info $2) (CeilF (L.rtRange $1)) $2 }
  | erf atom       { EUnOp (L.rtRange $1 <-> info $2) (Erf (L.rtRange $1)) $2 }
  | float_of atom  { EUnOp (L.rtRange $1 <-> info $2) (FloatOf (L.rtRange $1)) $2 }
  | index atom atom            { EIndex (L.rtRange $1 <-> info $3) $2 $3 }
  | shape_of atom              { EShapeOf (L.rtRange $1 <-> info $2) $2 }
  | check_index atom atom atom { ECheckIndex (L.rtRange $1 <-> info $4) $2 $3 $4 }
  | read_array atom atom       { EReadArray (L.rtRange $1 <-> info $3) $2 $3 }
  | read_array_float atom atom { EReadArrayFloat (L.rtRange $1 <-> info $3) $2 $3 }
  | write_array atom atom       { EWriteArray (L.rtRange $1 <-> info $3) $2 $3 }
  | write_array_float atom atom { EWriteArrayFloat (L.rtRange $1 <-> info $3) $2 $3 }
  | get_env_int atom           { EGetEnvInt (L.rtRange $1 <-> info $2) $2 }
  | get_env_string atom        { EGetEnvString (L.rtRange $1 <-> info $2) $2 }
  | stencil boundary atom atom { EStencil (L.rtRange $1 <-> info $4) $2 $3 $4 }

boundary :: { BoundaryCondition L.Range }
  : clamp                    { BClamp }
  | wrap                     { BWrap }
  | mirror                   { BMirror }
  | '(' constant atom ')'    { BConst $3 }

shapespec :: { [ShapeDim L.Range] }
  : '[' shapedims ']' { $2 }

shapedims :: { [ShapeDim L.Range] }
  : shapedim { [$1] }
  | shapedim ',' shapedims { $1 : $3 }

shapedim :: { ShapeDim L.Range }
  : all { ShapeAll (L.rtRange $1) }
  | any exp { ShapeAny (L.rtRange $1 <-> info $2) $2 }
  | exp { ShapeDim (info $1) $1 }

slicespec :: { [SliceDim L.Range] }
  : '[' slicedims ']' { $2 }

slicedims :: { [SliceDim L.Range] }
  : slicedim { [$1] }
  | slicedim ',' slicedims { $1 : $3 }

slicedim :: { SliceDim L.Range }
  : all { SliceAll (L.rtRange $1) }
  | '[' exp ',' exp ']' { SliceRange (L.rtRange $1 <-> L.rtRange $5) $2 $4 }

expvec :: { Exp L.Range }
  : '[' expVecList ']' { EVec (L.rtRange $1 <-> L.rtRange $3) $2 }

expVecList :: {[Exp L.Range]}
  : exp {[$1]}
  | exp ',' expVecList { $1 : $3 }

typeRecordFields :: { [(Var, Type)] }
  : typeRecordField { [$1] }
  | typeRecordField ',' typeRecordFields { $1 : $3 }

typeRecordField :: { (Var, Type) }
  : name ':' type { (unName $1, $3) }

recordFields :: { [(Var, Exp L.Range)] }
  : recordField { [$1] }
  | recordField ',' recordFields { $1 : $3 }

recordField :: { (Var, Exp L.Range) }
  : name '=' exp { (unName $1, $3) }

recordProj :: { Name L.Range }
  : '.' name { $2 }

atom :: { Exp L.Range }
  : atom_base many(recordProj) { applyRecordProjs $1 $2 }

atom_base :: { Exp L.Range }
  : integer                  { unTok $1 (\range (L.Integer int) -> EInt range int) }
  | float_lit                { unTok $1 (\range (L.Float f) -> EFloat range f) }
  | name                     { EVar (info $1) (unName $1) }
  | proj integer atom        { EProj (L.rtRange $1 <-> info $3) (unTok $2 (\range (L.Integer i) -> i)) $3}
  | expvec                   { $1 }
  | string                   { unTok $1 (\range (L.String string) -> EString range string) }
  | '{' recordFields '}'     { ERecord (L.rtRange $1 <-> L.rtRange $3) (normalizeRecordFields $2) }
  | '(' ')'                  { EUnit (L.rtRange $1 <-> L.rtRange $2) }
  | '(' exp ',' exp ')'     { EPair (L.rtRange $1 <-> L.rtRange $5) $2 $4 }
  | '(' exp ')'              { $2 }
  | true                     { unTok $1 (\range _ -> EBool range True) }
  | false                    { unTok $1 (\range _ -> EBool range False) }
  
  -- Arithmetic operators
  | '(' '+' ')'              { EOp (L.rtRange $1 <-> L.rtRange $3) (Plus (L.rtRange $2)) }
  | '(' '-' ')'              { EOp (L.rtRange $1 <-> L.rtRange $3) (Minus (L.rtRange $2)) }
  | '(' '*' ')'              { EOp (L.rtRange $1 <-> L.rtRange $3) (Times (L.rtRange $2)) }
  | '(' '/' ')'              { EOp (L.rtRange $1 <-> L.rtRange $3) (Divide (L.rtRange $2)) }
  -- Comparison operators
  | '(' '=' ')'              { EOp (L.rtRange $1 <-> L.rtRange $3) (Eq (L.rtRange $2)) }
  | '(' '<>' ')'             { EOp (L.rtRange $1 <-> L.rtRange $3) (Neq (L.rtRange $2)) }
  | '(' '<' ')'              { EOp (L.rtRange $1 <-> L.rtRange $3) (Lt (L.rtRange $2)) }
  | '(' '<=' ')'             { EOp (L.rtRange $1 <-> L.rtRange $3) (Le (L.rtRange $2)) }
  | '(' '>' ')'              { EOp (L.rtRange $1 <-> L.rtRange $3) (Gt (L.rtRange $2)) }
  | '(' '>=' ')'             { EOp (L.rtRange $1 <-> L.rtRange $3) (Ge (L.rtRange $2)) }
  -- Logical operators
  | '(' '&' ')'              { EOp (L.rtRange $1 <-> L.rtRange $3) (And (L.rtRange $2)) }
  | '(' '|' ')'              { EOp (L.rtRange $1 <-> L.rtRange $3) (Or (L.rtRange $2)) }
  -- Float arithmetic operators (first-class)
  | '(' '+.' ')'             { EOp (L.rtRange $1 <-> L.rtRange $3) (PlusF (L.rtRange $2)) }
  | '(' '-.' ')'             { EOp (L.rtRange $1 <-> L.rtRange $3) (MinusF (L.rtRange $2)) }
  | '(' '*.' ')'             { EOp (L.rtRange $1 <-> L.rtRange $3) (TimesF (L.rtRange $2)) }
  | '(' '/.' ')'             { EOp (L.rtRange $1 <-> L.rtRange $3) (DivideF (L.rtRange $2)) }
  -- Float comparison operators (first-class)
  | '(' '=.' ')'             { EOp (L.rtRange $1 <-> L.rtRange $3) (EqF (L.rtRange $2)) }
  | '(' '<>.' ')'            { EOp (L.rtRange $1 <-> L.rtRange $3) (NeqF (L.rtRange $2)) }
  | '(' '<.' ')'             { EOp (L.rtRange $1 <-> L.rtRange $3) (LtF (L.rtRange $2)) }
  | '(' '<=.' ')'            { EOp (L.rtRange $1 <-> L.rtRange $3) (LeF (L.rtRange $2)) }
  | '(' '>.' ')'             { EOp (L.rtRange $1 <-> L.rtRange $3) (GtF (L.rtRange $2)) }
  | '(' '>=.' ')'            { EOp (L.rtRange $1 <-> L.rtRange $3) (GeF (L.rtRange $2)) }

patVec :: {[Pat L.Range]}
  : pat {[$1]}
  | pat ',' patVec { $1 : $3 }

pat :: { Pat L.Range }
  : name                       { PVar (info $1) (unName $1) }
  | '[' patVec ']'             { PVec (L.rtRange $1 <-> L.rtRange $3) $2 }
  | name bound atom            { PBound (info $1 <-> info $3) (unName $1) $3 }


optional(p)
  :   { Nothing }
  | p { Just $1 }

many_rev(p)
  :               { [] }
  | many_rev(p) p { $2 : $1 }

many(p)
  : many_rev(p) { reverse $1 }

{
parseError :: L.RangedToken -> L.Alex a
parseError _ = do
  (L.AlexPn _ line column, _, _, _) <- L.alexGetInput
  L.alexError $ "Parse error at line " <> show line <> ", column " <> show column

lexer :: (L.RangedToken -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)

-- | Build a simple node by extracting its token type and range.
unTok :: L.RangedToken -> (L.Range -> L.Token -> a) -> a
unTok (L.RangedToken tok range) ctor = ctor range tok

-- | Unsafely extracts the the metainformation field of a node.
info :: Foldable f => f a -> a
info = fromJust . getFirst . foldMap pure

-- | Performs the union of two ranges by creating a new range starting at the
-- start position of the first range, and stopping at the stop position of the
-- second range.
-- Invariant: The LHS range starts before the RHS range.
(<->) :: L.Range -> L.Range -> L.Range
L.Range a1 _ <-> L.Range _ b2 = L.Range a1 b2


mkTyVec :: Type -> Type -> Type
mkTyVec ty1 ty2 =
    let
      flatten :: Type -> [Type]
      flatten (TyCons a b) = flatten a ++ flatten b
      -- TyUnit is the tuple terminator; it does not represent an element
      flatten TyUnit = []
      flatten t = [t]

      rebuild :: [Type] -> Type
      rebuild [] = tyUnit
      rebuild (t:ts) = tyCons t (rebuild ts)
    in rebuild (flatten ty1 ++ flatten ty2)

-- | Generate a fresh lambda name from the source position of the 'fn' token.
-- Uses line and column so every lambda in the source gets a unique name.
mkLambdaName :: L.RangedToken -> Var
mkLambdaName rt =
  let L.Range (L.AlexPn _ line col) _ = L.rtRange rt
  in BS.pack ("__lambda_" ++ show line ++ "_" ++ show col)

applyRecordProjs :: Exp L.Range -> [Name L.Range] -> Exp L.Range
applyRecordProjs = foldl applyOne
  where
    applyOne expr field = ERecordProj (info expr <-> info field) expr (unName field)

}
