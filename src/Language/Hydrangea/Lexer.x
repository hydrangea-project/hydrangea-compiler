{
-- |
-- Module: Language.Hydrangea.Lexer
--
-- Alex-based lexer for Hydrangea source text.
module Language.Hydrangea.Lexer
  (
    Alex
  , AlexPosn (..)
  , alexGetInput
  , alexError
  , runAlex
  , alexMonadScan

  , Range (..)
  , RangedToken (..)
  , Token (..)
  , noRange
  ) where
import Control.Monad
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
}

-- In the middle, we insert our definitions for the lexer, which will generate the lexemes for our grammar.
%wrapper "monadUserState-bytestring"

$digit = [0-9]
$alpha = [a-zA-Z]

@id = ($alpha | \_) ($alpha | $digit | \_ | \' | \?)*

tokens :-

<0> $white+ ;

-- Comments
<0>       "(*" { nestComment `andBegin` comment }
<0>       "*)" { \_ _ -> alexError "Error: unexpected closing comment" }
<comment> "(*" { nestComment }
<comment> "*)" { unnestComment }
<comment> .    ;
<comment> \n   ;

-- Keywords
<0> let     { tok Let }
<0> in      { tok In }
<0> if      { tok If }
<0> then    { tok Then }
<0> else    { tok Else }
<0> fn      { tok Fn }
<0> forall  { tok Forall }
<0> generate { tok Generate }
<0> map      { tok Map }
<0> zipwith  { tok ZipWith }
<0> reduce   { tok Reduce }
<0> reduce_generate { tok ReduceGenerate }
<0> foldl_while { tok FoldlWhile }
<0> foldl    { tok Foldl }
<0> scan     { tok Scan }
<0> segmented_reduce { tok SegmentedReduce }
<0> sort_indices { tok SortIndices }
<0> iota        { tok Iota }
<0> make_index  { tok MakeIndex }
<0> bound       { tok Bound }
<0> coo_sum_duplicates { tok COOSumDuplicates }
<0> csr_from_sorted_coo { tok CSRFromSortedCOO }
<0> index       { tok Index }
<0> shape_of    { tok ShapeOf }
<0> check_index { tok CheckIndex }
<0> replicate   { tok Replicate }
<0> slice       { tok Slice }
<0> reshape     { tok Reshape }
<0> fill        { tok Fill }
<0> read_array_float { tok ReadArrayFloat }
<0> read_array  { tok ReadArray }
<0> write_array_float { tok WriteArrayFloat }
<0> write_array  { tok WriteArray }
<0> get_env_int { tok GetEnvInt }
<0> get_env_string { tok GetEnvString }
<0> permute  { tok Permute }
<0> scatter  { tok Scatter }
<0> scatter_guarded { tok ScatterGuarded }
<0> gather   { tok Gather }
<0> stencil  { tok Stencil }
<0> clamp    { tok Clamp }
<0> wrap     { tok Wrap }
<0> mirror   { tok Mirror }
<0> constant { tok Constant }
<0> fst     { tok TFst }
<0> snd     { tok TSnd }
<0> sqrt     { tok Sqrt }
<0> expf     { tok ExpF }
<0> log      { tok Log }
<0> sin      { tok Sin }
<0> cos      { tok Cos }
<0> abs_f    { tok AbsF }
<0> floor_f  { tok FloorF }
<0> ceil_f   { tok CeilF }
<0> erf      { tok Erf }
<0> float_of    { tok FloatOf }
<0> int_of_float { tok IntOf }
<0> int     { tok TInt }
<0> bool    { tok TBool }
<0> float   { tok TFloat }
<0> true    { tok BTrue }
<0> false   { tok BFalse }
<0> All     { tok All }
<0> Any     { tok Any }

-- Arithmetic operators
<0> "+"     { tok Plus }
<0> "-"     { tok Minus }
<0> "*"     { tok Times }
<0> "/"     { tok Divide }
<0> "%"     { tok Percent }

-- Float arithmetic operators
<0> "+."    { tok TPlusF }
<0> "-."    { tok TMinusF }
<0> "*."    { tok TTimesF }
<0> "/."    { tok TDivideF }

-- Lambda arrow
<0> "=>"    { tok FatArrow }

-- Comparison operators
<0> "="     { tok Eq }
<0> "<>"    { tok Neq }
<0> "<"     { tok Lt }
<0> "<="    { tok Le }
<0> ">"     { tok Gt }
<0> ">="    { tok Ge }

-- Float comparison operators
<0> "=."    { tok TEqF }
<0> "<>."   { tok TNeqF }
<0> "<."    { tok TLtF }
<0> "<=."   { tok TLeF }
<0> ">."    { tok TGtF }
<0> ">=."   { tok TGeF }

-- Logical operators
<0> "&"     { tok And }
<0> "|"     { tok Or }

-- Parenthesis
<0> "("     { tok LPar }
<0> ")"     { tok RPar }
<0> "{"     { tok LBrace }
<0> "}"     { tok RBrace }

-- Lists and tuples
<0> "["     { tok LBrack }
<0> "]"     { tok RBrack }
<0> ","     { tok Comma }
<0> proj    { tok Proj }

-- Types
<0> ":"     { tok Colon }
<0> "->"    { tok Arrow }
<0> "."     { tok Period }

-- Identifiers
<0> @id     { tokId }

-- Constants
<0> $digit+ "." $digit+ ("e" | "E") ("-" | "+")? $digit+ { tokFloat }
<0> $digit+ "." $digit+             { tokFloat }
<0> $digit+                         { tokInteger }

-- Strings
<0> \"        { enterString `andBegin` string }
<string> \"   { exitString `andBegin` 0 }
<string> \\\\ { emit '\\' }
<string> \\\" { emit '"' }
<string> \\n  { emit '\n' }
<string> \\t  { emit '\t' }
<string> .    { emitCurrent }


{
-- At the bottom, we may insert more Haskell definitions, such as data structures, auxiliary functions, etc.
data AlexUserState = AlexUserState
  { nestLevel :: Int
  , stringStart :: AlexPosn
  , stringBuffer :: [Char]
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
  { nestLevel = 0
  , stringStart = AlexPn 0 0 0
  , stringBuffer = []
  }

get :: Alex AlexUserState
get = Alex $ \s -> Right (s, alex_ust s)

put :: AlexUserState -> Alex ()
put s' = Alex $ \s -> Right (s{alex_ust = s'}, ())

modify :: (AlexUserState -> AlexUserState) -> Alex ()
modify f = Alex $ \s -> Right (s{alex_ust = f (alex_ust s)}, ())

alexEOF :: Alex RangedToken
alexEOF = do
  startCode <- alexGetStartCode
  when (startCode == comment) $
    alexError "Error: unclosed comment"
  when (startCode == string) $
    alexError "Error: unclosed string"
  (pos, _, _, _) <- alexGetInput
  pure $ RangedToken EOF (Range pos pos)

-- | Half-open source range carried by lexer and parser nodes.
data Range = Range
  { start :: AlexPosn
  , stop :: AlexPosn
  } deriving (Eq, Show, Ord)

-- | Token paired with its source range.
data RangedToken = RangedToken
  { rtToken :: Token
  , rtRange :: Range
  } deriving (Eq, Show)

-- | Sentinel range used in synthesized nodes and tests.
noRange :: Range
noRange = Range { start = (AlexPn 0 0 0), stop = (AlexPn 0 0 0) }

-- | Tokens recognized by the Hydrangea lexer.
data Token
  -- Identifiers
  = Identifier ByteString
  -- Constants
  | String ByteString
  | Integer Integer
  | Float Double
  -- Keywords
  | Let
  | In
  | If
  | Then
  | Else
  | Fn
  | FatArrow
  | Forall
  | TInt
  | TBool
  | TFloat
  | BTrue
  | BFalse
  -- Arithmetic operators
  | Plus
  | Minus
  | Times
  | Divide
  | Percent
  -- Float arithmetic operators
  | TPlusF
  | TMinusF
  | TTimesF
  | TDivideF
  -- Comparison operators
  | Eq
  | Neq
  | Lt
  | Le
  | Gt
  | Ge
  -- Float comparison operators
  | TEqF
  | TNeqF
  | TLtF
  | TLeF
  | TGtF
  | TGeF
  -- Logical operators
  | And
  | Or
  -- Parenthesis
  | LPar
  | RPar
  | LBrace
  | RBrace
  -- Vector
  | Comma
  | LBrack
  | RBrack
  | Proj
  | Generate
  | Map
  | ZipWith
  | Reduce
  | ReduceGenerate
  | Foldl
  | FoldlWhile
  | Scan
  | SegmentedReduce
  | SortIndices
  | Iota
  | MakeIndex
  | Bound
  | COOSumDuplicates
  | CSRFromSortedCOO
  | Index
  | ShapeOf
  | CheckIndex
  | Replicate
  | Slice
  | Reshape
  | Fill
  | Permute
  | Scatter
  | ScatterGuarded
  | Gather
  | Stencil
  | Clamp
  | Wrap
  | Mirror
  | Constant
  -- Pair primitives
  | TFst
  | TSnd
  -- Math functions
  | Sqrt
  | ExpF
  | Log
  | Sin
  | Cos
  | AbsF
  | FloorF
  | CeilF
  | Erf
  | FloatOf
  | IntOf
  | ReadArray
  | ReadArrayFloat
  | WriteArray
  | WriteArrayFloat
  | GetEnvInt
  | GetEnvString
  | All
  | Any
  -- Types
  | Colon
  | Arrow
  | Period
  -- EOF
  | EOF
  deriving (Eq, Show)

mkRange :: AlexInput -> Int64 -> Range
mkRange (start, _, str, _) len = Range{start = start, stop = stop}
  where
    stop = BS.foldl' alexMove start $ BS.take len str

tokId :: AlexAction RangedToken
tokId inp@(_, _, str, _) len =
  pure RangedToken
    { rtToken = Identifier $ BS.take len str
    , rtRange = mkRange inp len
    }

tok :: Token -> AlexAction RangedToken
tok ctor inp len =
  pure RangedToken
    { rtToken = ctor
    , rtRange = mkRange inp len
    }

tokInteger :: AlexAction RangedToken
tokInteger inp@(_, _, str, _) len =
  pure RangedToken
    { rtToken = Integer $ read $ BS.unpack $ BS.take len str
    , rtRange = mkRange inp len
    }

tokFloat :: AlexAction RangedToken
tokFloat inp@(_, _, str, _) len =
  pure RangedToken
    { rtToken = Float $ read $ BS.unpack $ BS.take len str
    , rtRange = mkRange inp len
    }

enterString :: AlexAction RangedToken
enterString inp@(pos, _, _, _) len = do
  modify $ \s -> s{stringStart = pos, stringBuffer = '"' : stringBuffer s}
  skip inp len

exitString :: AlexAction RangedToken
exitString inp@(pos, _, _, _) len = do
  s <- get
  put s{stringStart = AlexPn 0 0 0, stringBuffer = []}
  pure RangedToken
    { rtToken = String $ BS.pack $ reverse $ '"' : stringBuffer s
    , rtRange = Range (stringStart s) (alexMove pos '"')
    }

emit :: Char -> AlexAction RangedToken
emit c inp@(_, _, str, _) len = do
  modify $ \s -> s{stringBuffer = c : stringBuffer s}
  skip inp len

emitCurrent :: AlexAction RangedToken
emitCurrent inp@(_, _, str, _) len = do
  modify $ \s -> s{stringBuffer = BS.head str : stringBuffer s}
  skip inp len

nestComment :: AlexAction RangedToken
nestComment input len = do
  modify $ \s -> s{nestLevel = nestLevel s + 1}
  skip input len

unnestComment :: AlexAction RangedToken
unnestComment input len = do
  state <- get
  let level = nestLevel state - 1
  put state{nestLevel = level}
  when (level == 0) $
    alexSetStartCode 0
  skip input len

scanMany :: ByteString -> Either String [RangedToken]
scanMany input = runAlex input go
  where
    go = do
      output <- alexMonadScan
      if rtToken output == EOF
        then pure [output]
        else (output :) <$> go
}
