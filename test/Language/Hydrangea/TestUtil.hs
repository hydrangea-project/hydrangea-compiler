-- | Shared assertion combinators for pass/codegen specs.
--
-- Prefer these over ad-hoc @isInfixOf@ chains so tests pin the tokens that are
-- actually the contract (ABI names, pragmas) and tolerate incidental spacing /
-- interleaving. For pass output, prefer matching the IR ADT over any string
-- match at all.
--
-- Don't match C comments or the names codegen invents for its own
-- temporaries: neither is a contract.  When a test needs to know which
-- strategy the compiler picked, read it off the CFG ('parallelStrategies');
-- when it needs to know what the C does, match the construct that does it
-- ('ompPragmas', 'lineWithAll', 'cFunctionBody').
module Language.Hydrangea.TestUtil
  ( containsAll
  , containsNone
  , containsInOrder
    -- * Generated C
  , stripCComments
  , ompPragmas
  , lineWithAll
  , cFunctionBody
  , callsFunction
  , memoCacheSlot
  , shouldBeMemoized
  , shouldNotBeMemoized
    -- * CFG
  , loopsIn
  , programLoops
  , parallelStrategies
  ) where

import Data.Char (isAlphaNum, isSpace)
import Data.List (dropWhileEnd, isInfixOf, isPrefixOf, isSuffixOf, stripPrefix, tails)
import Language.Hydrangea.CFG
  ( ExecPolicy(..)
  , LoopSpec(..)
  , ParallelSpec(..)
  , ParallelStrategy
  , Proc(..)
  , Program(..)
  , Stmt(..)
  )
import Test.Hspec (Expectation, expectationFailure)

-- | All needles appear somewhere in the haystack (order-independent).
containsAll :: [String] -> String -> Bool
containsAll needles hay = all (`isInfixOf` hay) needles

-- | None of the needles appear.
containsNone :: [String] -> String -> Bool
containsNone needles hay = not (any (`isInfixOf` hay) needles)

-- | Each needle appears (as a substring) in the given order, but not
-- necessarily adjacent. Tolerant to interleaved/reformatted output between
-- the anchors.
containsInOrder :: [String] -> String -> Bool
containsInOrder [] _ = True
containsInOrder (n : ns) hay =
  case [ drop (length n) t | t <- tails hay, n `isPrefixOf` t ] of
    (rest : _) -> containsInOrder ns rest
    []         -> False

------------------------------------------------------------------------
-- Generated C
------------------------------------------------------------------------

-- | Remove @/* ... */@ and @// ...@ comments.  The generator never puts
-- comment markers inside string literals it means as code, so a plain scan
-- is enough.
stripCComments :: String -> String
stripCComments s = case s of
  '/' : '*' : rest -> ' ' : stripCComments (skipBlock rest)
  '/' : '/' : rest -> stripCComments (dropWhile (/= '\n') rest)
  c : rest -> c : stripCComments rest
  [] -> []
  where
    skipBlock r = case r of
      '*' : '/' : r' -> r'
      _ : r' -> skipBlock r'
      [] -> []

-- | Every @#pragma omp@ line, with comments removed and runs of whitespace
-- collapsed, in source order.  Compare against it with 'shouldContain' so a
-- failure prints the pragmas the generator did emit rather than the whole
-- C file.
ompPragmas :: String -> [String]
ompPragmas src =
  [ unwords (words l)
  | l <- lines (stripCComments src)
  , "#pragma omp" `isPrefixOf` dropWhile isSpace l
  ]

-- | Some single line contains every needle.  For checks like "the
-- reduction pragma is on a parallel-for", where the needles appearing on
-- different lines would not mean the same thing.
lineWithAll :: [String] -> String -> Bool
lineWithAll needles src = any (containsAll needles) (lines (stripCComments src))

-- | The body lines of the C function named @name@, if one is defined: the
-- lines after its @... name(...) {@ header up to the closing @}@ in column 0.
cFunctionBody :: String -> String -> Maybe [String]
cFunctionBody name src = go (lines src)
  where
    go ls = case ls of
      [] -> Nothing
      l : rest
        | isHeader l -> Just (takeWhile (/= "}") rest)
        | otherwise -> go rest
    isHeader l =
      startsInColumn0 l
        && "{" `isSuffixOf` dropWhileEnd isSpace l
        && callsFunction name [l]
    startsInColumn0 l = case l of
      c : _ -> not (isSpace c)
      [] -> False

-- | Some line mentions @name(@ as a whole identifier (so @main@ does not
-- match @hyd_main(@).
callsFunction :: String -> [String] -> Bool
callsFunction name = any hasCall
  where
    hasCall l = go ' ' l
    go _ [] = False
    go prev s@(c : rest)
      | not (isIdent prev)
      , Just after <- stripPrefix name s
      , "(" `isPrefixOf` after = True
      | otherwise = go c rest
    isIdent ch = isAlphaNum ch || ch == '_'

-- | The file-scope slot a zero-argument proc caches its result in, if its
-- body is guarded by one: the body opens with @if (slot == NULL) {@ and the
-- file declares @static ... slot = NULL;@.
memoCacheSlot :: String -> String -> Maybe String
memoCacheSlot name src = do
  body <- cFunctionBody name src
  firstLine : _ <- Just (dropWhile (all isSpace) body)
  slot <- stripPrefix "if (" (dropWhile isSpace firstLine)
            >>= stripSuffix " == NULL) {" . dropWhileEnd isSpace
  if any (isStaticNullDecl slot) (lines src) then Just slot else Nothing
  where
    stripSuffix suf s
      | suf `isSuffixOf` s = Just (take (length s - length suf) s)
      | otherwise = Nothing
    isStaticNullDecl slot l =
      "static " `isPrefixOf` l && (" " ++ slot ++ " = NULL;") `isSuffixOf` l

-- | The proc's C function computes its result once and caches it.
shouldBeMemoized :: String -> String -> Expectation
shouldBeMemoized src name =
  case (cFunctionBody name src, memoCacheSlot name src) of
    (Nothing, _) -> expectationFailure ("no C function named " ++ name)
    (Just _, Just _) -> pure ()
    (Just body, Nothing) ->
      expectationFailure
        (name ++ " is not guarded by a file-scope cache; its body is:\n" ++ unlines body)

-- | The proc's C function exists and recomputes its result on every call.
shouldNotBeMemoized :: String -> String -> Expectation
shouldNotBeMemoized src name =
  case (cFunctionBody name src, memoCacheSlot name src) of
    (Nothing, _) -> expectationFailure ("no C function named " ++ name)
    (Just _, Nothing) -> pure ()
    (Just _, Just slot) ->
      expectationFailure (name ++ " caches its result in " ++ slot)

------------------------------------------------------------------------
-- CFG
------------------------------------------------------------------------

-- | Every loop in a statement list, outermost first, with its body.
loopsIn :: [Stmt] -> [(LoopSpec, [Stmt])]
loopsIn = concatMap go
  where
    go stmt = case stmt of
      SLoop spec body -> (spec, body) : loopsIn body
      SIf _ thn els -> loopsIn thn ++ loopsIn els
      SParallelRegion body -> loopsIn body
      _ -> []

-- | Every loop in every proc of a program.
programLoops :: Program -> [(LoopSpec, [Stmt])]
programLoops (Program procs) = concatMap (loopsIn . procBody) procs

-- | The strategy of every parallel or worksharing loop, in program order.
-- This is what the parallelizer decided; the C backend only renders it.
parallelStrategies :: Program -> [ParallelStrategy]
parallelStrategies prog =
  [ psStrategy ps
  | (spec, _) <- programLoops prog
  , Just ps <- [parallelSpec (lsExec spec)]
  ]
  where
    parallelSpec ex = case ex of
      Parallel ps -> Just ps
      Workshare ps -> Just ps
      _ -> Nothing
