{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module: Language.Hydrangea.ErrorFormat
--
-- Formatting helpers for type-checking and evaluation errors.
module Language.Hydrangea.ErrorFormat
  ( formatTypeError
  , formatEvalError
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Language.Hydrangea.Infer (TypeError(..))
import Language.Hydrangea.Interpreter (EvalError(..))
import Language.Hydrangea.Lexer (AlexPosn(..), Range(..))
import System.Console.ANSI

typeErrorSummary :: TypeError -> (Maybe Range, String)
typeErrorSummary err =
  case err of
    UnboundVar mr v -> (mr, "Unbound variable: " ++ BS.unpack v)
    Infinite mr _ _ -> (mr, "Infinite type (occurs check)")
    Mismatch mr _ _ -> (mr, "Type mismatch")
    InvalidPoly mr _ -> (mr, "Invalid polymorphic type")
    DuplicateRecordField mr field -> (mr, "Duplicate record field: " ++ BS.unpack field)
    MissingRecordField mr field -> (mr, "Missing record field: " ++ BS.unpack field)
    UnsatConstraints _ -> (Nothing, "Unsatisfiable constraints (UnsatConstraints)")
    MiscError mr -> (mr, "Type error")

formatHeader :: Maybe FilePath -> Maybe Range -> String -> String
formatHeader mfile mr short =
  case (mfile, mr) of
    (Just file, Just (Range start _)) -> file ++ ":" ++ pos start ++ ": " ++ short
    (Nothing, Just (Range start _)) -> "line " ++ pos start ++ ": " ++ short
    _ -> short

formatSnippet :: Maybe Range -> Maybe ByteString -> String
formatSnippet mr mcontent =
  case (mr, mcontent) of
    (Just (Range (AlexPn _ sline scol) (AlexPn _ eline ecol)), Just content) ->
      let ls = BS.lines content
          idx = max 0 (sline - 1)
          srcLine = if idx < length ls then BS.unpack (ls !! idx) else ""
          startCol = max 1 scol
          endCol = if sline == eline then max startCol ecol else length srcLine + 1
          caretLen = max 1 (endCol - startCol)
          caretLine = replicate (startCol - 1) ' ' ++ replicate caretLen '^'
      in "\n" ++ show sline ++ " | " ++ srcLine ++ "\n" ++ "    | " ++ caretLine
    _ -> ""

-- | Format a 'TypeError' with an optional filename and source snippet.
formatTypeError :: Maybe FilePath -> Maybe ByteString -> TypeError -> String
formatTypeError mfile mcontent err =
  let (mr, short) = typeErrorSummary err
      header = formatHeader mfile mr short
      snippet = formatSnippet mr mcontent
  in colorize header "\n" ++ snippet

-- | Format an 'EvalError' as a short runtime error message.
formatEvalError :: Maybe FilePath -> Maybe ByteString -> EvalError -> String
formatEvalError _ _ err =
  case err of
    UnboundVariable v -> "Runtime error: unbound variable: " ++ BS.unpack v
    TypeError msg -> "Type error: " ++ msg
    IndexOutOfBounds msg -> "Index out of bounds: " ++ msg
    DivisionByZero -> "Division by zero"
    MismatchedPatterns msg -> "Pattern match failed: " ++ msg
    InvalidArrayOperation msg -> "Invalid array operation: " ++ msg
    ArityMismatch msg -> "Arity mismatch: " ++ msg
    RuntimeError msg -> "Runtime error: " ++ msg

-- Helpers
pos :: AlexPosn -> String
pos (AlexPn _ line col) = show line ++ ":" ++ show col

-- Colorize header using ANSI terminal codes. Uses red for error header.
colorize :: String -> String -> String
colorize hdr rest = setSGRCode [SetColor Foreground Vivid Red] ++ hdr ++ setSGRCode [Reset] ++ rest
