{-# LANGUAGE OverloadedStrings #-}

-- | The pretty printer must print a program the parser reads back as the same
-- tree.  Each test parses a program, prints it, parses the result and compares
-- the two trees (ignoring source ranges).
module Language.Hydrangea.PrettySpec (spec) where

import Control.Monad (forM_)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List (isSuffixOf)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Text.PrettyPrint.HughesPJClass (prettyShow)

import Language.Hydrangea.Frontend (readDecs)
import Language.Hydrangea.Pretty ()
import Language.Hydrangea.Syntax (Dec)
import Test.Hspec

-- | Print the program, parse it back and compare with the original parse.
roundTrips :: BS.ByteString -> Expectation
roundTrips src =
  case readDecs src of
    Left err -> expectationFailure ("original does not parse: " ++ err)
    Right decs -> do
      let printed = unlines (map prettyShow decs)
      case readDecs (BS.pack printed) of
        Left err ->
          expectationFailure ("printed program does not parse: " ++ err ++ "\n" ++ printed)
        Right decs' -> strip decs' `shouldBe` strip decs

strip :: [Dec a] -> [Dec ()]
strip = map (fmap (const ()))

-- | Every @.hyd@ file under a directory, recursively.
hydFiles :: FilePath -> IO [FilePath]
hydFiles dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure []
    else do
      entries <- listDirectory dir
      concat <$> mapM visit [dir </> e | e <- entries]
  where
    visit path = do
      isDir <- doesDirectoryExist path
      if isDir
        then hydFiles path
        else pure [path | ".hyd" `isSuffixOf` path]

spec :: Spec
spec = describe "pretty printer" $ do
  describe "round-trips through the parser" $ do
    let cases =
          [ ("operator precedence", "let x = 1 + 2 * 3 - 4 / 5 % 6")
          , ("left-nested subtraction", "let x = (1 - 2) - 3")
          , ("right-nested subtraction", "let x = 1 - (2 - 3)")
          , ("nested comparisons", "let x = (1 < 2) = (3 < 4)")
          , ("logic over comparisons", "let x = 1 < 2 & 3 = 4 | false")
          , ("float operators", "let x = 1.0 +. 2.0 *. 3.0 -. (4.0 -. 5.0)")
          , ("float compare under int add", "let x = 1 + 2 <. 3")
          , ("small and large floats", "let x = 1.0e-3 +. 2.5e10")
          , ("prefix minus", "let x = -1 + -(2 * 3) * -4")
          , ("double prefix minus", "let x = - (-1)")
          , ("application and builtins", "let f x y = x + y\nlet z = f (f 1 2) (index [0] (generate [3] (fn [i] => i)))")
          , ("builtin as an operand", "let x = index [0] (map (fn v => v * 2) (fill [3] 1)) + 1")
          , ("unary builtins", "let x = float_of (1 + 2) +. sqrt 4.0 +. float_of 3")
          , ("operator values", "let x = reduce (+) 0 (fill [3] 1)\nlet y = scatter (+.) (fill [4] 0.0) (generate [4] (fn [i] => i)) (fill [4] 1.0)")
          , ("comparison operator value", "let m = reduce_generate (fn a b => if a < b then b else a) 0 [4] (fn [i] => i)")
          , ("if as an operand", "let x = (if true then 1 else 2) + 3")
          , ("let as an operand", "let x = (let y = 1 in y) * 2")
          , ("let as an argument", "let f x = x\nlet z = f (let y = 1 in y)")
          , ("dangling else", "let x = if true then (if false then 1) else 2")
          , ("let in a declaration body", "let x = let y = 1 in let z = y + 1 in z")
          , ("patterns", "let f [i, j] (a, b) = i + j + a + b\nlet g [i bound 4] = i")
          , ("bound let", "let x = let y bound 10 = 3 in y")
          , ("where clause", "let f [i, j] where i < j & bound i (j + 1) = i")
          , ("pairs, records and projection", "let p = (1, (2.0, true))\nlet r = {a = 1, b = 2}\nlet s = r.a + proj 0 [1, 2]")
          , ("projection of a parenthesised expression", "let s = ({a = 1}).a")
          , ("strings and env", "let n = get_env_int \"N\"\nlet a = read_array_float [n, 2] \"data.csv\"")
          , ("replicate and slice", "let a = generate [4] (fn [i] => i)\nlet b = replicate [2, All] a\nlet c = slice [[1, 2]] a")
          , ("stencil boundaries", "let a = fill [5] 1.0\nlet b = stencil (constant 0.0) (fn acc => acc (-1) +. acc 1) a\nlet c = stencil clamp (fn acc => acc 0) a")
          , ("iterate_until", "let a = iterate_until 100 0 (fn x => x + 1) (fn x => x >= 7)")
          , ("regroup declaration", "let regroup mx a b = if a < b then b else a")
          ]
    forM_ cases $ \(name, src) ->
      it name $ roundTrips src

  describe "round-trips every .hyd program in the repository" $ do
    files <- runIO (concat <$> mapM hydFiles ["examples", "bench", "demo"])
    forM_ files $ \path ->
      it path $ do
        src <- BS.readFile path
        case readDecs src of
          -- Only programs the parser accepts are in scope.
          Left _ -> pendingWith "does not parse"
          Right _ -> roundTrips src

