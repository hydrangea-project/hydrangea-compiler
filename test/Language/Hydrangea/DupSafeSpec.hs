{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Fusion must never copy a nondeterministic accumulation into a place
-- where it is evaluated more than once.  The interpreter is deterministic and
-- cannot observe a duplicated scatter, so these tests check the structure of
-- the fused program: how many scatters it evaluates, and where.
--
-- The checks walk the fused AST rather than its pretty-printed text, so they
-- do not depend on how the printer lays the program out.
module Language.Hydrangea.DupSafeSpec (spec) where

import Data.ByteString.Char8 qualified as SBS
import Data.ByteString.Lazy.Char8 qualified as BS

import Language.Hydrangea.DupSafe (universe)
import Language.Hydrangea.Frontend
import Language.Hydrangea.Lexer (Range)
import Language.Hydrangea.Syntax
import Test.Hspec

-- | The fused program.
fused :: BS.ByteString -> IO [Dec Range]
fused src =
  case readDecs src of
    Left err -> fail ("parse error: " ++ err)
    Right parsed -> pure (preprocessDecs parsed)

-- | Every expression in the program, top-level bodies and all they contain.
allExps :: [Dec Range] -> [Exp Range]
allExps decs = concat [universe body | Dec _ _ _ _ _ body <- decs]

-- | Every expression in one top-level declaration.
expsOf :: Var -> [Dec Range] -> [Exp Range]
expsOf name decs = concat [universe body | Dec _ v _ _ _ body <- decs, v == name]

isScatter :: Exp a -> Bool
isScatter = \case
  EScatter {}         -> True
  EScatterGuarded {}  -> True
  EScatterGenerate {} -> True
  EScatterChain {}    -> True
  EScatterGen {}      -> True
  _                   -> False

-- | How many scatters the program evaluates (counted syntactically).
scatterCount :: [Exp a] -> Int
scatterCount = length . filter isScatter

-- | Is the source variable @name@ still bound by a local @let@ (i.e. not
-- inlined at its uses)?  Preprocessing renames binders apart (@h@ becomes
-- @h__uniq_4@), so compare against the name the program was written with.
letBound :: Var -> [Exp a] -> Bool
letBound name es =
  not (null [() | ELetIn _ (Dec _ v _ _ _ _) _ <- es, sourceName v == name])
  where
    sourceName v = case SBS.breakSubstring "__" (BS.toStrict v) of
      (base, _) -> BS.fromStrict base

-- | Does a scatter sit inside a generate's per-element function?
scatterInGenerateFn :: [Exp a] -> Bool
scatterInGenerateFn es = or [any isScatter (universe f) | EGenerate _ _ f <- es]

-- | Does a map sit inside the array argument of a replicate?
mapInsideReplicate :: [Exp a] -> Bool
mapInsideReplicate es = or [any isMap (universe arr) | EReplicate _ _ arr <- es]
  where
    isMap = \case
      EMap {} -> True
      _       -> False

-- | A float histogram whose value depends on the accumulation order.
prelude :: [BS.ByteString]
prelude =
  [ "let bucket [i] = (i * 7) % 4"
  , "let weight [i] = (float_of i) *. 0.1"
  , "let hist = scatter (+.) (fill [4] 0.0) (generate [64] bucket) (generate [64] weight)"
  ]


spec :: Spec
spec = describe "fusion never duplicates an accumulation" $ do
  it "does not inline a multi-use generate whose body scatters" $ do
    out <- fused $ BS.unlines
      [ "let bucket [i] = (i * 7) % 4"
      , "let weight [i] = (float_of i) *. 0.1"
      , "let main ="
      , "  let h = generate [4] (let f [b] ="
      , "            index [b] (scatter (+.) (fill [4] 0.0) (generate [64] bucket) (generate [64] weight))"
      , "          in f) in"
      , "  (index [0] h) -. (index [0] h)"
      ]
    scatterCount (allExps out) `shouldBe` 1

  it "sees an accumulation behind a local function call" $ do
    out <- fused $ BS.unlines
      [ "let bucket [i] = (i * 7) % 4"
      , "let weight [i] = (float_of i) *. 0.1"
      , "let main ="
      , "  let acc [b] = index [b] (scatter (+.) (fill [4] 0.0) (generate [64] bucket) (generate [64] weight)) in"
      , "  let h = generate [4] acc in"
      , "  (index [0] h) -. (index [0] h)"
      ]
    letBound "h" (expsOf "main" out) `shouldBe` True

  it "sees an accumulating function passed as an argument" $ do
    out <- fused $ BS.unlines
      [ "let bucket [i] = (i * 7) % 4"
      , "let weight [i] = (float_of i) *. 0.1"
      , "let acc [b] = index [b] (scatter (+.) (fill [4] 0.0) (generate [64] bucket) (generate [64] weight))"
      , "let twice g = let h = generate [4] g in (index [0] h) -. (index [0] h)"
      , "let main = twice acc"
      ]
    letBound "h" (allExps out) `shouldBe` True

  it "does not beta-substitute an accumulating argument used twice" $ do
    out <- fused $ BS.unlines $ prelude ++
      [ "let main = (let d y = y -. y in d) (index [0] (scatter (+.) (fill [4] 0.0) (generate [64] bucket) (generate [64] weight)))"
      ]
    -- One scatter for `hist`, exactly one for main.
    scatterCount (expsOf "hist" out) `shouldBe` 1
    scatterCount (expsOf "main" out) `shouldBe` 1

  it "does not move a single-use scatter into a per-element function body" $ do
    out <- fused $ BS.unlines
      [ "let bucket [i] = (i * 7) % 4"
      , "let weight [i] = (float_of i) *. 0.1"
      , "let main ="
      , "  let s = scatter (+.) (fill [4] 0.0) (generate [64] bucket) (generate [64] weight) in"
      , "  generate [8] (let g [i] = index [0] s in g)"
      ]
    let mainExps = expsOf "main" out
    scatterCount mainExps `shouldBe` 1
    scatterInGenerateFn mainExps `shouldBe` False

  it "still inlines a multi-use generate whose body cannot accumulate" $ do
    out <- fused $ BS.unlines
      [ "let main ="
      , "  let g = generate [8] (let f [i] = i * 2 in f) in"
      , "  zipwith (+) g g"
      ]
    letBound "g" (expsOf "main" out) `shouldBe` False

  it "does not commute a map with an accumulating function out of a replicate" $ do
    out <- fused $ BS.unlines
      [ "let bucket [i] = (i * 7) % 4"
      , "let weight [i] = (float_of i) *. 0.1"
      , "let main ="
      , "  let xs = reify (generate [4] (fn [i] => i)) in"
      , "  replicate [2, All] (map (fn b => index [b] (scatter (+.) (fill [4] 0.0) (generate [64] bucket) (generate [64] weight))) xs)"
      ]
    mapInsideReplicate (expsOf "main" out) `shouldBe` True

  it "still commutes a map with a plain function out of a replicate" $ do
    out <- fused $ BS.unlines
      [ "let main ="
      , "  let xs = reify (generate [4] (fn [i] => i)) in"
      , "  replicate [2, All] (map (fn b => b * 3) xs)"
      ]
    mapInsideReplicate (expsOf "main" out) `shouldBe` False
