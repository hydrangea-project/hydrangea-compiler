{-# LANGUAGE OverloadedStrings #-}

-- | Fusion must never copy a nondeterministic accumulation into a place
-- where it is evaluated more than once.  The interpreter is deterministic and
-- cannot observe a duplicated scatter, so these tests check the structure of
-- the fused program: how many scatters it evaluates, and where.
module Language.Hydrangea.DupSafeSpec (spec) where

import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List (isInfixOf, isPrefixOf, tails)
import Text.PrettyPrint (render)
import Text.PrettyPrint.HughesPJClass (pPrint)

import Language.Hydrangea.Frontend
import Test.Hspec

-- | The fused program, pretty-printed.
fused :: BS.ByteString -> IO String
fused src =
  case readDecs src of
    Left err -> fail ("parse error: " ++ err)
    Right parsed -> pure (unlines (map (render . pPrint) (preprocessDecs parsed)))

count :: String -> String -> Int
count needle hay = length (filter (needle `isPrefixOf`) (tails hay))

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
    count "scatter" out `shouldBe` 1

  it "sees an accumulation behind a local function call" $ do
    out <- fused $ BS.unlines
      [ "let bucket [i] = (i * 7) % 4"
      , "let weight [i] = (float_of i) *. 0.1"
      , "let main ="
      , "  let acc [b] = index [b] (scatter (+.) (fill [4] 0.0) (generate [64] bucket) (generate [64] weight)) in"
      , "  let h = generate [4] acc in"
      , "  (index [0] h) -. (index [0] h)"
      ]
    "let h" `isInfixOf` out `shouldBe` True

  it "sees an accumulating function passed as an argument" $ do
    out <- fused $ BS.unlines
      [ "let bucket [i] = (i * 7) % 4"
      , "let weight [i] = (float_of i) *. 0.1"
      , "let acc [b] = index [b] (scatter (+.) (fill [4] 0.0) (generate [64] bucket) (generate [64] weight))"
      , "let twice g = let h = generate [4] g in (index [0] h) -. (index [0] h)"
      , "let main = twice acc"
      ]
    "let h" `isInfixOf` out `shouldBe` True

  it "does not beta-substitute an accumulating argument used twice" $ do
    out <- fused $ BS.unlines $ prelude ++
      [ "let main = (let d y = y -. y in d) (index [0] (scatter (+.) (fill [4] 0.0) (generate [64] bucket) (generate [64] weight)))"
      ]
    -- One scatter for `hist`, exactly one for main.
    count "scatter" out `shouldBe` 2

  it "does not move a single-use scatter into a per-element function body" $ do
    out <- fused $ BS.unlines
      [ "let bucket [i] = (i * 7) % 4"
      , "let weight [i] = (float_of i) *. 0.1"
      , "let main ="
      , "  let s = scatter (+.) (fill [4] 0.0) (generate [64] bucket) (generate [64] weight) in"
      , "  generate [8] (let g [i] = index [0] s in g)"
      ]
    "= scatter" `isInfixOf` out `shouldBe` True

  it "still inlines a multi-use generate whose body cannot accumulate" $ do
    out <- fused $ BS.unlines
      [ "let main ="
      , "  let g = generate [8] (let f [i] = i * 2 in f) in"
      , "  zipwith (+) g g"
      ]
    "let g" `isInfixOf` out `shouldBe` False

  it "does not commute a map with an accumulating function out of a replicate" $ do
    out <- fused $ BS.unlines
      [ "let bucket [i] = (i * 7) % 4"
      , "let weight [i] = (float_of i) *. 0.1"
      , "let main ="
      , "  let xs = reify (generate [4] (fn [i] => i)) in"
      , "  replicate [2, All] (map (fn b => index [b] (scatter (+.) (fill [4] 0.0) (generate [64] bucket) (generate [64] weight))) xs)"
      ]
    mapInsideReplicate out `shouldBe` True

  it "still commutes a map with a plain function out of a replicate" $ do
    out <- fused $ BS.unlines
      [ "let main ="
      , "  let xs = reify (generate [4] (fn [i] => i)) in"
      , "  replicate [2, All] (map (fn b => b * 3) xs)"
      ]
    mapInsideReplicate out `shouldBe` False
  where
    -- In main's printed body, does "replicate" come before "map"?
    mapInsideReplicate out =
      let mainLine = concat [l | l <- lines out, "let main" `isPrefixOf` l]
          firstAt needle = length (takeWhile (not . (needle `isPrefixOf`)) (tails mainLine))
       in firstAt "replicate" < firstAt "map"
