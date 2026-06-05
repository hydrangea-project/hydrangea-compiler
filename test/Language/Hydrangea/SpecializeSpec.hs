{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.SpecializeSpec (spec) where

import Test.Hspec
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List (isInfixOf, isPrefixOf)

import Language.Hydrangea.Frontend (readDecs, preprocessDecs, monomorphizeProgram)
import Language.Hydrangea.Infer (defaultInferOptions)
import Language.Hydrangea.Syntax (Dec(..))

-- | Parse, preprocess (uniquify/normalize/fuse), and run the monomorphization
-- pass, returning the resulting top-level declaration names as strings.
monoNames :: BS.ByteString -> IO [String]
monoNames src =
  case readDecs src of
    Left err -> error ("parse error: " ++ err)
    Right decs -> do
      mono <- monomorphizeProgram defaultInferOptions (preprocessDecs decs)
      pure [BS.unpack n | Dec _ n _ _ _ _ <- mono]

specClones :: [String] -> [String]
specClones = filter ("__spec_" `isInfixOf`)

spec :: Spec
spec = describe "Language.Hydrangea.Specialize (monomorphization)" $ do

  it "is the identity on a fully monomorphic program" $ do
    names <- monoNames $ BS.pack $ unlines
      [ "let a = 1"
      , "let b = a + 1"
      ]
    -- No specialization clones are introduced; the original bindings remain.
    specClones names `shouldBe` []
    names `shouldContain` ["a"]
    names `shouldContain` ["b"]

  it "specializes a polymorphic helper applied at a single ground type" $ do
    names <- monoNames $ BS.pack $ unlines
      [ "let id x = x"
      , "let main = foldl (fn acc x => acc +. x) 0.0"
      , "             (id (generate [4] (fn [i] => float_of i)))"
      ]
    -- The polymorphic 'id' is specialized to a single clone, and the original
    -- (now fully specialized away) is dropped.
    length (specClones names) `shouldBe` 1
    names `shouldNotContain` ["id"]
    names `shouldContain` ["main"]

  it "specializes one polymorphic helper at two distinct element types" $ do
    names <- monoNames $ BS.pack $ unlines
      [ "let dup a = zipwith (fn x y => x) a a"
      , "let main ="
      , "  let fs = foldl (fn acc x => acc +. x) 0.0"
      , "             (dup (generate [4] (fn [i] => float_of i))) in"
      , "  let is = foldl (fn acc x => acc + x) 0"
      , "             (dup (generate [4] (fn [i] => i))) in"
      , "  fs +. (float_of is)"
      ]
    -- Two distinct call sites yield two specialized clones.
    length (specClones names) `shouldBe` 2
    names `shouldNotContain` ["dup"]

  it "retains an uncalled (exported) polymorphic helper" $ do
    names <- monoNames $ BS.pack $ unlines
      [ "let step a = map (fn x => x) a"
      ]
    -- With no call site to specialize against, the polymorphic original is kept
    -- so it is still emitted, and no clone is produced.
    specClones names `shouldBe` []
    names `shouldContain` ["step"]

  it "does not duplicate a shared monomorphic helper" $ do
    names <- monoNames $ BS.pack $ unlines
      [ "let addone x = x + 1"
      , "let a = addone 1"
      , "let b = addone 2"
      ]
    -- 'addone' is monomorphic, so it is shared (not cloned) across both uses.
    specClones names `shouldBe` []
    length (filter ("addone" `isPrefixOf`) names) `shouldBe` 1