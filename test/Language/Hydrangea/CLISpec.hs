{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.CLISpec (spec) where

import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List (isInfixOf)
import Language.Hydrangea.CLI
  ( allTopLevelProcsFlag
  , mainFlag
  , resolvePruneToMain
  , selectProgramDecs
  )
import Language.Hydrangea.Frontend (compileToCOptIO, readDecs)
import Language.Hydrangea.Lexer (Range)
import Language.Hydrangea.Syntax (Dec(..))
import Test.Hspec

parseDecsOrFail :: BS.ByteString -> IO [Dec Range]
parseDecsOrFail src =
  case readDecs src of
    Left err -> fail err
    Right decs -> pure decs

spec :: Spec
spec = do
  describe "resolvePruneToMain" $ do
    it "prunes to main by default" $ do
      resolvePruneToMain [] `shouldBe` Right True

    it "keeps --main as a compatibility alias for the default" $ do
      resolvePruneToMain [mainFlag] `shouldBe` Right True

    it "supports opting out to keep all top-level procedures" $ do
      resolvePruneToMain [allTopLevelProcsFlag] `shouldBe` Right False

    it "rejects conflicting entrypoint flags" $ do
      resolvePruneToMain [mainFlag, allTopLevelProcsFlag]
        `shouldSatisfy` either (isInfixOf "--all-top-level-procs") (const False)

  describe "selectProgramDecs" $ do
    it "keeps only the declarations reachable from main by default" $ do
      decs <- parseDecsOrFail $ BS.pack $ unlines
        [ "let helper = 1"
        , "let dead = 2"
        , "let main = helper"
        ]
      case selectProgramDecs [] decs of
        Left err -> expectationFailure err
        Right kept -> do
          let names = [name | Dec _ name _ _ _ <- kept]
          names `shouldBe` ["helper", "main"]

    it "keeps every top-level declaration with the opt-out flag" $ do
      decs <- parseDecsOrFail $ BS.pack $ unlines
        [ "let helper = 1"
        , "let dead = 2"
        , "let main = helper"
        ]
      case selectProgramDecs [allTopLevelProcsFlag] decs of
        Left err -> expectationFailure err
        Right kept -> do
          let names = [name | Dec _ name _ _ _ <- kept]
          names `shouldBe` ["helper", "dead", "main"]

    it "changes the generated C surface when dead top-level kernels are kept" $ do
      decs <- parseDecsOrFail $ BS.pack $ unlines
        [ "let helper = 1"
        , "let dead = 2"
        , "let main = helper"
        ]
      prunedDecs <- case selectProgramDecs [] decs of
        Left err -> expectationFailure err >> fail err
        Right kept -> pure kept
      keptDecs <- case selectProgramDecs [allTopLevelProcsFlag] decs of
        Left err -> expectationFailure err >> fail err
        Right kept -> pure kept
      prunedC <- compileToCOptIO False prunedDecs
      unprunedC <- compileToCOptIO False keptDecs
      prunedC `shouldNotSatisfy` isInfixOf "dead(void)"
      unprunedC `shouldSatisfy` isInfixOf "dead(void)"
