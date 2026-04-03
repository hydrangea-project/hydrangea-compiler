{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.PruneSpec (spec) where

import Test.Hspec
import qualified Data.ByteString.Lazy.Char8 as BS
import Language.Hydrangea.Frontend (readDecs)
import Language.Hydrangea.Prune (pruneDecsToMain)
import Language.Hydrangea.Syntax (Dec(..))

spec :: Spec
spec = describe "pruneDecsToMain" $ do
  it "returns error when last top-level binding is not named main" $ do
    let src = BS.pack "let a = 1\nlet b = 2\n"
    case readDecs src of
      Left e -> expectationFailure e
      Right decs -> case pruneDecsToMain decs of
        Left _ -> pure ()
        Right _ -> expectationFailure "Expected error when last binding not named main"

  it "keeps only reachable top-level bindings from main" $ do
    let src = BS.pack $ unlines
          [ "let a = 1"
          , "let unused = 3"
          , "let f x = x + a"
          , "let main = f 10"
          ]
    case readDecs src of
      Left e -> expectationFailure e
      Right decs -> case pruneDecsToMain decs of
        Left err -> expectationFailure err
        Right pr -> do
          let names = [v | Dec _ v _ _ _ _ <- pr]
          names `shouldSatisfy` ("unused" `notElem`)
          names `shouldSatisfy` ("a" `elem`)
          names `shouldSatisfy` ("f" `elem`)
          names `shouldSatisfy` ("main" `elem`)

  it "keeps mutually recursive functions" $ do
    let src = BS.pack $ unlines
          [ "let a = b 1"
          , "let b x = if x = 0 then 1 else a"
          , "let main = a"
          ]
    case readDecs src of
      Left e -> expectationFailure e
      Right decs -> case pruneDecsToMain decs of
        Left err -> expectationFailure err
        Right pr -> do
          let names = [v | Dec _ v _ _ _ _ <- pr]
          names `shouldSatisfy` ("a" `elem`)
          names `shouldSatisfy` ("b" `elem`)
          names `shouldSatisfy` ("main" `elem`)

  it "does not treat locally-shadowed names as references to top-level" $ do
    let src = BS.pack $ unlines
          [ "let unusedFoo = 999"
          , "let f x = let foo = 10 in foo + x"
          , "let main = f 1"
          ]
    case readDecs src of
      Left e -> expectationFailure e
      Right decs -> case pruneDecsToMain decs of
        Left err -> expectationFailure err
        Right pr -> do
          let names = [v | Dec _ v _ _ _ _ <- pr]
          names `shouldSatisfy` ("f" `elem`)
          names `shouldSatisfy` ("main" `elem`)
          names `shouldSatisfy` (not . ("unusedFoo" `elem`))

  it "handles pattern-bound parameter names and drops unrelated bindings" $ do
    let src = BS.pack $ unlines
          [ "let p [x,y] = x + y"
          , "let orphan = 5"
          , "let main = p [1,2]"
          ]
    case readDecs src of
      Left e -> expectationFailure e
      Right decs -> case pruneDecsToMain decs of
        Left err -> expectationFailure err
        Right pr -> do
          let names = [v | Dec _ v _ _ _ _ <- pr]
          names `shouldSatisfy` ("p" `elem`)
          names `shouldSatisfy` ("main" `elem`)
          names `shouldSatisfy` (not . ("orphan" `elem`))

  it "drops indirect unreachable bindings" $ do
    let src = BS.pack $ unlines
          [ "let a = 1"
          , "let b = a + 2"
          , "let c = 3"
          , "let main = b"
          ]
    case readDecs src of
      Left e -> expectationFailure e
      Right decs -> case pruneDecsToMain decs of
        Left err -> expectationFailure err
        Right pr -> do
          let names = [v | Dec _ v _ _ _ _ <- pr]
          names `shouldSatisfy` ("a" `elem`)
          names `shouldSatisfy` ("b" `elem`)
          names `shouldSatisfy` ("main" `elem`)
          names `shouldSatisfy` (not . ("c" `elem`))
