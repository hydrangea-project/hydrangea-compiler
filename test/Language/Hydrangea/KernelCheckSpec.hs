{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.KernelCheckSpec
--
-- Tests for the --kernel fusion-completeness check (checkKernelFused).
--
-- The check verifies that after the fusion pass, the named kernel's fused body
-- contains no free references to any other ORIGINAL top-level binding names.
-- Bindings that were never referenced by the kernel (dead code) trivially pass
-- the check.  Bindings that ARE still referenced after fusion were not
-- fused/inlined (an error is reported naming them).
module Language.Hydrangea.KernelCheckSpec (spec) where

import Data.ByteString.Lazy.Char8 qualified as BS
import Language.Hydrangea.Frontend (readDecs)
import Language.Hydrangea.Prune (checkKernelFused)
import Test.Hspec

-- | Parse declarations from source and run checkKernelFused on them.
runCheck :: BS.ByteString -> BS.ByteString -> Either String ()
runCheck src kernelName =
  case readDecs src of
    Left err -> Left ("Parse error: " ++ err)
    Right decs -> checkKernelFused decs kernelName

spec :: Spec
spec = describe "checkKernelFused" $ do

  describe "success cases" $ do

    it "accepts a kernel that has no other top-level bindings" $ do
      let src = "let kernel = fill [4] 1"
      runCheck src "kernel" `shouldBe` Right ()

    it "accepts a kernel when the other binding is dead code (not referenced)" $ do
      -- 'unused' is defined but kernel's body doesn't reference it, so it's
      -- dead code and trivially passes the check.
      let src = BS.unlines
            [ "let unused = fill [4] 99"
            , "let kernel = fill [4] 0"
            ]
      runCheck src "kernel" `shouldBe` Right ()

    it "accepts a kernel when it uses only local let-bindings (all fused away)" $ do
      -- All helpers are LOCAL, not top-level, so after fusion the kernel is
      -- fully self-contained and no other top-level names are referenced.
      let src = BS.unlines
            [ "let kernel = let xs = generate [4] (let f [i] = i in f)"
            , "             in map (let g x = x + 1 in g) xs"
            ]
      runCheck src "kernel" `shouldBe` Right ()

    it "accepts when all other top-level bindings are unreachable from kernel" $ do
      let src = BS.unlines
            [ "let a = fill [2] 10"
            , "let b = fill [2] 20"
            , "let kernel = fill [3] 0"
            ]
      runCheck src "kernel" `shouldBe` Right ()

  describe "failure cases" $ do

    it "fails when kernel references a top-level scalar binding" $ do
      -- 'n' is a top-level binding referenced in kernel's body.  The fusion
      -- pass cannot inline top-level binding references, so 'n' remains.
      let src = BS.unlines
            [ "let n = 10"
            , "let kernel = fill [n] 0"
            ]
      case runCheck src "kernel" of
        Left msg -> msg `shouldContain` "n"
        Right () -> expectationFailure "Expected kernel check to fail for un-fused binding 'n'"

    it "fails when kernel references a top-level array binding" $ do
      -- 'helper' is a top-level binding; fuseDecs does not inline across
      -- top-level boundaries, so it remains a free reference in kernel.
      let src = BS.unlines
            [ "let helper = generate [8] (let f [i] = i in f)"
            , "let kernel = map (let g x = x + 1 in g) helper"
            ]
      case runCheck src "kernel" of
        Left msg -> msg `shouldContain` "helper"
        Right () -> expectationFailure "Expected kernel check to fail for un-fused binding 'helper'"

    it "reports all unfused binding names in the error message" $ do
      let src = BS.unlines
            [ "let n = 10"
            , "let m = 20"
            , "let kernel = fill [n + m] 0"
            ]
      case runCheck src "kernel" of
        Left msg -> do
          msg `shouldContain` "n"
          msg `shouldContain` "m"
        Right () -> expectationFailure "Expected kernel check to fail"

    it "includes 'Kernel check failed' in the error message" $ do
      let src = BS.unlines
            [ "let dep = fill [4] 1"
            , "let kernel = map (let f x = x in f) dep"
            ]
      case runCheck src "kernel" of
        Left msg -> msg `shouldContain` "Kernel check failed"
        Right () -> expectationFailure "Expected kernel check to fail"

  describe "error cases" $ do

    it "returns Left when the kernel name does not exist" $ do
      let src = "let foo = fill [3] 1"
      case runCheck src "kernel" of
        Left msg -> msg `shouldContain` "kernel"
        Right () -> expectationFailure "Expected an error for missing kernel name"

    it "returns Left for empty declaration list" $ do
      checkKernelFused [] "kernel" `shouldBe`
        Left "Kernel 'kernel' not found in top-level declarations"

