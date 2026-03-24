{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Language.Hydrangea.PairSpec
--
-- Tests for the Pair type: parsing, type inference, interpretation, lowering,
-- and C code generation.
module Language.Hydrangea.PairSpec (spec) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List (isInfixOf)
import Test.Hspec

import Language.Hydrangea.Frontend
import Language.Hydrangea.Interpreter (Value(..))
import Language.Hydrangea.Syntax

-- ---------------------------------------------------------------------------
-- Helpers

expectType :: ByteString -> Polytype -> Expectation
expectType src expected =
  case readExp src of
    Left err -> expectationFailure err
    Right exp' -> do
      res <- typeCheckExp exp'
      case res of
        Left msg -> expectationFailure msg
        Right actual -> stripPreds actual `shouldBe` stripPreds expected

stripPreds :: Polytype -> Polytype
stripPreds (Forall vars _ ty) = Forall vars [] (stripRefines ty)

stripRefines :: Type -> Type
stripRefines ty = case ty of
  TyRefine _ inner -> stripRefines inner
  TyCons t1 t2     -> TyCons (stripRefines t1) (stripRefines t2)
  TyFun t1 t2      -> TyFun (stripRefines t1) (stripRefines t2)
  TyArray s e      -> TyArray (stripRefines s) (stripRefines e)
  TyPair t1 t2     -> TyPair (stripRefines t1) (stripRefines t2)
  other            -> other

expectTypeError :: ByteString -> Expectation
expectTypeError src =
  case readExp src of
    Left err -> expectationFailure $ "Parse error: " ++ err
    Right exp' -> do
      res <- typeCheckExp exp'
      case res of
        Left _  -> pure ()
        Right t -> expectationFailure $ "Expected type error, got: " ++ show t

expectVal :: ByteString -> Value -> Expectation
expectVal src expected = do
  res <- readAndEval src
  case res of
    Left err -> expectationFailure $ "Eval error: " ++ err
    Right v  -> v `shouldBe` expected

emitC :: ByteString -> IO String
emitC src = case readDecs src of
  Left err -> fail $ "Parse error: " ++ err
  Right ds -> pure $ compileToCOpt ds

-- ---------------------------------------------------------------------------

spec :: Spec
spec = do

  describe "Pair - parsing and type inference" $ do

    it "infers (Int, Int) from integer pair literal" $
      expectType "(1, 2)" (Forall [] [] (TyPair TyInt TyInt))

    it "infers (Float, Int) from float/int pair" $
      expectType "(3.14, 0)" (Forall [] [] (TyPair TyFloat TyInt))

    it "infers (Int, Float) from int/float pair" $
      expectType "(0, 3.14)" (Forall [] [] (TyPair TyInt TyFloat))

    it "infers (Float, Float) from two floats" $
      expectType "(1.0, 2.0)" (Forall [] [] (TyPair TyFloat TyFloat))

    it "infers fst type from pair" $
      expectType "fst (1, 2.0)" (Forall [] [] TyInt)

    it "infers snd type from pair" $
      expectType "snd (1, 2.0)" (Forall [] [] TyFloat)

    it "infers nested pair type" $
      expectType "((1.0, 2.0), 3)" (Forall [] [] (TyPair (TyPair TyFloat TyFloat) TyInt))

    it "infers fst of nested pair" $
      expectType "fst ((1.0, 2.0), 3)" (Forall [] [] (TyPair TyFloat TyFloat))

    it "infers snd of nested pair" $
      expectType "snd ((1.0, 2.0), 3)" (Forall [] [] TyInt)

    it "rejects fst on a non-pair, non-shape argument" $
      expectTypeError "fst 42"

  describe "Pair - interpretation" $ do

    it "evaluates integer pair" $
      expectVal "(1, 2)" (VPair (VInt 1) (VInt 2))

    it "evaluates fst on integer pair" $
      expectVal "fst (10, 20)" (VInt 10)

    it "evaluates snd on integer pair" $
      expectVal "snd (10, 20)" (VInt 20)

    it "evaluates float/int pair" $
      expectVal "(3.14, 0)" (VPair (VFloat 3.14) (VInt 0))

    it "evaluates nested pair" $
      expectVal "((1.0, 2.0), 3)" (VPair (VPair (VFloat 1.0) (VFloat 2.0)) (VInt 3))

    it "evaluates fst of nested pair" $
      expectVal "fst ((1.0, 2.0), 3)" (VPair (VFloat 1.0) (VFloat 2.0))

    it "evaluates pair in let binding" $
      expectVal "let p = (42, 7) in fst p" (VInt 42)

    it "evaluates pair built from variables" $
      expectVal "let x = 10 in let y = 20 in (x, y)" (VPair (VInt 10) (VInt 20))

    it "evaluates foldl accumulating a flat pair" $
      expectVal
        "let main = foldl (fn acc _ => (fst acc + 1, snd acc + 2)) (0, 0) (fill [3] 0) in fst main"
        (VInt 3)

    it "evaluates foldl snd component of flat pair" $
      expectVal
        "let main = foldl (fn acc _ => (fst acc + 1, snd acc + 2)) (0, 0) (fill [3] 0) in snd main"
        (VInt 6)

    it "evaluates nested pair foldl" $
      expectVal
        ( "let main = foldl "
        <> "  (fn acc _ => let xy = fst acc in ((fst xy +. 1.0, snd xy +. 1.0), snd acc + 1))"
        <> "  ((0.0, 0.0), 0) (fill [3] 0) in snd main"
        )
        (VInt 3)

  describe "Pair - C code generation" $ do

    it "emits pair struct typedef for (Float, Int)" $ do
      c <- emitC "let main = (0.0, 0)"
      c `shouldSatisfy` isInfixOf "hyd_pair_fi_t"
      c `shouldSatisfy` isInfixOf "double fst"
      c `shouldSatisfy` isInfixOf "int64_t snd"

    it "emits pair struct typedef for (Int, Int)" $ do
      c <- emitC "let main = (1, 2)"
      c `shouldSatisfy` isInfixOf "hyd_pair_ii_t"

    it "emits pair struct typedef for (Float, Float)" $ do
      c <- emitC "let main = (1.0, 2.0)"
      c `shouldSatisfy` isInfixOf "hyd_pair_ff_t"

    it "emits RPairMake as struct initializer" $ do
      c <- emitC "let main = (1.0, 42)"
      c `shouldSatisfy` isInfixOf ".fst ="
      c `shouldSatisfy` isInfixOf ".snd ="

    it "emits RPairFst as .fst field access" $ do
      c <- emitC "let main = fst (1.0, 42)"
      c `shouldSatisfy` isInfixOf ".fst"

    it "emits RPairSnd as .snd field access" $ do
      c <- emitC "let main = snd (1.0, 42)"
      c `shouldSatisfy` isInfixOf ".snd"

    it "generates correct foldl loop with pair accumulator" $ do
      c <- emitC $ "let main = snd (foldl "
              <> "(fn acc _ => (fst acc +. 1.0, snd acc + 1)) "
              <> "(0.0, 0) (fill [3] 0))"
      c `shouldSatisfy` isInfixOf "hyd_pair_fi_t"
      c `shouldSatisfy` isInfixOf "for ("
      -- The accumulator variable should be updated inside the loop
      c `shouldSatisfy` isInfixOf ".snd"

    it "generates nested pair struct typedef" $ do
      c <- emitC "let main = ((1.0, 2.0), 3)"
      -- nested pair: fst is (Float,Float), outer is ((Float,Float), Int)
      c `shouldSatisfy` isInfixOf "hyd_pair_ff_t"

  describe "Pair - IO compilation with type threading" $ do

    it "IO compilation produces correct C for (Float, Int) pair" $ do
      case readDecs "let main = (3.14, 42)" of
        Left err -> expectationFailure $ "Parse error: " ++ err
        Right ds -> do
          csrc <- compileToCOptIO False ds
          csrc `shouldSatisfy` isInfixOf "hyd_pair_fi_t"
          csrc `shouldSatisfy` isInfixOf "double fst"
          csrc `shouldSatisfy` isInfixOf "int64_t snd"

    it "IO compilation produces correct C for foldl with pair accumulator" $ do
      let src = "let main = snd (foldl (fn acc _ => (fst acc +. 1.0, snd acc + 1)) (0.0, 0) (fill [5] 0))"
      case readDecs (BS.pack src) of
        Left err -> expectationFailure $ "Parse error: " ++ err
        Right ds -> do
          csrc <- compileToCOptIO False ds
          csrc `shouldSatisfy` isInfixOf "hyd_pair_fi_t"

    it "IO compilation and pure compilation agree on pair output" $ do
      let src = "let main = (1, 2)"
      case readDecs (BS.pack src) of
        Left err -> expectationFailure $ "Parse error: " ++ err
        Right ds -> do
          let cPure = compileToCOpt ds
          cIO <- compileToCOptIO False ds
          -- Both should define the pair struct (possibly in different order)
          cPure `shouldSatisfy` isInfixOf "hyd_pair_ii_t"
          cIO   `shouldSatisfy` isInfixOf "hyd_pair_ii_t"

    it "supports arrays of pairs in emitted C" $ do
      let src = unlines
            [ "let arr = generate [3] (let f [i] = (i, i + 1) in f)"
            , "let main = snd (index [1] arr)"
            ]
      case readDecs (BS.pack src) of
        Left err -> expectationFailure $ "Parse error: " ++ err
        Right ds -> do
          csrc <- compileToCOptIO False ds
          csrc `shouldNotSatisfy` isInfixOf "#error"
          csrc `shouldSatisfy` isInfixOf "((hyd_pair_ii_t*)(void*)"
          csrc `shouldSatisfy` isInfixOf ".snd"
