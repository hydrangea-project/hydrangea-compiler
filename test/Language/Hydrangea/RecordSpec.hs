{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.RecordSpec (spec) where

import Data.ByteString.Lazy.Char8 qualified as BS
import Data.List (isInfixOf)
import Data.Map qualified as Map
import Test.Hspec

import Language.Hydrangea.CFG (Program(..), Proc(..))
import Language.Hydrangea.CFGCore (CType(..))
import Language.Hydrangea.Frontend
import Language.Hydrangea.Interpreter (Value(..))
import Language.Hydrangea.Syntax

expectType :: String -> Polytype -> Expectation
expectType src expected =
  case readExp (fromString src) of
    Left err -> expectationFailure err
    Right exp' -> do
      res <- typeCheckExp exp'
      case res of
        Left msg -> expectationFailure msg
        Right actual -> stripPreds actual `shouldBe` stripPreds expected

expectTypeError :: String -> String -> Expectation
expectTypeError src expectedFragment =
  case readExp (fromString src) of
    Left err -> expectationFailure $ "Parse error: " ++ err
    Right exp' -> do
      res <- typeCheckExp exp'
      case res of
        Left msg -> msg `shouldSatisfy` isInfixOf expectedFragment
        Right t -> expectationFailure $ "Expected type error, got: " ++ show t

expectVal :: String -> Value -> Expectation
expectVal src expected = do
  res <- readAndEval (fromString src)
  case res of
    Left err -> expectationFailure $ "Eval error: " ++ err
    Right v -> v `shouldBe` expected

emitC :: String -> IO String
emitC src =
  case readDecs (fromString src) of
    Left err -> fail $ "Parse error: " ++ err
    Right ds -> compileToCOptIO False ds

lowerFromSource :: String -> IO (Program, Proc)
lowerFromSource src =
  case readDecs (fromString src) of
    Left err -> fail $ "Parse error: " ++ err
    Right ds -> do
      prog@(Program procs) <- lowerToCFG2WithTypes ds
      case procs of
        proc:_ -> pure (prog, proc)
        [] -> fail "Expected at least one proc"

stripPreds :: Polytype -> Polytype
stripPreds (Forall vars _ ty) = Forall vars [] (stripRefines ty)

stripRefines :: Type -> Type
stripRefines ty = case ty of
  TyRefine _ inner -> stripRefines inner
  TyCons t1 t2 -> TyCons (stripRefines t1) (stripRefines t2)
  TyFun t1 t2 -> TyFun (stripRefines t1) (stripRefines t2)
  TyArray s e -> TyArray (stripRefines s) (stripRefines e)
  TyPair t1 t2 -> TyPair (stripRefines t1) (stripRefines t2)
  TyRecord fields -> tyRecord [(field, stripRefines fieldTy) | (field, fieldTy) <- fields]
  other -> other

fromString :: String -> BS.ByteString
fromString = BS.pack

spec :: Spec
spec = do
  describe "Record - parsing and type inference" $ do
    it "infers structural record types from literals" $
      expectType
        "{x = 1, y = 2.0}"
        (Forall [] [] (tyRecord [("x", TyInt), ("y", TyFloat)]))

    it "normalizes field order in type annotations" $
      expectType
        "let r : {y : float, x : int} = {x = 1, y = 2.0} in r.x"
        (Forall [] [] TyInt)

    it "rejects duplicate fields in a record literal" $
      expectTypeError "{x = 1, x = 2}" "Duplicate record field"

    it "rejects missing fields during projection" $
      expectTypeError "{x = 1}.y" "Missing record field"

  describe "Record - interpretation" $ do
    it "evaluates record projection" $
      expectVal "let r = {x = 1, y = 2} in r.y" (VInt 2)

  describe "Record - lowering and code generation" $ do
    it "threads CTRecord through procTypeEnv" $ do
      (_prog, Proc { procTypeEnv = tenv }) <- lowerFromSource "let main = {x = 1, y = 2.0}"
      let records = [fields | (_, CTRecord fields) <- Map.toList tenv]
      records `shouldSatisfy` (not . null)
      records `shouldContain` [[("x", CTInt64), ("y", CTDouble)]]

    it "emits record struct typedefs and designated initializers" $ do
      c <- emitC "let main = {x = 1, y = 2.0}"
      c `shouldSatisfy` isInfixOf "typedef struct"
      c `shouldSatisfy` isInfixOf "int64_t x;"
      c `shouldSatisfy` isInfixOf "double y;"
      c `shouldSatisfy` isInfixOf ".x ="
      c `shouldSatisfy` isInfixOf ".y ="

    it "emits named field access in generated C" $ do
      c <- emitC "let main = let r = {x = 1, y = 2.0} in r.x"
      c `shouldSatisfy` isInfixOf ".x"

    it "supports arrays of records in emitted C" $ do
      c <- emitC $ unlines
        [ "let arr = generate [3] (let f [i] = {x = i, y = i + 1} in f)"
        , "let main = let r = index [1] arr in r.y"
        ]
      c `shouldNotSatisfy` isInfixOf "#error"
      c `shouldSatisfy` isInfixOf "((hyd_record_x_i_y_i_t*)(void*)"
      c `shouldSatisfy` isInfixOf ".y"
