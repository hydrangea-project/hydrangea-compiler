{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.InterpreterSpec (spec) where

import Control.Monad.Except (runExceptT)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.String (fromString)
import Data.Either (isLeft)
import Language.Hydrangea.Frontend
import Language.Hydrangea.Syntax
import Language.Hydrangea.Interpreter
import System.Directory (getTemporaryDirectory, removeFile)
import System.FilePath ((</>))
import Test.Hspec

-- Helper to evaluate a string expression and compare the result value
expectEval :: ByteString -> Value -> Expectation
expectEval src expected = do
  res <- readAndEval src
  case res of
    Left err -> expectationFailure $ "Parse error: " ++ err
    Right actual -> actual `shouldBe` expected

-- Helper to expect an evaluation error
expectEvalError :: ByteString -> Expectation
expectEvalError src = do
  res <- readAndEval src
  case res of
    Left _ -> pure ()
    Right val -> expectationFailure $ "Expected error but got: " ++ show val

-- Helper to evaluate an expression and get its value
evalString :: ByteString -> IO (Either String Value)
evalString src = case readExp src of
  Left err -> pure $ Left err
  Right exp' -> do
    res <- evalExpFrontend exp'
    pure $ case res of
      Left err -> Left $ show err
      Right val -> Right val

-- Evaluate a string expression after typechecking and fusion
evalFusedString :: ByteString -> IO (Either String Value)
evalFusedString src =
  case readExp src of
    Left err -> pure $ Left err
    Right exp' -> do
      fusedRes <- fuseExpAfterTypeCheck exp'
      case fusedRes of
        Left err -> pure $ Left err
        Right fused -> do
          res <- evalExpFrontend fused
          pure $ case res of
            Left err -> Left $ show err
            Right val -> Right val

expectFusedSame :: ByteString -> Expectation
expectFusedSame src = do
  original <- evalString src
  fused <- evalFusedString src
  case (original, fused) of
    (Right v1, Right v2) -> v1 `shouldBe` v2
    (Left err, _) -> expectationFailure $ "Original eval failed: " ++ err
    (_, Left err) -> expectationFailure $ "Fused eval failed: " ++ err

spec :: Spec
spec = do
  describe "Interpreter - Primitives" $ do
    it "evaluates integer literals" $ do
      expectEval "42" (VInt 42)
      expectEval "-10" (VInt (-10))
      expectEval "0" (VInt 0)

    it "evaluates boolean literals" $ do
      expectEval "true" (VBool True)
      expectEval "false" (VBool False)

    it "evaluates unit" $ do
      expectEval "()" VUnit

  describe "Interpreter - Arithmetic" $ do
    it "evaluates addition" $ do
      expectEval "1 + 2" (VInt 3)
      expectEval "10 + (-5)" (VInt 5)

    it "evaluates subtraction" $ do
      expectEval "5 - 3" (VInt 2)
      expectEval "0 - 10" (VInt (-10))

    it "evaluates multiplication" $ do
      expectEval "3 * 4" (VInt 12)
      expectEval "5 * 0" (VInt 0)

    it "evaluates division" $ do
      expectEval "10 / 2" (VInt 5)
      expectEval "7 / 2" (VInt 3)

    it "evaluates negation" $ do
      expectEval "-42" (VInt (-42))
      expectEval "-(5 + 3)" (VInt (-8))

    it "evaluates complex arithmetic" $ do
      expectEval "1 + 2 * 3" (VInt 7)

  describe "Interpreter - Comparisons" $ do
    it "evaluates equality" $ do
      expectEval "1 = 1" (VBool True)
      expectEval "1 = 2" (VBool False)
      expectEval "true = true" (VBool True)

    it "evaluates inequality" $ do
      expectEval "1 <> 2" (VBool True)
      expectEval "1 <> 1" (VBool False)

    it "evaluates less than" $ do
      expectEval "1 < 2" (VBool True)
      expectEval "2 < 1" (VBool False)
      expectEval "1 < 1" (VBool False)

    it "evaluates less than or equal" $ do
      expectEval "1 <= 2" (VBool True)
      expectEval "1 <= 1" (VBool True)
      expectEval "2 <= 1" (VBool False)

    it "evaluates greater than" $ do
      expectEval "2 > 1" (VBool True)
      expectEval "1 > 2" (VBool False)

    it "evaluates greater than or equal" $ do
      expectEval "2 >= 1" (VBool True)
      expectEval "1 >= 1" (VBool True)

  describe "Interpreter - Boolean Logic" $ do
    it "evaluates boolean OR (disjunction)" $ do
      expectEval "true | false" (VBool True)
      expectEval "false | false" (VBool False)
      expectEval "true | true" (VBool True)

  describe "Interpreter - Let Bindings" $ do
    it "evaluates simple let bindings" $ do
      expectEval "let x = 10 in x" (VInt 10)
      expectEval "let x = 5 in x + x" (VInt 10)

    it "evaluates let bindings with expressions" $ do
      expectEval "let x = 1 + 2 in x * 3" (VInt 9)

    it "evaluates nested let bindings" $ do
      expectEval "let x = 10 in let y = 20 in x + y" (VInt 30)

    it "evaluates function definitions" $ do
      expectEval "let f x = x + 1 in f 5" (VInt 6)
      expectEval "let f x = x * 2 in f 3" (VInt 6)

    it "evaluates multi-argument functions" $ do
      expectEval "let f x y = x + y in f 2 3" (VInt 5)
      expectEval "let f x y z = x + y + z in f 1 2 3" (VInt 6)

  describe "Interpreter - If-Then-Else" $ do
    it "evaluates if-then-else with true condition" $ do
      expectEval "if true then 10 else 20" (VInt 10)

    it "evaluates if-then-else with false condition" $ do
      expectEval "if false then 10 else 20" (VInt 20)

    it "evaluates if-then-else with computed condition" $ do
      expectEval "if 5 > 3 then 100 else 200" (VInt 100)
      expectEval "if 5 < 3 then 100 else 200" (VInt 200)

    it "evaluates if-then with true condition" $ do
      expectEval "if true then 10" (VInt 10)
      expectEval "if false then 10" VUnit

  describe "Interpreter - Arrays" $ do
    it "evaluates fill to create arrays" $ do
      expectEval "fill [3] 5" (VArray [3] [VInt 5, VInt 5, VInt 5])
      expectEval "fill [2] true" (VArray [2] [VBool True, VBool True])

    it "evaluates generate to create arrays" $ do
      expectEval "let f [i] = i in generate [3] f" (VArray [3] [VInt 0, VInt 1, VInt 2])
      expectEval "let f [i] = i + 1 in generate [3] f" (VArray [3] [VInt 1, VInt 2, VInt 3])

    it "evaluates index to extract array elements" $ do
      expectEval "index [0] (fill [3] 5)" (VInt 5)
      expectEval "index [2] (fill [3] 5)" (VInt 5)

    it "evaluates check_index with in-bounds index" $ do
      expectEval "check_index [0] 999 (fill [3] 5)" (VInt 5)

    it "evaluates map over arrays" $ do
      expectEval "let f x = x + 1 in map f (generate [3] (let g [i] = i in g))" (VArray [3] [VInt 1, VInt 2, VInt 3])

    it "evaluates reduce on arrays" $ do
      let code = "let add x y = x + y in reduce add 0 (generate [3] (let f [i] = i + 1 in f))"
      result <- evalString code
      case result of
        Right val -> val `shouldBe` (VArray [] [VInt 6])
        Left err -> expectationFailure err

    it "evaluates exclusive scan on arrays" $ do
      let code = "let add x y = x + y in scan add 0 (generate [4] (let f [i] = i + 1 in f))"
      result <- evalString code
      case result of
        Right val -> val `shouldBe` (VArray [4] [VInt 0, VInt 1, VInt 3, VInt 6])
        Left err -> expectationFailure err

    it "evaluates segmented_reduce over grouped integer values" $ do
      let code = unlines
            [ "let offsets = generate [4] (let f [i] = if i = 0 then 0 else if i = 1 then 2 else if i = 2 then 5 else 5 in f) in"
            , "let vals = generate [5] (let f [i] = if i = 0 then 1 else if i = 1 then 2 else if i = 2 then 10 else if i = 3 then 20 else 30 in f) in"
            , "segmented_reduce (let add acc x = acc + x in add) 0 offsets vals"
            ]
      result <- evalString (BS.pack code)
      case result of
        Right val -> val `shouldBe` (VArray [3] [VInt 3, VInt 60, VInt 0])
        Left err -> expectationFailure err

    it "evaluates sort_indices on integer arrays" $ do
      let code = unlines
            [ "let keys = generate [5] (let f [i] ="
            , "  if i = 0 then 3 else"
            , "  if i = 1 then 1 else"
            , "  if i = 2 then 3 else"
            , "  if i = 3 then 2 else 1 in f)"
            , "in sort_indices keys"
            ]
      result <- evalString (BS.pack code)
      case result of
        Right val -> val `shouldBe` (VArray [5] [VInt 1, VInt 4, VInt 3, VInt 0, VInt 2])
        Left err -> expectationFailure err

    it "evaluates coo_sum_duplicates on sorted COO triplets" $ do
      let code = unlines
            [ "let rows = generate [6] (let f [i] = if i = 0 then 0 else if i = 1 then 0 else if i = 2 then 1 else if i = 3 then 1 else if i = 4 then 2 else 2 in f) in"
            , "let cols = generate [6] (let f [i] = if i = 0 then 1 else if i = 1 then 1 else if i = 2 then 0 else if i = 3 then 2 else if i = 4 then 0 else 0 in f) in"
            , "let vals = generate [6] (let f [i] = if i = 0 then 1 else if i = 1 then 2 else if i = 2 then 10 else if i = 3 then 12 else if i = 4 then 20 else 21 in f) in"
            , "let coo = coo_sum_duplicates 3 4 6 rows cols vals in"
            , "coo.vals"
            ]
      result <- evalString (BS.pack code)
      case result of
        Right val -> val `shouldBe` (VArray [6] [VInt 3, VInt 10, VInt 12, VInt 41, VInt 0, VInt 0])
        Left err -> expectationFailure err

    it "evaluates csr_from_sorted_coo on canonical COO triplets" $ do
      let code = unlines
            [ "let rows = generate [4] (let f [i] = if i = 0 then 0 else if i = 1 then 1 else if i = 2 then 1 else 2 in f) in"
            , "let cols = generate [4] (let f [i] = if i = 0 then 1 else if i = 1 then 0 else if i = 2 then 2 else 0 in f) in"
            , "let vals = generate [4] (let f [i] = if i = 0 then 3 else if i = 1 then 10 else if i = 2 then 12 else 41 in f) in"
            , "let csr = csr_from_sorted_coo 3 4 4 rows cols vals in"
            , "csr.row_ptr"
            ]
      result <- evalString (BS.pack code)
      case result of
        Right val -> val `shouldBe` (VArray [4] [VInt 0, VInt 1, VInt 3, VInt 4])
        Left err -> expectationFailure err

    it "evaluates index on 0D arrays" $ do
      let code = "let add x y = x + y in index () (reduce add 0 (generate [3] (let f [i] = i + 1 in f)))"
      result <- evalString code
      case result of
        Right val -> val `shouldBe` VInt 6
        Left err -> expectationFailure err

    it "reduces along trailing axis for 2D arrays" $ do
      let code = "let f p = (proj 0 p * 10) + proj 1 p in let add x y = x + y in reduce add 0 (generate [2,3] f)"
      result <- evalString code
      case result of
        Right val -> val `shouldBe` VArray [2] [VInt 3, VInt 33]
        Left err -> expectationFailure err

    it "evaluates reshape to change shape" $ do
      let code = "let f _ = 1 in reshape [6] (generate [2,3] f)"
      result <- evalString code
      case result of
        Right val -> val `shouldBe` (VArray [6] [VInt 1, VInt 1, VInt 1, VInt 1, VInt 1, VInt 1])
        Left err -> expectationFailure err

  describe "Interpreter - Error Handling" $ do
    it "raises error on unbound variable" $ do
      result <- evalString "x"
      case result of
        Left _ -> pure ()
        Right _ -> expectationFailure "Should raise unbound variable error"

    it "raises error on division by zero" $ do
      result <- evalString "10 / 0"
      case result of
        Left _ -> pure ()
        Right _ -> expectationFailure "Should raise division by zero error"

    it "raises error on type mismatch in arithmetic" $ do
      result <- evalString "true + 5"
      case result of
        Left _ -> pure ()
        Right _ -> expectationFailure "Should raise type error"

  describe "Interpreter - Integration" $ do
    it "evaluates nested function applications" $ do
      expectEval "let f x = x + 1 in let g x = f (f x) in g 5" (VInt 7)

    it "evaluates closures capturing environment" $ do
      expectEval "let x = 10 in let f y = x + y in f 5" (VInt 15)

    it "evaluates multiple operations in sequence" $ do
      expectEval "let a = 1 in let b = 2 in let c = 3 in a + b + c" (VInt 6)

  describe "Interpreter - Fusion" $ do
    it "fuses map-map and preserves results" $ do
      expectFusedSame "let g x = x + 1 in let f x = x * 2 in map f (map g (generate [3] (let h [i] = i in h)))"

    it "fuses generate-map-reduce into reduce" $ do
      expectFusedSame "let g [i] = i + 1 in let f x = x * 2 in let add x y = x + y in reduce add 0 (map f (generate [3] g))"

    it "fuses map over zipwith" $ do
      expectFusedSame "let g x y = x + y in let f x = x + 1 in map f (zipwith g (generate [3] (let a [i] = i in a)) (generate [3] (let b [i] = i in b)))"

    it "fuses zipwith over generate" $ do
      expectFusedSame "let g x y = x + y in zipwith g (generate [3] (let a [i] = i in a)) (generate [3] (let b [i] = i + 1 in b))"

  describe "Interpreter - read_array" $ do
    it "reads a 1D array from a CSV file" $ do
      tmpDir <- getTemporaryDirectory
      let csvPath = tmpDir </> "hydrangea_test_1d.csv"
      writeFile csvPath "10,20,30"
      let code = "read_array [3] \"" <> fromString csvPath <> "\""
      result <- evalString code
      case result of
        Right val -> val `shouldBe` VArray [3] [VInt 10, VInt 20, VInt 30]
        Left err -> expectationFailure err
      removeFile csvPath

    it "reads a 2D array from a CSV file" $ do
      tmpDir <- getTemporaryDirectory
      let csvPath = tmpDir </> "hydrangea_test_2d.csv"
      writeFile csvPath "1,2,3,4,5,6"
      let code = "read_array [2,3] \"" <> fromString csvPath <> "\""
      result <- evalString code
      case result of
        Right val -> val `shouldBe` VArray [2,3] [VInt 1, VInt 2, VInt 3, VInt 4, VInt 5, VInt 6]
        Left err -> expectationFailure err
      removeFile csvPath

    it "reads array and applies map" $ do
      tmpDir <- getTemporaryDirectory
      let csvPath = tmpDir </> "hydrangea_test_map.csv"
      writeFile csvPath "1,2,3"
      let code = "let f x = x + 10 in map f (read_array [3] \"" <> fromString csvPath <> "\")"
      result <- evalString code
      case result of
        Right val -> val `shouldBe` VArray [3] [VInt 11, VInt 12, VInt 13]
        Left err -> expectationFailure err
      removeFile csvPath

    it "reads array and reduces it" $ do
      tmpDir <- getTemporaryDirectory
      let csvPath = tmpDir </> "hydrangea_test_reduce.csv"
      writeFile csvPath "1,2,3,4"
      let code = "let add x y = x + y in index () (reduce add 0 (read_array [4] \"" <> fromString csvPath <> "\"))"
      result <- evalString code
      case result of
        Right val -> val `shouldBe` VInt 10
        Left err -> expectationFailure err
      removeFile csvPath

  describe "Interpreter - Math functions" $ do
    it "sqrt 4.0 == 2.0" $
      expectEval "sqrt 4.0" (VFloat 2.0)
    it "sqrt 0.0 == 0.0" $
      expectEval "sqrt 0.0" (VFloat 0.0)
    it "sqrt 9.0 == 3.0" $
      expectEval "sqrt 9.0" (VFloat 3.0)
    it "log 1.0 == 0.0" $
      expectEval "log 1.0" (VFloat 0.0)
    it "expf 0.0 == 1.0" $
      expectEval "expf 0.0" (VFloat 1.0)
    it "sin 0.0 == 0.0" $
      expectEval "sin 0.0" (VFloat 0.0)
    it "cos 0.0 == 1.0" $
      expectEval "cos 0.0" (VFloat 1.0)
    it "abs_f (-3.5) == 3.5" $
      expectEval "abs_f (-3.5)" (VFloat 3.5)
    it "abs_f 3.5 == 3.5" $
      expectEval "abs_f 3.5" (VFloat 3.5)
    it "floor_f 2.7 == 2.0" $
      expectEval "floor_f 2.7" (VFloat 2.0)
    it "ceil_f 2.1 == 3.0" $
      expectEval "ceil_f 2.1" (VFloat 3.0)
    it "erf 0.0 is approximately 0.0" $ do
      result <- evalString "erf 0.0"
      case result of
        Right (VFloat v) -> v `shouldSatisfy` (\x -> abs x < 1.0e-6)
        Right other -> expectationFailure $ "Expected VFloat, got: " ++ show other
        Left err -> expectationFailure err
    it "math functions compose: sqrt (expf 0.0) == 1.0" $
      expectEval "sqrt (expf 0.0)" (VFloat 1.0)
    it "math functions work inside map" $
      expectEval "let add x y = x +. y in index () (reduce add 0.0 (map (fn x => sqrt x) (fill [4] 4.0)))"
                 (VFloat 8.0)
    it "log and expf are inverse: expf (log 2.0) close to 2.0" $ do
      result <- evalString "expf (log 2.0)"
      case result of
        Right (VFloat v) -> v `shouldSatisfy` (\x -> abs (x - 2.0) < 1.0e-6)
        Right other -> expectationFailure $ "Expected VFloat, got: " ++ show other
        Left err -> expectationFailure err

  describe "Interpreter - foldl" $ do
    it "foldl sum over [1..5] == 15" $
      expectEval "let add x y = x + y in foldl add 0 (fill [5] 1)"
                 (VInt 5)
    it "foldl sum over floats" $
      expectEval "let add x y = x +. y in foldl add 0.0 (fill [4] 1.0)"
                 (VFloat 4.0)
    it "foldl is left-associative (subtraction)" $
      expectEval "let sub x y = x - y in foldl sub 10 (fill [3] 1)"
                 (VInt 7)
    it "foldl over empty array returns init" $
      expectEval "let add x y = x + y in foldl add 42 (fill [0] 0)"
                 (VInt 42)
    it "foldl can count elements" $
      expectEval "let inc acc _ = acc + 1 in foldl inc 0 (fill [7] 0)"
                 (VInt 7)
    it "foldl max over array" $
      expectEval "let mymax a b = if a > b then a else b in foldl mymax 0 (fill [5] 3)"
                 (VInt 3)
    it "foldl result usable in expression" $
      expectEval "let add x y = x + y in (foldl add 0 (fill [3] 2)) + 1"
        (VInt 7)

  describe "Interpreter - scan" $ do
    it "scan over empty array returns empty array" $
      expectEval "let add x y = x + y in scan add 42 (fill [0] 0)"
        (VArray [0] [])

  describe "Interpreter - float_of" $ do
    it "float_of 0 == 0.0" $
      expectEval "float_of 0" (VFloat 0.0)
    it "float_of 5 == 5.0" $
      expectEval "float_of 5" (VFloat 5.0)
    it "float_of (-3) == -3.0" $
      expectEval "float_of (0 - 3)" (VFloat (-3.0))
    it "float_of in arithmetic" $
      expectEval "float_of 4 +. 0.5" (VFloat 4.5)
    it "float_of on get_env_int result" $
      expectEval "float_of (let n = 7 in n)" (VFloat 7.0)
