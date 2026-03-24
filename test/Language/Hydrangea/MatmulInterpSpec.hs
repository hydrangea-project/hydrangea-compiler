{-# LANGUAGE OverloadedStrings #-}

module Language.Hydrangea.MatmulInterpSpec (spec) where

import Test.Hspec
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import qualified Data.ByteString.Lazy.Char8 as BS
import Language.Hydrangea.Frontend (readDecs, evalDecsFrontend)
import Language.Hydrangea.Syntax (Dec(..))
import Language.Hydrangea.Interpreter (Value(..))
import qualified Data.Map as Map
import Data.List (intercalate)

spec :: Spec
spec = describe "Interpreter matmul correctness" $ do
  it "computes matrix multiply via generate/reduce_generate correctly" $ do
    withSystemTempDirectory "hydrangea-matmul-test" $ \tmp -> do
      let aPath = tmp </> "matA.csv"
          bPath = tmp </> "matB.csv"
          -- A is 2x3, B is 3x2 => result 2x2
          aRows = [[1.0,2.0,3.0],[4.0,5.0,6.0]] :: [[Double]]
          bRows = [[7.0,8.0],[9.0,10.0],[11.0,12.0]] :: [[Double]]
      -- write CSV files (row-major, each row on its own line)
      let writeCSV path rows = BS.writeFile path (BS.pack $ intercalate "\n" (map (intercalate "," . map show) rows) ++ "\n")
      writeCSV aPath aRows
      writeCSV bPath bRows

      -- construct Hydrangea program reading the two CSVs and performing matmul
      let src = BS.pack $ unlines
            [ "let M = 2"
            , "let K = 3"
            , "let N = 2"
            , "let A = read_array_float [M, K] \"" ++ aPath ++ "\""
            , "let B = read_array_float [K, N] \"" ++ bPath ++ "\""
            , "let add x y = x +. y"
            , "let mul x y = x *. y"
            , "let innerGen i j [idx] = mul (index [i, idx] A) (index [idx, j] B)"
            , "let matElem [i, j] = index () (reduce_generate add 0.0 [K] (innerGen i j))"
            , "let result = generate [M, N] matElem"
            ]

      case readDecs src of
        Left perr -> expectationFailure $ "Parse error: " ++ perr
        Right decs -> do
          eres <- evalDecsFrontend decs
          case eres of
            Left err -> expectationFailure $ "Interpreter error: " ++ show err
            Right env -> do
              -- find result binding
              let mvar = case [ v | Dec _ v _ _ _ <- decs, v == "result" ] of
                          (v:_) -> v
                          [] -> error "result var not found in parsed decls"
              case Map.lookup mvar env of
                Just (VArray shape vals) -> do
                  shape `shouldBe` [2,2]
                  -- expected multiplication
                  let expected = [ (1*7 + 2*9 + 3*11), (1*8 + 2*10 + 3*12)
                                 , (4*7 + 5*9 + 6*11), (4*8 + 5*10 + 6*12) ]
                  let asD = map (\v -> case v of
                                         VFloat f -> f
                                         _ -> error "expected float") vals
                  asD `shouldBe` expected
                other -> expectationFailure $ "Expected array result but got: " ++ show other
