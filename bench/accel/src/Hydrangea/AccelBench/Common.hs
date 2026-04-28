module Hydrangea.AccelBench.Common
  ( readEnvInt
  , readCSVInts
  , readCSVDoubles
  , writeCSVInts
  , writeCSVDoubles
  , writeCSVDoubles2D
  , toAccelVec
  , toAccelMat
  , fromAccelVec
  , fromAccelMat
  ) where

import Data.Array.Accelerate (Array, DIM1, DIM2, Z (..), (:.) (..))
import Data.Array.Accelerate qualified as A
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Vector.Unboxed as VU
import Control.Monad (filterM)
import Numeric (showFFloat)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.Environment (getEnv)
import System.FilePath (isAbsolute, takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)

readEnvInt :: String -> IO Int
readEnvInt name = do
  value <- getEnv name
  case reads value of
    [(n, "")] -> pure n
    _ -> do
      hPutStrLn stderr $ "invalid integer in " <> name <> ": " <> value
      fail "invalid environment variable"

readTokens :: FilePath -> IO [String]
readTokens path = do
  resolved <- resolvePathForRead path
  contents <- BSC.readFile resolved
  let normalised = BSC.map replaceSep contents
  pure $ words (BSC.unpack normalised)
  where
    replaceSep c
      | c == ',' || c == '\n' || c == '\r' || c == '\t' = ' '
      | otherwise = c

readCSVInts :: FilePath -> IO (VU.Vector Int)
readCSVInts path = VU.fromList . map read <$> readTokens path

readCSVDoubles :: FilePath -> IO (VU.Vector Double)
readCSVDoubles path = VU.fromList . map read <$> readTokens path

showDouble :: Double -> String
showDouble x = showFFloat (Just 17) x ""

writeCSVInts :: FilePath -> VU.Vector Int -> IO ()
writeCSVInts path xs = do
  resolved <- resolvePathForWrite path
  BSC.writeFile resolved . BSC.pack . unwordsWithCommas $ map show (VU.toList xs)

writeCSVDoubles :: FilePath -> VU.Vector Double -> IO ()
writeCSVDoubles path xs = do
  resolved <- resolvePathForWrite path
  BSC.writeFile resolved . BSC.pack . unwordsWithCommas $ map showDouble (VU.toList xs)

writeCSVDoubles2D :: FilePath -> Int -> Int -> VU.Vector Double -> IO ()
writeCSVDoubles2D path rows cols xs = do
  resolved <- resolvePathForWrite path
  BSC.writeFile resolved . BSC.pack $ unlines
    [ unwordsWithCommas [showDouble (xs VU.! (i * cols + j)) | j <- [0 .. cols - 1]]
    | i <- [0 .. rows - 1]
    ]

resolvePathForRead :: FilePath -> IO FilePath
resolvePathForRead path
  | isAbsolute path = pure path
  | otherwise = do
      let candidates = [path, "../../" </> path]
      existing <- filterM doesFileExist candidates
      pure $ case existing of
        (p : _) -> p
        [] -> path

resolvePathForWrite :: FilePath -> IO FilePath
resolvePathForWrite path
  | isAbsolute path = do
      createDirectoryIfMissing True (takeDirectory path)
      pure path
  | otherwise = do
      let candidates = [path, "../../" </> path]
          parentDirs = map takeDirectory candidates
      existingParents <- filterM doesDirectoryExist parentDirs
      let resolved = case existingParents of
            (d : _) ->
              if d == takeDirectory path then path else "../../" </> path
            [] -> path
      createDirectoryIfMissing True (takeDirectory resolved)
      pure resolved

unwordsWithCommas :: [String] -> String
unwordsWithCommas = concat . go
  where
    go [] = []
    go [x] = [x]
    go (x : xs) = (x ++ ",") : go xs

toAccelVec :: (A.Elt a, VU.Unbox a) => VU.Vector a -> Array DIM1 a
toAccelVec v = A.fromList (Z :. VU.length v) (VU.toList v)

toAccelMat :: (A.Elt a, VU.Unbox a) => Int -> Int -> VU.Vector a -> Array DIM2 a
toAccelMat rows cols v = A.fromList (Z :. rows :. cols) (VU.toList v)

fromAccelVec :: (A.Elt a, VU.Unbox a) => Int -> Array DIM1 a -> VU.Vector a
fromAccelVec n arr = VU.fromList (A.toList arr)
  where _ = n  -- n is only used for documentation; A.toList uses the array's shape

fromAccelMat :: (A.Elt a, VU.Unbox a) => Int -> Int -> Array DIM2 a -> VU.Vector a
fromAccelMat rows cols arr = VU.fromList (A.toList arr)
  where _ = (rows, cols)
