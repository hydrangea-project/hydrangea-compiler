module Hydrangea.RepaBench.Common
  ( Vec
  , Mat
  , readEnvInt
  , readCSVInts
  , readCSVDoubles
  , writeCSVInts
  , writeCSVDoubles
  , writeCSVDoubles2D
  , vecFromVector
  , matFromVector
  , vecToVector
  , matToVector
  , vecIndex
  , matIndex
  ) where

import qualified Data.Array.Repa as R
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Vector.Unboxed as VU
import Control.Monad (filterM)
import Numeric (showFFloat)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.Environment (getEnv)
import System.FilePath (isAbsolute, takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)

type Vec a = R.Array R.U R.DIM1 a
type Mat a = R.Array R.U R.DIM2 a

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

vecFromVector :: VU.Unbox a => VU.Vector a -> Vec a
vecFromVector v =
  R.computeUnboxedS $
    R.fromFunction (R.Z R.:. VU.length v) (\(R.Z R.:. i) -> v VU.! i)

matFromVector :: VU.Unbox a => Int -> Int -> VU.Vector a -> Mat a
matFromVector rows cols v =
  R.computeUnboxedS $
    R.fromFunction (R.Z R.:. rows R.:. cols) (\(R.Z R.:. i R.:. j) -> v VU.! (i * cols + j))

vecToVector :: VU.Unbox a => Int -> Vec a -> VU.Vector a
vecToVector n arr = VU.generate n (\i -> arr R.! (R.Z R.:. i))

matToVector :: VU.Unbox a => Int -> Int -> Mat a -> VU.Vector a
matToVector rows cols arr = VU.generate (rows * cols) $ \k ->
  let i = k `div` cols
      j = k `mod` cols
   in arr R.! (R.Z R.:. i R.:. j)

vecIndex :: VU.Unbox a => Vec a -> Int -> a
vecIndex arr i = arr R.! (R.Z R.:. i)

matIndex :: VU.Unbox a => Mat a -> Int -> Int -> a
matIndex arr i j = arr R.! (R.Z R.:. i R.:. j)
