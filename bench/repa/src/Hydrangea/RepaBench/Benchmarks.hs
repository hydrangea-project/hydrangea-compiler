{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}

module Hydrangea.RepaBench.Benchmarks
  ( benchmarkNames
  , runAllBenchmarks
  , runBenchmarkByName
  , runBenchmarkByNameTimed
  , loadNBodyInputs
  , runNBodyOutputs
  , runNBodyOutputsIO
  , runNBodyImperativeOutputsIO
  , runNBodyFusedOutputsIO
  , nBodyChecksum
  ) where

import Hydrangea.RepaBench.Common
import Hydrangea.RepaBench.Harness

import Control.Monad (forM_)
import Data.Array.Repa qualified as R
import Data.Function (on)
import Data.List (groupBy, sortOn, zip4)
import Data.Map.Strict qualified as M
import System.Exit (die)

foreign import ccall unsafe "erf"
  c_erf :: Double -> Double

erf :: Double -> Double
erf = c_erf

data Benchmark = Benchmark
  { benchmarkName :: String
  , benchmarkRun :: IO ()
  }

data NBodyInputs = NBodyInputs
  { nbodyN :: Int
  , nbodyXs :: R.Array R.U R.DIM1 Double
  , nbodyYs :: R.Array R.U R.DIM1 Double
  , nbodyZs :: R.Array R.U R.DIM1 Double
  , nbodyMs :: R.Array R.U R.DIM1 Double
  , nbodyVxs :: R.Array R.U R.DIM1 Double
  , nbodyVys :: R.Array R.U R.DIM1 Double
  , nbodyVzs :: R.Array R.U R.DIM1 Double
  }

benchmarkNames :: [String]
benchmarkNames = map benchmarkName benchmarks

benchmarks :: [Benchmark]
benchmarks =
  [ Benchmark "blackscholes" runBlackScholes
  , Benchmark "nbody" runNBody
  , Benchmark "nbody_imperative" runNBodyImperative
  , Benchmark "mandelbrot" runMandelbrot
  , Benchmark "spmv" runSpMV
  , Benchmark "matmul" runMatMul
  , Benchmark "weighted_histogram" runWeightedHistogram
  , Benchmark "guarded_weighted_histogram" runGuardedWeightedHistogram
  , Benchmark "coo_csr_build" runCooCsrBuild
  , Benchmark "graph_messages" runGraphMessages
  , Benchmark "voxel_rasterization" runVoxelRasterization
  , Benchmark "voxel_trilinear_splat" runVoxelTrilinearSplat
  , Benchmark "softmax" runSoftmax
  , Benchmark "coo_spmv" runCooSpmv
  ]

runAllBenchmarks :: IO ()
runAllBenchmarks = forM_ benchmarks $ \Benchmark{..} -> do
  putStrLn $ "=== " ++ benchmarkName ++ " ==="
  benchmarkRun

runBenchmarkByName :: String -> IO ()
runBenchmarkByName name =
  case lookup name [(benchmarkName b, benchmarkRun b) | b <- benchmarks] of
    Just action -> action
    Nothing -> die $ "unknown benchmark: " ++ name

sumAll :: (R.Source r e, R.Shape sh, Num e) => R.Array r sh e -> e
sumAll = R.sumAllS

blackScholesKernel
  :: Int
  -> R.Array R.U R.DIM1 Double
  -> R.Array R.U R.DIM1 Double
  -> R.Array R.U R.DIM1 Double
  -> R.Array R.U R.DIM1 Double
  -> R.Array R.U R.DIM1 Double
  -> IO (R.Array R.U R.DIM1 Double)
blackScholesKernel n spots strikes rates vols times =
  R.computeUnboxedP $
    R.fromFunction (R.Z R.:. n) $ \(R.Z R.:. i) ->
      let s = spots R.! (R.Z R.:. i)
          k = strikes R.! (R.Z R.:. i)
          r = rates R.! (R.Z R.:. i)
          sigma = vols R.! (R.Z R.:. i)
          t = times R.! (R.Z R.:. i)
          phi x = 0.5 * (1.0 + erf (x / sqrt 2.0))
          d1 = (log (s / k) + (r + 0.5 * sigma * sigma) * t) / (sigma * sqrt t)
          d2 = d1 - sigma * sqrt t
       in s * phi d1 - k * exp ((-1.0) * r * t) * phi d2

nBodyKernel
  :: Int
  -> R.Array R.U R.DIM1 Double
  -> R.Array R.U R.DIM1 Double
  -> R.Array R.U R.DIM1 Double
  -> R.Array R.U R.DIM1 Double
  -> R.Array R.U R.DIM1 Double
  -> R.Array R.U R.DIM1 Double
  -> R.Array R.U R.DIM1 Double
  -> (R.Array R.U R.DIM1 Double, R.Array R.U R.DIM1 Double, R.Array R.U R.DIM1 Double)
nBodyKernel n xs ys zs ms vxs vys vzs =
  (newXs, newYs, newZs)
  where
    eps2 = 0.01
    g = 6.674e-11
    dt = 0.01
    acc component =
      R.computeUnboxedS $
        R.fromFunction (R.Z R.:. n) $ \(R.Z R.:. i) ->
          let xi = xs R.! (R.Z R.:. i)
              yi = ys R.! (R.Z R.:. i)
              zi = zs R.! (R.Z R.:. i)
           in sumAll $
                R.fromFunction (R.Z R.:. n) $ \(R.Z R.:. j) ->
                  let dx = xs R.! (R.Z R.:. j) - xi
                      dy = ys R.! (R.Z R.:. j) - yi
                      dz = zs R.! (R.Z R.:. j) - zi
                      r2 = dx * dx + dy * dy + dz * dz + eps2
                      r3 = r2 * sqrt r2
                      mj = ms R.! (R.Z R.:. j)
                   in g * mj * component dx dy dz / r3
    accX = acc (\dx _ _ -> dx)
    accY = acc (\_ dy _ -> dy)
    accZ = acc (\_ _ dz -> dz)
    newVxs = R.computeUnboxedS $ R.zipWith (\v a -> v + a * dt) vxs accX
    newVys = R.computeUnboxedS $ R.zipWith (\v a -> v + a * dt) vys accY
    newVzs = R.computeUnboxedS $ R.zipWith (\v a -> v + a * dt) vzs accZ
    newXs = R.computeUnboxedS $ R.zipWith (\p v -> p + v * dt) xs newVxs
    newYs = R.computeUnboxedS $ R.zipWith (\p v -> p + v * dt) ys newVys
    newZs = R.computeUnboxedS $ R.zipWith (\p v -> p + v * dt) zs newVzs

loadNBodyInputs :: IO NBodyInputs
loadNBodyInputs = do
  n <- readEnvInt "NBODY_N"
  xs <- vecFromVector <$> readCSVDoubles "bench/nbody/xs.csv"
  ys <- vecFromVector <$> readCSVDoubles "bench/nbody/ys.csv"
  zs <- vecFromVector <$> readCSVDoubles "bench/nbody/zs.csv"
  ms <- vecFromVector <$> readCSVDoubles "bench/nbody/ms.csv"
  vxs <- vecFromVector <$> readCSVDoubles "bench/nbody/vxs.csv"
  vys <- vecFromVector <$> readCSVDoubles "bench/nbody/vys.csv"
  vzs <- vecFromVector <$> readCSVDoubles "bench/nbody/vzs.csv"
  pure NBodyInputs
    { nbodyN = n
    , nbodyXs = xs
    , nbodyYs = ys
    , nbodyZs = zs
    , nbodyMs = ms
    , nbodyVxs = vxs
    , nbodyVys = vys
    , nbodyVzs = vzs
    }

runNBodyOutputs :: NBodyInputs -> (R.Array R.U R.DIM1 Double, R.Array R.U R.DIM1 Double, R.Array R.U R.DIM1 Double)
{-# NOINLINE runNBodyOutputs #-}
runNBodyOutputs NBodyInputs{..} = nBodyKernel nbodyN nbodyXs nbodyYs nbodyZs nbodyMs nbodyVxs nbodyVys nbodyVzs

runNBodyOutputsIO :: NBodyInputs -> IO (R.Array R.U R.DIM1 Double, R.Array R.U R.DIM1 Double, R.Array R.U R.DIM1 Double)
{-# NOINLINE runNBodyOutputsIO #-}
runNBodyOutputsIO NBodyInputs{..} = nBodyKernelP nbodyN nbodyXs nbodyYs nbodyZs nbodyMs nbodyVxs nbodyVys nbodyVzs

runNBodyImperativeOutputsIO :: NBodyInputs -> IO (R.Array R.U R.DIM1 Double, R.Array R.U R.DIM1 Double, R.Array R.U R.DIM1 Double)
{-# NOINLINE runNBodyImperativeOutputsIO #-}
runNBodyImperativeOutputsIO NBodyInputs{..} =
  nBodyKernelImperativeP nbodyN nbodyXs nbodyYs nbodyZs nbodyMs nbodyVxs nbodyVys nbodyVzs

runNBodyFusedOutputsIO :: NBodyInputs -> IO (R.Array R.U R.DIM1 Double, R.Array R.U R.DIM1 Double, R.Array R.U R.DIM1 Double)
{-# NOINLINE runNBodyFusedOutputsIO #-}
runNBodyFusedOutputsIO NBodyInputs{..} =
  nBodyKernelFusedP nbodyN nbodyXs nbodyYs nbodyZs nbodyMs nbodyVxs nbodyVys nbodyVzs

nBodyChecksum :: (R.Array R.U R.DIM1 Double, R.Array R.U R.DIM1 Double, R.Array R.U R.DIM1 Double) -> Double
{-# NOINLINE nBodyChecksum #-}
nBodyChecksum (xs, ys, zs) = R.sumAllS xs + R.sumAllS ys + R.sumAllS zs

nBodyKernelP
  :: Int
  -> R.Array R.U R.DIM1 Double
  -> R.Array R.U R.DIM1 Double
  -> R.Array R.U R.DIM1 Double
  -> R.Array R.U R.DIM1 Double
  -> R.Array R.U R.DIM1 Double
  -> R.Array R.U R.DIM1 Double
  -> R.Array R.U R.DIM1 Double
  -> IO (R.Array R.U R.DIM1 Double, R.Array R.U R.DIM1 Double, R.Array R.U R.DIM1 Double)
nBodyKernelP n xs ys zs ms vxs vys vzs = do
  accX <- accP (\dx _ _ -> dx)
  accY <- accP (\_ dy _ -> dy)
  accZ <- accP (\_ _ dz -> dz)
  newVxs <- R.computeUnboxedP $ R.zipWith (\v a -> v + a * dt) vxs accX
  newVys <- R.computeUnboxedP $ R.zipWith (\v a -> v + a * dt) vys accY
  newVzs <- R.computeUnboxedP $ R.zipWith (\v a -> v + a * dt) vzs accZ
  newXs <- R.computeUnboxedP $ R.zipWith (\p v -> p + v * dt) xs newVxs
  newYs <- R.computeUnboxedP $ R.zipWith (\p v -> p + v * dt) ys newVys
  newZs <- R.computeUnboxedP $ R.zipWith (\p v -> p + v * dt) zs newVzs
  pure (newXs, newYs, newZs)
  where
    eps2 = 0.01
    g = 6.674e-11
    dt = 0.01

    accP component =
      R.computeUnboxedP $
        R.fromFunction (R.Z R.:. n) $ \(R.Z R.:. i) ->
          let xi = xs R.! (R.Z R.:. i)
              yi = ys R.! (R.Z R.:. i)
              zi = zs R.! (R.Z R.:. i)
           in sumAll $
                R.fromFunction (R.Z R.:. n) $ \(R.Z R.:. j) ->
                  let dx = xs R.! (R.Z R.:. j) - xi
                      dy = ys R.! (R.Z R.:. j) - yi
                      dz = zs R.! (R.Z R.:. j) - zi
                      r2 = dx * dx + dy * dy + dz * dz + eps2
                      r3 = r2 * sqrt r2
                      mj = ms R.! (R.Z R.:. j)
                   in g * mj * component dx dy dz / r3

nBodyKernelImperativeP
  :: Int
  -> R.Array R.U R.DIM1 Double
  -> R.Array R.U R.DIM1 Double
  -> R.Array R.U R.DIM1 Double
  -> R.Array R.U R.DIM1 Double
  -> R.Array R.U R.DIM1 Double
  -> R.Array R.U R.DIM1 Double
  -> R.Array R.U R.DIM1 Double
  -> IO (R.Array R.U R.DIM1 Double, R.Array R.U R.DIM1 Double, R.Array R.U R.DIM1 Double)
nBodyKernelImperativeP n xs ys zs ms vxs vys vzs = do
  accX <- accP (\dx _ _ -> dx)
  accY <- accP (\_ dy _ -> dy)
  accZ <- accP (\_ _ dz -> dz)
  newVxs <- R.computeUnboxedP $ R.zipWith (\v a -> v + a * dt) vxs accX
  newVys <- R.computeUnboxedP $ R.zipWith (\v a -> v + a * dt) vys accY
  newVzs <- R.computeUnboxedP $ R.zipWith (\v a -> v + a * dt) vzs accZ
  newXs <- R.computeUnboxedP $ R.zipWith (\p v -> p + v * dt) xs newVxs
  newYs <- R.computeUnboxedP $ R.zipWith (\p v -> p + v * dt) ys newVys
  newZs <- R.computeUnboxedP $ R.zipWith (\p v -> p + v * dt) zs newVzs
  pure (newXs, newYs, newZs)
  where
    eps2 = 0.01
    g = 6.674e-11
    dt = 0.01

    accP component =
      R.computeUnboxedP $
        R.fromFunction (R.Z R.:. n) $ \(R.Z R.:. i) ->
          let !xi = xs R.! (R.Z R.:. i)
              !yi = ys R.! (R.Z R.:. i)
              !zi = zs R.! (R.Z R.:. i)
              go !j !acc
                | j >= n = acc
                | otherwise =
                    let !dx = (xs R.! (R.Z R.:. j)) - xi
                        !dy = (ys R.! (R.Z R.:. j)) - yi
                        !dz = (zs R.! (R.Z R.:. j)) - zi
                        !r2 = dx * dx + dy * dy + dz * dz + eps2
                        !r3 = r2 * sqrt r2
                        !scale = g * (ms R.! (R.Z R.:. j)) / r3
                     in go (j + 1) (acc + component dx dy dz * scale)
           in go 0 0.0

-- Single-pass fused nbody: computes all three acceleration components in one
-- O(n²) traversal instead of three separate ones, saving 2 parallel barriers
-- and reading xs/ys/zs/ms only once per (i,j) pair instead of three times.
nBodyKernelFusedP
  :: Int
  -> R.Array R.U R.DIM1 Double
  -> R.Array R.U R.DIM1 Double
  -> R.Array R.U R.DIM1 Double
  -> R.Array R.U R.DIM1 Double
  -> R.Array R.U R.DIM1 Double
  -> R.Array R.U R.DIM1 Double
  -> R.Array R.U R.DIM1 Double
  -> IO (R.Array R.U R.DIM1 Double, R.Array R.U R.DIM1 Double, R.Array R.U R.DIM1 Double)
nBodyKernelFusedP n xs ys zs ms vxs vys vzs = do
  -- Single O(n²) parallel pass: all three acceleration components together
  accXYZ <- R.computeUnboxedP $
    R.fromFunction (R.Z R.:. n) $ \(R.Z R.:. i) ->
      let !xi = xs R.! (R.Z R.:. i)
          !yi = ys R.! (R.Z R.:. i)
          !zi = zs R.! (R.Z R.:. i)
          go !j !ax !ay !az
            | j >= n = (ax, ay, az)
            | otherwise =
                let !dx    = (xs R.! (R.Z R.:. j)) - xi
                    !dy    = (ys R.! (R.Z R.:. j)) - yi
                    !dz    = (zs R.! (R.Z R.:. j)) - zi
                    !r2    = dx*dx + dy*dy + dz*dz + eps2
                    !r3    = r2 * sqrt r2
                    !scale = g * (ms R.! (R.Z R.:. j)) / r3
                in go (j+1) (ax + dx*scale) (ay + dy*scale) (az + dz*scale)
       in go 0 0.0 0.0 0.0
  -- Fused velocity + position update: new velocity, then new position
  newVsXYZ <- R.computeUnboxedP $
    R.fromFunction (R.Z R.:. n) $ \(R.Z R.:. i) ->
      let (ax, ay, az) = accXYZ R.! (R.Z R.:. i)
       in ( (vxs R.! (R.Z R.:. i)) + ax * dt
          , (vys R.! (R.Z R.:. i)) + ay * dt
          , (vzs R.! (R.Z R.:. i)) + az * dt )
  newXs <- R.computeUnboxedP $
    R.fromFunction (R.Z R.:. n) $ \(R.Z R.:. i) ->
      let (vx', _, _) = newVsXYZ R.! (R.Z R.:. i)
       in (xs R.! (R.Z R.:. i)) + vx' * dt
  newYs <- R.computeUnboxedP $
    R.fromFunction (R.Z R.:. n) $ \(R.Z R.:. i) ->
      let (_, vy', _) = newVsXYZ R.! (R.Z R.:. i)
       in (ys R.! (R.Z R.:. i)) + vy' * dt
  newZs <- R.computeUnboxedP $
    R.fromFunction (R.Z R.:. n) $ \(R.Z R.:. i) ->
      let (_, _, vz') = newVsXYZ R.! (R.Z R.:. i)
       in (zs R.! (R.Z R.:. i)) + vz' * dt
  pure (newXs, newYs, newZs)
  where
    eps2 = 0.01
    g    = 6.674e-11
    dt   = 0.01

mandelbrotKernel :: Int -> Int -> Int -> IO (R.Array R.U R.DIM2 Double)
mandelbrotKernel width height iters =
  R.computeUnboxedP $
    R.fromFunction (R.Z R.:. width R.:. height) $ \(R.Z R.:. px R.:. py) ->
      let w = fromIntegral width
          h = fromIntegral height
          cx = (-2.5) + 3.5 * fromIntegral px / (w - 1.0)
          cy = (-1.0) + 2.0 * fromIntegral py / (h - 1.0)
          -- Direct tail-recursive loop: matches foldl_while semantics.
          -- Check |z|^2 > 4.0 before each update; break on escape.
          go !re !im !count
            | count >= iters        = count
            | re*re + im*im > 4.0   = count
            | otherwise =
                let !re' = re*re - im*im + cx
                    !im' = 2.0*re*im + cy
                in go re' im' (count + 1)
       in fromIntegral (go 0.0 0.0 0)

spmvKernel
  :: Int
  -> R.Array R.U R.DIM1 Int
  -> R.Array R.U R.DIM1 Double
  -> R.Array R.U R.DIM1 Int
  -> R.Array R.U R.DIM1 Double
  -> IO (R.Array R.U R.DIM1 Double)
spmvKernel nrows rowPtr values colIdx x =
  R.computeUnboxedP $
    R.fromFunction (R.Z R.:. nrows) $ \(R.Z R.:. i) ->
      let rowStart = rowPtr R.! (R.Z R.:. i)
          rowEnd = rowPtr R.! (R.Z R.:. (i + 1))
          go !acc !k
            | k >= rowEnd = acc
            | otherwise =
                let col = colIdx R.! (R.Z R.:. k)
                 in go (acc + values R.! (R.Z R.:. k) * x R.! (R.Z R.:. col)) (k + 1)
       in go 0.0 rowStart

matMulKernel
  :: Int
  -> Int
  -> Int
  -> R.Array R.U R.DIM2 Double
  -> R.Array R.U R.DIM2 Double
  -> IO (R.Array R.U R.DIM2 Double)
matMulKernel m k n a b =
  R.computeUnboxedP $
    R.fromFunction (R.Z R.:. m R.:. n) $ \(R.Z R.:. i R.:. j) ->
      let go !acc !t
            | t >= k    = acc
            | otherwise = go (acc + a R.! (R.Z R.:. i R.:. t) * b R.! (R.Z R.:. t R.:. j)) (t + 1)
       in go 0.0 0

softmaxKernel
  :: Int
  -> Int
  -> R.Array R.U R.DIM2 Double
  -> R.Array R.U R.DIM2 Double
softmaxKernel m n logits =
  let rowSums =
        R.computeUnboxedS $
          R.fromFunction (R.Z R.:. m) $ \(R.Z R.:. i) ->
            sumAll $
              R.fromFunction (R.Z R.:. n) $ \(R.Z R.:. k) ->
                exp (logits R.! (R.Z R.:. i R.:. k))
   in R.computeUnboxedS $
        R.fromFunction (R.Z R.:. m R.:. n) $ \(R.Z R.:. i R.:. j) ->
          exp (logits R.! (R.Z R.:. i R.:. j)) / rowSums R.! (R.Z R.:. i)

weightedHistogramKernel :: Int -> Int -> R.Array R.U R.DIM1 Int
weightedHistogramKernel n bins =
  let src = R.fromFunction (R.Z R.:. n) (\(R.Z R.:. i) -> i)
      idxs = R.computeUnboxedS $ R.map (\i -> (i * bins) `div` n) src
      weights = R.computeUnboxedS $ R.map (\i -> i * 3 + 1) src
      histMap = M.fromListWith (+) (zip (R.toList idxs) (R.toList weights))
   in R.computeUnboxedS $
        R.fromFunction (R.Z R.:. bins) $ \(R.Z R.:. b) ->
          M.findWithDefault 0 b histMap

guardedWeightedHistogramKernel :: Int -> Int -> Int -> R.Array R.U R.DIM1 Int
guardedWeightedHistogramKernel n bins keepPeriod =
  let src = R.fromFunction (R.Z R.:. n) (\(R.Z R.:. i) -> i)
      idxs = R.computeUnboxedS $ R.map (\i -> (i * bins) `div` n) src
      weights = R.computeUnboxedS $ R.map (\i -> i * 3 + 1) src
      keep = R.computeUnboxedS $ R.map (\i -> ((i `div` keepPeriod) * keepPeriod) == i) src
      histMap =
        M.fromListWith (+)
          [ (idx, weight)
          | (idx, weight, ok) <- zip3 (R.toList idxs) (R.toList weights) (R.toList keep)
          , ok
          ]
   in R.computeUnboxedS $
        R.fromFunction (R.Z R.:. bins) $ \(R.Z R.:. b) ->
          M.findWithDefault 0 b histMap

cooSpmvKernel
  :: Int
  -> Int
  -> R.Array R.U R.DIM1 Int
  -> R.Array R.U R.DIM1 Int
  -> R.Array R.U R.DIM1 Double
  -> R.Array R.U R.DIM1 Double
  -> R.Array R.U R.DIM1 Double
cooSpmvKernel nrows _nnz rowIdx colIdx values x =
  let contributions =
        [ (row, value * x R.! (R.Z R.:. col))
        | (row, col, value) <- zip3 (R.toList rowIdx) (R.toList colIdx) (R.toList values)
        ]
      rowMap = M.fromListWith (+) contributions
   in R.computeUnboxedS $
        R.fromFunction (R.Z R.:. nrows) $ \(R.Z R.:. i) ->
          M.findWithDefault 0.0 i rowMap

cooCsrBuildKernel
  :: Int
  -> Int
  -> Int
  -> Int
  -> (Int, Int, Int, Int)
cooCsrBuildKernel nrows ncols nnz dup =
  (rowPtrSum, colIdxSum, valsSum, length canonical)
  where
    groupOf i = i `div` dup
    imod x m = x - ((x `div` m) * m)
    rows = R.computeUnboxedS $ R.fromFunction (R.Z R.:. nnz) $ \(R.Z R.:. i) ->
      imod (groupOf i * 17 + 3) nrows
    cols = R.computeUnboxedS $ R.fromFunction (R.Z R.:. nnz) $ \(R.Z R.:. i) ->
      imod (groupOf i * 31 + 7) ncols
    vals = R.computeUnboxedS $ R.fromFunction (R.Z R.:. nnz) $ \(R.Z R.:. i) ->
      groupOf i * 13 + i + 1
    packed = R.computeUnboxedS $ R.zipWith (\r c -> r * ncols + c) rows cols
    entries =
      sortOn (\(k, _, _, _) -> k) $
        zip4
          (R.toList packed)
          (R.toList rows)
          (R.toList cols)
          (R.toList vals)
    canonical = combineEntries entries
    canonicalRows = map (\(_, r, _, _) -> r) canonical
    canonicalCols = map (\(_, _, c, _) -> c) canonical
    canonicalVals = map (\(_, _, _, v) -> v) canonical
    rowCounts = foldl addCount (replicate nrows 0) canonicalRows
    rowPtr = scanl (+) 0 rowCounts
    rowPtrSum = sum rowPtr
    colIdxSum = sum canonicalCols
    valsSum = sum canonicalVals

    addCount acc ix =
      take ix acc ++ [acc !! ix + 1] ++ drop (ix + 1) acc

    combineEntries :: [(Int, Int, Int, Int)] -> [(Int, Int, Int, Int)]
    combineEntries = map collapse . groupBy ((==) `on` (\(k, _, _, _) -> k))
      where
        collapse [] = error "empty group"
        collapse xs@((k, r, c, _) : _) = (k, r, c, sum [v | (_, _, _, v) <- xs])

graphMessagesKernel :: Int -> Int -> IO (R.Array R.U R.DIM1 Int)
graphMessagesKernel n degree = do
  let nnz = n * degree
  nodeVals <- R.computeUnboxedP $ R.fromFunction (R.Z R.:. n) (\(R.Z R.:. j) -> j + 1)
  messages <- R.computeUnboxedP $ R.fromFunction (R.Z R.:. nnz) $ \(R.Z R.:. k) ->
    let dst = k `div` degree
        edgeVal = k + 1
     in edgeVal * (nodeVals R.! (R.Z R.:. dst))
  R.computeUnboxedP $
    R.fromFunction (R.Z R.:. n) $ \(R.Z R.:. i) ->
      let go !acc !j
            | j >= degree = acc
            | otherwise   = go (acc + messages R.! (R.Z R.:. (i * degree + j))) (j + 1)
       in go 0 0

voxelRasterizationKernel :: Int -> Int -> Int -> Int -> Int -> R.Array R.U R.DIM1 Double
voxelRasterizationKernel n nx ny nz keepPeriod =
  let src = R.fromFunction (R.Z R.:. n) (\(R.Z R.:. p) -> p)
      idxs = R.computeUnboxedS $ R.map
        (\p ->
          let x = (p * 17 + 3) `mod` nx
              y = (p * 29 + 5) `mod` ny
              z = (p * 43 + 7) `mod` nz
           in ((z * ny) + y) * nx + x)
        src
      weights = R.computeUnboxedS $ R.map
        (\p ->
          let x = (p * 17 + 3) `mod` nx
              y = (p * 29 + 5) `mod` ny
              z = (p * 43 + 7) `mod` nz
           in fromIntegral (((x + 1) * (y + 2)) + (z * 3) + 1))
        src
      keep = R.computeUnboxedS $ R.map (\p -> ((p `div` keepPeriod) * keepPeriod) == p) src
      voxelMap =
        M.fromListWith (+)
          [ (idx, weight)
          | (idx, weight, ok) <- zip3 (R.toList idxs) (R.toList weights) (R.toList keep)
          , ok
          ]
   in R.computeUnboxedS $
        R.fromFunction (R.Z R.:. size) $ \(R.Z R.:. voxel) ->
          M.findWithDefault 0.0 voxel voxelMap
  where
    size = nx * ny * nz

voxelTrilinearSplatKernel :: Int -> Int -> Int -> Int -> Int -> R.Array R.U R.DIM1 Double
voxelTrilinearSplatKernel n nx ny nz keepPeriod =
  let src = R.fromFunction (R.Z R.:. contribs) (\(R.Z R.:. i) -> i)
      idxs = R.computeUnboxedS $ R.map
        (\i ->
          let p = i `div` 8
              c = i `mod` 8
              bx = c `mod` 2
              by = (c `div` 2) `mod` 2
              bz = (c `div` 4) `mod` 2
              x = (p * 17 + 3 + bx) `mod` nx
              y = (p * 29 + 5 + by) `mod` ny
              z = (p * 43 + 7 + bz) `mod` nz
           in ((z * ny) + y) * nx + x)
        src
      weights = R.computeUnboxedS $ R.map
        (\i ->
          let p = i `div` 8
              c = i `mod` 8
              bx = c `mod` 2
              by = (c `div` 2) `mod` 2
              bz = (c `div` 4) `mod` 2
              fx = (p * 5 + 1) `mod` 4
              fy = (p * 7 + 2) `mod` 4
              fz = (p * 11 + 3) `mod` 4
              wx = (1 - bx) * (4 - fx) + bx * fx
              wy = (1 - by) * (4 - fy) + by * fy
              wz = (1 - bz) * (4 - fz) + bz * fz
              base = (p * 3) + 1
           in fromIntegral (base * wx * wy * wz) / 64.0)
        src
      keep = R.computeUnboxedS $ R.map (\i -> let p = i `div` 8 in ((p `div` keepPeriod) * keepPeriod) == p) src
      voxelMap =
        M.fromListWith (+)
          [ (idx, weight)
          | (idx, weight, ok) <- zip3 (R.toList idxs) (R.toList weights) (R.toList keep)
          , ok
          ]
   in R.computeUnboxedS $
        R.fromFunction (R.Z R.:. size) $ \(R.Z R.:. voxel) ->
          M.findWithDefault 0.0 voxel voxelMap
  where
    contribs = n * 8
    size = nx * ny * nz

runBlackScholes :: IO ()
runBlackScholes = do
  n <- readEnvInt "BS_N"
  spots <- vecFromVector <$> readCSVDoubles "bench/blackscholes/spots.csv"
  strikes <- vecFromVector <$> readCSVDoubles "bench/blackscholes/strikes.csv"
  rates <- vecFromVector <$> readCSVDoubles "bench/blackscholes/rates.csv"
  vols <- vecFromVector <$> readCSVDoubles "bench/blackscholes/vols.csv"
  times <- vecFromVector <$> readCSVDoubles "bench/blackscholes/times.csv"
  result <- blackScholesKernel n spots strikes rates vols times
  writeCSVDoubles "bench/blackscholes/out.csv" (vecToVector n result)

runNBody :: IO ()
runNBody = do
  inputs <- loadNBodyInputs
  (newXs, newYs, newZs) <- runNBodyOutputsIO inputs
  writeCSVDoubles "bench/nbody/out_xs.csv" (vecToVector (nbodyN inputs) newXs)
  writeCSVDoubles "bench/nbody/out_ys.csv" (vecToVector (nbodyN inputs) newYs)
  writeCSVDoubles "bench/nbody/out_zs.csv" (vecToVector (nbodyN inputs) newZs)

runNBodyImperative :: IO ()
runNBodyImperative = do
  inputs <- loadNBodyInputs
  (newXs, newYs, newZs) <- runNBodyImperativeOutputsIO inputs
  writeCSVDoubles "bench/nbody/out_xs.csv" (vecToVector (nbodyN inputs) newXs)
  writeCSVDoubles "bench/nbody/out_ys.csv" (vecToVector (nbodyN inputs) newYs)
  writeCSVDoubles "bench/nbody/out_zs.csv" (vecToVector (nbodyN inputs) newZs)

runMandelbrot :: IO ()
runMandelbrot = do
  width <- readEnvInt "MAND_W"
  height <- readEnvInt "MAND_H"
  iters <- readEnvInt "MAND_ITERS"
  result <- mandelbrotKernel width height iters
  writeCSVDoubles2D "bench/mandelbrot/out.csv" width height (matToVector width height result)

runSpMV :: IO ()
runSpMV = do
  nrows <- readEnvInt "SPMV_NROWS"
  _nnz <- readEnvInt "SPMV_NNZ"
  _ncols <- readEnvInt "SPMV_NCOLS"
  values <- vecFromVector <$> readCSVDoubles "bench/spmv/values.csv"
  colIdx <- vecFromVector <$> readCSVInts "bench/spmv/col_idx.csv"
  rowPtr <- vecFromVector <$> readCSVInts "bench/spmv/row_ptr.csv"
  x <- vecFromVector <$> readCSVDoubles "bench/spmv/x.csv"
  result <- spmvKernel nrows rowPtr values colIdx x
  writeCSVDoubles "bench/spmv/out.csv" (vecToVector nrows result)

runMatMul :: IO ()
runMatMul = do
  m <- readEnvInt "MAT_M"
  k <- readEnvInt "MAT_K"
  n <- readEnvInt "MAT_N"
  a <- matFromVector m k <$> readCSVDoubles "bench/matmul/matA.csv"
  b <- matFromVector k n <$> readCSVDoubles "bench/matmul/matB.csv"
  result <- matMulKernel m k n a b
  writeCSVDoubles2D "bench/matmul/out.csv" m n (matToVector m n result)

runSoftmax :: IO ()
runSoftmax = do
  m <- readEnvInt "SOFTMAX_M"
  n <- readEnvInt "SOFTMAX_N"
  logits <- matFromVector m n <$> readCSVDoubles "bench/softmax/logits.csv"
  let result = softmaxKernel m n logits
  writeCSVDoubles2D "bench/softmax/out.csv" m n (matToVector m n result)

runWeightedHistogram :: IO ()
runWeightedHistogram = do
  n <- readEnvInt "WH_N"
  bins <- readEnvInt "WH_BINS"
  let hist = weightedHistogramKernel n bins
  print (R.sumAllS hist)

runGuardedWeightedHistogram :: IO ()
runGuardedWeightedHistogram = do
  n <- readEnvInt "GWH_N"
  bins <- readEnvInt "GWH_BINS"
  keepPeriod <- readEnvInt "GWH_KEEP_PERIOD"
  let hist = guardedWeightedHistogramKernel n bins keepPeriod
  print (R.sumAllS hist)

runCooSpmv :: IO ()
runCooSpmv = do
  nrows <- readEnvInt "COO_NROWS"
  _ncols <- readEnvInt "COO_NCOLS"
  nnz <- readEnvInt "COO_NNZ"
  rowIdx <- vecFromVector <$> readCSVInts "bench/coo_spmv/row_idx.csv"
  colIdx <- vecFromVector <$> readCSVInts "bench/coo_spmv/col_idx.csv"
  values <- vecFromVector <$> readCSVDoubles "bench/coo_spmv/values.csv"
  x <- vecFromVector <$> readCSVDoubles "bench/coo_spmv/x.csv"
  let result = cooSpmvKernel nrows nnz rowIdx colIdx values x
  writeCSVDoubles "bench/coo_spmv/out.csv" (vecToVector nrows result)

runCooCsrBuild :: IO ()
runCooCsrBuild = do
  nrows <- readEnvInt "COO_NROWS"
  ncols <- readEnvInt "COO_NCOLS"
  nnz <- readEnvInt "COO_NNZ"
  dup <- readEnvInt "COO_DUP_PERIOD"
  let (rowPtrSum, colIdxSum, valsSum, canonicalCount) = cooCsrBuildKernel nrows ncols nnz dup
  print (rowPtrSum + colIdxSum + valsSum + canonicalCount)

runGraphMessages :: IO ()
runGraphMessages = do
  n <- readEnvInt "GRAPH_NODES"
  degree <- readEnvInt "GRAPH_DEGREE"
  result <- graphMessagesKernel n degree
  print (R.sumAllS result)

runVoxelRasterization :: IO ()
runVoxelRasterization = do
  n <- readEnvInt "VOX_POINTS"
  nx <- readEnvInt "VOX_NX"
  ny <- readEnvInt "VOX_NY"
  nz <- readEnvInt "VOX_NZ"
  keepPeriod <- readEnvInt "VOX_KEEP_PERIOD"
  let result = voxelRasterizationKernel n nx ny nz keepPeriod
  print (R.sumAllS result)

runVoxelTrilinearSplat :: IO ()
runVoxelTrilinearSplat = do
  n <- readEnvInt "VSPLAT_POINTS"
  nx <- readEnvInt "VSPLAT_NX"
  ny <- readEnvInt "VSPLAT_NY"
  nz <- readEnvInt "VSPLAT_NZ"
  keepPeriod <- readEnvInt "VSPLAT_KEEP_PERIOD"
  let result = voxelTrilinearSplatKernel n nx ny nz keepPeriod
  print (R.sumAllS result)

-- ---------------------------------------------------------------------------
-- Timed variants: one per benchmark, using the timing harness
-- ---------------------------------------------------------------------------

runBenchmarkByNameTimed :: TimingOptions -> String -> IO ()
runBenchmarkByNameTimed opts name = case name of
  "blackscholes"               -> timedBlackScholes opts
  "nbody"                      -> timedNBody opts
  "mandelbrot"                 -> timedMandelbrot opts
  "spmv"                       -> timedSpMV opts
  "matmul"                     -> timedMatMul opts
  "weighted_histogram"         -> timedWeightedHistogram opts
  "guarded_weighted_histogram" -> timedGuardedWeightedHistogram opts
  "coo_csr_build"              -> timedCooCsrBuild opts
  "graph_messages"             -> timedGraphMessages opts
  "voxel_rasterization"        -> timedVoxelRasterization opts
  "voxel_trilinear_splat"      -> timedVoxelTrilinearSplat opts
  _ -> die $ "no timed harness for benchmark: " ++ name

timedBlackScholes :: TimingOptions -> IO ()
timedBlackScholes opts = runTimingHarnessIO "main" opts load run R.sumAllS
  where
    load = do
      n      <- readEnvInt "BS_N"
      spots  <- vecFromVector <$> readCSVDoubles "bench/blackscholes/spots.csv"
      strikes <- vecFromVector <$> readCSVDoubles "bench/blackscholes/strikes.csv"
      rates  <- vecFromVector <$> readCSVDoubles "bench/blackscholes/rates.csv"
      vols   <- vecFromVector <$> readCSVDoubles "bench/blackscholes/vols.csv"
      times  <- vecFromVector <$> readCSVDoubles "bench/blackscholes/times.csv"
      pure (n, spots, strikes, rates, vols, times)
    run (n, spots, strikes, rates, vols, times) =
      blackScholesKernel n spots strikes rates vols times

timedNBody :: TimingOptions -> IO ()
timedNBody opts = runTimingHarnessIO "main" opts loadNBodyInputs runNBodyFusedOutputsIO nBodyChecksum

timedMandelbrot :: TimingOptions -> IO ()
timedMandelbrot opts = runTimingHarnessIO "main" opts load run R.sumAllS
  where
    load = do
      w <- readEnvInt "MAND_W"
      h <- readEnvInt "MAND_H"
      i <- readEnvInt "MAND_ITERS"
      pure (w, h, i)
    run (w, h, i) = mandelbrotKernel w h i

timedSpMV :: TimingOptions -> IO ()
timedSpMV opts = runTimingHarnessIO "main" opts load run R.sumAllS
  where
    load = do
      nrows  <- readEnvInt "SPMV_NROWS"
      values <- vecFromVector <$> readCSVDoubles "bench/spmv/values.csv"
      colIdx <- vecFromVector <$> readCSVInts "bench/spmv/col_idx.csv"
      rowPtr <- vecFromVector <$> readCSVInts "bench/spmv/row_ptr.csv"
      x      <- vecFromVector <$> readCSVDoubles "bench/spmv/x.csv"
      pure (nrows, rowPtr, values, colIdx, x)
    run (nrows, rowPtr, values, colIdx, x) =
      spmvKernel nrows rowPtr values colIdx x

timedMatMul :: TimingOptions -> IO ()
timedMatMul opts = runTimingHarnessIO "main" opts load run R.sumAllS
  where
    load = do
      m <- readEnvInt "MAT_M"
      k <- readEnvInt "MAT_K"
      n <- readEnvInt "MAT_N"
      a <- matFromVector m k <$> readCSVDoubles "bench/matmul/matA.csv"
      b <- matFromVector k n <$> readCSVDoubles "bench/matmul/matB.csv"
      pure (m, k, n, a, b)
    run (m, k, n, a, b) = matMulKernel m k n a b

timedWeightedHistogram :: TimingOptions -> IO ()
timedWeightedHistogram opts =
  runTimingHarness "main" opts load run (fromIntegral . R.sumAllS)
  where
    load = do
      n    <- readEnvInt "WH_N"
      bins <- readEnvInt "WH_BINS"
      pure (n, bins)
    run (n, bins) = weightedHistogramKernel n bins

timedGuardedWeightedHistogram :: TimingOptions -> IO ()
timedGuardedWeightedHistogram opts =
  runTimingHarness "main" opts load run (fromIntegral . R.sumAllS)
  where
    load = do
      n    <- readEnvInt "GWH_N"
      bins <- readEnvInt "GWH_BINS"
      kp   <- readEnvInt "GWH_KEEP_PERIOD"
      pure (n, bins, kp)
    run (n, bins, kp) = guardedWeightedHistogramKernel n bins kp

timedCooCsrBuild :: TimingOptions -> IO ()
timedCooCsrBuild opts =
  runTimingHarness "main" opts load run (\(a, b, c, d) -> fromIntegral (a + b + c + d))
  where
    load = do
      nrows <- readEnvInt "COO_NROWS"
      ncols <- readEnvInt "COO_NCOLS"
      nnz   <- readEnvInt "COO_NNZ"
      dup   <- readEnvInt "COO_DUP_PERIOD"
      pure (nrows, ncols, nnz, dup)
    run (nrows, ncols, nnz, dup) = cooCsrBuildKernel nrows ncols nnz dup

timedGraphMessages :: TimingOptions -> IO ()
timedGraphMessages opts =
  runTimingHarnessIO "main" opts load run (fromIntegral . R.sumAllS)
  where
    load = do
      n      <- readEnvInt "GRAPH_NODES"
      degree <- readEnvInt "GRAPH_DEGREE"
      pure (n, degree)
    run (n, degree) = graphMessagesKernel n degree

timedVoxelRasterization :: TimingOptions -> IO ()
timedVoxelRasterization opts = runTimingHarness "main" opts load run R.sumAllS
  where
    load = do
      n    <- readEnvInt "VOX_POINTS"
      nx   <- readEnvInt "VOX_NX"
      ny   <- readEnvInt "VOX_NY"
      nz   <- readEnvInt "VOX_NZ"
      kp   <- readEnvInt "VOX_KEEP_PERIOD"
      pure (n, nx, ny, nz, kp)
    run (n, nx, ny, nz, kp) = voxelRasterizationKernel n nx ny nz kp

timedVoxelTrilinearSplat :: TimingOptions -> IO ()
timedVoxelTrilinearSplat opts = runTimingHarness "main" opts load run R.sumAllS
  where
    load = do
      n    <- readEnvInt "VSPLAT_POINTS"
      nx   <- readEnvInt "VSPLAT_NX"
      ny   <- readEnvInt "VSPLAT_NY"
      nz   <- readEnvInt "VSPLAT_NZ"
      kp   <- readEnvInt "VSPLAT_KEEP_PERIOD"
      pure (n, nx, ny, nz, kp)
    run (n, nx, ny, nz, kp) = voxelTrilinearSplatKernel n nx ny nz kp
