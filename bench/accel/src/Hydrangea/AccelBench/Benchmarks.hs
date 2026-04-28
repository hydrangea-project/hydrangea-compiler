{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Hydrangea.AccelBench.Benchmarks
  ( benchmarkNames
  , runAllBenchmarks
  , runBenchmarkByName
  , runBenchmarkByNameTimed
  ) where

import Hydrangea.AccelBench.Common
import Hydrangea.AccelBench.Harness

import Control.Monad (forM_)
import Data.Array.Accelerate
  ( Acc, Array, DIM1, DIM2, Z(..), (:.)(..)
  , pattern I1, pattern I2, pattern T3, pattern T4
  , Vector, Matrix
  , pattern Just_, pattern Nothing_
  , (<), (<=), (&&)
  )
import Data.Array.Accelerate qualified as A
import Data.Array.Accelerate.LLVM.Native qualified as CPU
import Prelude hiding ((<), (<=), (&&))
import System.Exit (die)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

sumAllDoubles :: Array DIM1 Double -> Double
sumAllDoubles = Prelude.sum . A.toList

sumAllDoubles2D :: Array DIM2 Double -> Double
sumAllDoubles2D = Prelude.sum . A.toList

sumAllInts :: Array DIM1 Int -> Double
sumAllInts = fromIntegral . (Prelude.sum :: [Int] -> Int) . A.toList

-- | Clenshaw evaluation of a Chebyshev series.
-- @cs@ = [c0, c1, …, cN] are Chebyshev coefficients in ascending degree order.
-- Evaluates Σ c_k T_k(t) using the stable three-term recurrence.
evalCheb :: [Double] -> A.Exp Double -> A.Exp Double
evalCheb []       _ = 0
evalCheb (c0:rest) t =
  let step (ak1, ak2) c = (A.constant c + 2 * t * ak1 - ak2, ak1)
      (a1, a2) = foldl step (0, 0) (reverse rest)
  in  A.constant c0 + t * a1 - a2

-- | Exp-free approximation to erf, max absolute error ≈ 5e-8.
-- Uses 4-piece Chebyshev evaluation; no transcendental function calls.
-- This replaces the A&S 7.1.26 formula which unconditionally calls exp(-x²).
--
--   Piece 1   |x| ∈ [0,       0.84375]: Cheb-6  for erf(x)/x  as fn of x²
--   Piece 2a  |x| ∈ [0.84375, 1.6]:    Cheb-8  for erf(x) directly
--   Piece 2b1 |x| ∈ [1.6,     2.5]:    Cheb-7  for erf(x) directly
--   Piece 2b2 |x| ∈ [2.5,     4.5]:    Cheb-9  for erf(x) directly
--   |x| ≥ 4.5: clamped to ±1
erfFast :: A.Exp Double -> A.Exp Double
erfFast x =
  let ax = abs x

      -- u1 maps x² ∈ [0, 0.84375²] → [-1, 1]
      u1 = 2 * ax * ax / 0.7119140625 - 1
      p1 = ax * evalCheb
              [ 1.01324254152290827e+00, -1.09305843540958084e-01
              ,  5.59140001151067767e-03, -2.31269082139996069e-04
              ,  7.88014867629312858e-06, -2.26982938232759150e-07
              ,  5.71702546634135651e-09 ]
              u1

      -- u2a maps x ∈ [0.84375, 1.6] → [-1, 1]
      u2a = 2 * (ax - 0.84375) / 0.75625 - 1
      p2a = evalCheb
              [ 8.93886178150128052e-01,  1.02437235668834059e-01
              , -2.21133968266726787e-02,  2.14792553316058617e-03
              ,  1.25394396382965130e-05, -2.38891946112290732e-05
              ,  1.71950726101029906e-06,  8.81826412428337292e-08
              , -1.82651772653009185e-08 ]
              u2a

      -- u2b1 maps x ∈ [1.6, 2.5] → [-1, 1]
      u2b1 = 2 * (ax - 1.6) / 0.9 - 1
      p2b1 = evalCheb
              [ 9.92274555550660575e-01,  1.05875650348229867e-02
              , -4.14369417399962532e-03,  1.02100882923373366e-03
              , -1.60180247370903060e-04,  1.39233742229831938e-05
              , -1.32804730126315880e-08, -1.64050184053516437e-07 ]
              u2b1

      -- u2b2 maps x ∈ [2.5, 4.5] → [-1, 1]
      u2b2 = 2 * (ax - 2.5) / 2.0 - 1
      p2b2 = evalCheb
              [ 9.99929727429349158e-01,  1.27887131926592755e-04
              , -9.64381694513239042e-05,  6.04082312503907118e-05
              , -3.14940219657540383e-05,  1.36718647907742693e-05
              , -4.91855527141691625e-06,  1.45116678686502115e-06
              , -3.37492970607292196e-07,  5.91800916408245535e-08 ]
              u2b2

      r = A.cond (ax < 0.84375) p1
            (A.cond (ax < 1.6) p2a
              (A.cond (ax < 2.5) p2b1
                (A.cond (ax < 4.5) p2b2 1.0)))

  in  signum x * r

-- ---------------------------------------------------------------------------
-- Benchmark registry
-- ---------------------------------------------------------------------------

data Benchmark = Benchmark
  { benchmarkName :: String
  , benchmarkRun  :: IO ()
  }

benchmarkNames :: [String]
benchmarkNames = map benchmarkName benchmarks

benchmarks :: [Benchmark]
benchmarks =
  [ Benchmark "blackscholes"               runBlackScholes
  , Benchmark "nbody"                      runNBody
  , Benchmark "mandelbrot"                 runMandelbrot
  , Benchmark "spmv"                       runSpMV
  , Benchmark "matmul"                     runMatMul
  , Benchmark "weighted_histogram"         runWeightedHistogram
  , Benchmark "guarded_weighted_histogram" runGuardedWeightedHistogram
  , Benchmark "graph_messages"             runGraphMessages
  , Benchmark "voxel_rasterization"        runVoxelRasterization
  , Benchmark "voxel_trilinear_splat"      runVoxelTrilinearSplat
  , Benchmark "softmax"                    runSoftmax
  , Benchmark "coo_spmv"                   runCooSpmv
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

-- ---------------------------------------------------------------------------
-- Kernels
-- ---------------------------------------------------------------------------

blackScholesKernel
  :: Acc (Vector Double) -> Acc (Vector Double) -> Acc (Vector Double)
  -> Acc (Vector Double) -> Acc (Vector Double)
  -> Acc (Vector Double)
blackScholesKernel spots strikes rates vols times =
  A.generate (A.shape spots) $ \(I1 i) ->
    let s     = spots   A.! I1 i
        k     = strikes A.! I1 i
        r     = rates   A.! I1 i
        sigma = vols    A.! I1 i
        t     = times   A.! I1 i
        phi x = 0.5 * (1.0 + erfFast (x / sqrt 2.0))
        d1 = (log (s / k) + (r + 0.5 * sigma * sigma) * t) / (sigma * sqrt t)
        d2 = d1 - sigma * sqrt t
    in s * phi d1 - k * exp ((-1.0) * r * t) * phi d2

-- | One N-body gravity step using the Accelerate-idiomatic pattern:
-- pack bodies, replicate to n×n, zipWith pairwise force, fold over j.
nBodyKernel
  :: Int
  -> Acc (Vector Double) -> Acc (Vector Double) -> Acc (Vector Double)
  -> Acc (Vector Double)
  -> Acc (Vector Double) -> Acc (Vector Double) -> Acc (Vector Double)
  -> (Acc (Vector Double), Acc (Vector Double), Acc (Vector Double))
nBodyKernel n xs ys zs ms vxs vys vzs =
  let bodies  = A.generate (A.constant (Z :. n)) $ \(I1 i) ->
                  T4 (xs A.! I1 i) (ys A.! I1 i) (zs A.! I1 i) (ms A.! I1 i)
      -- rows[i][j] = bodies[i]  (replicate each body across all j)
      rows    = A.replicate (A.lift (Z :. A.All :. n)) bodies
      -- cols[i][j] = bodies[j]  (replicate each body across all i)
      cols    = A.replicate (A.lift (Z :. n :. A.All)) bodies
      forces  = A.zipWith bodyAccel rows cols
      accsXYZ :: Acc (Vector (Double, Double, Double))
      accsXYZ = A.fold addAccel3 (A.constant (0.0, 0.0, 0.0)) forces
      dt = 0.01 :: A.Exp Double
      newXs = A.generate (A.constant (Z :. n)) $ \(I1 i) ->
                let T3 ax _ _ = accsXYZ A.! I1 i
                in xs A.! I1 i + (vxs A.! I1 i + ax * dt) * dt
      newYs = A.generate (A.constant (Z :. n)) $ \(I1 i) ->
                let T3 _ ay _ = accsXYZ A.! I1 i
                in ys A.! I1 i + (vys A.! I1 i + ay * dt) * dt
      newZs = A.generate (A.constant (Z :. n)) $ \(I1 i) ->
                let T3 _ _ az = accsXYZ A.! I1 i
                in zs A.! I1 i + (vzs A.! I1 i + az * dt) * dt
  in (newXs, newYs, newZs)
  where
    eps2 = 0.01 :: A.Exp Double
    g    = 6.674e-11 :: A.Exp Double

    bodyAccel
      :: A.Exp (Double, Double, Double, Double)
      -> A.Exp (Double, Double, Double, Double)
      -> A.Exp (Double, Double, Double)
    bodyAccel body_i body_j =
      let T4 xi yi zi _  = body_i
          T4 xj yj zj mj = body_j
          dx = xj - xi; dy = yj - yi; dz = zj - zi
          r2 = dx*dx + dy*dy + dz*dz + eps2
          r3 = r2 * sqrt r2
          sc = g * mj / r3
      in T3 (dx*sc) (dy*sc) (dz*sc)

    addAccel3
      :: A.Exp (Double, Double, Double)
      -> A.Exp (Double, Double, Double)
      -> A.Exp (Double, Double, Double)
    addAccel3 a b =
      let T3 ax1 ay1 az1 = a
          T3 ax2 ay2 az2 = b
      in T3 (ax1+ax2) (ay1+ay2) (az1+az2)

-- | Mandelbrot escape-count image using A.while per element.
mandelbrotKernel :: Int -> Int -> Int -> Acc (Matrix Double)
mandelbrotKernel width height iters =
  A.generate (A.constant (Z :. width :. height)) $ \(I2 px py) ->
    let w  = A.fromIntegral (A.constant width  :: A.Exp Int) :: A.Exp Double
        h  = A.fromIntegral (A.constant height :: A.Exp Int) :: A.Exp Double
        cx = (-2.5) + 3.5 * A.fromIntegral px / (w - 1.0)
        cy = (-1.0) + 2.0 * A.fromIntegral py / (h - 1.0)
        T3 _ _ count = A.while
          (\(T3 re im c) -> c < A.constant iters && re*re + im*im <= 4.0)
          (\(T3 re im c) ->
            let re' = re*re - im*im + cx
                im' = 2.0*re*im + cy
            in T3 re' im' (c + 1))
          (T3 (0.0 :: A.Exp Double) (0.0 :: A.Exp Double) (0 :: A.Exp Int))
    in A.fromIntegral count

-- | CSR sparse matrix-vector multiply using A.foldSeg over segment lengths.
spmvKernel
  :: Acc (Vector Int)    -- ^ rowPtr (length nrows+1)
  -> Acc (Vector Double) -- ^ values
  -> Acc (Vector Int)    -- ^ colIdx
  -> Acc (Vector Double) -- ^ x
  -> Acc (Vector Double)
spmvKernel rowPtr values colIdx x =
  let products = A.zipWith (*) values (A.gather colIdx x)
      segLens  = A.zipWith (-) (A.tail rowPtr) (A.init rowPtr)
  in A.foldSeg (+) 0.0 products segLens

-- | Matrix multiply via 3D replicate + zipWith + fold.
matMulKernel :: Acc (Matrix Double) -> Acc (Matrix Double) -> Acc (Matrix Double)
matMulKernel a b =
  let bT    = A.transpose b
      A.Z_ A.::. _m A.::. _k = A.shape a
      A.Z_ A.::. m  A.::. _  = A.shape a
      A.Z_ A.::. n  A.::. _  = A.shape bT
      aExp  = A.replicate (A.lift (Z :. A.All :. n :. A.All)) a
      bTExp = A.replicate (A.lift (Z :. m :. A.All :. A.All)) bT
  in A.fold (+) 0.0 (A.zipWith (*) aExp bTExp)

softmaxKernel :: Acc (Matrix Double) -> Acc (Matrix Double)
softmaxKernel logits =
  let A.Z_ A.::. _ A.::. n = A.shape logits
      expLogits    = A.map exp logits
      rowSums      = A.fold (+) 0.0 expLogits
      rowSumsBcast = A.replicate (A.lift (Z :. A.All :. n)) rowSums
  in A.zipWith (/) expLogits rowSumsBcast

weightedHistogramKernel :: Int -> Int -> Acc (Vector Int)
weightedHistogramKernel n bins =
  let idxs    = A.generate (A.constant (Z :. n)) $ \(I1 i) ->
                  (i * A.constant bins) `div` A.constant n
      weights = A.generate (A.constant (Z :. n)) $ \(I1 i) ->
                  i * 3 + 1
      zeros   = A.fill (A.constant (Z :. bins)) (0 :: A.Exp Int)
  in A.permute (+) zeros (\(I1 i) -> Just_ (I1 (idxs A.! I1 i))) weights

guardedWeightedHistogramKernel :: Int -> Int -> Int -> Acc (Vector Int)
guardedWeightedHistogramKernel n bins keepPeriod =
  let idxs    = A.generate (A.constant (Z :. n)) $ \(I1 i) ->
                  (i * A.constant bins) `div` A.constant n
      weights = A.generate (A.constant (Z :. n)) $ \(I1 i) ->
                  i * 3 + 1
      keep    = A.generate (A.constant (Z :. n)) $ \(I1 i) ->
                  (i `div` A.constant keepPeriod) * A.constant keepPeriod A.== i
      zeros   = A.fill (A.constant (Z :. bins)) (0 :: A.Exp Int)
  in A.permute (+) zeros
       (\(I1 i) -> A.cond (keep A.! I1 i) (Just_ (I1 (idxs A.! I1 i))) Nothing_)
       weights

-- | Graph message passing: each node aggregates edge*src over its degree incoming edges.
graphMessagesKernel :: Int -> Int -> Acc (Vector Int)
graphMessagesKernel n degree =
  let nnz      = n * degree
      nodeVals = A.generate (A.constant (Z :. n)) $ \(I1 j) -> j + 1
      messages = A.generate (A.constant (Z :. nnz)) $ \(I1 k) ->
                   let dst     = k `div` A.constant degree
                       edgeVal = k + 1
                   in edgeVal * (nodeVals A.! I1 dst)
      reshaped = A.reshape (A.constant (Z :. n :. degree)) messages
  in A.fold (+) 0 reshaped

voxelRasterizationKernel :: Int -> Int -> Int -> Int -> Int -> Acc (Vector Double)
voxelRasterizationKernel n nx ny nz keepPeriod =
  let size    = nx * ny * nz
      idxs    = A.generate (A.constant (Z :. n)) $ \(I1 p) ->
                  let x = (p * A.constant 17 + 3) `mod` A.constant nx
                      y = (p * A.constant 29 + 5) `mod` A.constant ny
                      z = (p * A.constant 43 + 7) `mod` A.constant nz
                  in ((z * A.constant ny) + y) * A.constant nx + x
      weights = A.generate (A.constant (Z :. n)) $ \(I1 p) ->
                  let x = (p * A.constant 17 + 3) `mod` A.constant nx
                      y = (p * A.constant 29 + 5) `mod` A.constant ny
                      z = (p * A.constant 43 + 7) `mod` A.constant nz
                  in A.fromIntegral (((x + 1) * (y + 2)) + (z * 3) + 1) :: A.Exp Double
      keep    = A.generate (A.constant (Z :. n)) $ \(I1 p) ->
                  (p `div` A.constant keepPeriod) * A.constant keepPeriod A.== p
      zeros   = A.fill (A.constant (Z :. size)) (0.0 :: A.Exp Double)
  in A.permute (+) zeros
       (\(I1 i) -> A.cond (keep A.! I1 i) (Just_ (I1 (idxs A.! I1 i))) Nothing_)
       weights

voxelTrilinearSplatKernel :: Int -> Int -> Int -> Int -> Int -> Acc (Vector Double)
voxelTrilinearSplatKernel n nx ny nz keepPeriod =
  let contribs = n * 8
      size     = nx * ny * nz
      idxs     = A.generate (A.constant (Z :. contribs)) $ \(I1 i) ->
                   let p  = i `div` 8;  c = i `mod` 8
                       bx = c `mod` 2;  by = (c `div` 2) `mod` 2;  bz = (c `div` 4) `mod` 2
                       x  = (p * A.constant 17 + 3 + bx) `mod` A.constant nx
                       y  = (p * A.constant 29 + 5 + by) `mod` A.constant ny
                       z  = (p * A.constant 43 + 7 + bz) `mod` A.constant nz
                   in ((z * A.constant ny) + y) * A.constant nx + x
      weights  = A.generate (A.constant (Z :. contribs)) $ \(I1 i) ->
                   let p  = i `div` 8;  c = i `mod` 8
                       bx = c `mod` 2;  by = (c `div` 2) `mod` 2;  bz = (c `div` 4) `mod` 2
                       fx = (p * 5 + 1) `mod` 4;  fy = (p * 7 + 2) `mod` 4
                       fz = (p * 11 + 3) `mod` 4
                       wx = (1 - bx) * (4 - fx) + bx * fx
                       wy = (1 - by) * (4 - fy) + by * fy
                       wz = (1 - bz) * (4 - fz) + bz * fz
                       base = (p * 3) + 1
                   in A.fromIntegral (base * wx * wy * wz) / 64.0 :: A.Exp Double
      keep     = A.generate (A.constant (Z :. contribs)) $ \(I1 i) ->
                   let p = i `div` 8
                   in (p `div` A.constant keepPeriod) * A.constant keepPeriod A.== p
      zeros    = A.fill (A.constant (Z :. size)) (0.0 :: A.Exp Double)
  in A.permute (+) zeros
       (\(I1 i) -> A.cond (keep A.! I1 i) (Just_ (I1 (idxs A.! I1 i))) Nothing_)
       weights

-- | COO sparse matrix-vector multiply via A.permute scatter-add.
cooSpmvKernel
  :: Int
  -> Acc (Vector Int)
  -> Acc (Vector Int)
  -> Acc (Vector Double)
  -> Acc (Vector Double)
  -> Acc (Vector Double)
cooSpmvKernel nrows rowIdx colIdx values x =
  let products = A.zipWith (*) values (A.gather colIdx x)
      zeros    = A.fill (A.constant (Z :. nrows)) (0.0 :: A.Exp Double)
  in A.permute (+) zeros (\(I1 i) -> Just_ (I1 (rowIdx A.! I1 i))) products

-- ---------------------------------------------------------------------------
-- One-shot runners
-- ---------------------------------------------------------------------------

runBlackScholes :: IO ()
runBlackScholes = do
  n       <- readEnvInt "BS_N"
  spots   <- A.use . toAccelVec <$> readCSVDoubles "bench/blackscholes/spots.csv"
  strikes <- A.use . toAccelVec <$> readCSVDoubles "bench/blackscholes/strikes.csv"
  rates   <- A.use . toAccelVec <$> readCSVDoubles "bench/blackscholes/rates.csv"
  vols    <- A.use . toAccelVec <$> readCSVDoubles "bench/blackscholes/vols.csv"
  times   <- A.use . toAccelVec <$> readCSVDoubles "bench/blackscholes/times.csv"
  let result = CPU.run $ blackScholesKernel spots strikes rates vols times
  writeCSVDoubles "bench/blackscholes/out.csv" (fromAccelVec n result)

runNBody :: IO ()
runNBody = do
  n   <- readEnvInt "NBODY_N"
  xs  <- A.use . toAccelVec <$> readCSVDoubles "bench/nbody/xs.csv"
  ys  <- A.use . toAccelVec <$> readCSVDoubles "bench/nbody/ys.csv"
  zs  <- A.use . toAccelVec <$> readCSVDoubles "bench/nbody/zs.csv"
  ms  <- A.use . toAccelVec <$> readCSVDoubles "bench/nbody/ms.csv"
  vxs <- A.use . toAccelVec <$> readCSVDoubles "bench/nbody/vxs.csv"
  vys <- A.use . toAccelVec <$> readCSVDoubles "bench/nbody/vys.csv"
  vzs <- A.use . toAccelVec <$> readCSVDoubles "bench/nbody/vzs.csv"
  let (nxsAcc, nysAcc, nzsAcc) = nBodyKernel n xs ys zs ms vxs vys vzs
      (newXs, newYs, newZs)    = CPU.run $ A.lift (nxsAcc, nysAcc, nzsAcc)
  writeCSVDoubles "bench/nbody/out_xs.csv" (fromAccelVec n newXs)
  writeCSVDoubles "bench/nbody/out_ys.csv" (fromAccelVec n newYs)
  writeCSVDoubles "bench/nbody/out_zs.csv" (fromAccelVec n newZs)

runMandelbrot :: IO ()
runMandelbrot = do
  width  <- readEnvInt "MAND_W"
  height <- readEnvInt "MAND_H"
  iters  <- readEnvInt "MAND_ITERS"
  let result = CPU.run $ mandelbrotKernel width height iters
  writeCSVDoubles2D "bench/mandelbrot/out.csv" width height (fromAccelMat width height result)

runSpMV :: IO ()
runSpMV = do
  nrows  <- readEnvInt "SPMV_NROWS"
  values <- A.use . toAccelVec <$> readCSVDoubles "bench/spmv/values.csv"
  colIdx <- A.use . toAccelVec <$> readCSVInts    "bench/spmv/col_idx.csv"
  rowPtr <- A.use . toAccelVec <$> readCSVInts    "bench/spmv/row_ptr.csv"
  x      <- A.use . toAccelVec <$> readCSVDoubles "bench/spmv/x.csv"
  let result = CPU.run $ spmvKernel rowPtr values colIdx x
  writeCSVDoubles "bench/spmv/out.csv" (fromAccelVec nrows result)

runMatMul :: IO ()
runMatMul = do
  m <- readEnvInt "MAT_M"
  k <- readEnvInt "MAT_K"
  n <- readEnvInt "MAT_N"
  a <- A.use . toAccelMat m k <$> readCSVDoubles "bench/matmul/matA.csv"
  b <- A.use . toAccelMat k n <$> readCSVDoubles "bench/matmul/matB.csv"
  let result = CPU.run $ matMulKernel a b
  writeCSVDoubles2D "bench/matmul/out.csv" m n (fromAccelMat m n result)

runSoftmax :: IO ()
runSoftmax = do
  m      <- readEnvInt "SOFTMAX_M"
  n      <- readEnvInt "SOFTMAX_N"
  logits <- A.use . toAccelMat m n <$> readCSVDoubles "bench/softmax/logits.csv"
  let result = CPU.run $ softmaxKernel logits
  writeCSVDoubles2D "bench/softmax/out.csv" m n (fromAccelMat m n result)

runWeightedHistogram :: IO ()
runWeightedHistogram = do
  n    <- readEnvInt "WH_N"
  bins <- readEnvInt "WH_BINS"
  let hist = CPU.run $ weightedHistogramKernel n bins
  print (Prelude.sum (A.toList hist) :: Int)

runGuardedWeightedHistogram :: IO ()
runGuardedWeightedHistogram = do
  n          <- readEnvInt "GWH_N"
  bins       <- readEnvInt "GWH_BINS"
  keepPeriod <- readEnvInt "GWH_KEEP_PERIOD"
  let hist = CPU.run $ guardedWeightedHistogramKernel n bins keepPeriod
  print (Prelude.sum (A.toList hist) :: Int)

runCooSpmv :: IO ()
runCooSpmv = do
  nrows  <- readEnvInt "COO_NROWS"
  rowIdx <- A.use . toAccelVec <$> readCSVInts    "bench/coo_spmv/row_idx.csv"
  colIdx <- A.use . toAccelVec <$> readCSVInts    "bench/coo_spmv/col_idx.csv"
  values <- A.use . toAccelVec <$> readCSVDoubles "bench/coo_spmv/values.csv"
  x      <- A.use . toAccelVec <$> readCSVDoubles "bench/coo_spmv/x.csv"
  let result = CPU.run $ cooSpmvKernel nrows rowIdx colIdx values x
  writeCSVDoubles "bench/coo_spmv/out.csv" (fromAccelVec nrows result)

runGraphMessages :: IO ()
runGraphMessages = do
  n      <- readEnvInt "GRAPH_NODES"
  degree <- readEnvInt "GRAPH_DEGREE"
  let result = CPU.run $ graphMessagesKernel n degree
  print (Prelude.sum (A.toList result) :: Int)

runVoxelRasterization :: IO ()
runVoxelRasterization = do
  n          <- readEnvInt "VOX_POINTS"
  nx         <- readEnvInt "VOX_NX"
  ny         <- readEnvInt "VOX_NY"
  nz         <- readEnvInt "VOX_NZ"
  keepPeriod <- readEnvInt "VOX_KEEP_PERIOD"
  let result = CPU.run $ voxelRasterizationKernel n nx ny nz keepPeriod
  print (Prelude.sum (A.toList result) :: Double)

runVoxelTrilinearSplat :: IO ()
runVoxelTrilinearSplat = do
  n          <- readEnvInt "VSPLAT_POINTS"
  nx         <- readEnvInt "VSPLAT_NX"
  ny         <- readEnvInt "VSPLAT_NY"
  nz         <- readEnvInt "VSPLAT_NZ"
  keepPeriod <- readEnvInt "VSPLAT_KEEP_PERIOD"
  let result = CPU.run $ voxelTrilinearSplatKernel n nx ny nz keepPeriod
  print (Prelude.sum (A.toList result) :: Double)

-- ---------------------------------------------------------------------------
-- Timed variants
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
  "graph_messages"             -> timedGraphMessages opts
  "voxel_rasterization"        -> timedVoxelRasterization opts
  "voxel_trilinear_splat"      -> timedVoxelTrilinearSplat opts
  "softmax"                    -> timedSoftmax opts
  "coo_spmv"                   -> timedCooSpmv opts
  _ -> die $ "no timed harness for benchmark: " ++ name

timedBlackScholes :: TimingOptions -> IO ()
timedBlackScholes opts = runTimingHarness "main" opts load run sumAllDoubles
  where
    load = do
      spots   <- A.use . toAccelVec <$> readCSVDoubles "bench/blackscholes/spots.csv"
      strikes <- A.use . toAccelVec <$> readCSVDoubles "bench/blackscholes/strikes.csv"
      rates   <- A.use . toAccelVec <$> readCSVDoubles "bench/blackscholes/rates.csv"
      vols    <- A.use . toAccelVec <$> readCSVDoubles "bench/blackscholes/vols.csv"
      times   <- A.use . toAccelVec <$> readCSVDoubles "bench/blackscholes/times.csv"
      pure (spots, strikes, rates, vols, times)
    run (spots, strikes, rates, vols, times) =
      CPU.run $ blackScholesKernel spots strikes rates vols times

timedNBody :: TimingOptions -> IO ()
timedNBody opts = runTimingHarness "main" opts load run chk
  where
    load = do
      n   <- readEnvInt "NBODY_N"
      xs  <- A.use . toAccelVec <$> readCSVDoubles "bench/nbody/xs.csv"
      ys  <- A.use . toAccelVec <$> readCSVDoubles "bench/nbody/ys.csv"
      zs  <- A.use . toAccelVec <$> readCSVDoubles "bench/nbody/zs.csv"
      ms  <- A.use . toAccelVec <$> readCSVDoubles "bench/nbody/ms.csv"
      vxs <- A.use . toAccelVec <$> readCSVDoubles "bench/nbody/vxs.csv"
      vys <- A.use . toAccelVec <$> readCSVDoubles "bench/nbody/vys.csv"
      vzs <- A.use . toAccelVec <$> readCSVDoubles "bench/nbody/vzs.csv"
      pure (n, xs, ys, zs, ms, vxs, vys, vzs)
    run (n, xs, ys, zs, ms, vxs, vys, vzs) =
      let (nxsAcc, nysAcc, nzsAcc) = nBodyKernel n xs ys zs ms vxs vys vzs
      in CPU.run $ A.lift (nxsAcc, nysAcc, nzsAcc)
    chk (r1, r2, r3) = sumAllDoubles r1 + sumAllDoubles r2 + sumAllDoubles r3

timedMandelbrot :: TimingOptions -> IO ()
timedMandelbrot opts = runTimingHarness "main" opts load run sumAllDoubles2D
  where
    load = do
      w <- readEnvInt "MAND_W"
      h <- readEnvInt "MAND_H"
      i <- readEnvInt "MAND_ITERS"
      pure (w, h, i)
    run (w, h, i) = CPU.run $ mandelbrotKernel w h i

timedSpMV :: TimingOptions -> IO ()
timedSpMV opts = runTimingHarness "main" opts load run sumAllDoubles
  where
    load = do
      values <- A.use . toAccelVec <$> readCSVDoubles "bench/spmv/values.csv"
      colIdx <- A.use . toAccelVec <$> readCSVInts    "bench/spmv/col_idx.csv"
      rowPtr <- A.use . toAccelVec <$> readCSVInts    "bench/spmv/row_ptr.csv"
      x      <- A.use . toAccelVec <$> readCSVDoubles "bench/spmv/x.csv"
      pure (rowPtr, values, colIdx, x)
    run (rowPtr, values, colIdx, x) = CPU.run $ spmvKernel rowPtr values colIdx x

timedMatMul :: TimingOptions -> IO ()
timedMatMul opts = runTimingHarness "main" opts load run sumAllDoubles2D
  where
    load = do
      m <- readEnvInt "MAT_M"
      k <- readEnvInt "MAT_K"
      n <- readEnvInt "MAT_N"
      a <- A.use . toAccelMat m k <$> readCSVDoubles "bench/matmul/matA.csv"
      b <- A.use . toAccelMat k n <$> readCSVDoubles "bench/matmul/matB.csv"
      pure (a, b)
    run (a, b) = CPU.run $ matMulKernel a b

timedSoftmax :: TimingOptions -> IO ()
timedSoftmax opts = runTimingHarness "main" opts load run sumAllDoubles2D
  where
    load = do
      m      <- readEnvInt "SOFTMAX_M"
      n      <- readEnvInt "SOFTMAX_N"
      logits <- A.use . toAccelMat m n <$> readCSVDoubles "bench/softmax/logits.csv"
      pure logits
    run logits = CPU.run $ softmaxKernel logits

timedWeightedHistogram :: TimingOptions -> IO ()
timedWeightedHistogram opts = runTimingHarness "main" opts load run sumAllInts
  where
    load = do
      n    <- readEnvInt "WH_N"
      bins <- readEnvInt "WH_BINS"
      pure (n, bins)
    run (n, bins) = CPU.run $ weightedHistogramKernel n bins

timedGuardedWeightedHistogram :: TimingOptions -> IO ()
timedGuardedWeightedHistogram opts = runTimingHarness "main" opts load run sumAllInts
  where
    load = do
      n  <- readEnvInt "GWH_N"
      bins <- readEnvInt "GWH_BINS"
      kp   <- readEnvInt "GWH_KEEP_PERIOD"
      pure (n, bins, kp)
    run (n, bins, kp) = CPU.run $ guardedWeightedHistogramKernel n bins kp

timedGraphMessages :: TimingOptions -> IO ()
timedGraphMessages opts = runTimingHarness "main" opts load run sumAllInts
  where
    load = do
      n      <- readEnvInt "GRAPH_NODES"
      degree <- readEnvInt "GRAPH_DEGREE"
      pure (n, degree)
    run (n, degree) = CPU.run $ graphMessagesKernel n degree

timedVoxelRasterization :: TimingOptions -> IO ()
timedVoxelRasterization opts = runTimingHarness "main" opts load run sumAllDoubles
  where
    load = do
      n  <- readEnvInt "VOX_POINTS"
      nx <- readEnvInt "VOX_NX"
      ny <- readEnvInt "VOX_NY"
      nz <- readEnvInt "VOX_NZ"
      kp <- readEnvInt "VOX_KEEP_PERIOD"
      pure (n, nx, ny, nz, kp)
    run (n, nx, ny, nz, kp) = CPU.run $ voxelRasterizationKernel n nx ny nz kp

timedVoxelTrilinearSplat :: TimingOptions -> IO ()
timedVoxelTrilinearSplat opts = runTimingHarness "main" opts load run sumAllDoubles
  where
    load = do
      n  <- readEnvInt "VSPLAT_POINTS"
      nx <- readEnvInt "VSPLAT_NX"
      ny <- readEnvInt "VSPLAT_NY"
      nz <- readEnvInt "VSPLAT_NZ"
      kp <- readEnvInt "VSPLAT_KEEP_PERIOD"
      pure (n, nx, ny, nz, kp)
    run (n, nx, ny, nz, kp) = CPU.run $ voxelTrilinearSplatKernel n nx ny nz kp

timedCooSpmv :: TimingOptions -> IO ()
timedCooSpmv opts = runTimingHarness "main" opts load run sumAllDoubles
  where
    load = do
      nrows  <- readEnvInt "COO_NROWS"
      rowIdx <- A.use . toAccelVec <$> readCSVInts    "bench/coo_spmv/row_idx.csv"
      colIdx <- A.use . toAccelVec <$> readCSVInts    "bench/coo_spmv/col_idx.csv"
      values <- A.use . toAccelVec <$> readCSVDoubles "bench/coo_spmv/values.csv"
      x      <- A.use . toAccelVec <$> readCSVDoubles "bench/coo_spmv/x.csv"
      pure (nrows, rowIdx, colIdx, values, x)
    run (nrows, rowIdx, colIdx, values, x) =
      CPU.run $ cooSpmvKernel nrows rowIdx colIdx values x
