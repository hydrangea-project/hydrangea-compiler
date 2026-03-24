# Hydrangea Benchmark Suite

This directory contains eleven scientific benchmarks for the Hydrangea compiler,
demonstrating array fusion, SIMD vectorization, and parallelization.

## Benchmarks

| Benchmark      | Dir             | Language features used |
|----------------|-----------------|------------------------|
| Black-Scholes  | `blackscholes/` | `map`, `sqrt`, `exp`, `log`, `erf` |
| N-body         | `nbody/`        | `generate`, `reduce_generate`, `sqrt`, scientific notation |
| Mandelbrot     | `mandelbrot/`   | `generate`, `foldl`, `fill`, `float_of` |
| SpMV (CSR)     | `spmv/`         | `generate`, `reduce_generate`, variable inner-loop bounds |
| Matrix Multiply| `matmul/`       | `generate`, `reduce_generate` (existing) |
| Weighted Histogram | `weighted_histogram/` | `generate`, `scatter`, `reduce` |
| Guarded Weighted Histogram | `guarded_weighted_histogram/` | `generate`, `scatter_guarded`, `map`, `reduce` |
| COO/CSR Build | `coo_csr_build/` | `sort_indices`, `gather`, `coo_sum_duplicates`, `csr_from_sorted_coo`, `reduce` |
| Graph Messages | `graph_messages/` | `gather`, `zipwith`, `segmented_reduce`, `reduce` |
| Voxel Rasterization | `voxel_rasterization/` | `generate`, `scatter_guarded`, `map`, `reduce` |
| Voxel Trilinear Splat | `voxel_trilinear_splat/` | `generate`, `scatter_guarded`, `reduce` |

## Quick Start

```bash
# From the repo root, compile all benchmarks:
cabal run hydrangea-compiler -- --emit-c bench/blackscholes/blackscholes.hyd
cabal run hydrangea-compiler -- --emit-c bench/nbody/nbody.hyd
cabal run hydrangea-compiler -- --emit-c bench/mandelbrot/mandelbrot.hyd
cabal run hydrangea-compiler -- --emit-c bench/spmv/spmv.hyd
cabal run hydrangea-compiler -- --emit-c bench/matmul/mat_mul_bench.hyd
cabal run hydrangea-compiler -- --emit-c bench/weighted_histogram/weighted_histogram.hyd
cabal run hydrangea-compiler -- --emit-c bench/guarded_weighted_histogram/guarded_weighted_histogram.hyd
cabal run hydrangea-compiler -- --emit-c bench/coo_csr_build/coo_csr_build.hyd
cabal run hydrangea-compiler -- --emit-c bench/graph_messages/graph_messages.hyd
cabal run hydrangea-compiler -- --emit-c bench/voxel_rasterization/voxel_rasterization.hyd
cabal run hydrangea-compiler -- --no-solver-check --emit-c bench/voxel_trilinear_splat/voxel_trilinear_splat.hyd

# Generate inputs (Python 3 required):
python3 bench/blackscholes/gen_input.py 1000000
python3 bench/nbody/gen_input.py 4096
python3 bench/spmv/gen_input.py 10000 10000 100000

# Run a benchmark (inputs must exist):
BS_N=1000000 cabal run hydrangea-compiler -- bench/blackscholes/blackscholes.hyd
NBODY_N=4096 cabal run hydrangea-compiler -- bench/nbody/nbody.hyd
MAND_W=1024 MAND_H=1024 MAND_ITERS=256 cabal run hydrangea-compiler -- bench/mandelbrot/mandelbrot.hyd
SPMV_NROWS=10000 SPMV_NCOLS=10000 SPMV_NNZ=100000 cabal run hydrangea-compiler -- bench/spmv/spmv.hyd
WH_N=1000000 WH_BINS=256 cabal run hydrangea-compiler -- bench/weighted_histogram/weighted_histogram.hyd
GWH_N=1000000 GWH_BINS=256 GWH_KEEP_PERIOD=3 cabal run hydrangea-compiler -- bench/guarded_weighted_histogram/guarded_weighted_histogram.hyd
VOX_POINTS=1000000 VOX_NX=64 VOX_NY=64 VOX_NZ=64 VOX_KEEP_PERIOD=3 cabal run hydrangea-compiler -- bench/voxel_rasterization/voxel_rasterization.hyd
VSPLAT_POINTS=1000000 VSPLAT_NX=64 VSPLAT_NY=64 VSPLAT_NZ=64 VSPLAT_KEEP_PERIOD=3 cabal run hydrangea-compiler -- --no-solver-check bench/voxel_trilinear_splat/voxel_trilinear_splat.hyd
```

## Per-Benchmark Run Scripts

Each benchmark directory has a `run.sh` that sweeps over a range of sizes:

```bash
bench/blackscholes/run.sh   # BS_SIZES="10000 100000 1000000" BS_CHECK_OUTPUT=1
bench/nbody/run.sh          # NBODY_SIZES="256 1024 4096"
bench/mandelbrot/run.sh     # MAND_SIZES="256 512 1024 2048"
bench/spmv/run.sh           # SPMV_CONFIGS="1000 1000 5000 ..."
bench/weighted_histogram/run.sh  # WH_SIZES="100000 1000000 5000000"
bench/guarded_weighted_histogram/run.sh  # GWH_SIZES="100000 1000000 5000000"
bench/coo_csr_build/run.sh  # COO_CONFIGS="1024 1024 100000 ..."
bench/graph_messages/run.sh  # GRAPH_SIZES="10000 100000 500000"
bench/voxel_rasterization/run.sh  # VOX_SIZES="100000 1000000 5000000"
bench/voxel_trilinear_splat/run.sh  # VSPLAT_SIZES="100000 1000000 5000000"
```

## Benchmark Details

### Black-Scholes (`blackscholes/`)
Prices N European options in a single parallel `map` with no intermediate arrays.
Five float arrays (spot, strike, rate, vol, time) are read from CSV.
Uses `sqrt`, `exp`, `log`, `erf` (Gaussian CDF via the error function).
`run.sh` regenerates deterministic inputs, times only `./hydrangea_out`, and by
default validates `out.csv` against `reference.csv` with
`bench/blackscholes/check_output.py`.
Current audit status: the generated C now reuses repeated pure unary math work
better, but GCC vectorization diagnostics still report the hot loop as missed
because it contains scalar libm calls plus tuple/shape-based indexing overhead.

Environment variables: `BS_N`, `BS_PARALLEL`, `BS_SIZES`, `BS_CC`,
`BS_CHECK_OUTPUT`, `BS_TOLERANCE`

### N-body (`nbody/`)
One Euler integration step for N gravitational particles (structure-of-arrays layout).
For each particle, `reduce_generate` sums pairwise forces. The outer `generate [n]`
should be parallelized; the inner reduction loop is the hot path.

Environment variable: `NBODY_N`

### Mandelbrot (`mandelbrot/`)
Fixed-iteration Mandelbrot set computation over a `width × height` grid.
Uses `foldl` to count iterations and `float_of` to convert pixel indices to
floating-point coordinates. The outer `generate [width, height]` should be
parallelized.

Environment variables: `MAND_W`, `MAND_H`, `MAND_ITERS`

### SpMV (`spmv/`)
Sparse matrix-vector multiply in CSR format.
Inner `reduce_generate` has a variable trip-count (row length that varies per row).
The compiler must classify the inner loop bound as `Unknown` (no vectorization);
the outer `generate [nrows]` should be parallelized.

Environment variables: `SPMV_NROWS`, `SPMV_NCOLS`, `SPMV_NNZ`

### Matrix Multiply (`matmul/`)
Dense matrix multiply (existing benchmark). `reduce_generate` fuses the inner
product with element generation, producing a single triply-nested loop.

Environment variables: `MAT_M`, `MAT_K`, `MAT_N`

### Weighted Histogram (`weighted_histogram/`)
Dense colliding `scatter (+)` over a fixed number of bins. The benchmark
generates `WH_N` weighted samples, routes them into `WH_BINS` dense bins,
and reduces the final histogram to a scalar checksum for compact output.

Environment variables: `WH_N`, `WH_BINS`

### Guarded Weighted Histogram (`guarded_weighted_histogram/`)
Masked/filtered variant of the weighted histogram. The benchmark generates
`GWH_N` weighted samples, routes them into `GWH_BINS` dense bins, and uses
`scatter_guarded` to skip samples that fail a deterministic validity test
controlled by `GWH_KEEP_PERIOD`. This exercises guarded scatter fusion and the
guarded atomic scatter OpenMP path.

Environment variables: `GWH_N`, `GWH_BINS`, `GWH_KEEP_PERIOD`

### COO/CSR Build (`coo_csr_build/`)
Sparse-construction benchmark that generates deterministic COO triplets, sorts
them into canonical row-major order, combines duplicates, converts to CSR, and
returns a scalar checksum. This directly exercises the new sort/sparse pipeline
without requiring external input files.

Environment variables: `COO_NROWS`, `COO_NCOLS`, `COO_NNZ`, `COO_DUP_PERIOD`

### Graph Messages (`graph_messages/`)
CSR-style graph message-passing benchmark with fixed out-degree. The benchmark
gathers destination-node features, constructs flat weighted edge messages, and
reduces them per source node with `segmented_reduce`, returning a scalar
checksum.

Environment variables: `GRAPH_NODES`, `GRAPH_DEGREE`

### Voxel Rasterization (`voxel_rasterization/`)
Point-cloud-style voxel deposition benchmark over a flattened `nx × ny × nz`
grid. The benchmark generates deterministic point coordinates and weights,
filters them with a simple keep-period predicate, deposits them with
`scatter_guarded (+)`, and returns a scalar checksum over the final voxel grid.

Environment variables: `VOX_POINTS`, `VOX_NX`, `VOX_NY`, `VOX_NZ`, `VOX_KEEP_PERIOD`

### Voxel Trilinear Splat (`voxel_trilinear_splat/`)
Higher-fanout voxel deposition benchmark. Each kept point contributes to all
eight corners of its containing cell using floating-point trilinear weights, so
the benchmark stresses guarded colliding scatter much harder than the single-cell
voxel rasterization variant. For now this benchmark is compiled with
`--no-solver-check` until refinement inference is improved for the fused
contribution pipeline.

Environment variables: `VSPLAT_POINTS`, `VSPLAT_NX`, `VSPLAT_NY`, `VSPLAT_NZ`, `VSPLAT_KEEP_PERIOD`

## Compiler Flags

```bash
--emit-c          # print generated C to stdout (don't run)
--output-c=<file> # write generated C to a file
--output-h=<file> # write generated export header (with --export-kernel)
--export-kernel   # export a named zero-argument top-level kernel
--print-fused     # print fused IR
--print-cfg       # print CFG after optimization
--keep-c          # keep intermediate .c file
--compile-only    # compile to binary but don't run
--no-parallel     # disable OpenMP parallelization
--no-solver-check # skip refinement-solver discharge after type inference
```
