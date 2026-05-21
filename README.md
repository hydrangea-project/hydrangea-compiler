# Hydrangea

Hydrangea is an experimental compiler for *functional array programs* with higher-order combinators and rank polymorphism.

The central idea is aggressive *array fusion*: chains of combinators like `map`, `zipWith`, `generate`, `scatter`, and `stencil` are fused away before any code is emitted, so the program never allocates the intermediate arrays that a naive evaluation would produce. The fused loop nests are then lowered into a CFG IR where tiling, vectorization, and OpenMP parallelization are applied before generating C code.

<a href="docs/images/ray_still_life.png">
  <img src="docs/images/ray_thumb.jpeg" alt="Ray Example" width="400"/>
</a>

## Examples

### Producer fusion — dot product

A dot product written with combinators looks like this:

```hydrangea
let dot xs ys = reduce (+) 0.0 (zipwith (*) xs ys)
```

The `zipwith` is a *producer* whose only consumer is the `reduce`. Fusion eliminates the intermediate product array entirely, giving a single accumulation loop. With OpenMP enabled the compiler emits a parallel reduction:

```c
double acc = 0.0;
#pragma omp parallel for simd simdlen(4) reduction(+:acc)
for (int64_t i = 0; i < n; i++) {
    acc += xs[i] * ys[i];
}
```

### Scatter fusion — weighted histogram

A weighted histogram routes each element of `data` into a bin while accumulating a squared value:

```hydrangea
let binFn   x = x % 64
let valueFn x = x * x

let hist = scatter (+) (fill [64] 0) (map binFn data) (map valueFn data)
```

Both `map` expressions derive from the same source, so the scatter-reindex fusion rule fires: neither mapped array is materialised. The compiler sees through the two maps and emits a single scatter loop that reads `data` once per element. With OpenMP enabled the scatter uses an atomic update:

```c
/* parallel loop */
#pragma omp parallel for /* scatter-atomic-add-int */
for (int64_t i = 0; i < n; i++) {
    int64_t bin = data[i] % 64;
    int64_t val = data[i] * data[i];
    #pragma omp atomic update
    hist[bin] += val;
}
```

### Stencil — 2D Laplacian

For stencil-shaped kernels the compiler can tile and parallelize the loop nest using its polyhedral backend (`--polyhedral`). Given:

```hydrangea
let laplacian img =
  stencil clamp
    (fn acc =>
      acc (-1) 0 + acc 1 0 + acc 0 (-1) + acc 0 1 - (4.0 * acc 0 0))
    img
```

With `--polyhedral` the generated C tiles the 2D iteration space into 32×32 blocks and distributes the tiles across threads:

```c
/* parallel map loop */
#pragma omp parallel for collapse(2)
for (int64_t i_tile = 0; i_tile < (h + 31) / 32; i_tile++) {
    for (int64_t j_tile = 0; j_tile < (w + 31) / 32; j_tile++) {
        int64_t i_start = 32 * i_tile;
        int64_t i_len   = (i_start + 32 < h) ? 32 : h - i_start;
        int64_t j_start = 32 * j_tile;
        int64_t j_len   = (j_start + 32 < w) ? 32 : w - j_start;

        for (int64_t ii = 0; ii < i_len; ii++) {
            for (int64_t jj = 0; jj < j_len; jj++) {
                int64_t i = ii + i_start, j = jj + j_start;
                out[i*w + j] =
                    in[(i-1)*W+j] + in[(i+1)*W+j] +
                    in[i*W+(j-1)] + in[i*W+(j+1)] +
                    (-4.0) * in[i*W+j];
            }
        }
    }
}
```

## Backends

The primary backend generates C99 with SIMDE-based vector intrinsics and optional OpenMP parallelization. There is also an Apple Metal backend for GPU experiments on macOS.

## How the compiler is organized

The source pipeline is: parse → type inference and refinement checking → `Uniquify` → `ShapeNormalize` → `Fusion` → lower to CFG with concrete types and ranks → CFG optimization passes → code generation. The C path applies tiling, explicit SIMD lowering, and optional parallelization; the Metal path optimizes and parallelizes without the SIMD layer.

The main source files are `app/Main.hs` (CLI), `src/Language/Hydrangea/Frontend.hs` (pipeline orchestration), `src/Language/Hydrangea/CFGPipeline.hs` (pass ordering), and `src/Language/Hydrangea/CodegenC.hs` / `CodegenMSL.hs` (code generation).

## Repository layout

```text
app/        CLI entry point and backend orchestration
src/        Compiler frontend, CFG pipeline, and code generation
runtime/    C runtime support used by generated programs
examples/   Small Hydrangea programs
demo/       Larger demos, including voxel, PPM, and SDL examples
test/       HSpec test suite
docs/       Additional documentation
```

## Running tests

```bash
cabal test
cabal test --test-show-details=always
```

Tests use `hspec-discover`, so new spec modules under `test/Language/Hydrangea/` are picked up automatically.

## More detail

CLI flags, export options, benchmark settings, and extended examples are in [`docs/cli-reference.md`](docs/cli-reference.md). Small programs to try first are in [`examples/`](examples); larger graphical demos are in [`demo/`](demo).

## License

BSD-3-Clause. See [`LICENSE`](LICENSE).
