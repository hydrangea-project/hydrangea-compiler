# Hydrangea

Hydrangea is an experimental compiler for *functional array programs* with *higher-order combinators and rank polymorphism*.

Hydrangea uses aggressive *array fusion* to eliminate intermediary arrays, so functional array programs that work on immutable arrays are lowered into efficient loop nests. From there, Hydrangea uses polyhedral optimization to produce highly efficient C code with OpenMP parallelization.

In this repository, there are a set of benchmarks and graphical demos that show off what the compiler can do.

<a href="docs/images/ray_still_life.png">
  <img src="docs/images/ray_thumb.jpeg" alt="Ray Example" width="400"/>
</a>

## What it is good at

- Writing array programs in a compact, functional style
- Fusing chains of array combinators such as `map`, `zipWith`, `generate`, `gather`, and `scatter`
- Lowering into an imperative CFG that can be optimized, vectorized, and parallelized
- Emitting C that can be inspected, compiled, benchmarked, or embedded

## Example compiler output

For stencil-shaped kernels, Hydrangea can reschedule loop nests into a form that is friendlier to locality and parallel execution.

Given a 2D Laplacian stencil like this:

```hydrangea
let laplacian img =
  stencil clamp
    (fn acc =>
      acc (-1) 0 + acc 1 0 + acc 0 (-1) + acc 0 1 - (4 * acc 0 0))
    img
```

The compiler outpus C like this (simplified from generated output):

```c
/* boundary handling omitted here; this is the tiled interior */
#pragma omp parallel for collapse(2)
for (int64_t i_tile = 1; i_tile < n - 1; i_tile += 32) {
    for (int64_t j_tile = 1; j_tile < m - 1; j_tile += 32) {
        int64_t i_stop = (i_tile + 32 < n - 1) ? (i_tile + 32) : (n - 1);
        int64_t j_stop = (j_tile + 32 < m - 1) ? (j_tile + 32) : (m - 1);

        for (int64_t i = i_tile; i < i_stop; i++) {
            for (int64_t j = j_tile; j < j_stop; j++) {
                int64_t ij = i * m + j;
                int64_t up = (i - 1) * m + j;
                int64_t down = (i + 1) * m + j;
                int64_t left = i * m + (j - 1);
                int64_t right = i * m + (j + 1);

                out[ij] = in[up] + in[down] + in[left] + in[right] - 4 * in[ij];
            }
        }
    }
}
```

That is a snapshot of what Hydrangea is for: start from a compact array combinator like `stencil`, then lower it into a loop nest that can be tiled, reordered, parallelized, and inspected.


## Backends

- **C backend**: the primary backend. It generates C99, uses SIMDE for vector code, and can add OpenMP parallelization when the toolchain supports it.
- **Apple Metal backend**: useful for GPU-oriented experiments on macOS. It is narrower in scope and still evolving.

## How the compiler is organized

Hydrangea starts with a source AST, runs type inference plus refinement checks, then preprocesses declarations through `Uniquify`, `ShapeNormalize`, and `Fusion`. After that it lowers the program into a CFG IR, applies various optimizations, and finally emits either C or Metal code.

The main entry points are:

- `app/Main.hs` for the CLI
- `src/Language/Hydrangea/Frontend.hs` for the high-level pipeline
- `src/Language/Hydrangea/CFGPipeline.hs` for CFG passes
- `src/Language/Hydrangea/CodegenC.hs` and `src/Language/Hydrangea/CodegenMSL.hs` for code generation

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

- CLI flags, export options, benchmark settings, and extended examples: [`docs/cli-reference.md`](docs/cli-reference.md)
- Small source programs to try first: [`examples/`](examples)
- Larger demos: [`demo/`](demo)

## License

BSD-3-Clause. See [`LICENSE`](LICENSE).
