# Hydrangea

Hydrangea is an experimental compiler for a functional array language with higher-order combinators and rank polymorphism. The main target is optimized C with fusion, SIMD vectorization, and OpenMP parallelization. There is also an Apple Metal backend for macOS, but the C path is the one to reach for first.

## What it is good at

- Writing array programs in a compact, functional style
- Fusing chains of array combinators such as `map`, `zipWith`, `generate`, `gather`, and `scatter`
- Lowering into an imperative CFG that can be optimized, vectorized, and parallelized
- Emitting C that can be inspected, compiled, benchmarked, or embedded

## Quick start

```bash
# Build the compiler
cabal build

# Interpret a small program
cabal run hydrangea-compiler -- --interp examples/simple.hyd

# Generate C for a larger example
cabal run hydrangea-compiler -- --emit-c examples/mat_mul.hyd

# Compile and run with the default C backend
cabal run hydrangea-compiler -- examples/mat_mul.hyd
```

If you are on macOS and want parallel C code, use `nix develop` or point `--cc` / `CC` at a GCC with OpenMP support. The system clang usually is not enough for that path.

## Backends

- **C backend**: the primary backend. It generates C99, uses SIMDE for vector code, and can add OpenMP parallelization when the toolchain supports it.
- **Apple Metal backend**: useful for GPU-oriented experiments on macOS. It is narrower in scope and still evolving.

## How the compiler is organized

Hydrangea starts with a source AST, runs type inference plus refinement checks, then preprocesses declarations through `Uniquify`, `ShapeNormalize`, and `Fusion`. After that it lowers the program into a CFG IR, runs optimization, tiling, SIMD vectorization, and OpenMP parallelization, and finally emits either C or Metal code.

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
demo/       Larger demos, including voxel and SDL examples
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
