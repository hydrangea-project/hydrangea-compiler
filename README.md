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

## Example compiler output

Given a dot product procedure like this (assuming arrays `a` and `b`):

```
reduce (+) 0 (zipwith (*) a b) 
```

The compiler will produce C code like this:

```c
hyd_array_t* t6 = a();
hyd_tuple_t t7 = t6->shape;
hyd_tuple_t out_shp8 = hyd_shape_init(t7);
int64_t red_dim9 = hyd_shape_last(t7);
double acc11 = 0.0;
int64_t k12__vec_trips = (red_dim9 / 4LL);
hyd_float64x4_t acc11__vec = hyd_vec_set1_f64x4(0LL);
hyd_array_t* t17 = b();

#pragma omp parallel for
for (int64_t k12__vec_i = 0; k12__vec_i < k12__vec_trips; k12__vec_i++) {
    int64_t k12__vec_base = (k12__vec_i * 4LL);
    hyd_float64x4_t val14__vec = hyd_vec_loadu_f64x4(((double*)(void*)t6->data) + k12__vec_base);
    hyd_float64x4_t val16__vec = hyd_vec_loadu_f64x4(((double*)(void*)t17->data) + k12__vec_base);
    hyd_float64x4_t t18__vec = hyd_vec_mul_f64x4(val14__vec, val16__vec);
    acc11__vec = hyd_vec_add_f64x4(acc11__vec, t18__vec);
}

acc11 = hyd_vec_reduce_add_f64x4(acc11__vec);
int64_t k12__tail_start = (k12__vec_trips * 4LL);
int64_t k12__tail_len = (red_dim9 - k12__tail_start);
for (int64_t k12__tail_i = 0; k12__tail_i < k12__tail_len; k12__tail_i++) {
    int64_t k12 = (k12__tail_start + k12__tail_i);
    double val14 = (((double*)(void*)t6->data)[k12]);
    double val16 = (((double*)(void*)t17->data)[k12]);
    double t18 = (val14 * val16);
    acc11 = (acc11 + t18);
}
```

The `hyd_vec_add_f64x4` and similar operations are SIMD intrinsics, implemented with the portable [SIMDE library](https://github.com/simd-everywhere/simde).


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
