# Hydrangea CLI reference

This document keeps the operational detail that does not need to live on the project front page: build commands, backend notes, compiler flags, and extended examples.

## Building

### Prerequisites

- GHC 9.4+ and Cabal
- A C compiler with OpenMP support if you want parallel generated C
- On macOS, Xcode command-line tools for the Metal backend (`xcrun`, `clang`, Metal toolchain)

### Cabal commands

```bash
# Build the compiler
cabal build

# Run the compiler
cabal run hydrangea-compiler -- [flags] [file.hyd]

# Run tests
cabal test

# Clean build artifacts
cabal clean
```

### Nix

```bash
# Enter a development shell with GHC, Cabal, Alex, Happy, and GCC/OpenMP
nix develop

# Build the wrapped compiler package
nix build .#hydrangea-compiler

# Run the compiler with GCC selected for generated C/OpenMP builds
nix run .#hydrangea-compiler -- examples/dot.hyd
```

Inside `nix develop`, the shell exports `CC` to Nix-provided GCC so Hydrangea can successfully use `-fopenmp` when running generated parallel C code. This matters most on macOS, where the system clang usually does not provide working OpenMP support for this workflow.

### Dev container

The repository includes a `.devcontainer/` setup for editors such as VS Code. It provides GHC 9.10.1, Cabal, GCC, OpenMP support through GCC, and `z3`.

## Backends

Hydrangea currently has two code generation targets.

### C backend

The C backend is the primary path. It produces C99 code with:

- SIMD vectorization via SIMDE intrinsics, configurable with `--simd-width=<n>`
- OpenMP parallelization for multi-core CPU execution
- Exportable kernels with a stable C ABI via `--export-kernel=<name>`
- Benchmark scaffolding via `--benchmark=<name>`

### Apple Metal backend

The Metal backend generates MSL (Metal Shading Language) compute kernels and drives the Metal toolchain with `xcrun metal` and `xcrun metallib`.

Current capabilities include:

- Multi-kernel dispatch when a program lowers to several GPU-eligible procedures
- Atomic scatter-add for `int` and `float`
- 1-D parallel and vector-map kernels
- Scalar and 1-D array inputs and outputs
- Inner serial loops and conditionals inside the kernel body
- Export mode via `--export-metal-kernel=<name>` for embedding in larger applications

Current limitations:

- No dynamic allocation inside kernels
- No `RFlatToNd` or `RNdToFlat` inside kernel bodies
- No 2-D or higher-dimensional index spaces in the current implementation
- CPU-side OpenMP parallelization is skipped when using the Metal backend

## Command-line usage

```bash
hydrangea-compiler [flags] [file]
```

If no file is provided, Hydrangea reads from stdin.

### Inspection and output flags

| Flag | Description |
|------|-------------|
| `--interp` | Interpret the program instead of compiling generated C and running it |
| `--print-fused` | Print the fused AST after array-fusion optimization |
| `--print-cfg` | Print the optimized control-flow graph |
| `--print-cfg-raw` | Print the unoptimized control-flow graph |
| `--emit-c` | Print generated C code to stdout |
| `--output-c=<file>` | Write generated C code to a file |
| `--output-h=<file>` | Write the generated export header to a file; requires `--export-kernel` |
| `--compile-only` | Compile generated C to a binary, but do not execute it |
| `--keep-c` | Keep intermediate `hydrangea_out.c` and `hydrangea_out` artifacts in the current directory |
| `--keep-metal` | Keep intermediate Metal artifacts in the current directory as `hydrangea_out.metal` and `hydrangea_out.m` |

### Program selection and optimization flags

| Flag | Description |
|------|-------------|
| `--main` | Keep only declarations reachable from the final top-level `main` binding; this is the default behavior and the flag remains for compatibility |
| `--all-top-level-procs` | Preserve every top-level declaration instead of pruning to `main` |
| `--no-parallel` | Disable OpenMP-oriented parallelization and compile generated C without `-fopenmp` |
| `--tiling` | Opt in to loop-tiling in the CFG optimization pipeline (default: off) |
| `--explicit-vectorization` | Opt in to explicit SIMD lowering (`RVec*` IR / intrinsic-based C emission) on the C backend (default: off) |
| `--no-solver-check` | Skip refinement-solver discharge after type inference |
| `--simd-width=<n>` | Select the SIMD vector width used by the C backend (default: `4`) |
| `--prune-dead-procs` | Prune unused generated procedures during code generation |
| `--kernel=<name>` | Assert that the named top-level kernel fully fused away references to other original top-level bindings; fail if fusion leaves dependencies behind |
| `--metal-kernel=<name>` | When using `--metal`, choose which lowered procedure should be emitted as the Metal kernel; otherwise the backend selects a candidate automatically |

### Export, backend, and benchmarking flags

| Flag | Description |
|------|-------------|
| `--export-kernel=<name>` | Export a named zero-argument top-level kernel with a stable C ABI; currently supported only when emitting C with `--emit-c` or `--output-c` |
| `--cc=<compiler>` | Use a specific C compiler for generated code; otherwise the CLI uses `$CC` or falls back to `cc` |
| `--benchmark=<name>` | Generate benchmark scaffolding for the named zero-argument top-level procedure and report timing statistics |
| `--bench-warmup=<n>` | Use `n` warmup iterations for `--benchmark` (default: `3`) |
| `--bench-iters=<n>` | Use `n` timed measurement iterations for `--benchmark` (default: `10`) |
| `--metal` | Use the Apple Metal backend instead of the default C backend |

Without `--interp`, the default mode type-checks, compiles, and runs the selected program. By default this uses the C backend; `--metal` switches to the Metal backend.

## Examples

```bash
# Interpret a program
cabal run hydrangea-compiler -- --interp examples/simple.hyd

# Generate and display C code
cabal run hydrangea-compiler -- --emit-c examples/mat_mul.hyd

# Generate C with a narrower SIMD width
cabal run hydrangea-compiler -- --emit-c --simd-width=2 examples/mat_mul.hyd

# Opt in to tiling and explicit vector lowering
cabal run hydrangea-compiler -- \
  --emit-c \
  --tiling \
  --explicit-vectorization \
  examples/mat_mul.hyd

# Validate that a fused kernel has no remaining top-level dependencies
cabal run hydrangea-compiler -- --kernel=dot examples/dot.hyd

# Export a kernel plus generated header for C/C++ embedding
cabal run hydrangea-compiler -- \
  --output-c=/tmp/scene.c \
  --output-h=/tmp/scene.h \
  --export-kernel=voxel_scene \
  demo/voxel_ppm/voxel_scene.hyd

# Generate benchmark-enabled C for a zero-argument kernel
cabal run hydrangea-compiler -- \
  --emit-c \
  --benchmark=voxel_scene \
  --bench-warmup=5 \
  --bench-iters=20 \
  demo/voxel_ppm/voxel_scene.hyd

# Compile and run from the `main` entrypoint
cabal run hydrangea-compiler -- examples/mat_mul.hyd

# Preserve all top-level procedures
cabal run hydrangea-compiler -- --all-top-level-procs examples/mat_mul.hyd

# Print the optimized CFG
cabal run hydrangea-compiler -- --print-cfg examples/dot.hyd

# Compile without parallelization
cabal run hydrangea-compiler -- --no-parallel examples/mat_mul.hyd

# Force a specific C compiler
cabal run hydrangea-compiler -- --cc=gcc-15 examples/mat_mul.hyd

# Run the Metal backend on macOS
cabal run hydrangea-compiler -- --metal examples/dot.hyd

# Select a specific Metal kernel and keep the generated sources
cabal run hydrangea-compiler -- \
  --metal \
  --metal-kernel=dot \
  --keep-metal \
  --compile-only \
  examples/dot.hyd

# Build the voxel PPM demo
demo/voxel_ppm/build.sh

# Render the same demo at a larger output size
demo/voxel_ppm/build.sh /tmp/voxel_demo.ppm 128

# Build and run the SDL ray tracing demo
demo/ray_sdl/build.sh
demo/ray_sdl/out/ray_sdl

# Build and run the scan-family SDL demo
demo/scan_sdl/build.sh
demo/scan_sdl/out/scan_sdl

# Build and run the Metal-backed SDL ray tracing demo on macOS
demo/ray_sdl/metal_build.sh
demo/ray_sdl/out/ray_sdl_metal
```

## Running tests

```bash
# Run all tests
cabal test

# Run with verbose output
cabal test --test-show-details=always

# Run a specific test module
cabal test -- --pattern "LexerSpec"

# Run a specific test by name
cabal test -- --pattern "lexer tokenizes identifiers"
```
