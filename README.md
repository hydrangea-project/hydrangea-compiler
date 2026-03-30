# Hydrangea

Hydrangea is an experimental compiler for a functional, higher-order, rank-polymorphic array programming language. It aims to make array code feel concise and compositional while still compiling to optimized C with fusion, SIMD vectorization, and OpenMP parallelization, with an experimental Apple Metal backend now in progress.

## Overview

Hydrangea combines:
- Functional, higher-order array programming with rank polymorphism
- ML-style syntax with Hindley-Milner type inference
- Fusion of array combinators such as `map`, `zipWith`, `generate`, and `gather`
- C code generation with SIMD vectorization and OpenMP parallelization
- An experimental, work-in-progress Apple Metal backend for GPU-oriented execution on macOS

## Repository Structure

```
├── src/Language/Hydrangea/   # Compiler source code
│   ├── Lexer.x               # Alex lexer
│   ├── Parser.y              # Happy parser
│   ├── Syntax.hs             # AST definitions
│   ├── Infer.hs              # Type inference
│   ├── Fusion.hs             # Array fusion
│   ├── CFG*.hs               # Control flow graph and optimizations
│   ├── Vectorize.hs          # Vectorization pass
│   ├── Parallelize.hs        # Parallelization pass
│   ├── CodegenC.hs           # C code generation
│   ├── CodegenMSL.hs         # Experimental Apple Metal / MSL code generation
│   └── ...
├── app/                      # Executable entry point and backend drivers
├── test/                     # HSpec test suite
├── examples/                 # Sample Hydrangea programs (*.hyd)
├── runtime/                  # C runtime support
└── third_party/              # External dependencies (SIMDE)
```

## Building

### Prerequisites

- GHC 9.4+ and Cabal
- C compiler with OpenMP support (for parallel execution)
- On macOS, Xcode command-line tools are required for the experimental Metal backend (`xcrun`, `clang`, Metal toolchain)

### Cabal Commands

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

Inside `nix develop`, the shell exports `CC` to Nix-provided GCC so the compiler can successfully
invoke `-fopenmp` when running generated parallel C code, including on macOS where the system
clang usually does not provide OpenMP support.

### Dev Container

The repository also includes a `.devcontainer/` setup for editors such as VS Code. It provides
GHC 9.10.1 together with Cabal, GCC, OpenMP support via GCC, and `z3`.

## Backends

Hydrangea currently has two code generation targets:

- **C backend** — the primary and more mature path, generating C that can be compiled and run with SIMD and OpenMP support.
- **Apple Metal backend (experimental / WIP)** — generates a `.metal` compute kernel plus an Objective-C harness and drives the Metal toolchain via `xcrun`. This path is currently aimed at macOS / Apple Silicon workflows and has a narrower supported feature set than the C backend.

The current Metal backend implementation is intentionally limited. In `CodegenMSL.hs`, the documented initial scope includes:

- 1-D parallel or map-style kernels
- Scalar and 1-D array inputs and outputs
- `Int64` and `Double`-based programs, with Metal codegen demoting `Double` to `float`
- Inner serial loops and conditionals inside the selected kernel body

It is still a work in progress and should be treated as experimental.

## Command-Line Usage

```bash
hydrangea-compiler [flags] [file]
```

If no file is provided, reads from stdin.

### Flags

#### Inspection and output

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

#### Program selection and optimization

| Flag | Description |
|------|-------------|
| `--main` | Keep only declarations reachable from the final top-level `main` binding; this is the default behavior and the flag is retained for compatibility |
| `--all-top-level-procs` | Preserve every top-level declaration instead of pruning to `main` |
| `--no-parallel` | Disable OpenMP-oriented parallelization and compile generated C without `-fopenmp` |
| `--no-solver-check` | Skip refinement-solver discharge after type inference |
| `--simd-width=<2|4>` | Select the SIMD vector width used by the C backend (default: `4`) |
| `--prune-dead-procs` | Prune unused generated procedures during code generation |
| `--kernel=<name>` | Assert that the named top-level kernel fully fused away references to other original top-level bindings; fails if fusion leaves dependencies behind |
| `--metal-kernel=<name>` | When using `--metal`, choose which lowered procedure should be emitted as the Metal kernel; otherwise the backend selects a candidate automatically |

#### Export, backend, and benchmarking

| Flag | Description |
|------|-------------|
| `--export-kernel=<name>` | Export a named zero-argument top-level kernel with a stable C ABI; currently supported only when emitting C with `--emit-c` or `--output-c` |
| `--cc=<compiler>` | Use a specific C compiler for generated code (otherwise the CLI uses `$CC` or falls back to `cc`) |
| `--benchmark=<name>` | Generate benchmark scaffolding for the named zero-argument top-level procedure and report timing statistics |
| `--bench-warmup=<n>` | Use `n` warmup iterations for `--benchmark` (default: `3`) |
| `--bench-iters=<n>` | Use `n` timed measurement iterations for `--benchmark` (default: `10`) |
| `--metal` | Use the experimental Apple Metal backend instead of the default C backend |

Without `--interp`, the default mode type-checks, compiles, and runs the selected program. By default this uses the C backend; `--metal` switches to the experimental Metal backend.

### Examples

```bash
# Interpret a program
cabal run hydrangea-compiler -- --interp examples/simple.hyd

# Generate and display C code
cabal run hydrangea-compiler -- --emit-c examples/mat_mul.hyd

# Generate C with a narrower SIMD width
cabal run hydrangea-compiler -- --emit-c --simd-width=2 examples/mat_mul.hyd

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

# Compile and run from the `main` entrypoint (default behavior)
cabal run hydrangea-compiler -- examples/mat_mul.hyd

# Keep the older behavior and preserve all top-level procedures
cabal run hydrangea-compiler -- --all-top-level-procs examples/mat_mul.hyd

# Print the optimized CFG
cabal run hydrangea-compiler -- --print-cfg examples/dot.hyd

# Compile without parallelization
cabal run hydrangea-compiler -- --no-parallel examples/mat_mul.hyd

# Force a specific C compiler for generated code
cabal run hydrangea-compiler -- --cc=gcc-15 examples/mat_mul.hyd

# Run the experimental Metal backend on macOS
cabal run hydrangea-compiler -- --metal examples/dot.hyd

# Select a specific Metal kernel and keep the generated .metal/.m sources
cabal run hydrangea-compiler -- \
  --metal \
  --metal-kernel=dot \
  --keep-metal \
  --compile-only \
  examples/dot.hyd

# Build the voxel PPM demo (defaults to a high-detail 1920x1080 render)
demo/voxel_ppm/build.sh

# Render the same demo at a larger output size
demo/voxel_ppm/build.sh /tmp/voxel_demo.ppm 128
```

## Running Tests

```bash
# Run all tests
cabal test

# Run with verbose output
cabal test --test-show-details=always

# Run specific test module
cabal test -- --pattern "LexerSpec"

# Run specific test by name
cabal test -- --pattern "lexer tokenizes identifiers"
```

## Example Programs

The `examples/` directory contains sample Hydrangea programs:

- `simple.hyd` - Basic arithmetic and functions
- `arrays.hyd` - Array operations
- `mat_mul.hyd` - Matrix multiplication
- `mat_mul_fused.hyd` - Fused matrix multiplication
- `mat_mul_parallel.hyd` - Parallel matrix multiplication
- `dot.hyd` - Dot product
- `complete.hyd` - Comprehensive example

## Compiler Pipeline

1. **Lexing/Parsing** - Source → AST
2. **Type Inference** - Hindley-Milner style inference
3. **Uniquification** - Make variable names unique
4. **Fusion** - Fuse array operations
5. **Lowering to CFG** - Convert to control flow graph
6. **Optimization** - CFG optimizations
7. **Analysis** - Loop analysis for vectorization
8. **Vectorization** - SIMD vectorization
9. **Parallelization** - OpenMP parallelization
10. **Code Generation** - Primarily C code with SIMD intrinsics, plus an experimental Metal/MSL backend on macOS

## License

BSD-3-Clause (see LICENSE file)
