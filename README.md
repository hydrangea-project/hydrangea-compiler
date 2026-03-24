# Hydrangea

A research programming language with a functional core, array operations, and automatic parallelization.

## Overview

Hydrangea is a functional programming language with:
- ML-style syntax and type inference
- Array operations with automatic fusion
- C code generation with SIMD vectorization
- Automatic parallelization support

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
│   └── ...
├── app/                      # Executable entry point
├── test/                     # HSpec test suite
├── examples/                 # Sample Hydrangea programs (*.hyd)
├── runtime/                  # C runtime support
└── third_party/              # External dependencies (SIMDE)
```

## Building

### Prerequisites

- GHC 9.4+ and Cabal
- C compiler with OpenMP support (for parallel execution)

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

## Command-Line Usage

```bash
hydrangea-compiler [flags] [file]
```

If no file is provided, reads from stdin.

### Flags

| Flag | Description |
|------|-------------|
| `--interp` | Interpret the program (default mode without flags compiles and runs) |
| `--emit-c` | Print generated C code to stdout |
| `--output-c=<file>` | Write generated C code to a file |
| `--output-h=<file>` | Write the generated export header to a file (requires `--export-kernel`) |
| `--export-kernel=<name>` | Export a named zero-argument top-level kernel with a stable C ABI |
| `--main` | Keep only the declarations reachable from the final top-level `main` binding (now the default; retained for compatibility) |
| `--all-top-level-procs` | Keep the old behavior and preserve every top-level declaration |
| `--print-fused` | Print fused AST after array fusion optimization |
| `--print-cfg` | Print optimized control flow graph |
| `--print-cfg-raw` | Print unoptimized control flow graph |
| `--no-parallel` | Disable automatic parallelization |
| `--no-solver-check` | Skip refinement-solver discharge after type inference |
| `--keep-c` | Keep intermediate C files after compilation |
| `--compile-only` | Compile to binary but do not execute |

### Examples

```bash
# Interpret a program
cabal run hydrangea-compiler -- --interp examples/simple.hyd

# Generate and display C code
cabal run hydrangea-compiler -- --emit-c examples/mat_mul.hyd

# Export a kernel plus generated header for C/C++ embedding
cabal run hydrangea-compiler -- \
  --output-c=/tmp/scene.c \
  --output-h=/tmp/scene.h \
  --export-kernel=voxel_scene \
  demo/voxel_ppm/voxel_scene.hyd

# Compile and run from the `main` entrypoint (default behavior)
cabal run hydrangea-compiler -- examples/mat_mul.hyd

# Keep the older behavior and preserve all top-level procedures
cabal run hydrangea-compiler -- --all-top-level-procs examples/mat_mul.hyd

# Print the optimized CFG
cabal run hydrangea-compiler -- --print-cfg examples/dot.hyd

# Compile without parallelization
cabal run hydrangea-compiler -- --no-parallel examples/mat_mul.hyd

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
10. **Code Generation** - C code with SIMD intrinsics

## License

BSD-3-Clause (see LICENSE file)
