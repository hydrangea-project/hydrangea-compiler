#!/usr/bin/env bash
# Run matmul benchmark: generate inputs, compile via hydrangea, compile C, run and measure
set -euo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
M=${MAT_M:-1024}
N=${MAT_N:-1024}
K=${MAT_K:-1024}
CC_BIN=${MAT_CC:-${CC:-gcc-15}}
PARALLEL=${MAT_PARALLEL:-1}

echo "Generating matrices: M=$M K=$K N=$N"
gcc -O2 -o "$HERE/gen_mats" "$HERE/gen_mats.c"
"$HERE/gen_mats" "$M" "$K" "$HERE/matA.csv"
"$HERE/gen_mats" "$K" "$N" "$HERE/matB.csv"

export MAT_M=$M
export MAT_N=$N
export MAT_K=$K

echo "Compiling Hydrangea program to C and native binary"
PARALLEL_FLAG=""
if [ "$PARALLEL" != "1" ]; then
  PARALLEL_FLAG="--no-parallel"
fi
cabal run hydrangea-compiler -- --main --prune-dead-procs --keep-c $PARALLEL_FLAG --emit-c bench/matmul/mat_mul_bench.hyd > "$HERE/out.c"

echo "Compiling emitted C with $CC_BIN"
"$CC_BIN" -O3 -march=native -fopenmp -std=c99 -I runtime -I third_party/simde -o "$HERE/hyd_out" "$HERE/out.c" runtime/hyd_write_csv.c || {
  echo "C compile failed, printing emitted C to out.c"; exit 1
}

echo "Running benchmark (OMP_NUM_THREADS=${OMP_NUM_THREADS:-1})"
export OMP_NUM_THREADS=${OMP_NUM_THREADS:-1}
time "$HERE/hyd_out"

echo "Output written to bench/matmul/out.csv"
