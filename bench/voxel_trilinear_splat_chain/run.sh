#!/usr/bin/env bash
# Run chained trilinear voxel splat benchmark with varying point counts.
# Compiles the benchmark once, then times only execution for each size.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
BENCH="$ROOT/bench/voxel_trilinear_splat_chain"
cd "$ROOT"

PARALLEL="${VSCHAIN_PARALLEL:-1}"
SIZES="${VSCHAIN_SIZES:-100000 500000 2000000}"
NX="${VSCHAIN_NX:-64}"
NY="${VSCHAIN_NY:-64}"
NZ="${VSCHAIN_NZ:-64}"
KEEP_PERIOD="${VSCHAIN_KEEP_PERIOD:-3}"
CC_BIN="${VSCHAIN_CC:-${CC:-gcc-15}}"

echo "=== Building Hydrangea compiler ==="
cabal build hydrangea-compiler 2>&1 | grep -v "^Build profile" | grep -v "^In order" || true

echo "=== Compiling chained trilinear voxel splat benchmark to native binary ==="
PARALLEL_FLAG=""
if [ "$PARALLEL" != "1" ]; then
  PARALLEL_FLAG="--no-parallel"
fi

VSCHAIN_POINTS=1024 VSCHAIN_NX="$NX" VSCHAIN_NY="$NY" VSCHAIN_NZ="$NZ" VSCHAIN_KEEP_PERIOD="$KEEP_PERIOD" \
  cabal run hydrangea-compiler -- --main --prune-dead-procs --cc="$CC_BIN" \
  --no-solver-check $PARALLEL_FLAG --compile-only --keep-c "$BENCH/voxel_trilinear_splat_chain.hyd" > /dev/null

echo "=== Chained Trilinear Voxel Splat Benchmark (parallel=$PARALLEL, grid=${NX}x${NY}x${NZ}, keep_period=$KEEP_PERIOD, cc=$CC_BIN) ==="
for n in $SIZES; do
  echo -n "points=$n contributions=$((n * 24)) grid=${NX}x${NY}x${NZ} ... "
  export VSCHAIN_POINTS="$n"
  export VSCHAIN_NX="$NX"
  export VSCHAIN_NY="$NY"
  export VSCHAIN_NZ="$NZ"
  export VSCHAIN_KEEP_PERIOD="$KEEP_PERIOD"
  { time ./hydrangea_out > /dev/null; } 2>&1 | grep real | awk '{print $2}'
done
