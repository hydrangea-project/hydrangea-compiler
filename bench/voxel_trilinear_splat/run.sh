#!/usr/bin/env bash
# Run trilinear voxel splatting benchmark with varying point counts.
# Compiles the benchmark once, then times only execution for each size.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
BENCH="$ROOT/bench/voxel_trilinear_splat"
cd "$ROOT"

PARALLEL="${VSPLAT_PARALLEL:-1}"
SIZES="${VSPLAT_SIZES:-100000 1000000 5000000}"
NX="${VSPLAT_NX:-64}"
NY="${VSPLAT_NY:-64}"
NZ="${VSPLAT_NZ:-64}"
KEEP_PERIOD="${VSPLAT_KEEP_PERIOD:-3}"
CC_BIN="${VSPLAT_CC:-${CC:-gcc-15}}"

echo "=== Building Hydrangea compiler ==="
cabal build hydrangea-compiler 2>&1 | grep -v "^Build profile" | grep -v "^In order" || true

echo "=== Compiling trilinear voxel splat benchmark to native binary ==="
PARALLEL_FLAG=""
if [ "$PARALLEL" != "1" ]; then
  PARALLEL_FLAG="--no-parallel"
fi

VSPLAT_POINTS=1024 VSPLAT_NX="$NX" VSPLAT_NY="$NY" VSPLAT_NZ="$NZ" VSPLAT_KEEP_PERIOD="$KEEP_PERIOD" \
  cabal run hydrangea-compiler -- --main --prune-dead-procs --cc="$CC_BIN" \
  --no-solver-check $PARALLEL_FLAG --compile-only --keep-c "$BENCH/voxel_trilinear_splat.hyd" > /dev/null

echo "=== Trilinear Voxel Splat Benchmark (parallel=$PARALLEL, grid=${NX}x${NY}x${NZ}, keep_period=$KEEP_PERIOD, cc=$CC_BIN) ==="
for n in $SIZES; do
  echo -n "points=$n contributions=$((n * 8)) grid=${NX}x${NY}x${NZ} ... "
  export VSPLAT_POINTS="$n"
  export VSPLAT_NX="$NX"
  export VSPLAT_NY="$NY"
  export VSPLAT_NZ="$NZ"
  export VSPLAT_KEEP_PERIOD="$KEEP_PERIOD"
  { time ./hydrangea_out > /dev/null; } 2>&1 | grep real | awk '{print $2}'
done
