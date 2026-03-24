#!/usr/bin/env bash
# Run sparse voxel / point-cloud rasterization benchmark with varying point counts.
# Compiles the benchmark once, then times only execution for each size.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
BENCH="$ROOT/bench/voxel_rasterization"
cd "$ROOT"

PARALLEL="${VOX_PARALLEL:-1}"
SIZES="${VOX_SIZES:-100000 1000000 5000000}"
NX="${VOX_NX:-64}"
NY="${VOX_NY:-64}"
NZ="${VOX_NZ:-64}"
KEEP_PERIOD="${VOX_KEEP_PERIOD:-3}"
CC_BIN="${VOX_CC:-${CC:-gcc-15}}"

echo "=== Building Hydrangea compiler ==="
cabal build hydrangea-compiler 2>&1 | grep -v "^Build profile" | grep -v "^In order" || true

echo "=== Compiling voxel rasterization benchmark to native binary ==="
PARALLEL_FLAG=""
if [ "$PARALLEL" != "1" ]; then
  PARALLEL_FLAG="--no-parallel"
fi

VOX_POINTS=1024 VOX_NX="$NX" VOX_NY="$NY" VOX_NZ="$NZ" VOX_KEEP_PERIOD="$KEEP_PERIOD" \
  cabal run hydrangea-compiler -- --main --prune-dead-procs --cc="$CC_BIN" \
  $PARALLEL_FLAG --compile-only --keep-c "$BENCH/voxel_rasterization.hyd" > /dev/null

echo "=== Voxel Rasterization Benchmark (parallel=$PARALLEL, grid=${NX}x${NY}x${NZ}, keep_period=$KEEP_PERIOD, cc=$CC_BIN) ==="
for n in $SIZES; do
  echo -n "points=$n grid=${NX}x${NY}x${NZ} voxels=$((NX * NY * NZ)) ... "
  export VOX_POINTS="$n"
  export VOX_NX="$NX"
  export VOX_NY="$NY"
  export VOX_NZ="$NZ"
  export VOX_KEEP_PERIOD="$KEEP_PERIOD"
  { time ./hydrangea_out > /dev/null; } 2>&1 | grep real | awk '{print $2}'
done
