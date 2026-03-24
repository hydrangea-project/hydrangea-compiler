#!/usr/bin/env bash
# Run guarded weighted histogram benchmark with varying input sizes.
# Compiles the benchmark once, then times only execution for each size.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
BENCH="$ROOT/bench/guarded_weighted_histogram"
cd "$ROOT"

PARALLEL="${GWH_PARALLEL:-1}"
SIZES="${GWH_SIZES:-100000 1000000 5000000}"
BINS="${GWH_BINS:-256}"
KEEP_PERIOD="${GWH_KEEP_PERIOD:-3}"
CC_BIN="${GWH_CC:-${CC:-gcc-15}}"

echo "=== Building Hydrangea compiler ==="
cabal build hydrangea-compiler 2>&1 | grep -v "^Build profile" | grep -v "^In order" || true

echo "=== Compiling guarded weighted histogram benchmark to native binary ==="
PARALLEL_FLAG=""
if [ "$PARALLEL" != "1" ]; then
  PARALLEL_FLAG="--no-parallel"
fi

GWH_N=1024 GWH_BINS="$BINS" GWH_KEEP_PERIOD="$KEEP_PERIOD" \
  cabal run hydrangea-compiler -- --main --prune-dead-procs --cc="$CC_BIN" \
  $PARALLEL_FLAG --compile-only --keep-c "$BENCH/guarded_weighted_histogram.hyd" > /dev/null

echo "=== Guarded Weighted Histogram Benchmark (parallel=$PARALLEL, bins=$BINS, keep_period=$KEEP_PERIOD, cc=$CC_BIN) ==="
for n in $SIZES; do
  echo -n "N=$n ... "
  export GWH_N="$n"
  export GWH_BINS="$BINS"
  export GWH_KEEP_PERIOD="$KEEP_PERIOD"
  { time ./hydrangea_out > /dev/null; } 2>&1 | grep real | awk '{print $2}'
done
