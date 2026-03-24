#!/usr/bin/env bash
# Run weighted histogram benchmark with varying input sizes.
# Compiles the benchmark once, then times only execution for each size.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
BENCH="$ROOT/bench/weighted_histogram"
cd "$ROOT"

PARALLEL="${WH_PARALLEL:-1}"
SIZES="${WH_SIZES:-100000 1000000 5000000}"
BINS="${WH_BINS:-256}"
CC_BIN="${WH_CC:-${CC:-gcc-15}}"

echo "=== Building Hydrangea compiler ==="
cabal build hydrangea-compiler 2>&1 | grep -v "^Build profile" | grep -v "^In order" || true

echo "=== Compiling weighted histogram benchmark to native binary ==="
PARALLEL_FLAG=""
if [ "$PARALLEL" != "1" ]; then
  PARALLEL_FLAG="--no-parallel"
fi

WH_N=1024 WH_BINS="$BINS" \
  cabal run hydrangea-compiler -- --main --prune-dead-procs --cc="$CC_BIN" \
  $PARALLEL_FLAG --compile-only --keep-c "$BENCH/weighted_histogram.hyd" > /dev/null

echo "=== Weighted Histogram Benchmark (parallel=$PARALLEL, bins=$BINS, cc=$CC_BIN) ==="
for n in $SIZES; do
  echo -n "N=$n ... "
  export WH_N="$n"
  export WH_BINS="$BINS"
  { time ./hydrangea_out > /dev/null; } 2>&1 | grep real | awk '{print $2}'
done
