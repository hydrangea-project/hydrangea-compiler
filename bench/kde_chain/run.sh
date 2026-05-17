#!/usr/bin/env bash
# Run chained KDE benchmark with varying input sizes.
# Compiles the benchmark once, then times only execution for each size.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
BENCH="$ROOT/bench/kde_chain"
cd "$ROOT"

PARALLEL="${KDECHAIN_PARALLEL:-1}"
SIZES="${KDECHAIN_SIZES:-100000 1000000 5000000}"
BINS="${KDECHAIN_BINS:-1024}"
KEEP_PERIOD="${KDECHAIN_KEEP_PERIOD:-5}"
CC_BIN="${KDECHAIN_CC:-${CC:-gcc-15}}"

echo "=== Building Hydrangea compiler ==="
cabal build hydrangea-compiler 2>&1 | grep -v "^Build profile" | grep -v "^In order" || true

echo "=== Compiling chained KDE benchmark to native binary ==="
PARALLEL_FLAG=""
if [ "$PARALLEL" != "1" ]; then
  PARALLEL_FLAG="--no-parallel"
fi

KDECHAIN_N=1024 KDECHAIN_BINS="$BINS" KDECHAIN_KEEP_PERIOD="$KEEP_PERIOD" \
  cabal run hydrangea-compiler -- --main --prune-dead-procs --cc="$CC_BIN" \
  --no-solver-check $PARALLEL_FLAG --compile-only --keep-c "$BENCH/kde_chain.hyd" > /dev/null

echo "=== Chained KDE Benchmark (parallel=$PARALLEL, bins=$BINS, keep_period=$KEEP_PERIOD, cc=$CC_BIN) ==="
for n in $SIZES; do
  echo -n "N=$n contributions=$((n * 9)) ... "
  export KDECHAIN_N="$n"
  export KDECHAIN_BINS="$BINS"
  export KDECHAIN_KEEP_PERIOD="$KEEP_PERIOD"
  { time ./hydrangea_out > /dev/null; } 2>&1 | grep real | awk '{print $2}'
done
