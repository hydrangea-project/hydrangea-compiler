#!/usr/bin/env bash
# Run graph message-passing benchmark with varying node counts.
# Compiles the benchmark once, then times only execution for each size.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
BENCH="$ROOT/bench/graph_messages"
cd "$ROOT"

PARALLEL="${GRAPH_PARALLEL:-1}"
SIZES="${GRAPH_SIZES:-10000 100000 500000}"
DEGREE="${GRAPH_DEGREE:-16}"
CC_BIN="${GRAPH_CC:-${CC:-gcc-15}}"

echo "=== Building Hydrangea compiler ==="
cabal build hydrangea-compiler 2>&1 | grep -v "^Build profile" | grep -v "^In order" || true

echo "=== Compiling graph message benchmark to native binary ==="
PARALLEL_FLAG=""
if [ "$PARALLEL" != "1" ]; then
  PARALLEL_FLAG="--no-parallel"
fi

GRAPH_NODES=128 GRAPH_DEGREE="$DEGREE" \
  cabal run hydrangea-compiler -- --main --prune-dead-procs --cc="$CC_BIN" \
  $PARALLEL_FLAG --compile-only --keep-c "$BENCH/graph_messages.hyd" > /dev/null

echo "=== Graph Message Benchmark (parallel=$PARALLEL, degree=$DEGREE, cc=$CC_BIN) ==="
for n in $SIZES; do
  echo -n "nodes=$n degree=$DEGREE edges=$((n * DEGREE)) ... "
  export GRAPH_NODES="$n"
  export GRAPH_DEGREE="$DEGREE"
  { time ./hydrangea_out > /dev/null; } 2>&1 | grep real | awk '{print $2}'
done
