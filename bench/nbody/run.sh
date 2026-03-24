#!/usr/bin/env bash
# Run N-body benchmark with varying N.
# Compiles the benchmark once, then times only the execution for each input size
# to avoid measuring cabal/C compilation overhead.
set -e
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
BENCH="$ROOT/bench/nbody"
cd "$ROOT"

PARALLEL="${NBODY_PARALLEL:-0}"
SIZES="${NBODY_SIZES:-256 1024 4096}"
CC_BIN="${NBODY_CC:-${CC:-gcc-15}}"

# Build the Hydrangea compiler if not already up-to-date.
echo "=== Building Hydrangea compiler ==="
cabal build hydrangea-compiler 2>&1 | grep -v "^Build profile" | grep -v "^In order" || true

# Compile the benchmark to a native binary once (reused for all sizes).
echo "=== Compiling N-body benchmark to C ==="
PARALLEL_FLAG=""
if [ "$PARALLEL" = "1" ]; then
  PARALLEL_FLAG=""  # no --no-parallel → OpenMP enabled
else
  PARALLEL_FLAG="--no-parallel"
fi
NBODY_N=256 cabal run hydrangea-compiler -- --main --prune-dead-procs --cc="$CC_BIN" $PARALLEL_FLAG --compile-only --keep-c "$BENCH/nbody.hyd" > /dev/null

echo "=== N-body Benchmark (parallel=$PARALLEL) ==="
for n in $SIZES; do
  echo -n "N=$n ... "
  python3 "$BENCH/gen_input.py" "$n" 2>/dev/null
  export NBODY_N=$n
  { time ./hydrangea_out > /dev/null; } 2>&1 | grep real | awk '{print $2}'
done
