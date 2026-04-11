#!/usr/bin/env bash
# Run N-body benchmark with varying N.
# Compiles the benchmark once, then times only the execution for each input size
# to avoid measuring cabal/C compilation overhead.
set -e
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
BENCH="$ROOT/bench/nbody"
cd "$ROOT"

PARALLEL="${NBODY_PARALLEL:-0}"
BENCH_KERNEL="${NBODY_BENCH_KERNEL:-benchmark_kernel}"
BENCH_WARMUP="${NBODY_BENCH_WARMUP:-3}"
BENCH_ITERS="${NBODY_BENCH_ITERS:-10}"
# Size selection:
# - NBODY_SIZES: space-separated sweep list (highest priority)
# - NBODY_N: single-size convenience override
# - default: built-in sweep
if [ -n "${NBODY_SIZES:-}" ]; then
  SIZES="$NBODY_SIZES"
elif [ -n "${NBODY_N:-}" ]; then
  SIZES="$NBODY_N"
else
  SIZES="256 1024 4096"
fi
CC_BIN="${NBODY_CC:-${CC:-gcc-15}}"
COMPILE_N="${NBODY_N:-${SIZES%% *}}"

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
NBODY_N="$COMPILE_N" cabal run hydrangea-compiler -- \
  --all-top-level-procs \
  --benchmark="$BENCH_KERNEL" \
  --bench-warmup="$BENCH_WARMUP" \
  --bench-iters="$BENCH_ITERS" \
  --prune-dead-procs \
  --cc="$CC_BIN" \
  $PARALLEL_FLAG \
  --compile-only \
  --keep-c \
  "$BENCH/nbody.hyd" > /dev/null

echo "=== N-body Benchmark (parallel=$PARALLEL) ==="
for n in $SIZES; do
  echo -n "N=$n ... "
  python3 "$BENCH/gen_input.py" "$n" 2>/dev/null
  export NBODY_N=$n
  ./hydrangea_out 2>&1 1>/dev/null | grep "benchmark\\[" | tail -n 1
done
