#!/usr/bin/env bash
# Run Mandelbrot benchmark with varying image sizes.
# Compiles the benchmark once, then times only the execution for each input size
# to avoid measuring cabal/C compilation overhead.
set -e
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
BENCH="$ROOT/bench/mandelbrot"
cd "$ROOT"

PARALLEL="${MAND_PARALLEL:-0}"
ITERS="${MAND_ITERS:-256}"
SIZES="${MAND_SIZES:-256 512 1024 2048}"

# Build the Hydrangea compiler if not already up-to-date.
echo "=== Building Hydrangea compiler ==="
cabal build hydrangea-compiler 2>&1 | grep -v "^Build profile" | grep -v "^In order" || true

# Compile the benchmark to a native binary once (reused for all sizes).
echo "=== Compiling Mandelbrot benchmark to C ==="
PARALLEL_FLAG=""
if [ "$PARALLEL" = "1" ]; then
  PARALLEL_FLAG=""
else
  PARALLEL_FLAG="--no-parallel"
fi
MAND_W=256 MAND_H=256 MAND_ITERS=$ITERS cabal run hydrangea-compiler -- $PARALLEL_FLAG --compile-only --keep-c "$BENCH/mandelbrot.hyd" > /dev/null

echo "=== Mandelbrot Benchmark (max_iters=$ITERS, parallel=$PARALLEL) ==="
for sz in $SIZES; do
  echo -n "${sz}x${sz} ... "
  export MAND_W=$sz MAND_H=$sz MAND_ITERS=$ITERS
  { time ./hydrangea_out > /dev/null; } 2>&1 | grep real | awk '{print $2}'
done
