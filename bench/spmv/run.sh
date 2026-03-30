#!/usr/bin/env bash
# Run SpMV benchmark with varying matrix sizes.
# Compiles the benchmark once, then times only the execution for each input size
# to avoid measuring cabal/C compilation overhead.
set -e
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
BENCH="$ROOT/bench/spmv"
cd "$ROOT"

PARALLEL="${SPMV_PARALLEL:-0}"
# (nrows, ncols, nnz) triplets
CONFIGS="${SPMV_CONFIGS:-1000 1000 5000 10000 10000 50000 50000 50000 250000}"

# Build the Hydrangea compiler if not already up-to-date.
echo "=== Building Hydrangea compiler ==="
cabal build hydrangea-compiler 2>&1 | grep -v "^Build profile" | grep -v "^In order" || true

# Compile the benchmark to a native binary once (reused for all sizes).
echo "=== Compiling SpMV benchmark to C ==="
PARALLEL_FLAG=""
if [ "$PARALLEL" = "1" ]; then
  PARALLEL_FLAG=""
else
  PARALLEL_FLAG="--no-parallel"
fi
# Generate a small input for compilation; the binary reads env vars at runtime.
actual_nnz=$(python3 "$BENCH/gen_input.py" 100 100 500)
SPMV_NROWS=100 SPMV_NCOLS=100 SPMV_NNZ=$actual_nnz cabal run hydrangea-compiler -- $PARALLEL_FLAG --compile-only --keep-c "$BENCH/spmv.hyd" > /dev/null

echo "=== SpMV Benchmark (parallel=$PARALLEL) ==="
set -- $CONFIGS
while [ $# -ge 3 ]; do
  nrows=$1; ncols=$2; nnz=$3; shift 3
  echo -n "nrows=$nrows ncols=$ncols nnz~$nnz ... "
  actual_nnz=$(python3 "$BENCH/gen_input.py" "$nrows" "$ncols" "$nnz")
  export SPMV_NROWS=$nrows SPMV_NCOLS=$ncols SPMV_NNZ=$actual_nnz
  { time ./hydrangea_out > /dev/null; } 2>&1 | grep real | awk '{print $2}'
done
