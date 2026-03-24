#!/usr/bin/env bash
# Run COO/CSR build benchmark with varying matrix sizes.
# Compiles the benchmark once, then times only execution for each configuration.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
BENCH="$ROOT/bench/coo_csr_build"
cd "$ROOT"

PARALLEL="${COO_PARALLEL:-1}"
DUP_PERIOD="${COO_DUP_PERIOD:-4}"
CONFIGS="${COO_CONFIGS:-1024 1024 100000 4096 4096 500000 8192 8192 1000000}"
CC_BIN="${COO_CC:-${CC:-gcc-15}}"

echo "=== Building Hydrangea compiler ==="
cabal build hydrangea-compiler 2>&1 | grep -v "^Build profile" | grep -v "^In order" || true

echo "=== Compiling COO/CSR benchmark to native binary ==="
PARALLEL_FLAG=""
if [ "$PARALLEL" != "1" ]; then
  PARALLEL_FLAG="--no-parallel"
fi

COO_NROWS=128 COO_NCOLS=128 COO_NNZ=1024 COO_DUP_PERIOD="$DUP_PERIOD" \
  cabal run hydrangea-compiler -- --main --prune-dead-procs --cc="$CC_BIN" \
  $PARALLEL_FLAG --compile-only --keep-c "$BENCH/coo_csr_build.hyd" > /dev/null

echo "=== COO/CSR Build Benchmark (parallel=$PARALLEL, dup_period=$DUP_PERIOD, cc=$CC_BIN) ==="
set -- $CONFIGS
while [ $# -ge 3 ]; do
  nrows=$1; ncols=$2; nnz=$3; shift 3
  echo -n "nrows=$nrows ncols=$ncols nnz=$nnz ... "
  export COO_NROWS="$nrows"
  export COO_NCOLS="$ncols"
  export COO_NNZ="$nnz"
  export COO_DUP_PERIOD="$DUP_PERIOD"
  { time ./hydrangea_out > /dev/null; } 2>&1 | grep real | awk '{print $2}'
done
