#!/usr/bin/env bash
# Run Black-Scholes benchmark with varying N.
# Compiles the benchmark once, then times only execution for each input size.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
BENCH="$ROOT/bench/blackscholes"
cd "$ROOT"

PARALLEL="${BS_PARALLEL:-1}"
SIZES="${BS_SIZES:-10000 100000 1000000}"
TOLERANCE="${BS_TOLERANCE:-1e-6}"
CHECK_OUTPUT="${BS_CHECK_OUTPUT:-1}"
CC_BIN="${BS_CC:-${CC:-gcc-15}}"
TIME_FILE="$(mktemp "$BENCH/.blackscholes-time.XXXXXX")"

cleanup() {
  rm -f "$TIME_FILE"
}

trap cleanup EXIT

echo "=== Building Hydrangea compiler ==="
cabal build hydrangea-compiler 2>&1 | grep -v "^Build profile" | grep -v "^In order" || true

echo "=== Compiling Black-Scholes benchmark to native binary ==="
PARALLEL_FLAG=""
if [ "$PARALLEL" != "1" ]; then
  PARALLEL_FLAG="--no-parallel"
fi

BS_N=1024 cabal run hydrangea-compiler -- --main --prune-dead-procs --cc="$CC_BIN" \
  $PARALLEL_FLAG --compile-only --keep-c "$BENCH/blackscholes.hyd" > /dev/null

echo "=== Black-Scholes Benchmark (parallel=$PARALLEL, cc=$CC_BIN, check=$CHECK_OUTPUT, tol=$TOLERANCE) ==="
for n in $SIZES; do
  echo -n "N=$n ... "
  python3 "$BENCH/gen_input.py" "$n" > /dev/null
  rm -f "$BENCH/out.csv"
  export BS_N="$n"
  /usr/bin/time -p -o "$TIME_FILE" ./hydrangea_out > /dev/null
  runtime="$(awk '/^real / {print $2 "s"}' "$TIME_FILE")"
  if [ ! -f "$BENCH/out.csv" ]; then
    echo "missing benchmark output: $BENCH/out.csv" >&2
    exit 1
  fi
  if [ "$CHECK_OUTPUT" = "1" ]; then
    python3 "$BENCH/check_output.py" --tolerance "$TOLERANCE" > /dev/null
  fi
  echo "$runtime"
done
