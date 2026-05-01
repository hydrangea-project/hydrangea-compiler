#!/usr/bin/env bash
# Run the 2D stencil benchmark: generate input, compile via Hydrangea,
# compile C reference, run both, compare outputs, report timing.
#
# Two Hydrangea variants are benchmarked:
#   stencil_2d.hyd      -- uses 'stencil clamp', full (h x w) output with clamped boundary
#   stencil_interior.hyd -- uses generate+index, (h-2 x w-2) interior-only output
#
# The interior variant produces a clean 2D loop (no division/modulo, no SIf
# for clamping) and is a better apples-to-apples comparison with the C reference.
set -euo pipefail

HERE=$(cd "$(dirname "$0")" && pwd)
REPO_ROOT=$(cd "$HERE/../.." && pwd)

H=${STENCIL_H:-512}
W=${STENCIL_W:-512}
CC_BIN=${STENCIL_CC:-${CC:-gcc-15}}
PARALLEL=${STENCIL_PARALLEL:-1}
BENCH_WARMUP=${BENCH_WARMUP:-2}
BENCH_ITERS=${BENCH_ITERS:-5}

# On macOS, probe Homebrew GCC if CC not set
if [[ "$(uname)" == "Darwin" && -z "${CC:-}" && "$CC_BIN" == "gcc-15" ]]; then
    for candidate in gcc-15 gcc-14 gcc-13 gcc-12; do
        if command -v "$candidate" &>/dev/null; then
            CC_BIN="$candidate"
            break
        fi
    done
fi

echo "=== 2D Stencil Benchmark: ${H}x${W} ==="
echo "CC=$CC_BIN  PARALLEL=$PARALLEL  WARMUP=$BENCH_WARMUP  ITERS=$BENCH_ITERS"

cd "$REPO_ROOT"

PARALLEL_FLAG=""
if [ "$PARALLEL" != "1" ]; then
    PARALLEL_FLAG="--no-parallel"
fi

# ---- Generate input ----
echo ""
echo "--- Generating ${H}x${W} input ---"
python3 bench/stencil/gen_input.py "$H" "$W" bench/stencil/input.csv

export STENCIL_H=$H STENCIL_W=$W BENCH_WARMUP=$BENCH_WARMUP BENCH_ITERS=$BENCH_ITERS

# ============================================================
# Variant 1: stencil clamp (full h x w, clamped boundary)
# ============================================================
echo ""
echo "========== Variant 1: stencil clamp (full ${H}x${W}) =========="

echo "--- Compiling C reference (full) ---"
"$CC_BIN" -O3 -march=native -fopenmp -std=c99 \
    -I bench/c_bench \
    -o bench/stencil/stencil_ref_bin \
    bench/stencil/stencil_ref.c -lm

echo "--- Running C reference (full) ---"
OMP_NUM_THREADS=${OMP_NUM_THREADS:-1} bench/stencil/stencil_ref_bin

echo "--- Compiling Hydrangea stencil_2d ---"
cabal run hydrangea-compiler -- \
    --main --prune-dead-procs --keep-c $PARALLEL_FLAG \
    --emit-c bench/stencil/stencil_2d.hyd \
    > bench/stencil/out.c

echo "--- Compiling emitted C with $CC_BIN ---"
"$CC_BIN" -O3 -march=native -fopenmp -std=c99 \
    -I runtime -I third_party/simde \
    -o bench/stencil/hyd_out \
    bench/stencil/out.c runtime/hyd_write_csv.c -lm

echo "--- Running Hydrangea stencil_2d ---"
OMP_NUM_THREADS=${OMP_NUM_THREADS:-1} bench/stencil/hyd_out

echo "--- Correctness check (full, max abs diff) ---"
python3 - <<'PYEOF'
import sys, re

def read_csv(path):
    with open(path) as f:
        return [float(x) for x in re.split(r'[,\n]+', f.read()) if x.strip()]

try:
    ref = read_csv("bench/stencil/ref_out.csv")
    hyd = read_csv("bench/stencil/out.csv")
except FileNotFoundError as e:
    print(f"SKIP correctness check: {e}")
    sys.exit(0)

if len(ref) != len(hyd):
    print(f"FAIL: length mismatch: ref={len(ref)} hyd={len(hyd)}")
    sys.exit(1)

max_diff = max(abs(r - h) for r, h in zip(ref, hyd))
print(f"max |ref - hyd| = {max_diff:.3e}")
if max_diff < 1e-9:
    print("PASS: outputs match")
else:
    print("FAIL: outputs differ beyond tolerance")
    sys.exit(1)
PYEOF

# ============================================================
# Variant 2: generate+index interior-only ((h-2) x (w-2))
# ============================================================
echo ""
echo "========== Variant 2: interior-only generate+index (${H}x${W} -> $((H-2))x$((W-2))) =========="

echo "--- Compiling C reference (interior) ---"
"$CC_BIN" -O3 -march=native -fopenmp -std=c99 \
    -I bench/c_bench \
    -o bench/stencil/stencil_interior_ref_bin \
    bench/stencil/stencil_interior_ref.c -lm

echo "--- Running C reference (interior) ---"
OMP_NUM_THREADS=${OMP_NUM_THREADS:-1} bench/stencil/stencil_interior_ref_bin

echo "--- Compiling Hydrangea stencil_interior ---"
cabal run hydrangea-compiler -- \
    --main --prune-dead-procs --keep-c $PARALLEL_FLAG \
    --emit-c bench/stencil/stencil_interior.hyd \
    > bench/stencil/out_interior.c

echo "--- Compiling emitted C with $CC_BIN ---"
"$CC_BIN" -O3 -march=native -fopenmp -std=c99 \
    -I runtime -I third_party/simde \
    -o bench/stencil/hyd_interior_out \
    bench/stencil/out_interior.c runtime/hyd_write_csv.c -lm

echo "--- Running Hydrangea stencil_interior ---"
OMP_NUM_THREADS=${OMP_NUM_THREADS:-1} bench/stencil/hyd_interior_out

echo "--- Correctness check (interior, max abs diff) ---"
python3 - <<'PYEOF'
import sys, re

def read_csv(path):
    with open(path) as f:
        return [float(x) for x in re.split(r'[,\n]+', f.read()) if x.strip()]

try:
    ref = read_csv("bench/stencil/ref_out_interior.csv")
    hyd = read_csv("bench/stencil/out_interior.csv")
except FileNotFoundError as e:
    print(f"SKIP correctness check: {e}")
    sys.exit(0)

if len(ref) != len(hyd):
    print(f"FAIL: length mismatch: ref={len(ref)} hyd={len(hyd)}")
    sys.exit(1)

max_diff = max(abs(r - h) for r, h in zip(ref, hyd))
print(f"max |ref - hyd| = {max_diff:.3e}")
if max_diff < 1e-9:
    print("PASS: outputs match")
else:
    print("FAIL: outputs differ beyond tolerance")
    sys.exit(1)
PYEOF

echo ""
echo "=== Done ==="
