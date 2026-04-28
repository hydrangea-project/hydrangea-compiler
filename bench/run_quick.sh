#!/usr/bin/env bash
# bench/run_quick.sh — run all (or one) benchmark at default size, print a table.
#
# Usage:
#   ./bench/run_quick.sh [<benchmark>] [--warmup=N] [--iters=N] [--cc=<compiler>]
#
# Columns: hydrangea (compiled C+OpenMP), repa, accelerate (LLVM CPU), C+OMP reference.
#
# Optional env overrides (set before calling):
#   BS_N, NBODY_N, MAND_W, MAND_H, MAND_ITERS, SPMV_NROWS, SPMV_NCOLS, SPMV_NNZ,
#   MAT_M, MAT_K, MAT_N, WH_N, WH_BINS, GWH_N, GWH_BINS, GWH_KEEP_PERIOD,
#   COO_NROWS, COO_NCOLS, COO_NNZ, COO_DUP_PERIOD, GRAPH_NODES, GRAPH_DEGREE,
#   VOX_POINTS, VOX_NX, VOX_NY, VOX_NZ, VOX_KEEP_PERIOD,
#   VSPLAT_POINTS, VSPLAT_NX, VSPLAT_NY, VSPLAT_NZ, VSPLAT_KEEP_PERIOD

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

WARMUP=3
ITERS=10
BENCH_FILTER=""
CC_OVERRIDE=""

for arg in "$@"; do
  case "$arg" in
    --warmup=*) WARMUP="${arg#--warmup=}" ;;
    --iters=*)  ITERS="${arg#--iters=}" ;;
    --cc=*)     CC_OVERRIDE="${arg#--cc=}" ;;
    --*) echo "Unknown option: $arg" >&2; exit 1 ;;
    *)   BENCH_FILTER="$arg" ;;
  esac
done

# Set up LLVM for GHC's LLVM backend (needed to build/run Repa/Accelerate benchmarks).
if [ -f ~/use-llvm.sh ]; then
  : "${LDFLAGS:=}" "${CPPFLAGS:=}"
  export LDFLAGS CPPFLAGS
  # shellcheck source=/dev/null
  source ~/use-llvm.sh 15
fi

# On macOS, the Accelerate LLVM JIT links with -lm which requires the SDK lib
# path.  Set LIBRARY_PATH so the JIT linker can find the stub.
if [[ "$(uname -s)" == "Darwin" ]] && [[ -z "${LIBRARY_PATH:-}" ]]; then
  _sdk_lib="$(xcrun --sdk macosx --show-sdk-path 2>/dev/null)/usr/lib" || true
  if [[ -d "${_sdk_lib:-}" ]]; then
    export LIBRARY_PATH="$_sdk_lib"
  fi
fi

# Find a C compiler that supports -fopenmp.
# Prefer Homebrew GCC over clang on macOS (avoids missing sysroot/libomp issues).
find_openmp_cc() {
  if [ -n "$CC_OVERRIDE" ]; then echo "$CC_OVERRIDE"; return; fi
  for candidate in gcc-15 gcc-14 gcc-13 gcc-12; do
    if command -v "$candidate" >/dev/null 2>&1; then echo "$candidate"; return; fi
  done
  # Fall back to whatever CC is (set by use-llvm.sh or environment)
  echo "${CC:-cc}"
}
BENCH_CC="$(find_openmp_cc)"

# Default sizes (honour env overrides from the caller)
: "${BS_N:=1000000}"
: "${NBODY_N:=4096}"
: "${MAND_W:=1024}" "${MAND_H:=1024}" "${MAND_ITERS:=256}"
: "${SPMV_NROWS:=10000}" "${SPMV_NCOLS:=10000}" "${SPMV_NNZ:=100000}"
: "${MAT_M:=512}" "${MAT_K:=512}" "${MAT_N:=512}"
: "${WH_N:=1000000}" "${WH_BINS:=256}"
: "${GWH_N:=1000000}" "${GWH_BINS:=256}" "${GWH_KEEP_PERIOD:=3}"
: "${COO_NROWS:=1024}" "${COO_NCOLS:=1024}" "${COO_NNZ:=100000}" "${COO_DUP_PERIOD:=4}"
: "${GRAPH_NODES:=100000}" "${GRAPH_DEGREE:=16}"
: "${VOX_POINTS:=1000000}" "${VOX_NX:=64}" "${VOX_NY:=64}" "${VOX_NZ:=64}" "${VOX_KEEP_PERIOD:=3}"
: "${VSPLAT_POINTS:=1000000}" "${VSPLAT_NX:=64}" "${VSPLAT_NY:=64}" "${VSPLAT_NZ:=64}" "${VSPLAT_KEEP_PERIOD:=3}"

export BS_N NBODY_N MAND_W MAND_H MAND_ITERS
export SPMV_NROWS SPMV_NCOLS SPMV_NNZ MAT_M MAT_K MAT_N
export WH_N WH_BINS GWH_N GWH_BINS GWH_KEEP_PERIOD
export COO_NROWS COO_NCOLS COO_NNZ COO_DUP_PERIOD
export GRAPH_NODES GRAPH_DEGREE
export VOX_POINTS VOX_NX VOX_NY VOX_NZ VOX_KEEP_PERIOD
export VSPLAT_POINTS VSPLAT_NX VSPLAT_NY VSPLAT_NZ VSPLAT_KEEP_PERIOD

cd "$REPO_ROOT"

# ---- Build phase -----------------------------------------------------------

echo "Building Hydrangea compiler..." >&2
cabal build hydrangea-compiler >/dev/null 2>&1

echo "Building Repa benchmarks..." >&2
REPA_BIN=""
if (cd bench/repa && cabal build >/dev/null 2>&1); then
  REPA_BIN="$(cd bench/repa && cabal list-bin repa-bench 2>/dev/null)"
else
  echo "WARNING: Repa build failed; repa results will show N/A" >&2
fi

echo "Building Accelerate benchmarks..." >&2
ACCEL_BIN=""
if (cd bench/accel && cabal build >/dev/null 2>&1); then
  ACCEL_BIN="$(cd bench/accel && cabal list-bin accel-bench 2>/dev/null)"
else
  echo "WARNING: Accelerate build failed; accel results will show N/A" >&2
fi

# ---- Utility functions -----------------------------------------------------

parse_min_ms() {
  grep -o 'min=[0-9.]*' | sed 's/min=//' | head -1
}

build_c() {
  local name="$1"
  local src="$REPO_ROOT/bench/$name/${name}_ref.c"
  local bin="$REPO_ROOT/bench/$name/${name}_ref"
  if [ ! -f "$src" ]; then
    echo "MISSING C src: $src" >&2; return 1
  fi
  if [ ! -f "$bin" ] || [ "$src" -nt "$bin" ]; then
    echo "  Building C reference: $name" >&2
    "$BENCH_CC" -O3 -march=native -fopenmp -std=c11 -lm \
      "$src" -o "$bin" 2>/dev/null \
      || { echo "  WARNING: C build failed for $name (OpenMP supported?)" >&2; return 1; }
  fi
}

gen_inputs() {
  local name="$1"
  case "$name" in
    blackscholes)
      python3 "$REPO_ROOT/bench/blackscholes/gen_input.py" "$BS_N" >/dev/null ;;
    nbody)
      python3 "$REPO_ROOT/bench/nbody/gen_input.py" "$NBODY_N" >/dev/null ;;
    spmv)
      python3 "$REPO_ROOT/bench/spmv/gen_input.py" \
        "$SPMV_NROWS" "$SPMV_NCOLS" "$SPMV_NNZ" >/dev/null ;;
    matmul)
      [ -f "$REPO_ROOT/bench/matmul/gen_mats" ] || \
        gcc -O2 -o "$REPO_ROOT/bench/matmul/gen_mats" \
          "$REPO_ROOT/bench/matmul/gen_mats.c"
      "$REPO_ROOT/bench/matmul/gen_mats" "$MAT_M" "$MAT_K" "$REPO_ROOT/bench/matmul/matA.csv"
      "$REPO_ROOT/bench/matmul/gen_mats" "$MAT_K" "$MAT_N" "$REPO_ROOT/bench/matmul/matB.csv" ;;
  esac
}

run_hydrangea() {
  local name="$1" hyd_file="$2" proc="$3" extra_flags="${4:-}"
  local result
  # shellcheck disable=SC2086
  # Hydrangea's benchmark harness writes timing to stderr; merge with stdout for parsing.
  result=$(cabal run hydrangea-compiler -- \
    --cc="$BENCH_CC" \
    --tiling --polyhedral \
    $extra_flags \
    --benchmark="$proc" \
    --bench-warmup="$WARMUP" --bench-iters="$ITERS" \
    "bench/$name/$hyd_file" 2>&1 | parse_min_ms) || true
  echo "${result:-N/A}"
}

run_repa() {
  local name="$1"
  if [ -z "$REPA_BIN" ] || [ ! -x "$REPA_BIN" ]; then echo "N/A"; return; fi
  local result
  result=$("$REPA_BIN" bench "$name" --warmup="$WARMUP" --iters="$ITERS" \
    2>/dev/null | parse_min_ms) || true
  echo "${result:-N/A}"
}

run_accel() {
  local name="$1"
  if [ -z "$ACCEL_BIN" ] || [ ! -x "$ACCEL_BIN" ]; then echo "N/A"; return; fi
  local result
  result=$("$ACCEL_BIN" bench "$name" --warmup="$WARMUP" --iters="$ITERS" \
    2>/dev/null | parse_min_ms) || true
  echo "${result:-N/A}"
}

run_c() {
  local name="$1"
  local bin="$REPO_ROOT/bench/$name/${name}_ref"
  if [ ! -x "$bin" ]; then echo "N/A"; return; fi
  local result
  result=$(BENCH_WARMUP="$WARMUP" BENCH_ITERS="$ITERS" \
    "$bin" 2>/dev/null | parse_min_ms) || true
  echo "${result:-N/A}"
}

run_bench() {
  local name="$1" hyd_file="$2" proc="${3:-main}" extra_flags="${4:-}"
  gen_inputs "$name"
  build_c "$name" 2>/dev/null || true
  local hyd repa accel c
  hyd=$(run_hydrangea "$name" "$hyd_file" "$proc" "$extra_flags")
  repa=$(run_repa "$name")
  accel=$(run_accel "$name")
  c=$(run_c "$name")
  printf "%-35s %15s %15s %15s %15s\n" "$name" "$hyd" "$repa" "$accel" "$c"
}

# ---- Table header ----------------------------------------------------------

printf "\n"
printf "%-35s %15s %15s %15s %15s\n" "benchmark" "hydrangea(ms)" "repa(ms)" "accel(ms)" "c+omp(ms)"
printf "%-35s %15s %15s %15s %15s\n" \
  "-----------------------------------" "---------------" "---------------" "---------------" "---------------"

# ---- Run benchmarks --------------------------------------------------------

ALL_BENCHMARKS=(
  blackscholes nbody mandelbrot spmv matmul
  weighted_histogram guarded_weighted_histogram
  coo_csr_build graph_messages voxel_rasterization voxel_trilinear_splat
)

for bname in "${ALL_BENCHMARKS[@]}"; do
  [ -n "$BENCH_FILTER" ] && [ "$bname" != "$BENCH_FILTER" ] && continue
  case "$bname" in
    blackscholes)
      run_bench blackscholes blackscholes.hyd main ;;
    nbody)
      run_bench nbody nbody.hyd main ;;
    mandelbrot)
      run_bench mandelbrot mandelbrot.hyd main ;;
    spmv)
      run_bench spmv spmv.hyd main ;;
    matmul)
      run_bench matmul mat_mul_bench.hyd main ;;
    weighted_histogram)
      run_bench weighted_histogram weighted_histogram.hyd main ;;
    guarded_weighted_histogram)
      run_bench guarded_weighted_histogram guarded_weighted_histogram.hyd main ;;
    coo_csr_build)
      run_bench coo_csr_build coo_csr_build.hyd main ;;
    graph_messages)
      run_bench graph_messages graph_messages.hyd main ;;
    voxel_rasterization)
      run_bench voxel_rasterization voxel_rasterization.hyd main ;;
    voxel_trilinear_splat)
      run_bench voxel_trilinear_splat voxel_trilinear_splat.hyd main "--no-solver-check" ;;
  esac
done

printf "\n"
