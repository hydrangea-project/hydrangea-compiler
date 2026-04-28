#!/usr/bin/env bash
# bench/run_sweep.sh — run all (or one) benchmark across 10 sizes, emit CSV per benchmark.
#
# Usage:
#   ./bench/run_sweep.sh [<benchmark>] [--warmup=N] [--iters=N] [--cc=<compiler>]
#
# Outputs: bench/results/<name>.csv with columns: size,hydrangea_ms,repa_ms,c_ms

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

# Set up LLVM for GHC's LLVM backend (needed to build/run Repa benchmarks).
# On macOS, probe Homebrew for an LLVM installation if llc/opt are not already on PATH.
if [[ "$(uname -s)" == "Darwin" ]] && ! command -v llc >/dev/null 2>&1; then
  if command -v brew >/dev/null 2>&1; then
    for _llvm_ver in 15 16 14 17; do
      _llvm_prefix="$(brew --prefix "llvm@${_llvm_ver}" 2>/dev/null)" || true
      if [[ -n "$_llvm_prefix" && -d "$_llvm_prefix/bin" ]]; then
        export PATH="$_llvm_prefix/bin:$PATH"
        break
      fi
    done
    unset _llvm_ver _llvm_prefix
  fi
fi

# Find a C compiler that supports -fopenmp.
# Prefer Homebrew GCC over clang on macOS (avoids missing sysroot/libomp issues).
find_openmp_cc() {
  if [ -n "$CC_OVERRIDE" ]; then echo "$CC_OVERRIDE"; return; fi
  for candidate in gcc-15 gcc-14 gcc-13 gcc-12; do
    if command -v "$candidate" >/dev/null 2>&1; then echo "$candidate"; return; fi
  done
  echo "${CC:-cc}"
}
BENCH_CC="$(find_openmp_cc)"

cd "$REPO_ROOT"
mkdir -p bench/results

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

# Build all C reference binaries up front
build_c() {
  local name="$1"
  local src="$REPO_ROOT/bench/$name/${name}_ref.c"
  local bin="$REPO_ROOT/bench/$name/${name}_ref"
  if [ ! -f "$src" ]; then echo "MISSING C src: $src" >&2; return 1; fi
  if [ ! -f "$bin" ] || [ "$src" -nt "$bin" ]; then
    echo "  Building C reference: $name" >&2
    "$BENCH_CC" -O3 -march=native -fopenmp -std=c11 -lm \
      "$src" -o "$bin" 2>/dev/null \
      || { echo "  WARNING: C build failed for $name" >&2; return 1; }
  fi
}

echo "Building C reference binaries..." >&2
for bname in blackscholes nbody mandelbrot spmv matmul \
  weighted_histogram guarded_weighted_histogram \
  coo_csr_build graph_messages voxel_rasterization voxel_trilinear_splat; do
  [ -n "$BENCH_FILTER" ] && [ "$bname" != "$BENCH_FILTER" ] && continue
  build_c "$bname" 2>/dev/null || true
done

# ---- Utility functions -----------------------------------------------------

parse_min_ms() {
  grep -o 'min=[0-9.]*' | sed 's/min=//' | head -1
}

gen_inputs() {
  local name="$1"
  # Always regenerate when sizes may have changed (sweep mode)
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

run_c() {
  local name="$1"
  local bin="$REPO_ROOT/bench/$name/${name}_ref"
  if [ ! -x "$bin" ]; then echo "N/A"; return; fi
  local result
  result=$(BENCH_WARMUP="$WARMUP" BENCH_ITERS="$ITERS" \
    "$bin" 2>/dev/null | parse_min_ms) || true
  echo "${result:-N/A}"
}

# run_sweep <name> <hyd_file> <proc> <extra_flags> <size_label> [env assignments...]
# Sets exported env vars from the trailing args, then runs all three variants.
sweep_one() {
  local name="$1" hyd_file="$2" proc="$3" extra_flags="$4" size="$5"
  shift 5
  # Apply size-specific env var assignments
  for assignment in "$@"; do
    export "${assignment?}"
  done
  gen_inputs "$name"
  local hyd repa c
  hyd=$(run_hydrangea "$name" "$hyd_file" "$proc" "$extra_flags")
  repa=$(run_repa "$name")
  c=$(run_c "$name")
  echo "$size,$hyd,$repa,$c"
}

run_sweep_for() {
  local name="$1" hyd_file="$2" proc="${3:-main}" extra_flags="${4:-}"
  local csv="$REPO_ROOT/bench/results/${name}.csv"
  echo "size,hydrangea_ms,repa_ms,c_ms" > "$csv"
  echo "  $name → $csv" >&2
}

# ---- Size tables -----------------------------------------------------------
# Each benchmark defines an array of "size env_var=value [env_var2=value2 ...]" entries.

sweep_blackscholes() {
  local csv="$REPO_ROOT/bench/results/blackscholes.csv"
  echo "  blackscholes → $csv" >&2
  echo "size,hydrangea_ms,repa_ms,c_ms" > "$csv"
  for n in 10000 30000 100000 300000 1000000 2000000 4000000 6000000 8000000 10000000; do
    sweep_one blackscholes blackscholes.hyd main "" "$n" "BS_N=$n" >> "$csv"
    echo "    size=$n done" >&2
  done
}

sweep_nbody() {
  local csv="$REPO_ROOT/bench/results/nbody.csv"
  echo "  nbody → $csv" >&2
  echo "size,hydrangea_ms,repa_ms,c_ms" > "$csv"
  for n in 128 256 512 1024 2048 3072 4096 5120 6144 8192; do
    sweep_one nbody nbody.hyd main "" "$n" "NBODY_N=$n" >> "$csv"
    echo "    size=$n done" >&2
  done
}

sweep_mandelbrot() {
  local csv="$REPO_ROOT/bench/results/mandelbrot.csv"
  echo "  mandelbrot → $csv" >&2
  echo "size,hydrangea_ms,repa_ms,c_ms" > "$csv"
  for w in 64 128 256 384 512 768 1024 1536 2048 3072; do
    export MAND_W=$w MAND_H=$w MAND_ITERS=256
    sweep_one mandelbrot mandelbrot.hyd main "" "$w" \
      "MAND_W=$w" "MAND_H=$w" "MAND_ITERS=256" >> "$csv"
    echo "    size=$w done" >&2
  done
}

sweep_spmv() {
  local csv="$REPO_ROOT/bench/results/spmv.csv"
  echo "  spmv → $csv" >&2
  echo "size,hydrangea_ms,repa_ms,c_ms" > "$csv"
  for nrows in 1000 2000 5000 10000 20000 50000 75000 100000 150000 200000; do
    local nnz=$((nrows * 10))
    sweep_one spmv spmv.hyd main "" "$nrows" \
      "SPMV_NROWS=$nrows" "SPMV_NCOLS=$nrows" "SPMV_NNZ=$nnz" >> "$csv"
    echo "    size=$nrows done" >&2
  done
}

sweep_matmul() {
  local csv="$REPO_ROOT/bench/results/matmul.csv"
  echo "  matmul → $csv" >&2
  echo "size,hydrangea_ms,repa_ms,c_ms" > "$csv"
  for m in 32 64 96 128 192 256 320 384 448 512; do
    sweep_one matmul mat_mul_bench.hyd main "" "$m" \
      "MAT_M=$m" "MAT_K=$m" "MAT_N=$m" >> "$csv"
    echo "    size=$m done" >&2
  done
}

sweep_weighted_histogram() {
  local csv="$REPO_ROOT/bench/results/weighted_histogram.csv"
  echo "  weighted_histogram → $csv" >&2
  echo "size,hydrangea_ms,repa_ms,c_ms" > "$csv"
  export WH_BINS=256
  for n in 100000 250000 500000 1000000 2000000 5000000 10000000 25000000 50000000 100000000; do
    sweep_one weighted_histogram weighted_histogram.hyd main "" "$n" \
      "WH_N=$n" "WH_BINS=256" >> "$csv"
    echo "    size=$n done" >&2
  done
}

sweep_guarded_weighted_histogram() {
  local csv="$REPO_ROOT/bench/results/guarded_weighted_histogram.csv"
  echo "  guarded_weighted_histogram → $csv" >&2
  echo "size,hydrangea_ms,repa_ms,c_ms" > "$csv"
  export GWH_BINS=256 GWH_KEEP_PERIOD=3
  for n in 100000 250000 500000 1000000 2000000 5000000 10000000 25000000 50000000 100000000; do
    sweep_one guarded_weighted_histogram guarded_weighted_histogram.hyd main "" "$n" \
      "GWH_N=$n" "GWH_BINS=256" "GWH_KEEP_PERIOD=3" >> "$csv"
    echo "    size=$n done" >&2
  done
}

sweep_coo_csr_build() {
  local csv="$REPO_ROOT/bench/results/coo_csr_build.csv"
  echo "  coo_csr_build → $csv" >&2
  echo "size,hydrangea_ms,repa_ms,c_ms" > "$csv"
  export COO_NROWS=1024 COO_NCOLS=1024 COO_DUP_PERIOD=4
  for nnz in 10000 25000 50000 100000 250000 500000 1000000 2000000 5000000 10000000; do
    sweep_one coo_csr_build coo_csr_build.hyd main "" "$nnz" \
      "COO_NNZ=$nnz" "COO_NROWS=1024" "COO_NCOLS=1024" "COO_DUP_PERIOD=4" >> "$csv"
    echo "    size=$nnz done" >&2
  done
}

sweep_graph_messages() {
  local csv="$REPO_ROOT/bench/results/graph_messages.csv"
  echo "  graph_messages → $csv" >&2
  echo "size,hydrangea_ms,repa_ms,c_ms" > "$csv"
  export GRAPH_DEGREE=16
  for n in 1000 2000 5000 10000 20000 50000 100000 200000 500000 1000000; do
    sweep_one graph_messages graph_messages.hyd main "" "$n" \
      "GRAPH_NODES=$n" "GRAPH_DEGREE=16" >> "$csv"
    echo "    size=$n done" >&2
  done
}

sweep_voxel_rasterization() {
  local csv="$REPO_ROOT/bench/results/voxel_rasterization.csv"
  echo "  voxel_rasterization → $csv" >&2
  echo "size,hydrangea_ms,repa_ms,c_ms" > "$csv"
  export VOX_NX=64 VOX_NY=64 VOX_NZ=64 VOX_KEEP_PERIOD=3
  for n in 100000 250000 500000 1000000 2000000 5000000 10000000 25000000 50000000 100000000; do
    sweep_one voxel_rasterization voxel_rasterization.hyd main "" "$n" \
      "VOX_POINTS=$n" "VOX_NX=64" "VOX_NY=64" "VOX_NZ=64" "VOX_KEEP_PERIOD=3" >> "$csv"
    echo "    size=$n done" >&2
  done
}

sweep_voxel_trilinear_splat() {
  local csv="$REPO_ROOT/bench/results/voxel_trilinear_splat.csv"
  echo "  voxel_trilinear_splat → $csv" >&2
  echo "size,hydrangea_ms,repa_ms,c_ms" > "$csv"
  export VSPLAT_NX=64 VSPLAT_NY=64 VSPLAT_NZ=64 VSPLAT_KEEP_PERIOD=3
  for n in 100000 250000 500000 1000000 2000000 5000000 10000000 25000000 50000000 100000000; do
    sweep_one voxel_trilinear_splat voxel_trilinear_splat.hyd main "--no-solver-check" "$n" \
      "VSPLAT_POINTS=$n" "VSPLAT_NX=64" "VSPLAT_NY=64" "VSPLAT_NZ=64" "VSPLAT_KEEP_PERIOD=3" >> "$csv"
    echo "    size=$n done" >&2
  done
}

# ---- Dispatch --------------------------------------------------------------

ALL_BENCHMARKS=(
  blackscholes nbody mandelbrot spmv matmul
  weighted_histogram guarded_weighted_histogram
  coo_csr_build graph_messages voxel_rasterization voxel_trilinear_splat
)

echo "Running size sweep (results → bench/results/)..." >&2

for bname in "${ALL_BENCHMARKS[@]}"; do
  [ -n "$BENCH_FILTER" ] && [ "$bname" != "$BENCH_FILTER" ] && continue
  case "$bname" in
    blackscholes)             sweep_blackscholes ;;
    nbody)                    sweep_nbody ;;
    mandelbrot)               sweep_mandelbrot ;;
    spmv)                     sweep_spmv ;;
    matmul)                   sweep_matmul ;;
    weighted_histogram)       sweep_weighted_histogram ;;
    guarded_weighted_histogram) sweep_guarded_weighted_histogram ;;
    coo_csr_build)            sweep_coo_csr_build ;;
    graph_messages)           sweep_graph_messages ;;
    voxel_rasterization)      sweep_voxel_rasterization ;;
    voxel_trilinear_splat)    sweep_voxel_trilinear_splat ;;
  esac
done

echo "Done." >&2
