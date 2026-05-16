#!/usr/bin/env bash
# bench/run_full_sweep.sh — run the full benchmark suite across size ranges and
# emit one CSV per benchmark with Hydrangea, Repa, Accelerate, and C+OMP timings.
#
# Usage:
#   ./bench/run_full_sweep.sh [<benchmark>] [--warmup=N] [--iters=N] [--cc=<compiler>] [--out-dir=<dir>] [--skip-uninformative]
#
# Outputs: <out-dir>/<benchmark>.csv with columns:
#   size,hydrangea_ms,repa_ms,accelerate_ms,c_omp_ms

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

WARMUP=3
ITERS=10
BENCH_FILTER=""
CC_OVERRIDE=""
OUT_DIR="$REPO_ROOT/bench/results/full_sweep"
SKIP_UNINFORMATIVE=0

for arg in "$@"; do
  case "$arg" in
    --warmup=*) WARMUP="${arg#--warmup=}" ;;
    --iters=*)  ITERS="${arg#--iters=}" ;;
    --cc=*)     CC_OVERRIDE="${arg#--cc=}" ;;
    --out-dir=*) OUT_DIR="${arg#--out-dir=}" ;;
    --skip-uninformative) SKIP_UNINFORMATIVE=1 ;;
    --*) echo "Unknown option: $arg" >&2; exit 1 ;;
    *)   BENCH_FILTER="$arg" ;;
  esac
done

skip_uninformative_repa() {
  [ "$SKIP_UNINFORMATIVE" = 1 ] || return 1
  case "$1" in
    weighted_histogram|guarded_weighted_histogram|voxel_rasterization|voxel_trilinear_splat) return 0 ;;
    *) return 1 ;;
  esac
}

skip_uninformative_accel() {
  [ "$SKIP_UNINFORMATIVE" = 1 ] || return 1
  [ "$1" = "blackscholes" ]
}

# Set up LLVM for GHC's LLVM backend (needed to build/run Repa and Accelerate).
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

# On macOS, the Accelerate LLVM JIT links with -lm which requires the SDK lib
# path. Set LIBRARY_PATH so the JIT linker can find the stub.
if [[ "$(uname -s)" == "Darwin" ]] && [[ -z "${LIBRARY_PATH:-}" ]]; then
  _sdk_lib="$(xcrun --sdk macosx --show-sdk-path 2>/dev/null)/usr/lib" || true
  if [[ -d "${_sdk_lib:-}" ]]; then
    export LIBRARY_PATH="$_sdk_lib"
  fi
fi

find_openmp_cc() {
  if [ -n "$CC_OVERRIDE" ]; then echo "$CC_OVERRIDE"; return; fi
  for candidate in gcc-15 gcc-14 gcc-13 gcc-12; do
    if command -v "$candidate" >/dev/null 2>&1; then echo "$candidate"; return; fi
  done
  echo "${CC:-cc}"
}

BENCH_CC="$(find_openmp_cc)"

# Default sizes (honour env overrides from the caller).
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
: "${STENCIL_H:=512}" "${STENCIL_W:=512}"
: "${JACOBI_H:=256}" "${JACOBI_W:=256}" "${JACOBI_ITERS:=50}"
: "${KDE_N:=1000000}" "${KDE_BINS:=1024}"

export BS_N NBODY_N MAND_W MAND_H MAND_ITERS
export SPMV_NROWS SPMV_NCOLS SPMV_NNZ MAT_M MAT_K MAT_N
export WH_N WH_BINS GWH_N GWH_BINS GWH_KEEP_PERIOD
export COO_NROWS COO_NCOLS COO_NNZ COO_DUP_PERIOD
export GRAPH_NODES GRAPH_DEGREE
export VOX_POINTS VOX_NX VOX_NY VOX_NZ VOX_KEEP_PERIOD
export VSPLAT_POINTS VSPLAT_NX VSPLAT_NY VSPLAT_NZ VSPLAT_KEEP_PERIOD
export STENCIL_H STENCIL_W
export JACOBI_H JACOBI_W JACOBI_ITERS
export KDE_N KDE_BINS

cd "$REPO_ROOT"
mkdir -p "$OUT_DIR"

if [ "$SKIP_UNINFORMATIVE" = 1 ]; then
  echo "Skipping uninformative comparisons: Accelerate blackscholes; Repa scatter kernels." >&2
fi

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

parse_min_ms() {
  grep -o 'min=[0-9.]*' | sed 's/min=//' | head -1
}

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
    stencil_interior)
      python3 "$REPO_ROOT/bench/stencil/gen_input.py" \
        "$STENCIL_H" "$STENCIL_W" "$REPO_ROOT/bench/stencil/input.csv" >/dev/null ;;
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
  if skip_uninformative_repa "$name"; then echo "N/A"; return; fi
  if [ -z "$REPA_BIN" ] || [ ! -x "$REPA_BIN" ]; then echo "N/A"; return; fi
  local result
  result=$("$REPA_BIN" bench "$name" --warmup="$WARMUP" --iters="$ITERS" \
    2>/dev/null | parse_min_ms) || true
  echo "${result:-N/A}"
}

run_accel() {
  local name="$1"
  if skip_uninformative_accel "$name"; then echo "N/A"; return; fi
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

sweep_one() {
  local name="$1" hyd_file="$2" proc="$3" extra_flags="$4" size="$5" csv="$6"
  shift 6
  for assignment in "$@"; do
    export "${assignment?}"
  done
  gen_inputs "$name"
  build_c "$name" 2>/dev/null || true
  local hyd repa accel c
  hyd=$(run_hydrangea "$name" "$hyd_file" "$proc" "$extra_flags")
  repa=$(run_repa "$name")
  accel=$(run_accel "$name")
  c=$(run_c "$name")
  printf "%s,%s,%s,%s,%s\n" "$size" "$hyd" "$repa" "$accel" "$c" >> "$csv"
}

write_csv_header() {
  printf "size,hydrangea_ms,repa_ms,accelerate_ms,c_omp_ms\n" > "$1"
}

sweep_blackscholes() {
  local csv="$OUT_DIR/blackscholes.csv"
  echo "  blackscholes -> $csv" >&2
  write_csv_header "$csv"
  for n in 10000 30000 100000 300000 1000000 2000000 4000000 6000000 8000000 10000000; do
    sweep_one blackscholes blackscholes.hyd main "" "$n" "$csv" "BS_N=$n"
  done
}

sweep_nbody() {
  local csv="$OUT_DIR/nbody.csv"
  echo "  nbody -> $csv" >&2
  write_csv_header "$csv"
  for n in 128 256 512 1024 2048 3072 4096 5120 6144 8192; do
    sweep_one nbody nbody.hyd main "" "$n" "$csv" "NBODY_N=$n"
  done
}

sweep_mandelbrot() {
  local csv="$OUT_DIR/mandelbrot.csv"
  echo "  mandelbrot -> $csv" >&2
  write_csv_header "$csv"
  for w in 64 128 256 384 512 768 1024 1536 2048 3072; do
    sweep_one mandelbrot mandelbrot.hyd main "" "$w" "$csv" \
      "MAND_W=$w" "MAND_H=$w" "MAND_ITERS=256"
  done
}

sweep_spmv() {
  local csv="$OUT_DIR/spmv.csv"
  echo "  spmv -> $csv" >&2
  write_csv_header "$csv"
  for nrows in 1000 2000 5000 10000 20000 50000 75000 100000 150000 200000; do
    local nnz=$((nrows * 10))
    sweep_one spmv spmv.hyd main "" "$nrows" "$csv" \
      "SPMV_NROWS=$nrows" "SPMV_NCOLS=$nrows" "SPMV_NNZ=$nnz"
  done
}

sweep_matmul() {
  local csv="$OUT_DIR/matmul.csv"
  echo "  matmul -> $csv" >&2
  write_csv_header "$csv"
  for m in 32 64 96 128 192 256 320 384 448 512; do
    sweep_one matmul mat_mul_bench.hyd main "" "$m" "$csv" \
      "MAT_M=$m" "MAT_K=$m" "MAT_N=$m"
  done
}

sweep_weighted_histogram() {
  local csv="$OUT_DIR/weighted_histogram.csv"
  echo "  weighted_histogram -> $csv" >&2
  write_csv_header "$csv"
  for n in 100000 250000 500000 1000000 2000000 5000000 10000000 25000000 50000000 100000000; do
    sweep_one weighted_histogram weighted_histogram.hyd main "" "$n" "$csv" \
      "WH_N=$n" "WH_BINS=256"
  done
}

sweep_guarded_weighted_histogram() {
  local csv="$OUT_DIR/guarded_weighted_histogram.csv"
  echo "  guarded_weighted_histogram -> $csv" >&2
  write_csv_header "$csv"
  for n in 100000 250000 500000 1000000 2000000 5000000 10000000 25000000 50000000 100000000; do
    sweep_one guarded_weighted_histogram guarded_weighted_histogram.hyd main "" "$n" "$csv" \
      "GWH_N=$n" "GWH_BINS=256" "GWH_KEEP_PERIOD=3"
  done
}

sweep_coo_csr_build() {
  local csv="$OUT_DIR/coo_csr_build.csv"
  echo "  coo_csr_build -> $csv" >&2
  write_csv_header "$csv"
  for nnz in 10000 25000 50000 100000 250000 500000 1000000 2000000 5000000 10000000; do
    sweep_one coo_csr_build coo_csr_build.hyd main "" "$nnz" "$csv" \
      "COO_NNZ=$nnz" "COO_NROWS=1024" "COO_NCOLS=1024" "COO_DUP_PERIOD=4"
  done
}

sweep_graph_messages() {
  local csv="$OUT_DIR/graph_messages.csv"
  echo "  graph_messages -> $csv" >&2
  write_csv_header "$csv"
  for n in 1000 2000 5000 10000 20000 50000 100000 200000 500000 1000000; do
    sweep_one graph_messages graph_messages.hyd main "" "$n" "$csv" \
      "GRAPH_NODES=$n" "GRAPH_DEGREE=16"
  done
}

sweep_voxel_rasterization() {
  local csv="$OUT_DIR/voxel_rasterization.csv"
  echo "  voxel_rasterization -> $csv" >&2
  write_csv_header "$csv"
  for n in 100000 250000 500000 1000000 2000000 5000000 10000000 25000000 50000000 100000000; do
    sweep_one voxel_rasterization voxel_rasterization.hyd main "" "$n" "$csv" \
      "VOX_POINTS=$n" "VOX_NX=64" "VOX_NY=64" "VOX_NZ=64" "VOX_KEEP_PERIOD=3"
  done
}

sweep_voxel_trilinear_splat() {
  local csv="$OUT_DIR/voxel_trilinear_splat.csv"
  echo "  voxel_trilinear_splat -> $csv" >&2
  write_csv_header "$csv"
  for n in 100000 250000 500000 1000000 2000000 5000000 10000000 25000000 50000000 100000000; do
    sweep_one voxel_trilinear_splat voxel_trilinear_splat.hyd main "--no-solver-check" "$n" "$csv" \
      "VSPLAT_POINTS=$n" "VSPLAT_NX=64" "VSPLAT_NY=64" "VSPLAT_NZ=64" "VSPLAT_KEEP_PERIOD=3"
  done
}

sweep_stencil_interior() {
  local csv="$OUT_DIR/stencil_interior.csv"
  echo "  stencil_interior -> $csv" >&2
  write_csv_header "$csv"
  local stencil_ref_bin="$REPO_ROOT/bench/stencil/stencil_interior_ref"
  for n in 64 128 256 384 512 768 1024 1536 2048 3072; do
    export STENCIL_H=$n STENCIL_W=$n
    python3 "$REPO_ROOT/bench/stencil/gen_input.py" \
      "$n" "$n" "$REPO_ROOT/bench/stencil/input.csv" >/dev/null
    local hyd repa accel c
    hyd=$(run_hydrangea stencil stencil_interior.hyd main "")
    repa=$(run_repa stencil_interior)
    accel=$(run_accel stencil_interior)
    if [ -x "$stencil_ref_bin" ]; then
      c=$(BENCH_WARMUP="$WARMUP" BENCH_ITERS="$ITERS" \
        "$stencil_ref_bin" 2>/dev/null | parse_min_ms) || true
      c="${c:-N/A}"
    else
      c="N/A"
    fi
    printf "%s,%s,%s,%s,%s\n" "$n" "$hyd" "$repa" "$accel" "$c" >> "$csv"
  done
}

sweep_jacobi_2d() {
  local csv="$OUT_DIR/jacobi_2d.csv"
  echo "  jacobi_2d -> $csv" >&2
  write_csv_header "$csv"
  local jacobi_ref_bin="$REPO_ROOT/bench/stencil/jacobi_2d_ref"
  for n in 64 128 256 384 512 768 1024 1536 2048; do
    export JACOBI_H=$n JACOBI_W=$n
    local hyd repa accel c
    hyd=$(run_hydrangea stencil jacobi_2d.hyd main "")
    repa=$(run_repa jacobi_2d)
    accel=$(run_accel jacobi_2d)
    if [ -x "$jacobi_ref_bin" ]; then
      c=$(BENCH_WARMUP="$WARMUP" BENCH_ITERS="$ITERS" \
        "$jacobi_ref_bin" 2>/dev/null | parse_min_ms) || true
      c="${c:-N/A}"
    else
      c="N/A"
    fi
    printf "%s,%s,%s,%s,%s\n" "$n" "$hyd" "$repa" "$accel" "$c" >> "$csv"
  done
}

sweep_kde() {
  local csv="$OUT_DIR/kde.csv"
  echo "  kde -> $csv" >&2
  write_csv_header "$csv"
  for n in 100000 250000 500000 1000000 2000000 5000000 10000000 25000000 50000000 100000000; do
    sweep_one kde kde.hyd main "" "$n" "$csv" \
      "KDE_N=$n" "KDE_BINS=1024"
  done
}

ALL_BENCHMARKS=(
  blackscholes nbody mandelbrot spmv matmul
  weighted_histogram guarded_weighted_histogram
  coo_csr_build graph_messages voxel_rasterization voxel_trilinear_splat
  stencil_interior jacobi_2d kde
)

echo "Running full benchmark CSV sweep (results -> $OUT_DIR)..." >&2

for bname in "${ALL_BENCHMARKS[@]}"; do
  [ -n "$BENCH_FILTER" ] && [ "$bname" != "$BENCH_FILTER" ] && continue
  case "$bname" in
    blackscholes)               sweep_blackscholes ;;
    nbody)                      sweep_nbody ;;
    mandelbrot)                 sweep_mandelbrot ;;
    spmv)                       sweep_spmv ;;
    matmul)                     sweep_matmul ;;
    weighted_histogram)         sweep_weighted_histogram ;;
    guarded_weighted_histogram) sweep_guarded_weighted_histogram ;;
    coo_csr_build)              sweep_coo_csr_build ;;
    graph_messages)             sweep_graph_messages ;;
    voxel_rasterization)        sweep_voxel_rasterization ;;
    voxel_trilinear_splat)      sweep_voxel_trilinear_splat ;;
    stencil_interior)           sweep_stencil_interior ;;
    jacobi_2d)                  sweep_jacobi_2d ;;
    kde)                        sweep_kde ;;
  esac
done

echo "Done." >&2
