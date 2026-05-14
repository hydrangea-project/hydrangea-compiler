#!/usr/bin/env bash
# bench/run_fusion_case_studies.sh — time all four fusion case study benchmarks
# in three variants: fused, reify-unfused, and --no-fusion.
#
# Outputs a markdown table suitable for pasting into fusion-case-studies.md.
#
# Usage:
#   ./bench/run_fusion_case_studies.sh [--warmup=N] [--iters=N] [--cc=<compiler>]
#
# Environment overrides:
#   CHAIN_N, GWH_N, GWH_BINS, GWH_KEEP_PERIOD, KDE_N, KDE_BINS,
#   VSPLAT_POINTS, VSPLAT_NX, VSPLAT_NY, VSPLAT_NZ, VSPLAT_KEEP_PERIOD

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

WARMUP=3
ITERS=10
CC_OVERRIDE=""

for arg in "$@"; do
  case "$arg" in
    --warmup=*) WARMUP="${arg#--warmup=}" ;;
    --iters=*)  ITERS="${arg#--iters=}" ;;
    --cc=*)     CC_OVERRIDE="${arg#--cc=}" ;;
    *) echo "Unknown option: $arg" >&2; exit 1 ;;
  esac
done

find_openmp_cc() {
  if [ -n "$CC_OVERRIDE" ]; then echo "$CC_OVERRIDE"; return; fi
  for candidate in gcc-15 gcc-14 gcc-13 gcc-12; do
    if command -v "$candidate" >/dev/null 2>&1; then echo "$candidate"; return; fi
  done
  echo "${CC:-cc}"
}
BENCH_CC="$(find_openmp_cc)"

cd "$REPO_ROOT"

echo "Building Hydrangea compiler..." >&2
cabal build hydrangea-compiler >/dev/null 2>&1

# Default benchmark sizes (honour environment overrides)
: "${CHAIN_N:=10000000}"
: "${GWH_N:=1000000}"     "${GWH_BINS:=256}"   "${GWH_KEEP_PERIOD:=3}"
: "${KDE_N:=1000000}"     "${KDE_BINS:=1024}"
: "${VSPLAT_POINTS:=1000000}"
: "${VSPLAT_NX:=64}"      "${VSPLAT_NY:=64}"   "${VSPLAT_NZ:=64}"
: "${VSPLAT_KEEP_PERIOD:=3}"

export CHAIN_N
export GWH_N GWH_BINS GWH_KEEP_PERIOD
export KDE_N KDE_BINS
export VSPLAT_POINTS VSPLAT_NX VSPLAT_NY VSPLAT_NZ VSPLAT_KEEP_PERIOD

parse_min_ms() {
  grep -o 'min=[0-9.]*' | sed 's/min=//' | head -1
}

# run_hyd <hyd-file-relative-to-repo> [extra compiler flags...]
run_hyd() {
  local hyd_file="$1"; shift
  local extra_flags="${*:-}"
  local result
  # shellcheck disable=SC2086
  result=$(cabal run -v0 hydrangea-compiler -- \
    --cc="$BENCH_CC" --tiling --polyhedral \
    $extra_flags \
    --benchmark=main \
    --bench-warmup="$WARMUP" --bench-iters="$ITERS" \
    "$hyd_file" 2>&1 | parse_min_ms) || true
  echo "${result:-N/A}"
}

echo "" >&2
echo "Timing configuration: warmup=$WARMUP iters=$ITERS cc=$BENCH_CC" >&2

# ---------------------------------------------------------------------------
# Case Study 1: map_chain
# ---------------------------------------------------------------------------
echo "  [1/4] map_chain (N=$CHAIN_N) ..." >&2
mc_fused=$(run_hyd bench/map_chain/map_chain.hyd)
mc_unfused=$(run_hyd bench/map_chain/map_chain_unfused.hyd)
mc_nofusion=$(run_hyd bench/map_chain/map_chain.hyd "--no-fusion")
echo "        fused=$mc_fused  unfused=$mc_unfused  no-fusion=$mc_nofusion" >&2

# ---------------------------------------------------------------------------
# Case Study 2: guarded_weighted_histogram
# ---------------------------------------------------------------------------
echo "  [2/4] guarded_weighted_histogram (N=$GWH_N) ..." >&2
gwh_fused=$(run_hyd bench/guarded_weighted_histogram/guarded_weighted_histogram.hyd)
gwh_unfused=$(run_hyd bench/guarded_weighted_histogram/guarded_weighted_histogram_unfused.hyd)
gwh_nofusion=$(run_hyd bench/guarded_weighted_histogram/guarded_weighted_histogram.hyd "--no-fusion")
echo "        fused=$gwh_fused  unfused=$gwh_unfused  no-fusion=$gwh_nofusion" >&2

# ---------------------------------------------------------------------------
# Case Study 3: kde
# ---------------------------------------------------------------------------
echo "  [3/4] kde (N=$KDE_N, bins=$KDE_BINS) ..." >&2
kde_fused=$(run_hyd bench/kde/kde.hyd)
kde_unfused=$(run_hyd bench/kde/kde_unfused.hyd)
kde_nofusion=$(run_hyd bench/kde/kde.hyd "--no-fusion")
echo "        fused=$kde_fused  unfused=$kde_unfused  no-fusion=$kde_nofusion" >&2

# ---------------------------------------------------------------------------
# Case Study 4: voxel_trilinear_splat
# ---------------------------------------------------------------------------
echo "  [4/4] voxel_trilinear_splat (points=$VSPLAT_POINTS, grid=${VSPLAT_NX}x${VSPLAT_NY}x${VSPLAT_NZ}) ..." >&2
vs_fused=$(run_hyd bench/voxel_trilinear_splat/voxel_trilinear_splat.hyd "--no-solver-check")
vs_unfused=$(run_hyd bench/voxel_trilinear_splat/voxel_trilinear_splat_unfused.hyd "--no-solver-check")
vs_nofusion=$(run_hyd bench/voxel_trilinear_splat/voxel_trilinear_splat.hyd "--no-fusion --no-solver-check")
echo "        fused=$vs_fused  unfused=$vs_unfused  no-fusion=$vs_nofusion" >&2

# ---------------------------------------------------------------------------
# Print markdown table to stdout
# ---------------------------------------------------------------------------
printf "\n"
printf "## Fusion case study timing results\n\n"
printf "Compiler: %s | Warmup: %s | Iterations: %s\n\n" "$BENCH_CC" "$WARMUP" "$ITERS"
printf "| Benchmark | Fused (ms) | reify-unfused (ms) | --no-fusion (ms) | Speedup (fused vs reify-unfused) |\n"
printf "|-----------|----------:|------------------:|----------------:|----------------------------------:|\n"

speedup() {
  local fused="$1" unfused="$2"
  if [[ "$fused" == "N/A" || "$unfused" == "N/A" ]]; then
    echo "N/A"
    return
  fi
  python3 -c "print(f'{float(\"$unfused\") / float(\"$fused\"):.2f}×')"
}

printf "| map_chain (N=%s) | %s | %s | %s | %s |\n" \
  "$CHAIN_N" "$mc_fused" "$mc_unfused" "$mc_nofusion" "$(speedup "$mc_fused" "$mc_unfused")"
printf "| guarded_weighted_histogram (N=%s) | %s | %s | %s | %s |\n" \
  "$GWH_N" "$gwh_fused" "$gwh_unfused" "$gwh_nofusion" "$(speedup "$gwh_fused" "$gwh_unfused")"
printf "| kde (N=%s, bins=%s) | %s | %s | %s | %s |\n" \
  "$KDE_N" "$KDE_BINS" "$kde_fused" "$kde_unfused" "$kde_nofusion" "$(speedup "$kde_fused" "$kde_unfused")"
printf "| voxel_trilinear_splat (points=%s) | %s | %s | %s | %s |\n" \
  "$VSPLAT_POINTS" "$vs_fused" "$vs_unfused" "$vs_nofusion" "$(speedup "$vs_fused" "$vs_unfused")"
printf "\n"
