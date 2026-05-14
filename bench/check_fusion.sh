#!/usr/bin/env bash
# bench/check_fusion.sh — verify that fused and unfused variants of the four
# fusion case study benchmarks produce identical results.
#
# Each benchmark returns a scalar checksum (reduce over the output array).
# Integer checksums must match exactly; floating-point checksums must agree to
# a relative tolerance (default 1e-6, overridable via --tol=).
#
# Usage:
#   ./bench/check_fusion.sh [--cc=<compiler>] [--tol=<float>]
#
# Environment overrides for problem sizes (kept small for speed):
#   CHECK_CHAIN_N, CHECK_GWH_N, CHECK_GWH_BINS, CHECK_GWH_KEEP_PERIOD,
#   CHECK_KDE_N, CHECK_KDE_BINS,
#   CHECK_VSPLAT_POINTS, CHECK_VSPLAT_NX, CHECK_VSPLAT_NY, CHECK_VSPLAT_NZ,
#   CHECK_VSPLAT_KEEP_PERIOD
#
# Exit code: 0 = all pass, 1 = one or more failures.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

FLOAT_TOL="${FLOAT_TOL:-1e-6}"
CC_OVERRIDE=""
FAILED=0

for arg in "$@"; do
  case "$arg" in
    --cc=*)  CC_OVERRIDE="${arg#--cc=}" ;;
    --tol=*) FLOAT_TOL="${arg#--tol=}" ;;
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

# get_result <hyd-path-relative-to-repo-root> [extra compiler flags...]
# Compiles and runs the benchmark once, extracts the scalar result from stdout.
# Handles two output formats:
#   Array (()) Float/Int  →  "[value] (shape: [])"
#   plain scalar          →  "value"   (bare number on its own line)
get_result() {
  local hyd_file="$1"; shift
  local extra_flags="${*:-}"
  local raw
  # shellcheck disable=SC2086
  raw=$(cabal run -v0 hydrangea-compiler -- \
    --cc="$BENCH_CC" --tiling --polyhedral \
    $extra_flags \
    "$hyd_file" 2>/dev/null)
  python3 - "$raw" <<'PYEOF'
import sys, re
data = sys.argv[1]
# Bracket format: "[value] (shape: [])"
m = re.search(r'\[(-?[0-9][0-9.eE+\-]*)\]\s*\(shape', data)
if m:
    print(m.group(1))
    sys.exit(0)
# Bare number on its own line
m = re.search(r'^(-?[0-9][0-9.eE+\-]*)$', data.strip(), re.MULTILINE)
if m:
    print(m.group(1))
    sys.exit(0)
sys.exit(1)
PYEOF
}

# check_int <label> <a> <b>  — fail if a != b
check_int() {
  local label="$1" a="$2" b="$3"
  if [ "$a" = "$b" ]; then
    printf "  PASS  %-55s (%s)\n" "$label" "$a"
  else
    printf "  FAIL  %-55s fused=%s unfused=%s\n" "$label" "$a" "$b"
    FAILED=$((FAILED + 1))
  fi
}

# check_float <label> <a> <b>  — fail if relative error > FLOAT_TOL
check_float() {
  local label="$1" a="$2" b="$3"
  local result
  result=$(python3 - "$a" "$b" "$FLOAT_TOL" <<'EOF'
import sys
a, b, tol = float(sys.argv[1]), float(sys.argv[2]), float(sys.argv[3])
rel = abs(a - b) / max(abs(a), abs(b), 1e-300)
status = "PASS" if rel <= tol else "FAIL"
print(f"{status} a={a:.6g} b={b:.6g} rel_err={rel:.2e}")
EOF
)
  if [[ "$result" == PASS* ]]; then
    printf "  PASS  %-55s (%s)\n" "$label" "${result#PASS }"
  else
    printf "  FAIL  %-55s (%s)\n" "$label" "${result#FAIL }"
    FAILED=$((FAILED + 1))
  fi
}

# Problem sizes — small enough to run quickly, large enough to catch sign errors
: "${CHECK_CHAIN_N:=100000}"
: "${CHECK_GWH_N:=100000}"   "${CHECK_GWH_BINS:=256}"  "${CHECK_GWH_KEEP_PERIOD:=3}"
: "${CHECK_KDE_N:=100000}"   "${CHECK_KDE_BINS:=1024}"
: "${CHECK_VSPLAT_POINTS:=10000}"
: "${CHECK_VSPLAT_NX:=16}"   "${CHECK_VSPLAT_NY:=16}"  "${CHECK_VSPLAT_NZ:=16}"
: "${CHECK_VSPLAT_KEEP_PERIOD:=3}"

echo ""
echo "=== Fusion correctness checks (cc=$BENCH_CC, tol=$FLOAT_TOL) ==="
echo ""

# ---------------------------------------------------------------------------
# Case Study 1: map_chain (float checksum)
# ---------------------------------------------------------------------------
echo "map_chain (CHAIN_N=$CHECK_CHAIN_N)"
r_fused=$(CHAIN_N="$CHECK_CHAIN_N" get_result bench/map_chain/map_chain.hyd)
r_unfused=$(CHAIN_N="$CHECK_CHAIN_N" get_result bench/map_chain/map_chain_unfused.hyd)
r_nofusion=$(CHAIN_N="$CHECK_CHAIN_N" get_result bench/map_chain/map_chain.hyd "--no-fusion")
check_float "map_chain: fused vs reify-unfused" "$r_fused" "$r_unfused"
check_float "map_chain: fused vs --no-fusion"   "$r_fused" "$r_nofusion"
echo ""

# ---------------------------------------------------------------------------
# Case Study 2: guarded_weighted_histogram (integer checksum)
# ---------------------------------------------------------------------------
echo "guarded_weighted_histogram (N=$CHECK_GWH_N, bins=$CHECK_GWH_BINS, keep_period=$CHECK_GWH_KEEP_PERIOD)"
r_fused=$(GWH_N="$CHECK_GWH_N" GWH_BINS="$CHECK_GWH_BINS" GWH_KEEP_PERIOD="$CHECK_GWH_KEEP_PERIOD" \
  get_result bench/guarded_weighted_histogram/guarded_weighted_histogram.hyd)
r_unfused=$(GWH_N="$CHECK_GWH_N" GWH_BINS="$CHECK_GWH_BINS" GWH_KEEP_PERIOD="$CHECK_GWH_KEEP_PERIOD" \
  get_result bench/guarded_weighted_histogram/guarded_weighted_histogram_unfused.hyd)
r_nofusion=$(GWH_N="$CHECK_GWH_N" GWH_BINS="$CHECK_GWH_BINS" GWH_KEEP_PERIOD="$CHECK_GWH_KEEP_PERIOD" \
  get_result bench/guarded_weighted_histogram/guarded_weighted_histogram.hyd "--no-fusion")
check_int "gwh: fused vs reify-unfused" "$r_fused" "$r_unfused"
check_int "gwh: fused vs --no-fusion"   "$r_fused" "$r_nofusion"
echo ""

# ---------------------------------------------------------------------------
# Case Study 3: kde (integer checksum)
# ---------------------------------------------------------------------------
echo "kde (N=$CHECK_KDE_N, bins=$CHECK_KDE_BINS)"
r_fused=$(KDE_N="$CHECK_KDE_N" KDE_BINS="$CHECK_KDE_BINS" \
  get_result bench/kde/kde.hyd)
r_unfused=$(KDE_N="$CHECK_KDE_N" KDE_BINS="$CHECK_KDE_BINS" \
  get_result bench/kde/kde_unfused.hyd)
r_nofusion=$(KDE_N="$CHECK_KDE_N" KDE_BINS="$CHECK_KDE_BINS" \
  get_result bench/kde/kde.hyd "--no-fusion")
check_int "kde: fused vs reify-unfused" "$r_fused" "$r_unfused"
check_int "kde: fused vs --no-fusion"   "$r_fused" "$r_nofusion"
echo ""

# ---------------------------------------------------------------------------
# Case Study 4: voxel_trilinear_splat (float checksum)
# ---------------------------------------------------------------------------
echo "voxel_trilinear_splat (points=$CHECK_VSPLAT_POINTS, grid=${CHECK_VSPLAT_NX}x${CHECK_VSPLAT_NY}x${CHECK_VSPLAT_NZ})"
export VSPLAT_POINTS="$CHECK_VSPLAT_POINTS" \
       VSPLAT_NX="$CHECK_VSPLAT_NX" VSPLAT_NY="$CHECK_VSPLAT_NY" VSPLAT_NZ="$CHECK_VSPLAT_NZ" \
       VSPLAT_KEEP_PERIOD="$CHECK_VSPLAT_KEEP_PERIOD"
r_fused=$(get_result bench/voxel_trilinear_splat/voxel_trilinear_splat.hyd "--no-solver-check")
r_unfused=$(get_result bench/voxel_trilinear_splat/voxel_trilinear_splat_unfused.hyd "--no-solver-check")
r_nofusion=$(get_result bench/voxel_trilinear_splat/voxel_trilinear_splat.hyd "--no-fusion --no-solver-check")
check_float "voxel_trilinear_splat: fused vs reify-unfused" "$r_fused" "$r_unfused"
check_float "voxel_trilinear_splat: fused vs --no-fusion"   "$r_fused" "$r_nofusion"
echo ""

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
if [ "$FAILED" -eq 0 ]; then
  echo "All fusion correctness checks passed."
  exit 0
else
  echo "$FAILED check(s) FAILED."
  exit 1
fi
