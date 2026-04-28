#!/usr/bin/env bash
set -euo pipefail

# Ensure LLVM is on PATH so cabal can find llc/opt for -fllvm compilation.
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

# On macOS, libm is part of libSystem and not found by the default linker search
# path.  Set LIBRARY_PATH to the SDK so the Accelerate JIT linker can resolve -lm.
if [[ "$(uname -s)" == "Darwin" ]] && [[ -z "${LIBRARY_PATH:-}" ]]; then
  SDK_LIB="$(xcrun --sdk macosx --show-sdk-path 2>/dev/null)/usr/lib" || true
  if [[ -d "$SDK_LIB" ]]; then
    export LIBRARY_PATH="$SDK_LIB"
  fi
fi

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$ROOT"

PROJECT="bench/accel/cabal.project"
BENCHES=(
  blackscholes
  nbody
  mandelbrot
  spmv
  matmul
  weighted_histogram
  guarded_weighted_histogram
  graph_messages
  voxel_rasterization
  voxel_trilinear_splat
  softmax
  coo_spmv
)

csv_len() {
  python3 - "$1" <<'PY'
from pathlib import Path
import sys
text = Path(sys.argv[1]).read_text().replace("\n", ",").strip().strip(",")
if not text:
    print(0)
else:
    print(len([part for part in text.split(",") if part.strip()]))
PY
}

csv_shape() {
  python3 - "$1" <<'PY'
from pathlib import Path
import sys
text = Path(sys.argv[1]).read_text().strip()
if not text:
    print("0 0")
    raise SystemExit
rows = [row for row in text.splitlines() if row.strip()]
if len(rows) == 1:
    cols = len([part for part in rows[0].replace(",", " ").split() if part])
    print(f"1 {cols}")
else:
    parsed = [[part for part in row.replace(",", " ").split() if part] for row in rows]
    print(f"{len(parsed)} {len(parsed[0])}")
PY
}

csv_max_plus_one() {
  python3 - "$1" <<'PY'
from pathlib import Path
import sys
text = Path(sys.argv[1]).read_text().replace("\n", ",").strip().strip(",")
vals = [int(float(part)) for part in text.split(",") if part.strip()]
print((max(vals) + 1) if vals else 0)
PY
}

echo "=== Building Accelerate benchmark project ==="
cabal build --project-file="$PROJECT" exe:accel-bench

BS_N="${BS_N:-$(csv_len "$ROOT/bench/blackscholes/spots.csv")}"
NBODY_N="${NBODY_N:-$(csv_len "$ROOT/bench/nbody/xs.csv")}"
SPMV_NROWS="${SPMV_NROWS:-$(( $(csv_len "$ROOT/bench/spmv/row_ptr.csv") - 1 ))}"
SPMV_NCOLS="${SPMV_NCOLS:-$(csv_len "$ROOT/bench/spmv/x.csv")}"
SPMV_NNZ="${SPMV_NNZ:-$(csv_len "$ROOT/bench/spmv/values.csv")}"
read -r MAT_A_ROWS MAT_A_COLS <<<"$(csv_shape "$ROOT/bench/matmul/matA.csv")"
read -r MAT_B_ROWS MAT_B_COLS <<<"$(csv_shape "$ROOT/bench/matmul/matB.csv")"
read -r SOFTMAX_M_FILE SOFTMAX_N_FILE <<<"$(csv_shape "$ROOT/bench/softmax/logits.csv")"
COO_NROWS="${COO_NROWS:-$(csv_max_plus_one "$ROOT/bench/coo_spmv/row_idx.csv")}"
COO_NCOLS="${COO_NCOLS:-$(csv_max_plus_one "$ROOT/bench/coo_spmv/col_idx.csv")}"
COO_NNZ="${COO_NNZ:-$(csv_len "$ROOT/bench/coo_spmv/values.csv")}"
MAT_M="${MAT_M:-$MAT_A_ROWS}"
MAT_K="${MAT_K:-$MAT_A_COLS}"
MAT_N="${MAT_N:-$MAT_B_COLS}"
SOFTMAX_M="${SOFTMAX_M:-$SOFTMAX_M_FILE}"
SOFTMAX_N="${SOFTMAX_N:-$SOFTMAX_N_FILE}"
WH_N="${WH_N:-1000000}"
WH_BINS="${WH_BINS:-256}"
GWH_N="${GWH_N:-1000000}"
GWH_BINS="${GWH_BINS:-256}"
GWH_KEEP_PERIOD="${GWH_KEEP_PERIOD:-3}"
GRAPH_NODES="${GRAPH_NODES:-100000}"
GRAPH_DEGREE="${GRAPH_DEGREE:-16}"
VOX_POINTS="${VOX_POINTS:-1000000}"
VOX_NX="${VOX_NX:-64}"
VOX_NY="${VOX_NY:-64}"
VOX_NZ="${VOX_NZ:-64}"
VOX_KEEP_PERIOD="${VOX_KEEP_PERIOD:-3}"
VSPLAT_POINTS="${VSPLAT_POINTS:-1000000}"
VSPLAT_NX="${VSPLAT_NX:-64}"
VSPLAT_NY="${VSPLAT_NY:-64}"
VSPLAT_NZ="${VSPLAT_NZ:-64}"
VSPLAT_KEEP_PERIOD="${VSPLAT_KEEP_PERIOD:-3}"

for bench in "${BENCHES[@]}"; do
  echo "--- $bench ---"
  case "$bench" in
    blackscholes)
      BS_N="$BS_N" \
        cabal run --project-file="$PROJECT" exe:accel-bench -- "$bench" ;;
    nbody)
      NBODY_N="$NBODY_N" \
        cabal run --project-file="$PROJECT" exe:accel-bench -- "$bench" ;;
    mandelbrot)
      MAND_W="${MAND_W:-1024}" MAND_H="${MAND_H:-1024}" MAND_ITERS="${MAND_ITERS:-256}" \
        cabal run --project-file="$PROJECT" exe:accel-bench -- "$bench" ;;
    spmv)
      SPMV_NROWS="$SPMV_NROWS" SPMV_NCOLS="$SPMV_NCOLS" SPMV_NNZ="$SPMV_NNZ" \
        cabal run --project-file="$PROJECT" exe:accel-bench -- "$bench" ;;
    matmul)
      MAT_M="$MAT_M" MAT_K="$MAT_K" MAT_N="$MAT_N" \
        cabal run --project-file="$PROJECT" exe:accel-bench -- "$bench" ;;
    softmax)
      SOFTMAX_M="$SOFTMAX_M" SOFTMAX_N="$SOFTMAX_N" \
        cabal run --project-file="$PROJECT" exe:accel-bench -- "$bench" ;;
    coo_spmv)
      COO_NROWS="$COO_NROWS" COO_NCOLS="$COO_NCOLS" COO_NNZ="$COO_NNZ" \
        cabal run --project-file="$PROJECT" exe:accel-bench -- "$bench" ;;
    weighted_histogram)
      WH_N="$WH_N" WH_BINS="$WH_BINS" \
        cabal run --project-file="$PROJECT" exe:accel-bench -- "$bench" ;;
    guarded_weighted_histogram)
      GWH_N="$GWH_N" GWH_BINS="$GWH_BINS" GWH_KEEP_PERIOD="$GWH_KEEP_PERIOD" \
        cabal run --project-file="$PROJECT" exe:accel-bench -- "$bench" ;;
    graph_messages)
      GRAPH_NODES="$GRAPH_NODES" GRAPH_DEGREE="$GRAPH_DEGREE" \
        cabal run --project-file="$PROJECT" exe:accel-bench -- "$bench" ;;
    voxel_rasterization)
      VOX_POINTS="$VOX_POINTS" VOX_NX="$VOX_NX" VOX_NY="$VOX_NY" VOX_NZ="$VOX_NZ" \
      VOX_KEEP_PERIOD="$VOX_KEEP_PERIOD" \
        cabal run --project-file="$PROJECT" exe:accel-bench -- "$bench" ;;
    voxel_trilinear_splat)
      VSPLAT_POINTS="$VSPLAT_POINTS" VSPLAT_NX="$VSPLAT_NX" VSPLAT_NY="$VSPLAT_NY" \
      VSPLAT_NZ="$VSPLAT_NZ" VSPLAT_KEEP_PERIOD="$VSPLAT_KEEP_PERIOD" \
        cabal run --project-file="$PROJECT" exe:accel-bench -- "$bench" ;;
  esac
done
