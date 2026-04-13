#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
DEMO_DIR="$ROOT/demo/ray_ppm"
OUT_DIR="$DEMO_DIR/out"
CC_BIN="${DEMO_CC:-${CC:-gcc-15}}"
CXX_BIN="${DEMO_CXX:-${CXX:-g++-15}}"
OUTPUT_IMAGE="${1:-$OUT_DIR/ray_still_life.ppm}"

mkdir -p "$OUT_DIR"

cd "$ROOT"

echo "=== Generating exported Hydrangea renderer ==="
cabal run hydrangea-compiler -- \
  --all-top-level-procs \
  --no-solver-check \
  --output-c="$OUT_DIR/ray_ppm.c" \
  --output-h="$OUT_DIR/ray_ppm.h" \
  --export-kernel=ray_ppm \
  "$DEMO_DIR/ray_ppm.hyd"

echo "=== Building generated C kernel ==="
"$CC_BIN" -O2 -std=c99 -fopenmp \
  -I"$OUT_DIR" -Iruntime -Ithird_party/simde \
  -c "$OUT_DIR/ray_ppm.c" \
  -o "$OUT_DIR/ray_ppm.o"

echo "=== Building C++ demo app ==="
"$CXX_BIN" -O2 -std=c++17 -fopenmp \
  -I"$OUT_DIR" -Iruntime -Ithird_party/simde \
  -c "$DEMO_DIR/main.cpp" \
  -o "$OUT_DIR/main.o"

echo "=== Linking demo ==="
"$CXX_BIN" -fopenmp \
  "$OUT_DIR/ray_ppm.o" \
  "$OUT_DIR/main.o" \
  -o "$OUT_DIR/ray_ppm_demo"

echo "=== Rendering PPM ==="
"$OUT_DIR/ray_ppm_demo" "$OUTPUT_IMAGE"
