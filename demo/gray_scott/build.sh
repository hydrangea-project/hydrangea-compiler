#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
DEMO_DIR="$ROOT/demo/gray_scott"
OUT_DIR="$DEMO_DIR/out"
CC_BIN="${DEMO_CC:-${CC:-gcc-15}}"
CXX_BIN="${DEMO_CXX:-${CXX:-g++-15}}"
OUTPUT_IMAGE="${1:-$OUT_DIR/gs_demo.ppm}"

mkdir -p "$OUT_DIR"

cd "$ROOT"

echo "=== Generating exported Hydrangea kernel ==="
cabal run hydrangea-compiler -- \
  --all-top-level-procs \
  --no-solver-check \
  --output-c="$OUT_DIR/gray_scott.c" \
  --output-h="$OUT_DIR/gray_scott.h" \
  --export-kernel=gray_scott \
  "$DEMO_DIR/gray_scott.hyd"

echo "=== Building generated C kernel ==="
"$CC_BIN" -O2 -std=c99 -fopenmp \
  -I"$OUT_DIR" -Iruntime -Ithird_party/simde \
  -c "$OUT_DIR/gray_scott.c" \
  -o "$OUT_DIR/gray_scott.o"

echo "=== Building C++ demo app ==="
"$CXX_BIN" -O2 -std=c++17 -fopenmp \
  -I"$OUT_DIR" -Iruntime -Ithird_party/simde \
  -c "$DEMO_DIR/main.cpp" \
  -o "$OUT_DIR/main.o"

echo "=== Linking demo ==="
"$CXX_BIN" -fopenmp \
  "$OUT_DIR/gray_scott.o" \
  "$OUT_DIR/main.o" \
  -o "$OUT_DIR/gray_scott_demo"

echo "=== Running simulation ==="
"$OUT_DIR/gray_scott_demo" "$OUTPUT_IMAGE"
