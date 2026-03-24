#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
DEMO_DIR="$ROOT/demo/voxel_ppm"
OUT_DIR="$DEMO_DIR/out"
CC_BIN="${DEMO_CC:-${CC:-gcc-15}}"
CXX_BIN="${DEMO_CXX:-${CXX:-g++-15}}"
OUTPUT_IMAGE="${1:-$OUT_DIR/voxel_demo.ppm}"
IMAGE_SIZE="${2:-${DEMO_IMAGE_SIZE:-1920x1080}}"

mkdir -p "$OUT_DIR"

cd "$ROOT"

echo "=== Generating exported Hydrangea kernel ==="
cabal run hydrangea-compiler -- \
  --no-solver-check \
  --output-c="$OUT_DIR/voxel_scene.c" \
  --output-h="$OUT_DIR/voxel_scene.h" \
  --export-kernel=voxel_scene \
  "$DEMO_DIR/voxel_scene.hyd"

echo "=== Building generated C kernel ==="
"$CC_BIN" -O2 -std=c99 -fopenmp \
  -I"$OUT_DIR" -Iruntime -Ithird_party/simde \
  -c "$OUT_DIR/voxel_scene.c" \
  -o "$OUT_DIR/voxel_scene.o"

echo "=== Building C++ demo app ==="
"$CXX_BIN" -O2 -std=c++17 \
  -I"$OUT_DIR" -Iruntime -Ithird_party/simde \
  -c "$DEMO_DIR/main.cpp" \
  -o "$OUT_DIR/main.o"

echo "=== Linking demo ==="
"$CXX_BIN" -fopenmp \
  "$OUT_DIR/voxel_scene.o" \
  "$OUT_DIR/main.o" \
  -o "$OUT_DIR/voxel_demo"

echo "=== Rendering PPM ==="
if [ -n "$IMAGE_SIZE" ]; then
  "$OUT_DIR/voxel_demo" "$OUTPUT_IMAGE" "$IMAGE_SIZE"
else
  "$OUT_DIR/voxel_demo" "$OUTPUT_IMAGE"
fi
