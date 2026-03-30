#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
DEMO_DIR="$ROOT/demo/voxel_sdl"
OUT_DIR="$DEMO_DIR/out"
CC_BIN="${DEMO_CC:-${CC:-gcc-15}}"
CXX_BIN="${DEMO_CXX:-${CXX:-g++-15}}"

mkdir -p "$OUT_DIR"
cd "$ROOT"

# ---------------------------------------------------------------------------
# Stage 1: Compile Hydrangea → C (320×240)
# ---------------------------------------------------------------------------
echo "=== Generating exported Hydrangea kernel (320×240) ==="
cabal run hydrangea-compiler -- \
  --all-top-level-procs \
  --no-solver-check \
  --output-c="$OUT_DIR/voxel_live_320.c" \
  --output-h="$OUT_DIR/voxel_live_320.h" \
  --export-kernel=voxel_render \
  "$DEMO_DIR/voxel_live_320.hyd"

# ---------------------------------------------------------------------------
# Stage 2: Compile generated C kernel
# ---------------------------------------------------------------------------
echo "=== Building generated C kernel ==="
"$CC_BIN" -O2 -std=c99 -fopenmp \
  -I"$OUT_DIR" -Iruntime -Ithird_party/simde \
  -c "$OUT_DIR/voxel_live_320.c" \
  -o "$OUT_DIR/voxel_live_320.o"

# ---------------------------------------------------------------------------
# Stage 3: Compile and link C++ SDL harness
# ---------------------------------------------------------------------------
echo "=== Building SDL harness ==="

if command -v sdl2-config &>/dev/null; then
  SDL2_CFLAGS="$(sdl2-config --cflags)"
  SDL2_LIBS="$(sdl2-config --libs)"
else
  echo "Warning: sdl2-config not found, trying -lSDL2 directly"
  SDL2_CFLAGS=""
  SDL2_LIBS="-lSDL2"
fi

"$CXX_BIN" -O2 -std=c++17 -fopenmp \
  $SDL2_CFLAGS \
  -DRENDER_W=320 -DRENDER_H=240 \
  -I"$OUT_DIR" -Iruntime -Ithird_party/simde \
  -include "$OUT_DIR/voxel_live_320.h" \
  -c "$DEMO_DIR/main.cpp" \
  -o "$OUT_DIR/main_320.o"

echo "=== Linking ==="
"$CXX_BIN" -fopenmp \
  "$OUT_DIR/voxel_live_320.o" \
  "$OUT_DIR/main_320.o" \
  $SDL2_LIBS \
  -o "$OUT_DIR/voxel_sdl_320"

echo ""
echo "=== Done ==="
echo "Run with:  $OUT_DIR/voxel_sdl_320"
echo ""
echo "Controls:"
echo "  Space        Pause / resume camera orbit"
echo "  R            Reset camera to start position"
echo "  1 / 2 / 4   Set display scale (1×, 2×, 4×)"
echo "  Q / Escape   Quit"
