#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
DEMO_DIR="$ROOT/demo/voxel_fancy_sdl"
OUT_DIR="$DEMO_DIR/out"
CC_BIN="${DEMO_CC:-${CC:-gcc-15}}"
CXX_BIN="${DEMO_CXX:-${CXX:-g++-15}}"

mkdir -p "$OUT_DIR"
cd "$ROOT"

# ---------------------------------------------------------------------------
# Stage 1: Compile Hydrangea -> C (640x480)
# ---------------------------------------------------------------------------
echo "=== Generating exported Hydrangea kernel (fancy 640x480) ==="
cabal run hydrangea-compiler -- \
  --all-top-level-procs \
  --no-solver-check \
  --output-c="$OUT_DIR/voxel_fancy.c" \
  --output-h="$OUT_DIR/voxel_fancy.h" \
  --export-kernel=voxel_render \
  "$DEMO_DIR/voxel_fancy.hyd"

# ---------------------------------------------------------------------------
# Stage 2: Compile generated C kernel
# ---------------------------------------------------------------------------
echo "=== Building generated C kernel ==="
"$CC_BIN" -O2 -std=c99 -fopenmp \
  -I"$OUT_DIR" -Iruntime -Ithird_party/simde \
  -c "$OUT_DIR/voxel_fancy.c" \
  -o "$OUT_DIR/voxel_fancy.o"

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
  -DRENDER_W=640 -DRENDER_H=480 \
  -I"$OUT_DIR" -Iruntime -Ithird_party/simde \
  -include "$OUT_DIR/voxel_fancy.h" \
  -c "$DEMO_DIR/main.cpp" \
  -o "$OUT_DIR/main.o"

echo "=== Linking ==="
"$CXX_BIN" -fopenmp \
  "$OUT_DIR/voxel_fancy.o" \
  "$OUT_DIR/main.o" \
  $SDL2_LIBS \
  -o "$OUT_DIR/voxel_fancy_sdl"

echo ""
echo "=== Done ==="
echo "Run with:  $OUT_DIR/voxel_fancy_sdl"
echo ""
echo "Controls:"
echo "  Space        Pause / resume camera orbit"
echo "  R            Reset camera to start position"
echo "  1 / 2 / 4   Set display scale (1x, 2x, 4x)"
echo "  Q / Escape   Quit"
