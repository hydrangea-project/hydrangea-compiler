#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
DEMO_DIR="$ROOT/demo/pic_sdl"
OUT_DIR="$DEMO_DIR/out"
CC_BIN="${DEMO_CC:-${CC:-gcc-15}}"
CXX_BIN="${DEMO_CXX:-${CXX:-g++-15}}"

mkdir -p "$OUT_DIR"
cd "$ROOT"

# ---------------------------------------------------------------------------
# Stage 1: Compile Hydrangea -> C
# ---------------------------------------------------------------------------
echo "=== Generating exported Hydrangea kernel ==="
cabal run hydrangea-compiler -- \
  --all-top-level-procs \
  --no-solver-check \
  --output-c="$OUT_DIR/pic_density.c" \
  --output-h="$OUT_DIR/pic_density.h" \
  --export-kernel=pic_step \
  "$DEMO_DIR/pic_density.hyd"

# ---------------------------------------------------------------------------
# Stage 2: Compile generated C kernel
# ---------------------------------------------------------------------------
echo "=== Building generated C kernel ==="
"$CC_BIN" -O2 -std=c99 -fopenmp \
  -I"$OUT_DIR" -Iruntime -Ithird_party/simde \
  -c "$OUT_DIR/pic_density.c" \
  -o "$OUT_DIR/pic_density.o"

# ---------------------------------------------------------------------------
# Stage 3: Compile and link C++ SDL harness
# ---------------------------------------------------------------------------
echo "=== Building SDL harness ==="

# Detect SDL2 via sdl2-config; fall back to bare -lSDL2
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
  -I"$OUT_DIR" -Iruntime -Ithird_party/simde \
  -c "$DEMO_DIR/main.cpp" \
  -o "$OUT_DIR/main.o"

echo "=== Linking ==="
"$CXX_BIN" -fopenmp \
  "$OUT_DIR/pic_density.o" \
  "$OUT_DIR/main.o" \
  $SDL2_LIBS \
  -o "$OUT_DIR/pic_sdl"

echo ""
echo "=== Done ==="
echo "Run with:  $OUT_DIR/pic_sdl"
echo ""
echo "Controls:"
echo "  Left mouse click/drag  Move the attractor"
echo "  R                      Reset simulation"
echo "  + / -                  Steps per frame (speed)"
echo "  Space                  Pause / resume"
echo "  Q / Escape             Quit"
