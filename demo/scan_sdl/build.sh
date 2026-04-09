#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
DEMO_DIR="$ROOT/demo/scan_sdl"
OUT_DIR="$DEMO_DIR/out"
CC_BIN="${DEMO_CC:-${CC:-gcc-15}}"
CXX_BIN="${DEMO_CXX:-${CXX:-g++-15}}"

mkdir -p "$OUT_DIR"
cd "$ROOT"

echo "=== Generating exported Hydrangea kernel ==="
cabal run hydrangea-compiler -- \
  --all-top-level-procs \
  --no-solver-check \
  --output-c="$OUT_DIR/scan_live.c" \
  --output-h="$OUT_DIR/scan_live.h" \
  --export-kernel=scan_frame \
  "$DEMO_DIR/scan_live.hyd"

echo "=== Building generated C kernel ==="
"$CC_BIN" -O2 -std=c99 -fopenmp \
  -I"$OUT_DIR" -Iruntime -Ithird_party/simde \
  -c "$OUT_DIR/scan_live.c" \
  -o "$OUT_DIR/scan_live.o"

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
  -I"$OUT_DIR" -Iruntime -Ithird_party/simde \
  -c "$DEMO_DIR/main.cpp" \
  -o "$OUT_DIR/main.o"

echo "=== Linking ==="
"$CXX_BIN" -fopenmp \
  "$OUT_DIR/scan_live.o" \
  "$OUT_DIR/main.o" \
  $SDL2_LIBS \
  -o "$OUT_DIR/scan_sdl"

echo ""
echo "=== Done ==="
echo "Run with:  $OUT_DIR/scan_sdl"
echo ""
echo "Controls:"
echo "  Left mouse click/drag  Move local emitter"
echo "  R                      Reset frame counter and emitter position"
echo "  1 / 2 / 4              Set display scale (1×, 2×, 4×)"
echo "  + / -                  Steps per frame (animation speed)"
echo "  Space                  Pause / resume"
echo "  Q / Escape             Quit"

