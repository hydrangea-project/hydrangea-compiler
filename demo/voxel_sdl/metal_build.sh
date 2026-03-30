#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
DEMO_DIR="$ROOT/demo/voxel_sdl"
OUT_DIR="$DEMO_DIR/out"

mkdir -p "$OUT_DIR"
cd "$ROOT"

# ---------------------------------------------------------------------------
# Stage 1: Generate Metal export
# ---------------------------------------------------------------------------
echo "=== Generating Metal export kernel ==="
cabal run hydrangea-compiler -- \
  --all-top-level-procs --no-solver-check \
  --metal \
  --export-metal-kernel=voxel_render \
  --output-metal="$OUT_DIR/voxel_live.metal" \
  --output-c="$OUT_DIR/voxel_live_metal.m" \
  --output-h="$OUT_DIR/voxel_live_metal.h" \
  "$DEMO_DIR/voxel_live.hyd"

# ---------------------------------------------------------------------------
# Stage 2: Compile .metal → .metallib
# ---------------------------------------------------------------------------
echo "=== Compiling Metal shader ==="
xcrun -sdk macosx metal -c -o "$OUT_DIR/voxel_live.air" "$OUT_DIR/voxel_live.metal"
xcrun -sdk macosx metallib -o "$OUT_DIR/voxel_live.metallib" "$OUT_DIR/voxel_live.air"

# ---------------------------------------------------------------------------
# Stage 3: Compile Metal export library
# ---------------------------------------------------------------------------
echo "=== Building Metal export library ==="

# Stub omp.h so the C helper functions compile without OpenMP
OMP_STUB_DIR="$OUT_DIR/omp_stub"
mkdir -p "$OMP_STUB_DIR"
cat > "$OMP_STUB_DIR/omp.h" << 'OMPEOF'
/* omp.h stub for Metal harness (GPU handles parallelism) */
#ifndef OMP_H_STUB
#define OMP_H_STUB
static inline int omp_get_max_threads(void) { return 1; }
static inline int omp_get_thread_num(void)  { return 0; }
static inline int omp_get_num_threads(void) { return 1; }
#endif
OMPEOF

clang -O2 -fobjc-arc \
  -I"$OUT_DIR" -Iruntime -Ithird_party/simde -I"$OMP_STUB_DIR" \
  -c "$OUT_DIR/voxel_live_metal.m" \
  -o "$OUT_DIR/voxel_live_metal.o"

# ---------------------------------------------------------------------------
# Stage 4: Compile and link C++ SDL harness
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

clang++ -O2 -std=c++17 \
  $SDL2_CFLAGS \
  -DUSE_METAL \
  -I"$OUT_DIR" -Iruntime -Ithird_party/simde \
  -c "$DEMO_DIR/main.cpp" \
  -o "$OUT_DIR/main_metal.o"

echo "=== Linking ==="
clang++ \
  "$OUT_DIR/voxel_live_metal.o" \
  "$OUT_DIR/main_metal.o" \
  $SDL2_LIBS \
  -framework Metal -framework Foundation \
  -o "$OUT_DIR/voxel_sdl_metal"

echo ""
echo "=== Done ==="
echo "Run with:  $OUT_DIR/voxel_sdl_metal"
echo ""
echo "Controls:"
echo "  Space        Pause / resume camera orbit"
echo "  R            Reset camera to start position"
echo "  1 / 2 / 4   Set display scale (1×, 2×, 4×)"
echo "  Q / Escape   Quit"
