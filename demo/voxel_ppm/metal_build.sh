#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
DEMO_DIR="$ROOT/demo/voxel_ppm"
OUT_DIR="$DEMO_DIR/out"
OUTPUT_IMAGE="${1:-$OUT_DIR/voxel_demo_metal.ppm}"

mkdir -p "$OUT_DIR"
cd "$ROOT"

echo "=== Compiling Metal kernel + harness ==="
cabal run hydrangea-compiler -- \
  --metal --keep-metal --compile-only \
  "$DEMO_DIR/render.hyd"

# Move generated sources into out dir
mv hydrangea_out.metal "$OUT_DIR/render.metal"
mv hydrangea_out.m     "$OUT_DIR/render_harness.m"

echo "=== Compiling Metal shader ==="
xcrun -sdk macosx metal -c -o "$OUT_DIR/render.air" "$OUT_DIR/render.metal"
xcrun -sdk macosx metallib -o "$OUT_DIR/render.metallib" "$OUT_DIR/render.air"

echo "=== Building Metal harness ==="
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
  -framework Metal -framework Foundation \
  -Iruntime -Ithird_party/simde -I"$OMP_STUB_DIR" \
  -o "$OUT_DIR/render_harness" \
  "$OUT_DIR/render_harness.m" \
  runtime/hyd_write_csv.c

echo "=== Running Metal renderer ==="
time "$OUT_DIR/render_harness" "$OUT_DIR/render.metallib" > "$OUT_DIR/render_raw.txt"

echo "=== Converting to PPM ==="
python3 - "$OUT_DIR/render_raw.txt" "$OUTPUT_IMAGE" 1080 1920 << 'PYEOF'
import sys, math, re

def clamp01(x):
    return max(0.0, min(1.0, x))

def smoothstep(edge0, edge1, x):
    t = clamp01((x - edge0) / max(1e-9, edge1 - edge0))
    return t * t * (3.0 - 2.0 * t)

def lerp(a, b, t):
    return tuple(a[i] * (1.0 - t) + b[i] * t for i in range(3))

def colorize(t):
    t = clamp01(t)
    shadow = smoothstep(0.0, 0.45, t)
    highlight = smoothstep(0.45, 1.0, t)
    deep = (0.05, 0.08, 0.16)
    mid = (0.9, 0.56, 0.2)
    hot = (1.0, 0.93, 0.8)
    return lerp(lerp(deep, mid, shadow), hot, 0.7 * highlight)

def background(v):
    zenith = (0.02, 0.03, 0.07)
    horizon = (0.12, 0.08, 0.1)
    return lerp(horizon, zenith, clamp01(v))

raw_path, out_path = sys.argv[1], sys.argv[2]
height, width = int(sys.argv[3]), int(sys.argv[4])

with open(raw_path) as f:
    text = f.read()
vals = [float(x) for x in re.findall(r'[+-]?(?:\d+\.?\d*|\.\d+)(?:[eE][+-]?\d+)?', text)]

assert len(vals) == height * width, f"expected {height*width} values, got {len(vals)}"

max_lum = max(vals) if vals else 0.0

pixels = bytearray(height * width * 3)
for y in range(height):
    for x in range(width):
        idx = y * width + x
        lum = vals[idx]
        normalized = lum / max_lum if max_lum > 0 else 0.0
        v = 1.0 - y / max(1, height - 1)
        nxv = (2.0 * (x + 0.5) / width) - 1.0
        nyv = (2.0 * (y + 0.5) / height) - 1.0
        vignette = 1.0 - 0.18 * clamp01(nxv * nxv + 0.7 * nyv * nyv)
        bg = tuple(vignette * c for c in background(v))
        fg = colorize(normalized ** 0.82)
        blend = smoothstep(0.02, 0.16, normalized)
        color = lerp(bg, fg, blend)
        off = idx * 3
        pixels[off]     = max(0, min(255, int(255.0 * color[0])))
        pixels[off + 1] = max(0, min(255, int(255.0 * color[1])))
        pixels[off + 2] = max(0, min(255, int(255.0 * color[2])))

with open(out_path, 'wb') as f:
    f.write(f"P6\n{width} {height}\n255\n".encode())
    f.write(pixels)

print(f"Wrote {out_path} ({width}x{height})")
PYEOF
