#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
DEMO_DIR="$ROOT/demo/point_cloud_sdl"
OUT_DIR="$DEMO_DIR/out"
CC_BIN="${DEMO_CC:-${CC:-gcc-15}}"
CXX_BIN="${DEMO_CXX:-${CXX:-g++-15}}"
BUNNY_CSV="$OUT_DIR/bunny_points.csv"
BUNNY_ARCHIVE_URL="http://graphics.stanford.edu/pub/3Dscanrep/bunny.tar.gz"
BUNNY_PLY_REL="bunny/reconstruction/bun_zipper_res2.ply"
PYTHON_BIN="${PYTHON:-}"

mkdir -p "$OUT_DIR"
cd "$ROOT"

echo "=== Preparing Stanford bunny point cloud ==="
if [[ ! -f "$BUNNY_CSV" ]]; then
  if ! command -v curl >/dev/null 2>&1; then
    echo "Error: curl is required to download the Stanford bunny archive." >&2
    exit 1
  fi
  if [[ -z "$PYTHON_BIN" ]]; then
    if command -v python >/dev/null 2>&1; then
      PYTHON_BIN="$(command -v python)"
    elif command -v python3 >/dev/null 2>&1; then
      PYTHON_BIN="$(command -v python3)"
    fi
  fi
  if [[ -z "$PYTHON_BIN" ]]; then
    echo "Error: python or python3 is required to convert the Stanford bunny PLY file." >&2
    exit 1
  fi

  tmpdir="$(mktemp -d)"
  trap 'rm -rf "$tmpdir"' EXIT
  curl -L --fail -o "$tmpdir/bunny.tar.gz" "$BUNNY_ARCHIVE_URL"
  tar -xzf "$tmpdir/bunny.tar.gz" -C "$tmpdir"

  "$PYTHON_BIN" - "$tmpdir/$BUNNY_PLY_REL" "$BUNNY_CSV" <<'PY'
import csv
import math
import sys

src, dst = sys.argv[1], sys.argv[2]
with open(src, "r", encoding="utf-8") as f:
    header_done = False
    vertex_count = None
    points = []
    for line in f:
        line = line.strip()
        if not header_done:
            if line.startswith("element vertex "):
                vertex_count = int(line.split()[-1])
            elif line == "end_header":
                header_done = True
            continue
        if not line:
            continue
        vals = line.split()
        if len(vals) < 3:
            continue
        points.append((float(vals[0]), float(vals[1]), float(vals[2])))
        if vertex_count is not None and len(points) >= vertex_count:
            break

if not points:
    raise SystemExit("No bunny vertices found in PLY.")

mins = [min(p[i] for p in points) for i in range(3)]
maxs = [max(p[i] for p in points) for i in range(3)]
center = [(lo + hi) * 0.5 for lo, hi in zip(mins, maxs)]
extent = max(hi - lo for lo, hi in zip(mins, maxs))
if extent <= 0.0:
    raise SystemExit("Degenerate bunny bounding box.")
scale = 2.0 / extent

with open(dst, "w", encoding="utf-8", newline="") as f:
    writer = csv.writer(f)
    for x, y, z in points:
        writer.writerow([
            (x - center[0]) * scale,
            (y - center[1]) * scale,
            (z - center[2]) * scale,
        ])
PY
else
  echo "Using cached bunny CSV: $BUNNY_CSV"
fi

echo "=== Generating exported Hydrangea kernel ==="
cabal run hydrangea-compiler -- \
  --all-top-level-procs \
  --no-solver-check \
  --output-c="$OUT_DIR/point_cloud_live.c" \
  --output-h="$OUT_DIR/point_cloud_live.h" \
  --export-kernel=point_cloud_frame \
  "$DEMO_DIR/point_cloud_live.hyd"

echo "=== Building generated C kernel ==="
"$CC_BIN" -O2 -std=c99 -fopenmp \
  -I"$OUT_DIR" -Iruntime -Ithird_party/simde \
  -c "$OUT_DIR/point_cloud_live.c" \
  -o "$OUT_DIR/point_cloud_live.o"

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
  "$OUT_DIR/point_cloud_live.o" \
  "$OUT_DIR/main.o" \
  $SDL2_LIBS \
  -o "$OUT_DIR/point_cloud_sdl"

echo ""
echo "=== Done ==="
echo "Run with:  $OUT_DIR/point_cloud_sdl"
echo ""
echo "Data source:"
echo "  Official Stanford bunny archive fetched on demand into: $BUNNY_CSV"
echo ""
echo "Controls:"
echo "  Left mouse drag        Rotate cloud"
echo "  1 / 2 / 3              Resolve mode: min / + / max"
echo "  Tab / M                Cycle resolve modes"
echo "  R                      Reset rotation and mode"
echo "  Q / Escape             Quit"
