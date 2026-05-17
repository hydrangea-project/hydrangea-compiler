#!/usr/bin/env bash
# bench/plot_full_sweep.sh — render per-benchmark PDFs from full-sweep CSVs.
#
# Usage:
#   ./bench/plot_full_sweep.sh [<benchmark>] [--csv-dir=<dir>] [--out-dir=<dir>]
#
# Input CSV columns:
#   size,hydrangea_ms,repa_ms,accelerate_ms,c_omp_ms
#
# The generated plots are styled for paper use: PDF output, serif fonts, and
# distinct line types/point markers that remain legible when printed in grayscale.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

CSV_DIR="$REPO_ROOT/bench/results/full_sweep"
OUT_DIR="$CSV_DIR/plots"
BENCH_FILTER=""

for arg in "$@"; do
  case "$arg" in
    --csv-dir=*) CSV_DIR="${arg#--csv-dir=}" ;;
    --out-dir=*) OUT_DIR="${arg#--out-dir=}" ;;
    --*) echo "Unknown option: $arg" >&2; exit 1 ;;
    *) BENCH_FILTER="$arg" ;;
  esac
done

if ! command -v gnuplot >/dev/null 2>&1; then
  echo "gnuplot is required but not installed or not on PATH." >&2
  exit 1
fi

mkdir -p "$OUT_DIR"

plot_csv() {
  local csv="$1"
  local base title outfile rows
  base="$(basename "$csv" .csv)"
  title="${base//_/ }"
  outfile="$OUT_DIR/$base.pdf"
  rows=$(( $(wc -l < "$csv") - 1 ))

  if [ "$rows" -le 0 ]; then
    echo "Skipping $csv (no data rows)" >&2
    return
  fi

  gnuplot <<EOF
set terminal pdfcairo size 3.45in,2.45in font "Times,9" enhanced color
set output "$outfile"
set datafile separator comma
set datafile missing "N/A"
set key top left Left reverse box opaque samplen 1.5 spacing 1.1 width 1
set border lw 1.0
set tics out scale 0.75 nomirror
set grid xtics ytics back lc rgb "#d9d9d9" lw 0.6
set logscale x 10
set format x "%.0f"
set format y "%.3g"
set xlabel "Problem size"
set ylabel "Time (ms)"
set title "$title" noenhanced font "Times,9"
set style line 1 lc rgb "#000000" lt 1 lw 2 pt 7 ps 0.7
set style line 2 lc rgb "#444444" lt 2 lw 2 pt 5 ps 0.7
set style line 3 lc rgb "#666666" lt 3 lw 2 pt 9 ps 0.7
set style line 4 lc rgb "#888888" lt 4 lw 2 pt 11 ps 0.7
plot "$csv" using 1:2 with linespoints ls 1 title "Hydrangea", \
     "" using 1:3 with linespoints ls 2 title "Repa", \
     "" using 1:4 with linespoints ls 3 title "Accelerate", \
     "" using 1:5 with linespoints ls 4 title "C+OMP"
unset output
EOF
}

shopt -s nullglob
csv_files=("$CSV_DIR"/*.csv)

if [ "${#csv_files[@]}" -eq 0 ]; then
  echo "No CSV files found in $CSV_DIR" >&2
  exit 1
fi

echo "Rendering plots from $CSV_DIR -> $OUT_DIR" >&2
for csv in "${csv_files[@]}"; do
  base="$(basename "$csv" .csv)"
  if [ -n "$BENCH_FILTER" ] && [ "$base" != "$BENCH_FILTER" ]; then
    continue
  fi
  echo "  $base.pdf" >&2
  plot_csv "$csv"
done

echo "Done." >&2
