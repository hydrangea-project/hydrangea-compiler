#!/usr/bin/env bash
# bench/plot_full_sweep_normalized.sh — normalize full-sweep CSVs against the
# C+OMP baseline, write gnuplot-ready CSVs, and render per-benchmark PDFs.
#
# Usage:
#   ./bench/plot_full_sweep_normalized.sh [<benchmark>] [--csv-dir=<dir>] [--data-dir=<dir>] [--out-dir=<dir>]
#
# Input CSV columns:
#   size,hydrangea_ms,repa_ms,accelerate_ms,c_omp_ms
#
# Output CSV columns:
#   size,hydrangea_rel,repa_rel,accelerate_rel,c_omp_rel
#
# The generated plots use log scales on both axes. A value of 1 on the y-axis
# corresponds to the C+OMP baseline.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

CSV_DIR="$REPO_ROOT/bench/results/full_sweep"
DATA_DIR="$CSV_DIR/normalized_c_omp"
OUT_DIR="$CSV_DIR/plots_normalized_c_omp"
BENCH_FILTER=""

for arg in "$@"; do
  case "$arg" in
    --csv-dir=*) CSV_DIR="${arg#--csv-dir=}" ;;
    --data-dir=*) DATA_DIR="${arg#--data-dir=}" ;;
    --out-dir=*) OUT_DIR="${arg#--out-dir=}" ;;
    --*) echo "Unknown option: $arg" >&2; exit 1 ;;
    *) BENCH_FILTER="$arg" ;;
  esac
done

if ! command -v python3 >/dev/null 2>&1; then
  echo "python3 is required but not installed or not on PATH." >&2
  exit 1
fi

if ! command -v gnuplot >/dev/null 2>&1; then
  echo "gnuplot is required but not installed or not on PATH." >&2
  exit 1
fi

mkdir -p "$DATA_DIR" "$OUT_DIR"

prepare_csv() {
  local src="$1" dst="$2"
  python3 - "$src" "$dst" <<'PY'
import csv
import sys

src, dst = sys.argv[1], sys.argv[2]
required = ["size", "hydrangea_ms", "repa_ms", "accelerate_ms", "c_omp_ms"]

with open(src, newline="") as infile:
    reader = csv.DictReader(infile)
    if reader.fieldnames is None:
        raise SystemExit(f"{src}: missing CSV header")
    missing = [name for name in required if name not in reader.fieldnames]
    if missing:
        raise SystemExit(f"{src}: missing required columns: {', '.join(missing)}")
    rows = list(reader)

with open(dst, "w", newline="") as outfile:
    writer = csv.DictWriter(
        outfile,
        fieldnames=["size", "hydrangea_rel", "repa_rel", "accelerate_rel", "c_omp_rel"],
    )
    writer.writeheader()
    for row in rows:
        baseline_text = row["c_omp_ms"]
        baseline = None
        if baseline_text and baseline_text != "N/A":
            baseline = float(baseline_text)
            if baseline <= 0:
                baseline = None

        out = {
            "size": row["size"],
            "c_omp_rel": "1" if baseline is not None else "N/A",
        }

        for src_key, dst_key in [
            ("hydrangea_ms", "hydrangea_rel"),
            ("repa_ms", "repa_rel"),
            ("accelerate_ms", "accelerate_rel"),
        ]:
            value_text = row[src_key]
            if baseline is None or not value_text or value_text == "N/A":
                out[dst_key] = "N/A"
            else:
                out[dst_key] = f"{float(value_text) / baseline:.12g}"

        writer.writerow(out)
PY
}

plot_csv() {
  local csv="$1"
  local base title outfile rows plot_cmd
  base="$(basename "$csv" .csv)"
  title="${base//_/ }"
  outfile="$OUT_DIR/$base.pdf"
  rows=$(( $(wc -l < "$csv") - 1 ))

  if [ "$rows" -le 0 ]; then
    echo "Skipping $csv (no data rows)" >&2
    return
  fi

  column_has_values() {
    local data_csv="$1" column="$2"
    awk -F, -v column="$column" '
      NR == 1 { next }
      $column != "" && $column != "N/A" { found = 1; exit }
      END { exit(found ? 0 : 1) }
    ' "$data_csv"
  }

  append_plot() {
    local clause="$1"
    if [ -n "$plot_cmd" ]; then
      plot_cmd+=", "
    fi
    plot_cmd+="$clause"
  }

  plot_cmd=""
  if column_has_values "$csv" 2; then
    append_plot "\"$csv\" using 1:2 with linespoints ls 1 title \"Hydrangea\""
  fi
  if column_has_values "$csv" 3; then
    append_plot "\"$csv\" using 1:3 with linespoints ls 2 title \"Repa\""
  fi
  if column_has_values "$csv" 4; then
    append_plot "\"$csv\" using 1:4 with linespoints ls 3 title \"Accelerate\""
  fi
  if column_has_values "$csv" 5; then
    append_plot "\"$csv\" using 1:5 with lines ls 4 title \"C+OMP baseline\""
  fi

  if [ -z "$plot_cmd" ]; then
    echo "Skipping $csv (no plottable series)" >&2
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
set logscale y 10
set format x "%.0f"
set format y "%.3g"
set xlabel "Problem size"
set ylabel "Runtime / C+OMP"
set title "$title" noenhanced font "Times,9"
set style line 1 lc rgb "#000000" lt 1 lw 2 pt 7 ps 0.7
set style line 2 lc rgb "#444444" lt 2 lw 2 pt 5 ps 0.7
set style line 3 lc rgb "#666666" lt 3 lw 2 pt 9 ps 0.7
set style line 4 lc rgb "#9a9a9a" lt 5 lw 1.5
plot $plot_cmd
unset output
EOF
}

shopt -s nullglob
csv_files=("$CSV_DIR"/*.csv)

if [ "${#csv_files[@]}" -eq 0 ]; then
  echo "No CSV files found in $CSV_DIR" >&2
  exit 1
fi

echo "Preparing normalized CSVs from $CSV_DIR -> $DATA_DIR" >&2
echo "Rendering plots from $DATA_DIR -> $OUT_DIR" >&2

matched=0
for csv in "${csv_files[@]}"; do
  base="$(basename "$csv" .csv)"
  if [ -n "$BENCH_FILTER" ] && [ "$base" != "$BENCH_FILTER" ]; then
    continue
  fi
  matched=1
  data_csv="$DATA_DIR/$base.csv"
  echo "  $base.csv -> $base.pdf" >&2
  prepare_csv "$csv" "$data_csv"
  plot_csv "$data_csv"
done

if [ "$matched" -eq 0 ]; then
  echo "No benchmark matched '$BENCH_FILTER' in $CSV_DIR" >&2
  exit 1
fi

echo "Done." >&2
