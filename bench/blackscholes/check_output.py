#!/usr/bin/env python3
"""Compare Black-Scholes benchmark output against the deterministic reference."""

from __future__ import annotations

import argparse
import math
from pathlib import Path
import sys


def read_csv(path: Path) -> list[float]:
    text = path.read_text().strip()
    if not text:
        return []
    return [float(part) for part in text.split(",")]


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Compare bench/blackscholes/out.csv against reference.csv."
    )
    parser.add_argument(
        "--output",
        type=Path,
        default=Path("bench/blackscholes/out.csv"),
        help="Hydrangea-generated CSV output to validate.",
    )
    parser.add_argument(
        "--reference",
        type=Path,
        default=Path("bench/blackscholes/reference.csv"),
        help="Reference CSV output to compare against.",
    )
    parser.add_argument(
        "--tolerance",
        type=float,
        default=1e-4,
        help="Maximum allowed absolute error per output element.",
    )
    args = parser.parse_args()

    if args.tolerance < 0.0:
        parser.error("--tolerance must be non-negative")

    output_vals = read_csv(args.output)
    reference_vals = read_csv(args.reference)

    if len(output_vals) != len(reference_vals):
        print(
            f"length mismatch: output has {len(output_vals)} values, "
            f"reference has {len(reference_vals)}",
            file=sys.stderr,
        )
        return 1

    worst_idx = -1
    worst_diff = 0.0
    for i, (out_val, ref_val) in enumerate(zip(output_vals, reference_vals)):
        diff = abs(out_val - ref_val)
        if diff > worst_diff:
            worst_idx = i
            worst_diff = diff
        if not math.isfinite(out_val):
            print(f"non-finite output at index {i}: {out_val}", file=sys.stderr)
            return 1
        if diff > args.tolerance:
            print(
                f"mismatch at index {i}: output={out_val:.17g}, "
                f"reference={ref_val:.17g}, abs_diff={diff:.17g}, "
                f"tolerance={args.tolerance:.17g}",
                file=sys.stderr,
            )
            return 1

    print(
        f"validated {len(output_vals)} values; max_abs_diff={worst_diff:.17g}"
        + (f" at index {worst_idx}" if worst_idx >= 0 else ""),
        file=sys.stderr,
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
