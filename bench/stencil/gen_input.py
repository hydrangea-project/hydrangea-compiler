#!/usr/bin/env python3
"""Generate a height x width CSV of float values for the stencil benchmark.

Usage:
  python3 gen_input.py <height> <width> [output_path]

Default output: bench/stencil/input.csv
Values are a deterministic pattern: sin(i*0.1 + j*0.07) scaled to [0,1].
"""

import math
import sys

def main():
    if len(sys.argv) < 3:
        print(f"usage: {sys.argv[0]} <height> <width> [output_path]", file=sys.stderr)
        sys.exit(1)

    h = int(sys.argv[1])
    w = int(sys.argv[2])
    out_path = sys.argv[3] if len(sys.argv) > 3 else "bench/stencil/input.csv"

    total = h * w
    values = [
        0.5 * (1.0 + math.sin(i * 0.1 + j * 0.07))
        for i in range(h)
        for j in range(w)
    ]

    with open(out_path, "w") as f:
        f.write(",".join(f"{v:.17g}" for v in values))
        f.write("\n")

    print(f"Generated {h}x{w} input at {out_path}")

if __name__ == "__main__":
    main()
