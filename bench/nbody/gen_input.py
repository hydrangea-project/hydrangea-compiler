#!/usr/bin/env python3
"""Generate input data for the N-body benchmark.
Usage: python3 gen_input.py <N>
Writes xs.csv, ys.csv, zs.csv, ms.csv, vxs.csv, vys.csv, vzs.csv
"""
import sys
import random
import os

def main():
    n = int(sys.argv[1]) if len(sys.argv) > 1 else 1000
    random.seed(7)
    outdir = os.path.dirname(os.path.abspath(__file__))

    def write_csv(name, vals):
        path = os.path.join(outdir, name)
        with open(path, 'w') as f:
            f.write(','.join(f'{v:.6f}' for v in vals))
        print(f"Wrote {path} ({n} values)")

    write_csv("xs.csv",  [random.uniform(-1.0, 1.0) for _ in range(n)])
    write_csv("ys.csv",  [random.uniform(-1.0, 1.0) for _ in range(n)])
    write_csv("zs.csv",  [random.uniform(-1.0, 1.0) for _ in range(n)])
    write_csv("ms.csv",  [random.uniform(1e20, 1e24) for _ in range(n)])
    write_csv("vxs.csv", [random.uniform(-100.0, 100.0) for _ in range(n)])
    write_csv("vys.csv", [random.uniform(-100.0, 100.0) for _ in range(n)])
    write_csv("vzs.csv", [random.uniform(-100.0, 100.0) for _ in range(n)])

if __name__ == '__main__':
    main()
