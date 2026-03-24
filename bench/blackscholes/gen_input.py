#!/usr/bin/env python3
"""Generate input data for the Black-Scholes benchmark.
Usage: python3 gen_input.py <N>
Writes spots.csv, strikes.csv, rates.csv, vols.csv, times.csv
"""
import sys
import random
import math
import os

def main():
    n = int(sys.argv[1]) if len(sys.argv) > 1 else 1000
    random.seed(42)
    outdir = os.path.dirname(os.path.abspath(__file__))

    spots   = [round(random.uniform(50.0, 150.0), 4) for _ in range(n)]
    strikes = [round(random.uniform(50.0, 150.0), 4) for _ in range(n)]
    rates   = [round(random.uniform(0.01, 0.10),  6) for _ in range(n)]
    vols    = [round(random.uniform(0.10, 0.50),  6) for _ in range(n)]
    times   = [round(random.uniform(0.25, 2.0),   4) for _ in range(n)]

    def write_csv(name, vals):
        path = os.path.join(outdir, name)
        with open(path, 'w') as f:
            f.write(','.join(str(v) for v in vals))
        print(f"Wrote {path} ({n} values)")

    write_csv("spots.csv",   spots)
    write_csv("strikes.csv", strikes)
    write_csv("rates.csv",   rates)
    write_csv("vols.csv",    vols)
    write_csv("times.csv",   times)

    # Reference implementation for verification
    def phi(x):
        return 0.5 * (1.0 + math.erf(x / math.sqrt(2.0)))

    def bs_call(s, k, r, sigma, t):
        d1 = (math.log(s/k) + (r + 0.5*sigma*sigma)*t) / (sigma*math.sqrt(t))
        d2 = d1 - sigma*math.sqrt(t)
        return s*phi(d1) - k*math.exp(-r*t)*phi(d2)

    ref = [round(bs_call(spots[i], strikes[i], rates[i], vols[i], times[i]), 6)
           for i in range(n)]
    write_csv("reference.csv", ref)

if __name__ == '__main__':
    main()
