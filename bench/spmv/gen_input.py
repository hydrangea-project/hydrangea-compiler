#!/usr/bin/env python3
"""Generate input data for the SpMV benchmark (random sparse matrix, CSR format).
Usage: python3 gen_input.py <nrows> <ncols> <nnz>
  nnz: total number of non-zeros (distributed evenly across rows)
Writes values.csv, col_idx.csv, row_ptr.csv, x.csv to the same directory as this script.
"""
import sys
import random
import os

def main():
    nrows = int(sys.argv[1]) if len(sys.argv) > 1 else 1000
    ncols = int(sys.argv[2]) if len(sys.argv) > 2 else 1000
    nnz   = int(sys.argv[3]) if len(sys.argv) > 3 else max(1, nrows * ncols // 100)
    random.seed(13)
    outdir = os.path.dirname(os.path.abspath(__file__))

    # Distribute nnz evenly across rows; each row gets approximately nnz/nrows non-zeros.
    nnz_per_row = max(1, min(nnz // nrows, ncols))

    values   = []
    col_idxs = []
    row_ptr  = [0]

    for _ in range(nrows):
        cols = sorted(random.sample(range(ncols), min(nnz_per_row, ncols)))
        for c in cols:
            values.append(round(random.uniform(-2.0, 2.0), 6))
            col_idxs.append(c)
        row_ptr.append(len(values))

    x = [round(random.uniform(-1.0, 1.0), 6) for _ in range(ncols)]

    def write_csv(name, vals):
        path = os.path.join(outdir, name)
        with open(path, 'w') as f:
            f.write(','.join(str(v) for v in vals))

    write_csv("values.csv",  values)
    write_csv("col_idx.csv", col_idxs)
    write_csv("row_ptr.csv", row_ptr)
    write_csv("x.csv",       x)

    # Print the actual nnz so the caller can export SPMV_NNZ correctly.
    print(len(values))

if __name__ == '__main__':
    main()
