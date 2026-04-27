/* bench/spmv/spmv_ref.c
 * Sparse Matrix-Vector Multiplication (CSR) — C+OpenMP reference.
 *
 * Environment variables:
 *   SPMV_NROWS   -- number of rows       (required)
 *   SPMV_NCOLS   -- number of columns    (required)
 *   SPMV_NNZ     -- number of non-zeros  (required)
 *   BENCH_WARMUP -- warmup iterations    (default 3)
 *   BENCH_ITERS  -- timed iterations     (default 10)
 *
 * Reads bench/spmv/{values,col_idx,row_ptr,x}.csv
 */

#include <stdlib.h>
#include "../c_bench/timing.h"

typedef struct {
    int nrows, ncols, nnz;
    double *values;
    int    *col_idx;
    int    *row_ptr;
    double *x;
    double *y;
} SPMVData;

static double spmv_run_once(void *vdata)
{
    SPMVData *d = (SPMVData *)vdata;
    int nrows = d->nrows;

    #pragma omp parallel for schedule(static)
    for (int i = 0; i < nrows; i++) {
        double sum = 0.0;
        for (int k = d->row_ptr[i]; k < d->row_ptr[i + 1]; k++) {
            sum += d->values[k] * d->x[d->col_idx[k]];
        }
        d->y[i] = sum;
    }

    double total = 0.0;
    for (int i = 0; i < nrows; i++) total += d->y[i];
    return total;
}

int main(void)
{
    int nrows  = hb_get_env_int("SPMV_NROWS");
    int ncols  = hb_get_env_int("SPMV_NCOLS");
    int nnz    = hb_get_env_int("SPMV_NNZ");
    int warmup = hb_get_env_int_or("BENCH_WARMUP", 3);
    int iters  = hb_get_env_int_or("BENCH_ITERS",  10);

    double *values  = (double *)malloc((size_t)nnz       * sizeof(double));
    int    *col_idx = (int    *)malloc((size_t)nnz       * sizeof(int));
    int    *row_ptr = (int    *)malloc((size_t)(nrows+1) * sizeof(int));
    double *x       = (double *)malloc((size_t)ncols     * sizeof(double));
    double *y       = (double *)malloc((size_t)nrows     * sizeof(double));

    hb_read_csv_doubles("bench/spmv/values.csv",  values,  nnz);
    hb_read_csv_ints   ("bench/spmv/col_idx.csv", col_idx, nnz);
    hb_read_csv_ints   ("bench/spmv/row_ptr.csv", row_ptr, nrows + 1);
    hb_read_csv_doubles("bench/spmv/x.csv",       x,       ncols);

    SPMVData d = { nrows, ncols, nnz, values, col_idx, row_ptr, x, y };
    hb_run_timed("main", warmup, iters, spmv_run_once, &d);

    free(values); free(col_idx); free(row_ptr); free(x); free(y);
    return 0;
}
