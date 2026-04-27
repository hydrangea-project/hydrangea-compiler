/* bench/matmul/matmul_ref.c
 * Dense matrix multiply C = A * B — C+OpenMP reference.
 *
 * Environment variables:
 *   MAT_M        -- rows of A / rows of C      (required)
 *   MAT_K        -- cols of A / rows of B      (required)
 *   MAT_N        -- cols of B / cols of C      (required)
 *   BENCH_WARMUP -- warmup iterations          (default 3)
 *   BENCH_ITERS  -- timed iterations           (default 10)
 *
 * Reads bench/matmul/matA.csv and bench/matmul/matB.csv
 */

#include <stdlib.h>
#include "../c_bench/timing.h"

typedef struct {
    int m, k, n;
    double *A, *B, *C;
} MatMulData;

static double matmul_run_once(void *vdata)
{
    MatMulData *d = (MatMulData *)vdata;
    int m = d->m, k = d->k, n = d->n;

    #pragma omp parallel for schedule(static)
    for (int i = 0; i < m; i++) {
        for (int j = 0; j < n; j++) {
            double sum = 0.0;
            for (int t = 0; t < k; t++) {
                sum += d->A[i * k + t] * d->B[t * n + j];
            }
            d->C[i * n + j] = sum;
        }
    }

    double total = 0.0;
    for (int i = 0; i < m * n; i++) total += d->C[i];
    return total;
}

int main(void)
{
    int m      = hb_get_env_int("MAT_M");
    int k      = hb_get_env_int("MAT_K");
    int n      = hb_get_env_int("MAT_N");
    int warmup = hb_get_env_int_or("BENCH_WARMUP", 3);
    int iters  = hb_get_env_int_or("BENCH_ITERS",  10);

    double *A = (double *)malloc((size_t)m * (size_t)k * sizeof(double));
    double *B = (double *)malloc((size_t)k * (size_t)n * sizeof(double));
    double *C = (double *)malloc((size_t)m * (size_t)n * sizeof(double));

    hb_read_csv_doubles("bench/matmul/matA.csv", A, m * k);
    hb_read_csv_doubles("bench/matmul/matB.csv", B, k * n);

    MatMulData d = { m, k, n, A, B, C };
    hb_run_timed("main", warmup, iters, matmul_run_once, &d);

    free(A); free(B); free(C);
    return 0;
}
