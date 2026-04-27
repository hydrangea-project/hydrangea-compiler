/* bench/guarded_weighted_histogram/guarded_weighted_histogram_ref.c
 * Guarded weighted histogram — C+OpenMP reference (scatter with mask + atomics).
 *
 * Generates N integers, filters them with a keep-period predicate, and
 * accumulates integer weights into WH_BINS bins atomically.
 *
 * Environment variables:
 *   GWH_N           -- number of input elements (required)
 *   GWH_BINS        -- number of histogram bins  (required)
 *   GWH_KEEP_PERIOD -- keep every k-th element   (required)
 *   BENCH_WARMUP    -- warmup iterations          (default 3)
 *   BENCH_ITERS     -- timed iterations           (default 10)
 */

#include <stdlib.h>
#include <string.h>
#include "../c_bench/timing.h"

typedef struct {
    int n, bins, keep_period;
    int *hist;
} GWHData;

static double gwh_run_once(void *vdata)
{
    GWHData *d = (GWHData *)vdata;
    int n = d->n, bins = d->bins, kp = d->keep_period;
    memset(d->hist, 0, (size_t)bins * sizeof(int));

    #pragma omp parallel for schedule(static)
    for (int i = 0; i < n; i++) {
        /* keep predicate: (i / kp) * kp == i  ↔  i % kp == 0 */
        if ((i / kp) * kp == i) {
            int bucket = (i * bins) / n;
            int weight = i * 3 + 1;
            #pragma omp atomic
            d->hist[bucket] += weight;
        }
    }

    long sum = 0;
    for (int b = 0; b < bins; b++) sum += d->hist[b];
    return (double)sum;
}

int main(void)
{
    int n      = hb_get_env_int("GWH_N");
    int bins   = hb_get_env_int("GWH_BINS");
    int kp     = hb_get_env_int("GWH_KEEP_PERIOD");
    int warmup = hb_get_env_int_or("BENCH_WARMUP", 3);
    int iters  = hb_get_env_int_or("BENCH_ITERS",  10);

    int *hist = (int *)malloc((size_t)bins * sizeof(int));

    GWHData d = { n, bins, kp, hist };
    hb_run_timed("main", warmup, iters, gwh_run_once, &d);

    free(hist);
    return 0;
}
