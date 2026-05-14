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
    int64_t n, bins, keep_period;
    int64_t *hist;
} GWHData;

static double gwh_run_once(void *vdata)
{
    GWHData *d = (GWHData *)vdata;
    int64_t n = d->n, bins = d->bins, kp = d->keep_period;
    memset(d->hist, 0, (size_t)bins * sizeof(int64_t));

    #pragma omp parallel for schedule(static)
    for (int64_t i = 0; i < n; i++) {
        /* keep predicate: (i / kp) * kp == i  ↔  i % kp == 0 */
        if ((i / kp) * kp == i) {
            int64_t bucket = (i * bins) / n;
            int64_t weight = i * 3 + 1;
            #pragma omp atomic
            d->hist[bucket] += weight;
        }
    }

    int64_t sum = 0;
    for (int64_t b = 0; b < bins; b++) sum += d->hist[b];
    return (double)sum;
}

int main(void)
{
    int64_t n      = hb_get_env_int64("GWH_N");
    int64_t bins   = hb_get_env_int64("GWH_BINS");
    int64_t kp     = hb_get_env_int64("GWH_KEEP_PERIOD");
    int     warmup = hb_get_env_int_or("BENCH_WARMUP", 3);
    int     iters  = hb_get_env_int_or("BENCH_ITERS",  10);

    int64_t *hist = (int64_t *)malloc((size_t)bins * sizeof(int64_t));

    GWHData d = { n, bins, kp, hist };
    hb_run_timed("main", warmup, iters, gwh_run_once, &d);

    free(hist);
    return 0;
}
