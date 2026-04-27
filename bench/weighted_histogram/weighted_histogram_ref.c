/* bench/weighted_histogram/weighted_histogram_ref.c
 * Weighted histogram — C+OpenMP reference (scatter with atomic adds).
 *
 * Generates a deterministic stream of N integers, routes each into one of
 * WH_BINS bins by index formula, and atomically accumulates integer weights.
 * Returns sum of histogram as checksum.
 *
 * Environment variables:
 *   WH_N         -- number of input elements (required)
 *   WH_BINS      -- number of histogram bins (required)
 *   BENCH_WARMUP -- warmup iterations         (default 3)
 *   BENCH_ITERS  -- timed iterations          (default 10)
 */

#include <stdlib.h>
#include <string.h>
#include "../c_bench/timing.h"

typedef struct {
    int n, bins;
    int *hist;
} WHData;

static double wh_run_once(void *vdata)
{
    WHData *d = (WHData *)vdata;
    int n = d->n, bins = d->bins;
    memset(d->hist, 0, (size_t)bins * sizeof(int));

    #pragma omp parallel for schedule(static)
    for (int i = 0; i < n; i++) {
        int bucket = (i * bins) / n;
        int weight = i * 3 + 1;
        #pragma omp atomic
        d->hist[bucket] += weight;
    }

    long sum = 0;
    for (int b = 0; b < bins; b++) sum += d->hist[b];
    return (double)sum;
}

int main(void)
{
    int n      = hb_get_env_int("WH_N");
    int bins   = hb_get_env_int("WH_BINS");
    int warmup = hb_get_env_int_or("BENCH_WARMUP", 3);
    int iters  = hb_get_env_int_or("BENCH_ITERS",  10);

    int *hist = (int *)malloc((size_t)bins * sizeof(int));

    WHData d = { n, bins, hist };
    hb_run_timed("main", warmup, iters, wh_run_once, &d);

    free(hist);
    return 0;
}
