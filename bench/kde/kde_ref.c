/* bench/kde/kde_ref.c
 * 1-D Kernel Density Estimation (triangular tent kernel) — C+OpenMP reference.
 *
 * Each of KDE_N source samples contributes to three adjacent histogram bins.
 * Contribution k (0 <= k < n*3):
 *   sample  = k / 3
 *   offset  = k mod 3       (0=left neighbour, 1=centre, 2=right neighbour)
 *   centre  = (sample * bins) / n
 *   route   = centre + offset - 1
 *   weight  = (offset == 1) ? 2 : 1
 *   guard   = (route >= 0 && route < bins)
 *
 * Environment variables:
 *   KDE_N        -- number of source samples (required)
 *   KDE_BINS     -- number of output bins    (required)
 *   BENCH_WARMUP -- warmup iterations        (default 3)
 *   BENCH_ITERS  -- timed iterations         (default 10)
 */

#include <stdlib.h>
#include <string.h>
#include "../c_bench/timing.h"

typedef struct {
    int64_t n, bins;
    int64_t *density;
} KDEData;

static double kde_run_once(void *vdata)
{
    KDEData *d = (KDEData *)vdata;
    int64_t n = d->n, bins = d->bins;
    int64_t contribs = n * 3;
    memset(d->density, 0, (size_t)bins * sizeof(int64_t));

    #pragma omp parallel for schedule(static)
    for (int64_t k = 0; k < contribs; k++) {
        int64_t sample = k / 3;
        int64_t offset = k - sample * 3;   /* k mod 3, avoids % */
        int64_t centre = (sample * bins) / n;
        int64_t route  = centre + offset - 1;
        if (route >= 0 && route < bins) {
            int64_t weight = (offset == 1) ? 2 : 1;
            #pragma omp atomic
            d->density[route] += weight;
        }
    }

    int64_t sum = 0;
    for (int64_t b = 0; b < bins; b++) sum += d->density[b];
    return (double)sum;
}

int main(void)
{
    int64_t n    = hb_get_env_int64("KDE_N");
    int64_t bins = hb_get_env_int64("KDE_BINS");
    int     warmup = hb_get_env_int_or("BENCH_WARMUP", 3);
    int     iters  = hb_get_env_int_or("BENCH_ITERS",  10);

    int64_t *density = (int64_t *)malloc((size_t)bins * sizeof(int64_t));

    KDEData d = { n, bins, density };
    hb_run_timed("main", warmup, iters, kde_run_once, &d);

    free(density);
    return 0;
}
