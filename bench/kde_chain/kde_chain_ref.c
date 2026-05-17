/* bench/kde_chain/kde_chain_ref.c
 * Chained 1-D KDE benchmark — C+OpenMP reference.
 *
 * Applies three guarded tent-kernel deposition passes into one shared
 * histogram. Each phase jitters the kernel centre differently, scales weights
 * by phase, and keeps a different phase-shifted subset of samples.
 *
 * Environment variables:
 *   KDECHAIN_N            -- number of source samples (required)
 *   KDECHAIN_BINS         -- number of output bins    (required)
 *   KDECHAIN_KEEP_PERIOD  -- keep every k-th sample   (required)
 *   BENCH_WARMUP          -- warmup iterations        (default 3)
 *   BENCH_ITERS           -- timed iterations         (default 10)
 */

#include <stdlib.h>
#include <string.h>
#include "../c_bench/timing.h"

typedef struct {
    int64_t n, bins, keep_period;
    int64_t *density;
} KDEChainData;

static double kde_chain_run_once(void *vdata)
{
    KDEChainData *d = (KDEChainData *)vdata;
    int64_t n = d->n, bins = d->bins, kp = d->keep_period;
    memset(d->density, 0, (size_t)bins * sizeof(int64_t));

    #pragma omp parallel for schedule(static)
    for (int64_t s = 0; s < n; s++) {
        int64_t base = (s * bins) / n;
        for (int64_t phase = 0; phase < 3; phase++) {
            int64_t phase_sample = s + phase;
            if ((phase_sample / kp) * kp != phase_sample) continue;

            int64_t jitter = ((s * (phase + 3)) + phase) % 5 - 2;
            int64_t centre = base + jitter;

            for (int64_t offset = 0; offset < 3; offset++) {
                int64_t route = centre + offset - 1;
                if (route >= 0 && route < bins) {
                    int64_t peak = (offset == 1) ? 3 : 1;
                    int64_t weight = peak * (phase + 1);
                    #pragma omp atomic
                    d->density[route] += weight;
                }
            }
        }
    }

    int64_t sum = 0;
    for (int64_t b = 0; b < bins; b++) sum += d->density[b];
    return (double)sum;
}

int main(void)
{
    int64_t n    = hb_get_env_int64("KDECHAIN_N");
    int64_t bins = hb_get_env_int64("KDECHAIN_BINS");
    int64_t kp   = hb_get_env_int64("KDECHAIN_KEEP_PERIOD");
    int     warmup = hb_get_env_int_or("BENCH_WARMUP", 3);
    int     iters  = hb_get_env_int_or("BENCH_ITERS",  10);

    int64_t *density = (int64_t *)malloc((size_t)bins * sizeof(int64_t));

    KDEChainData d = { n, bins, kp, density };
    hb_run_timed("main", warmup, iters, kde_chain_run_once, &d);

    free(density);
    return 0;
}
