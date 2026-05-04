/* bench/stencil/jacobi_2d_ref.c
 * 2D Jacobi iteration using the 5-point averaging stencil with clamped boundary.
 * Matches jacobi_2d.hyd: each step computes output[i][j] = avg of 4 neighbours.
 *
 * Environment variables:
 *   JACOBI_H      -- grid height/rows  (required)
 *   JACOBI_W      -- grid width/cols   (required)
 *   JACOBI_ITERS  -- number of Jacobi sweeps (required)
 *   BENCH_WARMUP  -- warmup iterations (default 3)
 *   BENCH_ITERS   -- timed iterations  (default 10)
 *
 * Input is initialised to all-1.0 (matching jacobi_2d.hyd).
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "../c_bench/timing.h"

typedef struct {
    int h, w, n_iters;
    double *buf0, *buf1;
} JacobiData;

static inline int clamp(int v, int lo, int hi) {
    return v < lo ? lo : (v > hi ? hi : v);
}

static double jacobi_run_once(void *vdata) {
    JacobiData *d = (JacobiData *)vdata;
    int h = d->h, w = d->w;
    double *in = d->buf0, *out = d->buf1;

    for (int iter = 0; iter < d->n_iters; iter++) {
        #pragma omp parallel for schedule(static)
        for (int i = 0; i < h; i++) {
            for (int j = 0; j < w; j++) {
                int i0 = clamp(i - 1, 0, h - 1);
                int i1 = clamp(i + 1, 0, h - 1);
                int j0 = clamp(j - 1, 0, w - 1);
                int j1 = clamp(j + 1, 0, w - 1);
                out[i * w + j] =
                    (in[i0 * w + j] + in[i1 * w + j] +
                     in[i  * w + j0] + in[i  * w + j1]) * 0.25;
            }
        }
        double *tmp = in; in = out; out = tmp;
    }

    /* in points to the final result after the last swap */
    double checksum = 0.0;
    for (int k = 0; k < h * w; k++) checksum += in[k];
    return checksum;
}

int main(void) {
    int h       = hb_get_env_int("JACOBI_H");
    int w       = hb_get_env_int("JACOBI_W");
    int n_iters = hb_get_env_int("JACOBI_ITERS");
    int warmup  = hb_get_env_int_or("BENCH_WARMUP", 3);
    int iters   = hb_get_env_int_or("BENCH_ITERS",  10);

    double *buf0 = (double *)malloc((size_t)h * (size_t)w * sizeof(double));
    double *buf1 = (double *)malloc((size_t)h * (size_t)w * sizeof(double));

    /* Initialise to all 1.0, matching jacobi_2d.hyd */
    for (int k = 0; k < h * w; k++) buf0[k] = 1.0;

    JacobiData d = { h, w, n_iters, buf0, buf1 };
    hb_run_timed("jacobi_2d", warmup, iters, jacobi_run_once, &d);

    free(buf0);
    free(buf1);
    return 0;
}
