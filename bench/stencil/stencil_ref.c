/* bench/stencil/stencil_ref.c
 * 2D 5-point discrete Laplacian stencil — C+OpenMP reference.
 *
 * Environment variables:
 *   STENCIL_H    -- grid height/rows  (required)
 *   STENCIL_W    -- grid width/cols   (required)
 *   BENCH_WARMUP -- warmup iterations (default 3)
 *   BENCH_ITERS  -- timed iterations  (default 10)
 *
 * Reads bench/stencil/input.csv, writes bench/stencil/ref_out.csv.
 */

#include <stdlib.h>
#include <stdio.h>
#include "../c_bench/timing.h"

typedef struct {
    int h, w;
    double *in, *out;
} StencilData;

static inline int clamp(int v, int lo, int hi) {
    return v < lo ? lo : (v > hi ? hi : v);
}

static double stencil_run_once(void *vdata) {
    StencilData *d = (StencilData *)vdata;
    int h = d->h, w = d->w;
    double *in = d->in, *out = d->out;

    #pragma omp parallel for schedule(static)
    for (int i = 0; i < h; i++) {
        for (int j = 0; j < w; j++) {
            int i0 = clamp(i - 1, 0, h - 1);
            int i1 = clamp(i + 1, 0, h - 1);
            int j0 = clamp(j - 1, 0, w - 1);
            int j1 = clamp(j + 1, 0, w - 1);
            out[i * w + j] =
                in[i0 * w + j] + in[i1 * w + j] +
                in[i  * w + j0] + in[i  * w + j1] +
                in[i  * w + j] * (-4.0);
        }
    }

    double checksum = 0.0;
    for (int k = 0; k < h * w; k++) checksum += out[k];
    return checksum;
}

int main(void) {
    int h      = hb_get_env_int("STENCIL_H");
    int w      = hb_get_env_int("STENCIL_W");
    int warmup = hb_get_env_int_or("BENCH_WARMUP", 3);
    int iters  = hb_get_env_int_or("BENCH_ITERS",  10);

    double *in  = (double *)malloc((size_t)h * (size_t)w * sizeof(double));
    double *out = (double *)malloc((size_t)h * (size_t)w * sizeof(double));

    hb_read_csv_doubles("bench/stencil/input.csv", in, h * w);

    StencilData d = { h, w, in, out };
    hb_run_timed("stencil", warmup, iters, stencil_run_once, &d);

    /* Write output for correctness verification (one row per line, matching
       the Hydrangea runtime's hyd_write_array_csv 2D format) */
    FILE *f = fopen("bench/stencil/ref_out.csv", "w");
    if (!f) { fprintf(stderr, "error: cannot write ref_out.csv\n"); return 1; }
    for (int i = 0; i < h; i++) {
        for (int j = 0; j < w; j++) {
            if (j) fputc(',', f);
            fprintf(f, "%.17g", out[i * w + j]);
        }
        fputc('\n', f);
    }
    fclose(f);

    free(in);
    free(out);
    return 0;
}
