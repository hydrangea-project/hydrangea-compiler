/* bench/stencil/stencil_interior_ref.c
 * 2D 5-point discrete Laplacian — interior-only C+OpenMP reference.
 * Computes only the (h-2) x (w-2) interior elements; boundary pixels
 * are skipped.  This matches stencil_interior.hyd.
 *
 * Environment variables:
 *   STENCIL_H    -- grid height/rows  (required)
 *   STENCIL_W    -- grid width/cols   (required)
 *   BENCH_WARMUP -- warmup iterations (default 3)
 *   BENCH_ITERS  -- timed iterations  (default 10)
 *
 * Reads bench/stencil/input.csv, writes bench/stencil/ref_out_interior.csv.
 */

#include <stdlib.h>
#include <stdio.h>
#include "../c_bench/timing.h"

typedef struct {
    int h, w;
    double *in, *out;
} StencilData;

static double stencil_run_once(void *vdata) {
    StencilData *d = (StencilData *)vdata;
    int h = d->h, w = d->w;
    double *in = d->in, *out = d->out;
    int oh = h - 2, ow = w - 2;

    #pragma omp parallel for collapse(2) schedule(static)
    for (int i = 0; i < oh; i++) {
        for (int j = 0; j < ow; j++) {
            int gi = i + 1;  /* actual grid row */
            int gj = j + 1;  /* actual grid col */
            out[i * ow + j] =
                in[(gi - 1) * w + gj] +
                in[(gi + 1) * w + gj] +
                in[gi * w + (gj - 1)] +
                in[gi * w + (gj + 1)] +
                in[gi * w + gj] * (-4.0);
        }
    }

    double checksum = 0.0;
    for (int k = 0; k < oh * ow; k++) checksum += out[k];
    return checksum;
}

int main(void) {
    int h      = hb_get_env_int("STENCIL_H");
    int w      = hb_get_env_int("STENCIL_W");
    int warmup = hb_get_env_int_or("BENCH_WARMUP", 3);
    int iters  = hb_get_env_int_or("BENCH_ITERS",  10);
    int oh = h - 2, ow = w - 2;

    double *in  = (double *)malloc((size_t)h * (size_t)w * sizeof(double));
    double *out = (double *)malloc((size_t)oh * (size_t)ow * sizeof(double));

    hb_read_csv_doubles("bench/stencil/input.csv", in, h * w);

    StencilData d = { h, w, in, out };
    hb_run_timed("stencil_interior", warmup, iters, stencil_run_once, &d);

    /* Write output: one row per line, matching hyd_write_array_csv_float 2D format */
    FILE *f = fopen("bench/stencil/ref_out_interior.csv", "w");
    if (!f) { fprintf(stderr, "error: cannot write ref_out_interior.csv\n"); return 1; }
    for (int i = 0; i < oh; i++) {
        for (int j = 0; j < ow; j++) {
            if (j) fputc(',', f);
            fprintf(f, "%.17g", out[i * ow + j]);
        }
        fputc('\n', f);
    }
    fclose(f);

    free(in);
    free(out);
    return 0;
}
