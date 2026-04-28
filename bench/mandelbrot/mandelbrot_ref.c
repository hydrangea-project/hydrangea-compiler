/* bench/mandelbrot/mandelbrot_ref.c
 * Mandelbrot escape-iteration benchmark — C+OpenMP reference.
 *
 * Matches the Hydrangea (foldl_while) and Repa versions: for each pixel,
 * iterate until |z|^2 > 4.0 (checked before each update) or MAND_ITERS
 * steps complete.  Returns the iteration count at which escape occurred.
 *
 * Environment variables:
 *   MAND_W       -- image width   (required)
 *   MAND_H       -- image height  (required)
 *   MAND_ITERS   -- max iterations (required)
 *   BENCH_WARMUP -- warmup iterations (default 3)
 *   BENCH_ITERS  -- timed iterations  (default 10)
 */

#include <math.h>
#include <stdlib.h>
#include "../c_bench/timing.h"

typedef struct {
    int width, height, iters;
    double *out;
} MandelbrotData;

static double mandelbrot_run_once(void *vdata)
{
    MandelbrotData *d = (MandelbrotData *)vdata;
    int W = d->width, H = d->height, ITERS = d->iters;
    double wf = (double)W, hf = (double)H;

    #pragma omp parallel for collapse(2) schedule(static)
    for (int px = 0; px < W; px++) {
        for (int py = 0; py < H; py++) {
            double cx = -2.5 + 3.5 * (double)px / (wf - 1.0);
            double cy = -1.0 + 2.0 * (double)py / (hf - 1.0);
            double re = 0.0, im = 0.0;
            int count = 0;
            for (int k = 0; k < ITERS; k++) {
                double re2 = re * re;
                double im2 = im * im;
                if (re2 + im2 > 4.0) break;  /* escape: check before update */
                double re_new = re2 - im2 + cx;
                double im_new = 2.0 * re * im + cy;
                re = re_new;
                im = im_new;
                count++;
            }
            d->out[px * H + py] = (double)count;
        }
    }

    double sum = 0.0;
    for (int i = 0; i < W * H; i++) sum += d->out[i];
    return sum;
}

int main(void)
{
    int W      = hb_get_env_int("MAND_W");
    int H      = hb_get_env_int("MAND_H");
    int ITERS  = hb_get_env_int("MAND_ITERS");
    int warmup = hb_get_env_int_or("BENCH_WARMUP", 3);
    int iters  = hb_get_env_int_or("BENCH_ITERS",  10);

    double *out = (double *)malloc((size_t)W * (size_t)H * sizeof(double));

    MandelbrotData d = { W, H, ITERS, out };
    hb_run_timed("main", warmup, iters, mandelbrot_run_once, &d);

    free(out);
    return 0;
}
