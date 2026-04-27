/* bench/voxel_trilinear_splat/voxel_trilinear_splat_ref.c
 * Voxel trilinear splat benchmark — C+OpenMP reference (8 atomics per point).
 *
 * Each kept point (filtered by keep_period) contributes trilinear weights
 * to 8 neighbouring voxels.  Uses atomic double addition for correctness.
 * Checksum is the sum of all voxels.
 *
 * Environment variables:
 *   VSPLAT_POINTS      -- number of source points (required)
 *   VSPLAT_NX          -- voxel-grid x extent      (required)
 *   VSPLAT_NY          -- voxel-grid y extent      (required)
 *   VSPLAT_NZ          -- voxel-grid z extent      (required)
 *   VSPLAT_KEEP_PERIOD -- keep every k-th point    (required)
 *   BENCH_WARMUP       -- warmup iterations         (default 3)
 *   BENCH_ITERS        -- timed iterations          (default 10)
 */

#include <stdlib.h>
#include <string.h>
#include "../c_bench/timing.h"

typedef struct {
    int n, nx, ny, nz, keep_period;
    int nvox;
    double *voxels;
} VSplatData;

static double vsplat_run_once(void *vdata)
{
    VSplatData *d = (VSplatData *)vdata;
    int n = d->n, nx = d->nx, ny = d->ny, nz = d->nz;
    int kp = d->keep_period, nvox = d->nvox;
    int contribs = n * 8;

    memset(d->voxels, 0, (size_t)nvox * sizeof(double));

    #pragma omp parallel for schedule(static)
    for (int i = 0; i < contribs; i++) {
        int p = i / 8;
        /* keep predicate */
        if ((p / kp) * kp != p) continue;

        int c  = i % 8;
        int bx = c % 2;
        int by = (c / 2) % 2;
        int bz = (c / 4) % 2;

        int x = ((p * 17 + 3) + bx) % nx;
        int y = ((p * 29 + 5) + by) % ny;
        int z = ((p * 43 + 7) + bz) % nz;

        int fx = (p * 5  + 1) % 4;
        int fy = (p * 7  + 2) % 4;
        int fz = (p * 11 + 3) % 4;
        int wx = (1 - bx) * (4 - fx) + bx * fx;
        int wy = (1 - by) * (4 - fy) + by * fy;
        int wz = (1 - bz) * (4 - fz) + bz * fz;
        int base = (p * 3) + 1;
        double weight = (double)(base * wx * wy * wz) / 64.0;

        int idx = ((z * ny) + y) * nx + x;

        #pragma omp atomic
        d->voxels[idx] += weight;
    }

    double sum = 0.0;
    for (int v = 0; v < nvox; v++) sum += d->voxels[v];
    return sum;
}

int main(void)
{
    int n   = hb_get_env_int("VSPLAT_POINTS");
    int nx  = hb_get_env_int("VSPLAT_NX");
    int ny  = hb_get_env_int("VSPLAT_NY");
    int nz  = hb_get_env_int("VSPLAT_NZ");
    int kp  = hb_get_env_int("VSPLAT_KEEP_PERIOD");
    int warmup = hb_get_env_int_or("BENCH_WARMUP", 3);
    int iters  = hb_get_env_int_or("BENCH_ITERS",  10);

    int nvox = nx * ny * nz;
    double *voxels = (double *)malloc((size_t)nvox * sizeof(double));

    VSplatData d = { n, nx, ny, nz, kp, nvox, voxels };
    hb_run_timed("main", warmup, iters, vsplat_run_once, &d);

    free(voxels);
    return 0;
}
