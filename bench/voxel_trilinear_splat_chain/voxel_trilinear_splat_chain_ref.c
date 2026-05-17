/* bench/voxel_trilinear_splat_chain/voxel_trilinear_splat_chain_ref.c
 * Chained trilinear voxel splat benchmark — C+OpenMP reference.
 *
 * Applies three deterministic trilinear splat passes from the same filtered
 * point cloud into a shared voxel grid. Each pass uses a different jitter
 * pattern so the overall workload matches the Hydrangea scatter-chain
 * benchmark.
 *
 * Environment variables:
 *   VSCHAIN_POINTS      -- number of source points (required)
 *   VSCHAIN_NX          -- voxel-grid x extent      (required)
 *   VSCHAIN_NY          -- voxel-grid y extent      (required)
 *   VSCHAIN_NZ          -- voxel-grid z extent      (required)
 *   VSCHAIN_KEEP_PERIOD -- keep every k-th point    (required)
 *   BENCH_WARMUP        -- warmup iterations         (default 3)
 *   BENCH_ITERS         -- timed iterations          (default 10)
 */

#include <stdlib.h>
#include <string.h>
#include "../c_bench/timing.h"

typedef struct {
    int64_t n, nx, ny, nz, keep_period;
    int64_t nvox;
    double *voxels;
} VSChainData;

static double vschain_run_once(void *vdata)
{
    VSChainData *d = (VSChainData *)vdata;
    int64_t n = d->n, nx = d->nx, ny = d->ny, nz = d->nz;
    int64_t kp = d->keep_period, nvox = d->nvox;

    memset(d->voxels, 0, (size_t)nvox * sizeof(double));

    #pragma omp parallel for schedule(static)
    for (int64_t p = 0; p < n; p++) {
        if ((p / kp) * kp != p) continue;

        for (int64_t phase = 0; phase < 3; phase++) {
            int64_t fx = (p * (5 + phase)  + (1 + phase))     % 4;
            int64_t fy = (p * (7 + phase)  + (2 + phase * 2)) % 4;
            int64_t fz = (p * (11 + phase) + (3 + phase * 3)) % 4;
            int64_t base = (p * (phase + 3)) + (phase + 1);

            for (int64_t c = 0; c < 8; c++) {
                int64_t bx = c % 2;
                int64_t by = (c / 2) % 2;
                int64_t bz = (c / 4) % 2;

                int64_t x = (p * (17 + phase * 2) + (3 + phase * 5)  + bx) % nx;
                int64_t y = (p * (29 + phase * 3) + (5 + phase * 7)  + by) % ny;
                int64_t z = (p * (43 + phase * 5) + (7 + phase * 11) + bz) % nz;

                int64_t wx = (1 - bx) * (4 - fx) + bx * fx;
                int64_t wy = (1 - by) * (4 - fy) + by * fy;
                int64_t wz = (1 - bz) * (4 - fz) + bz * fz;
                double weight = (double)(base * wx * wy * wz) / 64.0;
                int64_t idx = ((z * ny) + y) * nx + x;

                #pragma omp atomic
                d->voxels[idx] += weight;
            }
        }
    }

    double sum = 0.0;
    for (int64_t v = 0; v < nvox; v++) sum += d->voxels[v];
    return sum;
}

int main(void)
{
    int64_t n   = hb_get_env_int64("VSCHAIN_POINTS");
    int64_t nx  = hb_get_env_int64("VSCHAIN_NX");
    int64_t ny  = hb_get_env_int64("VSCHAIN_NY");
    int64_t nz  = hb_get_env_int64("VSCHAIN_NZ");
    int64_t kp  = hb_get_env_int64("VSCHAIN_KEEP_PERIOD");
    int     warmup = hb_get_env_int_or("BENCH_WARMUP", 3);
    int     iters  = hb_get_env_int_or("BENCH_ITERS",  10);

    int64_t nvox = nx * ny * nz;
    double *voxels = (double *)malloc((size_t)nvox * sizeof(double));

    VSChainData d = { n, nx, ny, nz, kp, nvox, voxels };
    hb_run_timed("main", warmup, iters, vschain_run_once, &d);

    free(voxels);
    return 0;
}
