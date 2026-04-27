/* bench/voxel_rasterization/voxel_rasterization_ref.c
 * Sparse voxel rasterization — C+OpenMP reference (scatter with atomics).
 *
 * Generates a deterministic point cloud, clips every k-th point, deposits
 * integer weights into a dense voxel grid using atomic float addition.
 * Checksum is the sum of all voxels.
 *
 * Environment variables:
 *   VOX_POINTS      -- number of source points (required)
 *   VOX_NX          -- voxel-grid x extent      (required)
 *   VOX_NY          -- voxel-grid y extent      (required)
 *   VOX_NZ          -- voxel-grid z extent      (required)
 *   VOX_KEEP_PERIOD -- keep every k-th point    (required)
 *   BENCH_WARMUP    -- warmup iterations         (default 3)
 *   BENCH_ITERS     -- timed iterations          (default 10)
 */

#include <stdlib.h>
#include <string.h>
#include "../c_bench/timing.h"

typedef struct {
    int n, nx, ny, nz, keep_period;
    int nvox;
    float *voxels;
} VoxData;

static double vox_run_once(void *vdata)
{
    VoxData *d = (VoxData *)vdata;
    int n = d->n, nx = d->nx, ny = d->ny, nz = d->nz;
    int kp = d->keep_period, nvox = d->nvox;

    memset(d->voxels, 0, (size_t)nvox * sizeof(float));

    #pragma omp parallel for schedule(static)
    for (int p = 0; p < n; p++) {
        /* keep predicate: (p / kp) * kp == p  ↔  p % kp == 0 */
        if ((p / kp) * kp != p) continue;

        int x = (p * 17 + 3) % nx;
        int y = (p * 29 + 5) % ny;
        int z = (p * 43 + 7) % nz;
        int idx = ((z * ny) + y) * nx + x;
        float weight = (float)(((x + 1) * (y + 2)) + (z * 3) + 1);

        #pragma omp atomic
        d->voxels[idx] += weight;
    }

    double sum = 0.0;
    for (int v = 0; v < nvox; v++) sum += (double)d->voxels[v];
    return sum;
}

int main(void)
{
    int n   = hb_get_env_int("VOX_POINTS");
    int nx  = hb_get_env_int("VOX_NX");
    int ny  = hb_get_env_int("VOX_NY");
    int nz  = hb_get_env_int("VOX_NZ");
    int kp  = hb_get_env_int("VOX_KEEP_PERIOD");
    int warmup = hb_get_env_int_or("BENCH_WARMUP", 3);
    int iters  = hb_get_env_int_or("BENCH_ITERS",  10);

    int nvox  = nx * ny * nz;
    float *voxels = (float *)malloc((size_t)nvox * sizeof(float));

    VoxData d = { n, nx, ny, nz, kp, nvox, voxels };
    hb_run_timed("main", warmup, iters, vox_run_once, &d);

    free(voxels);
    return 0;
}
