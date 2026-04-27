/* bench/nbody/nbody_ref.c
 * N-body gravitational simulation (single Euler step) — C+OpenMP reference.
 *
 * Environment variables:
 *   NBODY_N      -- number of particles (required)
 *   BENCH_WARMUP -- warmup iterations   (default 3)
 *   BENCH_ITERS  -- timed iterations    (default 10)
 *
 * Reads bench/nbody/{xs,ys,zs,ms,vxs,vys,vzs}.csv
 */

#include <math.h>
#include <stdlib.h>
#include "../c_bench/timing.h"

#define EPS2 0.01
#define G    6.674e-11
#define DT   0.01

typedef struct {
    int n;
    double *xs, *ys, *zs, *ms, *vxs, *vys, *vzs;
    double *new_xs, *new_ys, *new_zs;
} NBodyData;

static double nbody_run_once(void *vdata)
{
    NBodyData *d = (NBodyData *)vdata;
    int n = d->n;

    #pragma omp parallel for schedule(static)
    for (int i = 0; i < n; i++) {
        double xi = d->xs[i], yi = d->ys[i], zi = d->zs[i];
        double ax = 0.0, ay = 0.0, az = 0.0;
        for (int j = 0; j < n; j++) {
            double dx = d->xs[j] - xi;
            double dy = d->ys[j] - yi;
            double dz = d->zs[j] - zi;
            double r2 = dx*dx + dy*dy + dz*dz + EPS2;
            double r3 = r2 * sqrt(r2);
            double scale = G * d->ms[j] / r3;
            ax += scale * dx;
            ay += scale * dy;
            az += scale * dz;
        }
        double new_vx = d->vxs[i] + ax * DT;
        double new_vy = d->vys[i] + ay * DT;
        double new_vz = d->vzs[i] + az * DT;
        d->new_xs[i] = d->xs[i] + new_vx * DT;
        d->new_ys[i] = d->ys[i] + new_vy * DT;
        d->new_zs[i] = d->zs[i] + new_vz * DT;
    }

    double sum = 0.0;
    for (int i = 0; i < n; i++)
        sum += d->new_xs[i] + d->new_ys[i] + d->new_zs[i];
    return sum;
}

int main(void)
{
    int n      = hb_get_env_int("NBODY_N");
    int warmup = hb_get_env_int_or("BENCH_WARMUP", 3);
    int iters  = hb_get_env_int_or("BENCH_ITERS",  10);

    double *xs  = (double *)malloc((size_t)n * sizeof(double));
    double *ys  = (double *)malloc((size_t)n * sizeof(double));
    double *zs  = (double *)malloc((size_t)n * sizeof(double));
    double *ms  = (double *)malloc((size_t)n * sizeof(double));
    double *vxs = (double *)malloc((size_t)n * sizeof(double));
    double *vys = (double *)malloc((size_t)n * sizeof(double));
    double *vzs = (double *)malloc((size_t)n * sizeof(double));
    double *new_xs = (double *)malloc((size_t)n * sizeof(double));
    double *new_ys = (double *)malloc((size_t)n * sizeof(double));
    double *new_zs = (double *)malloc((size_t)n * sizeof(double));

    hb_read_csv_doubles("bench/nbody/xs.csv",  xs,  n);
    hb_read_csv_doubles("bench/nbody/ys.csv",  ys,  n);
    hb_read_csv_doubles("bench/nbody/zs.csv",  zs,  n);
    hb_read_csv_doubles("bench/nbody/ms.csv",  ms,  n);
    hb_read_csv_doubles("bench/nbody/vxs.csv", vxs, n);
    hb_read_csv_doubles("bench/nbody/vys.csv", vys, n);
    hb_read_csv_doubles("bench/nbody/vzs.csv", vzs, n);

    NBodyData d = { n, xs, ys, zs, ms, vxs, vys, vzs, new_xs, new_ys, new_zs };
    hb_run_timed("main", warmup, iters, nbody_run_once, &d);

    free(xs); free(ys); free(zs); free(ms);
    free(vxs); free(vys); free(vzs);
    free(new_xs); free(new_ys); free(new_zs);
    return 0;
}
