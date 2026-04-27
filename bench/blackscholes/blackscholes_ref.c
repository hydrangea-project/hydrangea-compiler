/* bench/blackscholes/blackscholes_ref.c
 * Black-Scholes European call price — C+OpenMP reference implementation.
 *
 * Environment variables:
 *   BS_N         -- number of options (required)
 *   BENCH_WARMUP -- warmup iterations  (default 3)
 *   BENCH_ITERS  -- timed iterations   (default 10)
 *
 * Reads inputs from bench/blackscholes/{spots,strikes,rates,vols,times}.csv
 * (relative to cwd; also tries ../../bench/... if run from inside bench dir).
 */

#include <math.h>
#include <stdlib.h>
#include "../c_bench/timing.h"

static inline double phi(double x)
{
    return 0.5 * (1.0 + erf(x / sqrt(2.0)));
}

typedef struct {
    int n;
    double *spots, *strikes, *rates, *vols, *times;
    double *out;
} BSData;

static double bs_run_once(void *vdata)
{
    BSData *d = (BSData *)vdata;
    int n = d->n;
    #pragma omp parallel for schedule(static)
    for (int i = 0; i < n; i++) {
        double s     = d->spots[i];
        double k     = d->strikes[i];
        double r     = d->rates[i];
        double sigma = d->vols[i];
        double t     = d->times[i];
        double d1 = (log(s / k) + (r + 0.5 * sigma * sigma) * t)
                    / (sigma * sqrt(t));
        double d2 = d1 - sigma * sqrt(t);
        d->out[i] = s * phi(d1) - k * exp(-r * t) * phi(d2);
    }
    double sum = 0.0;
    for (int i = 0; i < n; i++) sum += d->out[i];
    return sum;
}

int main(void)
{
    int n      = hb_get_env_int("BS_N");
    int warmup = hb_get_env_int_or("BENCH_WARMUP", 3);
    int iters  = hb_get_env_int_or("BENCH_ITERS",  10);

    double *spots   = (double *)malloc((size_t)n * sizeof(double));
    double *strikes = (double *)malloc((size_t)n * sizeof(double));
    double *rates   = (double *)malloc((size_t)n * sizeof(double));
    double *vols    = (double *)malloc((size_t)n * sizeof(double));
    double *times   = (double *)malloc((size_t)n * sizeof(double));
    double *out     = (double *)malloc((size_t)n * sizeof(double));

    hb_read_csv_doubles("bench/blackscholes/spots.csv",   spots,   n);
    hb_read_csv_doubles("bench/blackscholes/strikes.csv", strikes, n);
    hb_read_csv_doubles("bench/blackscholes/rates.csv",   rates,   n);
    hb_read_csv_doubles("bench/blackscholes/vols.csv",    vols,    n);
    hb_read_csv_doubles("bench/blackscholes/times.csv",   times,   n);

    BSData d = { n, spots, strikes, rates, vols, times, out };
    hb_run_timed("main", warmup, iters, bs_run_once, &d);

    free(spots); free(strikes); free(rates); free(vols); free(times); free(out);
    return 0;
}
