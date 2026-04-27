/* bench/c_bench/timing.h
 * Shared benchmark timing harness for C+OpenMP reference implementations.
 *
 * Usage in a benchmark file:
 *
 *   #include "../c_bench/timing.h"
 *
 *   typedef struct { ... } MyData;
 *
 *   static double run_once(void *vdata) {
 *       MyData *d = (MyData *)vdata;
 *       // ... kernel ...
 *       return checksum;
 *   }
 *
 *   int main(void) {
 *       int warmup = hb_get_env_int_or("BENCH_WARMUP", 3);
 *       int iters  = hb_get_env_int_or("BENCH_ITERS",  10);
 *       MyData d = { ... };
 *       hb_run_timed("main", warmup, iters, run_once, &d);
 *   }
 *
 * The output format is:
 *   benchmark[main]: min=X.XXX ms  mean=X.XXX ms
 * which matches the Repa and Hydrangea timing harnesses exactly.
 */

#pragma once

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/* --------------------------------------------------------------------------
 * Environment helpers
 * -------------------------------------------------------------------------- */

static inline int hb_get_env_int(const char *name)
{
    const char *val = getenv(name);
    if (!val) {
        fprintf(stderr, "error: required environment variable '%s' not set\n", name);
        exit(1);
    }
    char *end;
    long v = strtol(val, &end, 10);
    if (*end != '\0') {
        fprintf(stderr, "error: '%s' is not an integer: '%s'\n", name, val);
        exit(1);
    }
    return (int)v;
}

static inline int hb_get_env_int_or(const char *name, int fallback)
{
    const char *val = getenv(name);
    if (!val) return fallback;
    char *end;
    long v = strtol(val, &end, 10);
    if (*end != '\0') return fallback;
    return (int)v;
}

/* --------------------------------------------------------------------------
 * CSV I/O
 * -------------------------------------------------------------------------- */

/* Resolve a benchmark-relative path: try as-is first, then ../../<path>. */
static FILE *hb_open_csv(const char *path)
{
    FILE *f = fopen(path, "r");
    if (f) return f;
    char alt[4096];
    snprintf(alt, sizeof(alt), "../../%s", path);
    f = fopen(alt, "r");
    if (f) return f;
    fprintf(stderr, "error: cannot open '%s' (also tried '%s')\n", path, alt);
    exit(1);
}

/* Read up to max_n comma/newline/whitespace-separated doubles from path.
 * Returns actual count read. */
static inline int hb_read_csv_doubles(const char *path, double *out, int max_n)
{
    FILE *f = hb_open_csv(path);
    int n = 0;
    while (n < max_n) {
        double v;
        if (fscanf(f, " %lf", &v) != 1) break;
        out[n++] = v;
        int c = fgetc(f);
        if (c != ',' && c != EOF) ungetc(c, f);
    }
    fclose(f);
    return n;
}

/* Read up to max_n comma/newline/whitespace-separated ints from path.
 * Returns actual count read. */
static inline int hb_read_csv_ints(const char *path, int *out, int max_n)
{
    FILE *f = hb_open_csv(path);
    int n = 0;
    while (n < max_n) {
        int v;
        if (fscanf(f, " %d", &v) != 1) break;
        out[n++] = v;
        int c = fgetc(f);
        if (c != ',' && c != EOF) ungetc(c, f);
    }
    fclose(f);
    return n;
}

/* --------------------------------------------------------------------------
 * Timing harness
 * -------------------------------------------------------------------------- */

typedef double (*hb_kernel_fn)(void *data);

static inline double hb_clock_ms(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000.0 + ts.tv_nsec / 1e6;
}

static inline void hb_run_timed(const char *label,
                                 int warmup, int iters,
                                 hb_kernel_fn fn, void *data)
{
    /* Warmup */
    for (int i = 0; i < warmup; i++) {
        volatile double sink = fn(data);
        (void)sink;
    }

    double *times = (double *)malloc((size_t)iters * sizeof(double));
    if (!times) { fprintf(stderr, "error: malloc failed\n"); exit(1); }

    for (int i = 0; i < iters; i++) {
        double t0 = hb_clock_ms();
        volatile double sink = fn(data);
        (void)sink;
        times[i] = hb_clock_ms() - t0;
    }

    double min_ms = times[0], sum_ms = 0.0;
    for (int i = 0; i < iters; i++) {
        if (times[i] < min_ms) min_ms = times[i];
        sum_ms += times[i];
    }

    printf("benchmark[%s]: min=%.3f ms  mean=%.3f ms\n",
           label, min_ms, sum_ms / (double)iters);

    free(times);
}
