/* bench/coo_csr_build/coo_csr_build_ref.c
 * COO → CSR build benchmark — C reference (qsort + serial prefix scan).
 *
 * Generates deterministic COO triplets with controlled duplicate runs, sorts
 * them by packed key (row*ncols + col) using qsort, deduplicates adjacent
 * entries, builds CSR row_ptr via a serial prefix scan, and returns a scalar
 * checksum of row_ptr_sum + col_idx_sum + vals_sum + canonical_count.
 *
 * Environment variables:
 *   COO_NROWS      -- logical row count   (required)
 *   COO_NCOLS      -- logical column count (required)
 *   COO_NNZ        -- input triplet count  (required)
 *   COO_DUP_PERIOD -- duplicate run length (required)
 *   BENCH_WARMUP   -- warmup iterations    (default 3)
 *   BENCH_ITERS    -- timed iterations     (default 10)
 */

#include <stdlib.h>
#include <string.h>
#include "../c_bench/timing.h"

typedef struct {
    int packed_key;
    int row;
    int col;
    int val;
} CooEntry;

static int coo_cmp(const void *a, const void *b)
{
    int ka = ((const CooEntry *)a)->packed_key;
    int kb = ((const CooEntry *)b)->packed_key;
    return (ka > kb) - (ka < kb);
}

typedef struct {
    int nrows, ncols, nnz, dup;
    CooEntry *entries;
    CooEntry *canonical;
    int      *row_ptr;
} CooData;

static double coo_run_once(void *vdata)
{
    CooData *d = (CooData *)vdata;
    int nrows = d->nrows, ncols = d->ncols, nnz = d->nnz, dup = d->dup;

    /* Generate input triplets */
    for (int i = 0; i < nnz; i++) {
        int g = i / dup;
        int row = (g * 17 + 3) % nrows;
        int col = (g * 31 + 7) % ncols;
        int val = g * 13 + i + 1;
        d->entries[i].packed_key = row * ncols + col;
        d->entries[i].row = row;
        d->entries[i].col = col;
        d->entries[i].val = val;
    }

    /* Sort by packed key */
    qsort(d->entries, (size_t)nnz, sizeof(CooEntry), coo_cmp);

    /* Deduplicate adjacent entries with the same key */
    int canonical_count = 0;
    for (int i = 0; i < nnz; i++) {
        if (canonical_count == 0 ||
            d->entries[i].packed_key != d->canonical[canonical_count - 1].packed_key) {
            d->canonical[canonical_count++] = d->entries[i];
        } else {
            d->canonical[canonical_count - 1].val += d->entries[i].val;
        }
    }

    /* Build CSR row_ptr via prefix scan */
    memset(d->row_ptr, 0, (size_t)(nrows + 1) * sizeof(int));
    for (int i = 0; i < canonical_count; i++) {
        d->row_ptr[d->canonical[i].row + 1]++;
    }
    for (int i = 0; i < nrows; i++) {
        d->row_ptr[i + 1] += d->row_ptr[i];
    }

    /* Compute checksums */
    long row_ptr_sum = 0, col_idx_sum = 0, vals_sum = 0;
    for (int i = 0; i <= nrows; i++) row_ptr_sum += d->row_ptr[i];
    for (int i = 0; i < canonical_count; i++) {
        col_idx_sum += d->canonical[i].col;
        vals_sum    += d->canonical[i].val;
    }

    return (double)(row_ptr_sum + col_idx_sum + vals_sum + canonical_count);
}

int main(void)
{
    int nrows  = hb_get_env_int("COO_NROWS");
    int ncols  = hb_get_env_int("COO_NCOLS");
    int nnz    = hb_get_env_int("COO_NNZ");
    int dup    = hb_get_env_int("COO_DUP_PERIOD");
    int warmup = hb_get_env_int_or("BENCH_WARMUP", 3);
    int iters  = hb_get_env_int_or("BENCH_ITERS",  10);

    CooEntry *entries   = (CooEntry *)malloc((size_t)nnz       * sizeof(CooEntry));
    CooEntry *canonical = (CooEntry *)malloc((size_t)nnz       * sizeof(CooEntry));
    int      *row_ptr   = (int      *)malloc((size_t)(nrows+1) * sizeof(int));

    CooData d = { nrows, ncols, nnz, dup, entries, canonical, row_ptr };
    hb_run_timed("main", warmup, iters, coo_run_once, &d);

    free(entries); free(canonical); free(row_ptr);
    return 0;
}
