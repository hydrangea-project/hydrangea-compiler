#ifndef HYDRANGEA_RUNTIME_H
#define HYDRANGEA_RUNTIME_H

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <math.h>

#include "simde/x86/sse2.h"
#include "simde/x86/avx.h"

#define HYD_MAX_DIMS 8

#define HYD_SIMD_FLOAT64_WIDTH 2
#define HYD_SIMD_FLOAT32_WIDTH 4

typedef simde__m128d hyd_float64x2_t;
typedef simde__m128 hyd_float32x4_t;
typedef simde__m256d hyd_float64x4_t;

static inline hyd_float64x2_t hyd_vec_loadu_f64(double* addr) {
    return simde_mm_loadu_pd(addr);
}

static inline hyd_float64x2_t hyd_vec_load_f64(double* addr) {
    return simde_mm_load_pd(addr);
}

static inline void hyd_vec_storeu_f64(double* addr, hyd_float64x2_t v) {
    simde_mm_storeu_pd(addr, v);
}

static inline void hyd_vec_store_f64(double* addr, hyd_float64x2_t v) {
    simde_mm_store_pd(addr, v);
}

static inline hyd_float64x2_t hyd_vec_add_f64(hyd_float64x2_t a, hyd_float64x2_t b) {
    return simde_mm_add_pd(a, b);
}

static inline hyd_float64x2_t hyd_vec_sub_f64(hyd_float64x2_t a, hyd_float64x2_t b) {
    return simde_mm_sub_pd(a, b);
}

static inline hyd_float64x2_t hyd_vec_mul_f64(hyd_float64x2_t a, hyd_float64x2_t b) {
    return simde_mm_mul_pd(a, b);
}

static inline hyd_float64x2_t hyd_vec_div_f64(hyd_float64x2_t a, hyd_float64x2_t b) {
    return simde_mm_div_pd(a, b);
}

static inline hyd_float64x2_t hyd_vec_set1_f64(double v) {
    return simde_mm_set1_pd(v);
}

static inline hyd_float64x2_t hyd_vec_zero_f64(void) {
    return simde_mm_setzero_pd();
}

static inline hyd_float32x4_t hyd_vec_loadu_f32(float* addr) {
    return simde_mm_loadu_ps(addr);
}

static inline hyd_float32x4_t hyd_vec_load_f32(float* addr) {
    return simde_mm_load_ps(addr);
}

static inline void hyd_vec_storeu_f32(float* addr, hyd_float32x4_t v) {
    simde_mm_storeu_ps(addr, v);
}

static inline void hyd_vec_store_f32(float* addr, hyd_float32x4_t v) {
    simde_mm_store_ps(addr, v);
}

static inline hyd_float32x4_t hyd_vec_add_f32(hyd_float32x4_t a, hyd_float32x4_t b) {
    return simde_mm_add_ps(a, b);
}

static inline hyd_float32x4_t hyd_vec_sub_f32(hyd_float32x4_t a, hyd_float32x4_t b) {
    return simde_mm_sub_ps(a, b);
}

static inline hyd_float32x4_t hyd_vec_mul_f32(hyd_float32x4_t a, hyd_float32x4_t b) {
    return simde_mm_mul_ps(a, b);
}

static inline hyd_float32x4_t hyd_vec_div_f32(hyd_float32x4_t a, hyd_float32x4_t b) {
    return simde_mm_div_ps(a, b);
}

static inline hyd_float32x4_t hyd_vec_set1_f32(float v) {
    return simde_mm_set1_ps(v);
}

static inline hyd_float32x4_t hyd_vec_zero_f32(void) {
    return simde_mm_setzero_ps();
}

static inline double hyd_vec_reduce_add_f64(hyd_float64x2_t v) {
    return simde_mm_cvtsd_f64(v) + simde_mm_cvtsd_f64(simde_mm_unpackhi_pd(v, v));
}

static inline double hyd_vec_reduce_mul_f64(hyd_float64x2_t v) {
    return simde_mm_cvtsd_f64(v) * simde_mm_cvtsd_f64(simde_mm_unpackhi_pd(v, v));
}

static inline hyd_float64x4_t hyd_vec_loadu_f64x4(double* addr) {
    return simde_mm256_loadu_pd(addr);
}

static inline void hyd_vec_storeu_f64x4(double* addr, hyd_float64x4_t v) {
    simde_mm256_storeu_pd(addr, v);
}

static inline hyd_float64x4_t hyd_vec_add_f64x4(hyd_float64x4_t a, hyd_float64x4_t b) {
    return simde_mm256_add_pd(a, b);
}

static inline hyd_float64x4_t hyd_vec_sub_f64x4(hyd_float64x4_t a, hyd_float64x4_t b) {
    return simde_mm256_sub_pd(a, b);
}

static inline hyd_float64x4_t hyd_vec_mul_f64x4(hyd_float64x4_t a, hyd_float64x4_t b) {
    return simde_mm256_mul_pd(a, b);
}

static inline hyd_float64x4_t hyd_vec_div_f64x4(hyd_float64x4_t a, hyd_float64x4_t b) {
    return simde_mm256_div_pd(a, b);
}

static inline hyd_float64x4_t hyd_vec_set1_f64x4(double v) {
    return simde_mm256_set1_pd(v);
}

static inline hyd_float64x4_t hyd_vec_zero_f64x4(void) {
    return simde_mm256_setzero_pd();
}

static inline hyd_float64x4_t hyd_vec_sqrt_f64x4(hyd_float64x4_t v) {
    return simde_mm256_sqrt_pd(v);
}

static inline hyd_float64x4_t hyd_vec_log_f64x4(hyd_float64x4_t v) {
    double lanes[4];
    simde_mm256_storeu_pd(lanes, v);
    lanes[0] = log(lanes[0]); lanes[1] = log(lanes[1]);
    lanes[2] = log(lanes[2]); lanes[3] = log(lanes[3]);
    return simde_mm256_loadu_pd(lanes);
}

static inline hyd_float64x4_t hyd_vec_exp_f64x4(hyd_float64x4_t v) {
    double lanes[4];
    simde_mm256_storeu_pd(lanes, v);
    lanes[0] = exp(lanes[0]); lanes[1] = exp(lanes[1]);
    lanes[2] = exp(lanes[2]); lanes[3] = exp(lanes[3]);
    return simde_mm256_loadu_pd(lanes);
}

static inline hyd_float64x4_t hyd_vec_erf_f64x4(hyd_float64x4_t v) {
    double lanes[4];
    simde_mm256_storeu_pd(lanes, v);
    lanes[0] = erf(lanes[0]); lanes[1] = erf(lanes[1]);
    lanes[2] = erf(lanes[2]); lanes[3] = erf(lanes[3]);
    return simde_mm256_loadu_pd(lanes);
}

static inline double hyd_vec_reduce_add_f64x4(hyd_float64x4_t v) {
    simde__m128d lo = simde_mm256_extractf128_pd(v, 0);
    simde__m128d hi = simde_mm256_extractf128_pd(v, 1);
    simde__m128d s  = simde_mm_add_pd(lo, hi);
    return simde_mm_cvtsd_f64(s) + simde_mm_cvtsd_f64(simde_mm_unpackhi_pd(s, s));
}

static inline double hyd_vec_reduce_mul_f64x4(hyd_float64x4_t v) {
    simde__m128d lo = simde_mm256_extractf128_pd(v, 0);
    simde__m128d hi = simde_mm256_extractf128_pd(v, 1);
    simde__m128d p  = simde_mm_mul_pd(lo, hi);
    return simde_mm_cvtsd_f64(p) * simde_mm_cvtsd_f64(simde_mm_unpackhi_pd(p, p));
}

static inline hyd_float64x2_t hyd_vec_sqrt_f64(hyd_float64x2_t v) {
    return simde_mm_sqrt_pd(v);
}

static inline hyd_float64x2_t hyd_vec_log_f64(hyd_float64x2_t v) {
    double lanes[2];
    simde_mm_storeu_pd(lanes, v);
    lanes[0] = log(lanes[0]);
    lanes[1] = log(lanes[1]);
    return simde_mm_loadu_pd(lanes);
}

static inline hyd_float64x2_t hyd_vec_exp_f64(hyd_float64x2_t v) {
    double lanes[2];
    simde_mm_storeu_pd(lanes, v);
    lanes[0] = exp(lanes[0]);
    lanes[1] = exp(lanes[1]);
    return simde_mm_loadu_pd(lanes);
}

static inline hyd_float64x2_t hyd_vec_erf_f64(hyd_float64x2_t v) {
    double lanes[2];
    simde_mm_storeu_pd(lanes, v);
    lanes[0] = erf(lanes[0]);
    lanes[1] = erf(lanes[1]);
    return simde_mm_loadu_pd(lanes);
}

typedef struct {
    int64_t elems[HYD_MAX_DIMS];
    int ndims;
} hyd_tuple_t;

typedef struct {
    unsigned char* data;
    _Atomic int64_t refcount;
} hyd_array_storage_t;

typedef struct {
    hyd_array_storage_t* storage;
    unsigned char* data;
    size_t elem_size;
    hyd_tuple_t shape;
} hyd_array_t;

static hyd_tuple_t hyd_tuple_make(int n, ...) {
    hyd_tuple_t t;
    t.ndims = n;
    va_list args;
    va_start(args, n);
    for (int i = 0; i < n; i++) {
        t.elems[i] = va_arg(args, int64_t);
    }
    va_end(args);
    return t;
}

static int64_t hyd_shape_size(hyd_tuple_t shape) {
    int64_t size = 1;
    for (int i = 0; i < shape.ndims; i++) {
        size *= shape.elems[i];
    }
    return size;
}

static hyd_tuple_t hyd_shape_init(hyd_tuple_t shape) {
    hyd_tuple_t out;
    if (shape.ndims <= 0) {
        out.ndims = 0;
        return out;
    }
    out.ndims = shape.ndims - 1;
    for (int i = 0; i < out.ndims; i++) {
        out.elems[i] = shape.elems[i];
    }
    return out;
}

static int64_t hyd_shape_last(hyd_tuple_t shape) {
    if (shape.ndims <= 0) {
        return 0;
    }
    return shape.elems[shape.ndims - 1];
}

static hyd_tuple_t hyd_slice_shape(hyd_tuple_t srcShape, hyd_tuple_t sliceSpecs) {
    hyd_tuple_t outShape;
    outShape.ndims = srcShape.ndims;
    for (int i = 0; i < srcShape.ndims; i++) {
        int64_t start = sliceSpecs.elems[i * 2];
        int64_t len = sliceSpecs.elems[i * 2 + 1];
        if (start == -1 && len == -1) {
            outShape.elems[i] = srcShape.elems[i];
        } else {
            outShape.elems[i] = len;
        }
    }
    return outShape;
}

static hyd_array_t* hyd_array_make_header(hyd_array_storage_t* storage, hyd_tuple_t shape, size_t elem_size) {
    hyd_array_t* arr = (hyd_array_t*)malloc(sizeof(hyd_array_t));
    arr->storage = storage;
    arr->data = storage->data;
    arr->elem_size = elem_size;
    arr->shape = shape;
    return arr;
}

static hyd_array_t* hyd_array_alloc_bytes(hyd_tuple_t shape, size_t elem_size) {
    int64_t size = hyd_shape_size(shape);
    hyd_array_storage_t* storage = (hyd_array_storage_t*)malloc(sizeof(hyd_array_storage_t));
    storage->data = (unsigned char*)calloc((size_t)size, elem_size);
    storage->refcount = 1;
    return hyd_array_make_header(storage, shape, elem_size);
}

static hyd_array_t* hyd_array_alloc(hyd_tuple_t shape) {
    return hyd_array_alloc_bytes(shape, sizeof(int64_t));
}

static hyd_array_t* hyd_array_reshape_view(hyd_array_t* arr, hyd_tuple_t shape) {
    int64_t srcSize = hyd_shape_size(arr->shape);
    int64_t dstSize = hyd_shape_size(shape);
    if (srcSize != dstSize) {
        fprintf(stderr,
                "hydrangea: reshape view size mismatch (%lld elements to %lld)\n",
                (long long)srcSize,
                (long long)dstSize);
        exit(1);
    }
    arr->storage->refcount += 1;
    return hyd_array_make_header(arr->storage, shape, arr->elem_size);
}

static void hyd_array_free(hyd_array_t* arr) {
    if (arr == NULL) {
        return;
    }
    arr->storage->refcount -= 1;
    if (arr->storage->refcount == 0) {
        free(arr->storage->data);
        free(arr->storage);
    }
    free(arr);
}

static int64_t hyd_array_length(hyd_array_t* arr) {
    return hyd_shape_size(arr->shape);
}

static int64_t hyd_array_ndims(hyd_array_t* arr) {
    return arr->shape.ndims;
}

static int64_t hyd_array_dim(hyd_array_t* arr, int dim) {
    return arr->shape.elems[dim];
}

static int64_t* hyd_array_data_int64(hyd_array_t* arr) {
    return (int64_t*)(void*)arr->data;
}

static double* hyd_array_data_f64(hyd_array_t* arr) {
    return (double*)(void*)arr->data;
}

static hyd_tuple_t hyd_flat_to_nd(int64_t flat, hyd_tuple_t shape) {
    hyd_tuple_t idx;
    idx.ndims = shape.ndims;
    int64_t remaining = flat;
    for (int i = shape.ndims - 1; i >= 0; i--) {
        idx.elems[i] = remaining % shape.elems[i];
        remaining /= shape.elems[i];
    }
    return idx;
}

static int64_t hyd_nd_to_flat(hyd_tuple_t nd, hyd_tuple_t shape) {
    int64_t flat = 0;
    int64_t stride = 1;
    for (int i = shape.ndims - 1; i >= 0; i--) {
        flat += nd.elems[i] * stride;
        stride *= shape.elems[i];
    }
    return flat;
}

typedef struct {
    int64_t key;
    int64_t idx;
} hyd_sort_index_entry_t;

/* LSD radix sort on hyd_sort_index_entry_t arrays by key, 8 bits per pass.
   Determines the number of passes from the actual max key so short-key
   workloads (e.g. packed row*ncols+col with nrows=ncols=4096) use only 3
   passes instead of 8, cutting sort time roughly in half versus qsort. */
static void hyd_radix_sort_entries(hyd_sort_index_entry_t* a,
                                   hyd_sort_index_entry_t* b,
                                   int64_t n) {
    /* Find max key to determine number of 8-bit passes needed. */
    int64_t max_key = 0;
    for (int64_t i = 0; i < n; i++) {
        if (a[i].key > max_key) max_key = a[i].key;
    }
    int passes = 0;
    int64_t mk = max_key;
    while (mk) { passes++; mk >>= 8; }
    if (passes == 0) passes = 1;

    hyd_sort_index_entry_t* src = a, *dst = b;
    for (int p = 0; p < passes; p++) {
        int shift = p * 8;
        int64_t count[256];
        memset(count, 0, sizeof(count));
        for (int64_t i = 0; i < n; i++) count[(src[i].key >> shift) & 0xFF]++;
        int64_t sum = 0;
        for (int j = 0; j < 256; j++) { int64_t c = count[j]; count[j] = sum; sum += c; }
        for (int64_t i = 0; i < n; i++) {
            int64_t d = (src[i].key >> shift) & 0xFF;
            dst[count[d]++] = src[i];
        }
        hyd_sort_index_entry_t* tmp = src; src = dst; dst = tmp;
    }
    /* After an odd number of passes the result lives in b; copy it back to a. */
    if (passes % 2 == 1) {
        memcpy(a, b, (size_t)n * sizeof(hyd_sort_index_entry_t));
    }
}

static hyd_array_t* hyd_sort_indices(hyd_array_t* keys) {
    if (keys->shape.ndims != 1 || keys->elem_size != sizeof(int64_t)) {
        fprintf(stderr, "hyd_sort_indices: expected a 1D int64 array\n");
        exit(1);
    }
    int64_t n = hyd_shape_size(keys->shape);
    hyd_array_t* out = hyd_array_alloc(keys->shape);
    int64_t* key_data = (int64_t*)(void*)keys->data;
    int64_t* out_data = (int64_t*)(void*)out->data;
    hyd_sort_index_entry_t* entries =
        (hyd_sort_index_entry_t*)malloc((size_t)n * sizeof(hyd_sort_index_entry_t));
    hyd_sort_index_entry_t* tmp =
        (hyd_sort_index_entry_t*)malloc((size_t)n * sizeof(hyd_sort_index_entry_t));
    if (entries == NULL || tmp == NULL) {
        fprintf(stderr, "hyd_sort_indices: allocation failed\n");
        exit(1);
    }
    for (int64_t i = 0; i < n; i++) {
        entries[i].key = key_data[i];
        entries[i].idx = i;
    }
    hyd_radix_sort_entries(entries, tmp, n);
    for (int64_t i = 0; i < n; i++) {
        out_data[i] = entries[i].idx;
    }
    free(entries);
    free(tmp);
    return out;
}

static void hyd_print_int(int64_t v) {
    printf("%lld\n", (long long)v);
}

static void hyd_print_double(double v) {
    printf("%.17g\n", v);
}

static void hyd_print_array(hyd_array_t* arr) {
    int64_t size = hyd_shape_size(arr->shape);
    int64_t* data = (int64_t*)(void*)arr->data;
    printf("[");
    for (int64_t i = 0; i < size; i++) {
        if (i > 0) printf(", ");
        printf("%lld", (long long)data[i]);
    }
    printf("] (shape: [");
    for (int i = 0; i < arr->shape.ndims; i++) {
        if (i > 0) printf(", ");
        printf("%lld", (long long)arr->shape.elems[i]);
    }
    printf("])\n");
}

static void hyd_print_tuple(hyd_tuple_t t) {
    printf("(");
    for (int i = 0; i < t.ndims; i++) {
        if (i > 0) printf(", ");
        printf("%lld", (long long)t.elems[i]);
    }
    printf(")\n");
}

static inline double hyd_array_get_float(hyd_array_t* arr, int64_t idx) {
    double v;
    memcpy(&v, arr->data + ((size_t)idx * arr->elem_size), sizeof(double));
    return v;
}

static inline void hyd_array_set_float(hyd_array_t* arr, int64_t idx, double v) {
    memcpy(arr->data + ((size_t)idx * arr->elem_size), &v, sizeof(double));
}

static void hyd_print_float_array(hyd_array_t* arr) {
    int64_t size = hyd_shape_size(arr->shape);
    printf("[");
    for (int64_t i = 0; i < size; i++) {
        if (i > 0) printf(", ");
        double v;
        memcpy(&v, arr->data + ((size_t)i * arr->elem_size), sizeof(double));
        printf("%.17g", v);
    }
    printf("] (shape: [");
    for (int i = 0; i < arr->shape.ndims; i++) {
        if (i > 0) printf(", ");
        printf("%lld", (long long)arr->shape.elems[i]);
    }
    printf("])\n");
}

/* Generic pair printing using _Generic (C11). */
#define _hyd_print_val(x) _Generic((x), \
    double: printf("%.17g", (double)(x)), \
    float:  printf("%.17g", (double)(x)), \
    default: printf("%lld", (long long)(x)))
#define hyd_print_pair(a, b) do { \
    printf("("); _hyd_print_val(a); printf(", "); _hyd_print_val(b); printf(")\n"); \
} while(0)

static hyd_array_t* hyd_read_float_array_csv(const char* filename, hyd_tuple_t shape) {
    hyd_array_t* arr = hyd_array_alloc(shape);
    int64_t size = hyd_shape_size(shape);
    FILE* f = fopen(filename, "r");
    if (!f) { fprintf(stderr, "read_array_float: cannot open '%s'\n", filename); exit(1); }
    for (int64_t i = 0; i < size; i++) {
        if (i > 0) {
            int c;
            while ((c = fgetc(f)) != EOF && (c == ',' || c == ' ' || c == '\n' || c == '\r'));
            if (c == EOF) { fprintf(stderr, "read_array_float: too few values\n"); exit(1); }
            ungetc(c, f);
        }
        double val;
        if (fscanf(f, "%lf", &val) != 1) {
            fprintf(stderr, "read_array_float: parse error at element %lld\n", (long long)i);
            exit(1);
        }
        memcpy(arr->data + ((size_t)i * arr->elem_size), &val, sizeof(double));
    }
    fclose(f);
    return arr;
}

static hyd_array_t* hyd_read_array_csv(const char* filename, hyd_tuple_t shape) {
    hyd_array_t* arr = hyd_array_alloc(shape);
    int64_t size = hyd_shape_size(shape);
    FILE* f = fopen(filename, "r");
    if (!f) { fprintf(stderr, "read_array: cannot open '%s'\n", filename); exit(1); }
    for (int64_t i = 0; i < size; i++) {
        if (i > 0) {
            int c;
            /* Skip commas and whitespace */
            while ((c = fgetc(f)) != EOF && (c == ',' || c == ' ' || c == '\n' || c == '\r'));
            if (c == EOF) { fprintf(stderr, "read_array: too few values\n"); exit(1); }
            ungetc(c, f);
        }
        if (fscanf(f, "%lld", &((int64_t*)(void*)arr->data)[i]) != 1) {
            fprintf(stderr, "read_array: parse error at element %lld\n", (long long)i);
            exit(1);
        }
    }
    fclose(f);
    return arr;
}

/* Backwards-compatible extern helpers used by generated C. Some lowered
   code calls `getenv_int` / `getenv_string` and runtime helper files (in
   runtime/) provide these symbols. Declare them here so emitted C can
   call them without implicit-declaration warnings. */

extern int64_t getenv_int(const char* name);
extern const char* getenv_string(const char* name);

static int64_t hyd_get_env_int(const char* name) {
    char* val = getenv(name);
    if (!val) {
        fprintf(stderr, "get_env_int: environment variable '%s' not set\n", name);
        exit(1);
    }
    char* end;
    long result = strtol(val, &end, 10);
    if (*end != '\0' || val == end) {
        fprintf(stderr, "get_env_int: cannot parse '%s' as int\n", val);
        exit(1);
    }
    return result;
}

static const char* hyd_get_env_string(const char* name) {
    char* val = getenv(name);
    if (!val) {
        fprintf(stderr, "get_env_string: environment variable '%s' not set\n", name);
        exit(1);
    }
    return val;
}

/* Also declare the CSV write helpers (implemented in runtime/hyd_write_csv.c)
   so emitted C that calls `hyd_write_array_csv` / `hyd_write_array_csv_float`
   has prototypes available at compile time. */
extern int64_t hyd_write_array_csv(hyd_array_t* arr, const char* filename);
extern int64_t hyd_write_array_csv_float(hyd_array_t* arr, const char* filename);

#endif /* HYDRANGEA_RUNTIME_H */
