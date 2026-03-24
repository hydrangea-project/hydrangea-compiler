// Small runtime helpers for writing arrays to CSV used by lowering.
#include <stdio.h>
#include <stdlib.h>
#include "hydrangea_runtime.h"

static void hyd_write_row_int(FILE* f, hyd_array_t* arr, int64_t start, int64_t count) {
    for (int64_t j = 0; j < count; ++j) {
        if (j) fputc(',', f);
        fprintf(f, "%lld", (long long)((int64_t*)(void*)arr->data)[start + j]);
    }
    fputc('\n', f);
}

static void hyd_write_row_float(FILE* f, hyd_array_t* arr, int64_t start, int64_t count) {
    for (int64_t j = 0; j < count; ++j) {
        if (j) fputc(',', f);
        double v;
        memcpy(&v, arr->data + ((size_t)(start + j) * arr->elem_size), sizeof(double));
        fprintf(f, "%.17g", v);
    }
    fputc('\n', f);
}

int64_t hyd_write_array_csv(hyd_array_t* arr, const char* filename) {
    FILE* f = fopen(filename, "w");
    if (!f) { perror("fopen"); exit(1); }
    int ndims = arr->shape.ndims;
    int64_t size = hyd_shape_size(arr->shape);
    if (ndims == 2) {
        int64_t rows = arr->shape.elems[0];
        int64_t cols = arr->shape.elems[1];
        for (int64_t i = 0; i < rows; ++i) {
            hyd_write_row_int(f, arr, i * cols, cols);
        }
    } else {
        hyd_write_row_int(f, arr, 0, size);
    }
    fclose(f);
    return 0;
}

int64_t hyd_write_array_csv_float(hyd_array_t* arr, const char* filename) {
    FILE* f = fopen(filename, "w");
    if (!f) { perror("fopen"); exit(1); }
    int ndims = arr->shape.ndims;
    int64_t size = hyd_shape_size(arr->shape);
    if (ndims == 2) {
        int64_t rows = arr->shape.elems[0];
        int64_t cols = arr->shape.elems[1];
        for (int64_t i = 0; i < rows; ++i) {
            hyd_write_row_float(f, arr, i * cols, cols);
        }
    } else {
        hyd_write_row_float(f, arr, 0, size);
    }
    fclose(f);
    return 0;
}

// Expose getenv helpers used by lowering
int64_t getenv_int(const char* name) {
    char* val = getenv(name);
    if (!val) { fprintf(stderr, "environment variable %s not set\n", name); exit(1); }
    return atoll(val);
}

const char* getenv_string(const char* name) {
    char* val = getenv(name);
    if (!val) { fprintf(stderr, "environment variable %s not set\n", name); exit(1); }
    return val;
}
