#include "hydrangea_runtime.h"
#include <omp.h>

static int __scalar_computed_h = 0;
static int64_t __scalar_cache_h;
int64_t h(void) {
    if (!__scalar_computed_h) {
        __scalar_computed_h = 1;
        int64_t t0 = getenv_int("STENCIL_H");
        __scalar_cache_h = t0;
    }
    return __scalar_cache_h;
}
static int __scalar_computed_w = 0;
static int64_t __scalar_cache_w;
int64_t w(void) {
    if (!__scalar_computed_w) {
        __scalar_computed_w = 1;
        int64_t t1 = getenv_int("STENCIL_W");
        __scalar_cache_w = t1;
    }
    return __scalar_cache_w;
}
static hyd_array_t* __cache_input = NULL;
hyd_array_t* input(void) {
    if (__cache_input == NULL) {
        int64_t t2 = h();
        int64_t t3 = w();
        hyd_tuple_t t4 = (hyd_tuple_t){.ndims=2,.elems={(int64_t)t2, (int64_t)t3}};
        hyd_array_t* t5 = hyd_read_float_array_csv("bench/stencil/input.csv", t4);
        __cache_input = t5;
    }
    return hyd_array_reshape_view(__cache_input, __cache_input->shape);
}
static hyd_array_t* __cache_laplacian = NULL;
hyd_array_t* laplacian(void) {
    if (__cache_laplacian == NULL) {
        int64_t t6 = h();
        int64_t t7 = (t6 - 2LL);
        int64_t t8 = w();
        int64_t t9 = (t8 - 2LL);
        hyd_tuple_t t10 = (hyd_tuple_t){.ndims=2,.elems={(int64_t)t7, (int64_t)t9}};
        hyd_array_t* arr11 = hyd_array_alloc_bytes(t10, sizeof(double));
        hyd_array_t* t72 = input();
        double t109 = (-4.0);
        hyd_tuple_t shp73 = t72->shape;
        /* parallel map loop */
        #pragma omp parallel for collapse(2)
        for (int64_t i14 = 0; i14 < t7; i14++) {
            for (int64_t j15 = 0; j15 < t9; j15++) {
                int64_t t68 = (1LL + i14);
                int64_t t69 = (t68 - 1LL);
                int64_t t70 = (1LL + j15);
                hyd_tuple_t t71 = (hyd_tuple_t){.ndims=2,.elems={(int64_t)t69, (int64_t)t70}};
                int64_t off74 = (((long)(t69)) * shp73.elems[1] + (long)(t70));
                double val67 = (((double*)(void*)t72->data)[off74]);
                int64_t t77 = (1LL + t68);
                hyd_tuple_t t79 = (hyd_tuple_t){.ndims=2,.elems={(int64_t)t77, (int64_t)t70}};
                int64_t off82 = (((long)(t77)) * shp73.elems[1] + (long)(t70));
                double val75 = (((double*)(void*)t72->data)[off82]);
                double t83 = (val67 + val75);
                int64_t t87 = (t70 - 1LL);
                hyd_tuple_t t88 = (hyd_tuple_t){.ndims=2,.elems={(int64_t)t68, (int64_t)t87}};
                int64_t off91 = (((long)(t68)) * shp73.elems[1] + (long)(t87));
                double val84 = (((double*)(void*)t72->data)[off91]);
                double t92 = (t83 + val84);
                int64_t t96 = (1LL + t70);
                hyd_tuple_t t97 = (hyd_tuple_t){.ndims=2,.elems={(int64_t)t68, (int64_t)t96}};
                int64_t off100 = (((long)(t68)) * shp73.elems[1] + (long)(t96));
                double val93 = (((double*)(void*)t72->data)[off100]);
                double t101 = (t92 + val93);
                hyd_tuple_t t105 = (hyd_tuple_t){.ndims=2,.elems={(int64_t)t68, (int64_t)t70}};
                int64_t off108 = (((long)(t68)) * shp73.elems[1] + (long)(t70));
                double val102 = (((double*)(void*)t72->data)[off108]);
                double t110 = (t109 * val102);
                double t111 = (t101 + t110);
                int64_t mul18 = (i14 * t9);
                int64_t flat19 = (j15 + mul18);
                (((double*)(void*)arr11->data)[flat19]) = t111;
            }
        }
        hyd_array_free(t72);
        __cache_laplacian = arr11;
    }
    return hyd_array_reshape_view(__cache_laplacian, __cache_laplacian->shape);
}
int64_t hyd_main(void) {
    hyd_array_t* t112 = laplacian();
    int64_t _discarded = hyd_write_array_csv_float(t112, "bench/stencil/out_interior.csv");
    hyd_array_free(t112);
    return 0;
}

int main(void) {
    int64_t h_result = h();
    hyd_print_int(h_result);
    int64_t w_result = w();
    hyd_print_int(w_result);
    hyd_array_t* input_result = input();
    hyd_print_float_array(input_result);
    hyd_array_t* laplacian_result = laplacian();
    hyd_print_float_array(laplacian_result);
    int64_t hyd_main_result = hyd_main();
    hyd_print_int(hyd_main_result);
    return 0;
}
