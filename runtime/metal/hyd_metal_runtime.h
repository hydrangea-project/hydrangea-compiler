#pragma once
/*
 * hyd_metal_runtime.h — lightweight helpers for the Hydrangea Metal harness.
 *
 * Included by the generated ObjC harness (harness.m), not by .metal kernels.
 * Shape helpers needed inside kernels are inlined in the generated .metal file.
 */

#include <stdint.h>
#include <stddef.h>

#define HYD_METAL_MAX_DIMS 8

/* Packed shape descriptor used when passing shape info to the harness. */
typedef struct {
    uint32_t elems[HYD_METAL_MAX_DIMS];
    int      ndims;
} hyd_metal_shape_t;

/* Convert a Hydrangea int64_t shape tuple to a uint32_t array suitable for a
 * Metal constant buffer. */
static inline void
hyd_metal_shape_from_int64(const int64_t* src, int ndims, uint32_t* dst)
{
    for (int i = 0; i < ndims; i++)
        dst[i] = (uint32_t)src[i];
}

/* Compute the number of elements in a flat shape descriptor. */
static inline uint64_t
hyd_metal_shape_size(const uint32_t* shape, int ndims)
{
    uint64_t n = 1;
    for (int i = 0; i < ndims; i++)
        n *= (uint64_t)shape[i];
    return n;
}
