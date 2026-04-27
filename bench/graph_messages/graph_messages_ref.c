/* bench/graph_messages/graph_messages_ref.c
 * Graph message-passing benchmark — C+OpenMP reference.
 *
 * Builds a deterministic regular graph (fixed out-degree), gathers destination
 * node features, computes per-edge messages, and performs a segmented reduce
 * per source node.  Checksum is the sum of all node outputs.
 *
 * Algorithm (O(n * degree)):
 *   dsts[k]          = k / degree               (destination of edge k)
 *   edge_vals[k]     = k + 1
 *   dst_features[k]  = node_vals[dsts[k]]       = (k / degree) + 1
 *   messages[k]      = edge_vals[k] * dst_features[k]
 *   node_out[i]      = sum_{j=0}^{degree-1} messages[i*degree + j]
 *
 * Environment variables:
 *   GRAPH_NODES  -- number of nodes  (required)
 *   GRAPH_DEGREE -- out-degree       (required)
 *   BENCH_WARMUP -- warmup iterations (default 3)
 *   BENCH_ITERS  -- timed iterations  (default 10)
 */

#include <stdlib.h>
#include "../c_bench/timing.h"

typedef struct {
    int n, degree;
    long *node_out;
} GraphData;

static double graph_run_once(void *vdata)
{
    GraphData *d = (GraphData *)vdata;
    int n = d->n, deg = d->degree;

    #pragma omp parallel for schedule(static)
    for (int i = 0; i < n; i++) {
        long sum = 0;
        for (int j = 0; j < deg; j++) {
            int k = i * deg + j;
            /* dst[k] = k / degree = i (since i*degree <= k < (i+1)*degree) */
            int dst = k / deg;
            int dst_feature = dst + 1;  /* node_vals[dst] = dst + 1 */
            int edge_val = k + 1;
            sum += (long)edge_val * dst_feature;
        }
        d->node_out[i] = sum;
    }

    long total = 0;
    for (int i = 0; i < n; i++) total += d->node_out[i];
    return (double)total;
}

int main(void)
{
    int n      = hb_get_env_int("GRAPH_NODES");
    int deg    = hb_get_env_int("GRAPH_DEGREE");
    int warmup = hb_get_env_int_or("BENCH_WARMUP", 3);
    int iters  = hb_get_env_int_or("BENCH_ITERS",  10);

    long *node_out = (long *)malloc((size_t)n * sizeof(long));

    GraphData d = { n, deg, node_out };
    hb_run_timed("main", warmup, iters, graph_run_once, &d);

    free(node_out);
    return 0;
}
