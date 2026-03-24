// Simple binary CSV generator for matrices used by the benchmark.
// Writes row-major CSV files: M rows, N columns.
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main(int argc, char** argv) {
    if (argc != 4) {
        fprintf(stderr, "Usage: %s M N out.csv\n", argv[0]);
        return 1;
    }
    int M = atoi(argv[1]);
    int N = atoi(argv[2]);
    const char* out = argv[3];

    FILE* f = fopen(out, "w");
    if (!f) { perror("fopen"); return 1; }
    srand(0);
    for (int i = 0; i < M; ++i) {
        for (int j = 0; j < N; ++j) {
            double v = (double)(rand()) / (double)RAND_MAX;
            fprintf(f, "%.17g", v);
            if (j + 1 < N) fputc(',', f);
        }
        fputc('\n', f);
    }
    fclose(f);
    return 0;
}
