# Hydrangea benchmark suite
# Usage:
#   make bench       -- compile + generate inputs + run all benchmarks
#   make compile     -- compile only
#   make inputs      -- generate only
#   make run         -- run only (requires compile + inputs first)
#   make clean-bench -- remove generated C/binary/output files

COMPILER  ?= cabal run hydrangea-compiler --
ROOT      := $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))/../
CC        ?= gcc
CFLAGS    ?= -O3 -march=native -fopenmp -lm

BENCHMARKS := blackscholes nbody mandelbrot spmv matmul weighted_histogram guarded_weighted_histogram coo_csr_build graph_messages voxel_rasterization voxel_trilinear_splat

# Default sizes for each benchmark
BS_N       ?= 1000000
NBODY_N    ?= 4096
MAND_W     ?= 1024
MAND_H     ?= 1024
MAND_ITERS ?= 256
SPMV_NROWS ?= 10000
SPMV_NCOLS ?= 10000
SPMV_NNZ   ?= 100000
MAT_M      ?= 512
MAT_K      ?= 512
MAT_N      ?= 512
WH_N       ?= 1000000
WH_BINS    ?= 256
GWH_N      ?= 1000000
GWH_BINS   ?= 256
GWH_KEEP_PERIOD ?= 3
COO_NROWS  ?= 1024
COO_NCOLS  ?= 1024
COO_NNZ    ?= 100000
COO_DUP_PERIOD ?= 4
GRAPH_NODES ?= 100000
GRAPH_DEGREE ?= 16
VOX_POINTS ?= 1000000
VOX_NX     ?= 64
VOX_NY     ?= 64
VOX_NZ     ?= 64
VOX_KEEP_PERIOD ?= 3
VSPLAT_POINTS ?= 1000000
VSPLAT_NX   ?= 64
VSPLAT_NY   ?= 64
VSPLAT_NZ   ?= 64
VSPLAT_KEEP_PERIOD ?= 3

.PHONY: bench compile inputs run clean-bench $(BENCHMARKS)

bench: compile inputs run

compile:
	@echo "=== Compiling benchmarks ==="
	@for b in $(BENCHMARKS); do \
	  echo "  $$b..."; \
	  if [ "$$b" = "voxel_trilinear_splat" ]; then \
	    $(COMPILER) --no-solver-check --emit-c bench/$$b/*.hyd > /dev/null; \
	  else \
	    $(COMPILER) --emit-c bench/$$b/*.hyd > /dev/null; \
	  fi; \
	done

# Generate C outputs next to .hyd files
blackscholes/blackscholes.c: blackscholes/blackscholes.hyd
	cd $(ROOT) && $(COMPILER) --emit-c bench/blackscholes/blackscholes.hyd > bench/blackscholes/blackscholes.c

nbody/nbody.c: nbody/nbody.hyd
	cd $(ROOT) && $(COMPILER) --emit-c bench/nbody/nbody.hyd > bench/nbody/nbody.c

mandelbrot/mandelbrot.c: mandelbrot/mandelbrot.hyd
	cd $(ROOT) && $(COMPILER) --emit-c bench/mandelbrot/mandelbrot.hyd > bench/mandelbrot/mandelbrot.c

spmv/spmv.c: spmv/spmv.hyd
	cd $(ROOT) && $(COMPILER) --emit-c bench/spmv/spmv.hyd > bench/spmv/spmv.c

inputs:
	@echo "=== Generating inputs ==="
	@python3 bench/blackscholes/gen_input.py $(BS_N) 2>/dev/null || python3 blackscholes/gen_input.py $(BS_N)
	@python3 bench/nbody/gen_input.py $(NBODY_N)    2>/dev/null || python3 nbody/gen_input.py $(NBODY_N)
	@python3 bench/spmv/gen_input.py $(SPMV_NROWS) $(SPMV_NCOLS) $(SPMV_NNZ) 2>/dev/null || \
	  python3 spmv/gen_input.py $(SPMV_NROWS) $(SPMV_NCOLS) $(SPMV_NNZ)

run:
	@echo "=== Running benchmarks ==="
	@echo "--- Black-Scholes (N=$(BS_N)) ---"
	@BS_N=$(BS_N) time cabal run hydrangea-compiler -- bench/blackscholes/blackscholes.hyd 2>&1 | tail -2
	@echo "--- N-body (N=$(NBODY_N)) ---"
	@NBODY_N=$(NBODY_N) time cabal run hydrangea-compiler -- bench/nbody/nbody.hyd 2>&1 | tail -2
	@echo "--- Mandelbrot ($(MAND_W)x$(MAND_H), iters=$(MAND_ITERS)) ---"
	@MAND_W=$(MAND_W) MAND_H=$(MAND_H) MAND_ITERS=$(MAND_ITERS) \
	  time cabal run hydrangea-compiler -- bench/mandelbrot/mandelbrot.hyd 2>&1 | tail -2
	@echo "--- SpMV ($(SPMV_NROWS)x$(SPMV_NCOLS), nnz=$(SPMV_NNZ)) ---"
	@SPMV_NROWS=$(SPMV_NROWS) SPMV_NCOLS=$(SPMV_NCOLS) SPMV_NNZ=$(SPMV_NNZ) \
	  time cabal run hydrangea-compiler -- bench/spmv/spmv.hyd 2>&1 | tail -2
	@echo "--- Weighted Histogram (N=$(WH_N), bins=$(WH_BINS)) ---"
	@WH_N=$(WH_N) WH_BINS=$(WH_BINS) time cabal run hydrangea-compiler -- bench/weighted_histogram/weighted_histogram.hyd 2>&1 | tail -2
	@echo "--- Guarded Weighted Histogram (N=$(GWH_N), bins=$(GWH_BINS), keep_period=$(GWH_KEEP_PERIOD)) ---"
	@GWH_N=$(GWH_N) GWH_BINS=$(GWH_BINS) GWH_KEEP_PERIOD=$(GWH_KEEP_PERIOD) time cabal run hydrangea-compiler -- bench/guarded_weighted_histogram/guarded_weighted_histogram.hyd 2>&1 | tail -2
	@echo "--- COO/CSR Build (nrows=$(COO_NROWS), ncols=$(COO_NCOLS), nnz=$(COO_NNZ), dup=$(COO_DUP_PERIOD)) ---"
	@COO_NROWS=$(COO_NROWS) COO_NCOLS=$(COO_NCOLS) COO_NNZ=$(COO_NNZ) COO_DUP_PERIOD=$(COO_DUP_PERIOD) \
	  time cabal run hydrangea-compiler -- bench/coo_csr_build/coo_csr_build.hyd 2>&1 | tail -2
	@echo "--- Graph Messages (nodes=$(GRAPH_NODES), degree=$(GRAPH_DEGREE)) ---"
	@GRAPH_NODES=$(GRAPH_NODES) GRAPH_DEGREE=$(GRAPH_DEGREE) \
	  time cabal run hydrangea-compiler -- bench/graph_messages/graph_messages.hyd 2>&1 | tail -2
	@echo "--- Voxel Rasterization (points=$(VOX_POINTS), grid=$(VOX_NX)x$(VOX_NY)x$(VOX_NZ), keep_period=$(VOX_KEEP_PERIOD)) ---"
	@VOX_POINTS=$(VOX_POINTS) VOX_NX=$(VOX_NX) VOX_NY=$(VOX_NY) VOX_NZ=$(VOX_NZ) VOX_KEEP_PERIOD=$(VOX_KEEP_PERIOD) \
	  time cabal run hydrangea-compiler -- bench/voxel_rasterization/voxel_rasterization.hyd 2>&1 | tail -2
	@echo "--- Voxel Trilinear Splat (points=$(VSPLAT_POINTS), grid=$(VSPLAT_NX)x$(VSPLAT_NY)x$(VSPLAT_NZ), keep_period=$(VSPLAT_KEEP_PERIOD)) ---"
	@VSPLAT_POINTS=$(VSPLAT_POINTS) VSPLAT_NX=$(VSPLAT_NX) VSPLAT_NY=$(VSPLAT_NY) VSPLAT_NZ=$(VSPLAT_NZ) VSPLAT_KEEP_PERIOD=$(VSPLAT_KEEP_PERIOD) \
	  time cabal run hydrangea-compiler -- --no-solver-check bench/voxel_trilinear_splat/voxel_trilinear_splat.hyd 2>&1 | tail -2

clean-bench:
	rm -f blackscholes/blackscholes.c blackscholes/out.csv blackscholes/blackscholes
	rm -f nbody/nbody.c nbody/out_xs.csv nbody/nbody
	rm -f mandelbrot/mandelbrot.c mandelbrot/out.csv mandelbrot/mandelbrot
	rm -f spmv/spmv.c spmv/out.csv spmv/spmv
	rm -f blackscholes/spots.csv blackscholes/strikes.csv blackscholes/rates.csv
	rm -f blackscholes/vols.csv blackscholes/times.csv
	rm -f nbody/xs.csv nbody/ys.csv nbody/zs.csv nbody/ms.csv
	rm -f nbody/vxs.csv nbody/vys.csv nbody/vzs.csv
	rm -f spmv/values.csv spmv/col_idx.csv spmv/row_ptr.csv spmv/x.csv
