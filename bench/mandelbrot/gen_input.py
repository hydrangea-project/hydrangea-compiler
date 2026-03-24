#!/usr/bin/env python3
"""Generate a Mandelbrot benchmark description (no input files needed — all params are env vars)."""
import sys
print("Mandelbrot uses no input files. Set env vars:")
print("  MAND_W=<width> MAND_H=<height> MAND_ITERS=<max_iters>")
