# Array kernels and primitives

This page lists the array-oriented primitives supported by Hydrangea today.
For each entry, the syntax matches the surface language, and the argument
descriptions follow the compiler's current naming.

## Core combinators

| Kernel | Syntax | Arguments | Result |
|---|---|---|---|
| `generate` | `generate [shape] fn` | `shape`: array shape; `fn`: index function (`[i, j, ...] -> value`) | Array of the requested shape |
| `fill` | `fill [shape] value` | `shape`: array shape; `value`: element value | Array of the requested shape |
| `map` | `map fn array` | `fn`: unary function; `array`: input array | Array with the same shape |
| `zipwith` | `zipwith fn array1 array2` | `fn`: binary function; `array1`, `array2`: same-shaped arrays | Array with the same shape |
| `reduce` | `reduce fn init array` | `fn`: accumulator function; `init`: initial accumulator; `array`: input array | Array with the trailing/rightmost dimension removed |
| `reduce_generate` | `reduce_generate combine init shape gen` | `combine`: accumulator function; `init`: initial accumulator; `shape`: generated shape; `gen`: generator function | Reduced array without materializing the intermediate |
| `foldl` | `foldl fn init array` | `fn`: strict left-fold function; `init`: accumulator; `array`: 1-D array | Final accumulator value |
| `foldl_while` | `foldl_while pred fn init array` | `pred`: continuation predicate; `fn`: step function; `init`: accumulator; `array`: 1-D array | Final accumulator value |

## Scans and segmented reduction

| Kernel | Syntax | Arguments | Result |
|---|---|---|---|
| `scan` | `scan fn init array` | `fn`: step function; `init`: initial state; `array`: 1-D array | Exclusive prefix-scan array |
| `scan_inclusive` | `scan_inclusive fn init array` | `fn`: step function; `init`: initial state; `array`: 1-D array | Inclusive prefix-scan array |
| `scanr` | `scanr fn init array` | `fn`: step function; `init`: initial state; `array`: 1-D array | Exclusive right-to-left scan |
| `scanr_inclusive` | `scanr_inclusive fn init array` | `fn`: step function; `init`: initial state; `array`: 1-D array | Inclusive right-to-left scan |
| `segmented_reduce` | `segmented_reduce fn init offsets vals` | `fn`: step function; `init`: initial state; `offsets`, `vals`: 1-D arrays | One reduced value per segment |

## Indexing, scattering, and gathering

| Kernel | Syntax | Arguments | Result |
|---|---|---|---|
| `sort_indices` | `sort_indices keys` | `keys`: 1-D integer array | Permutation array that sorts `keys` |
| `iota` | `iota n` | `n`: integer length | `Array[n, Int]` containing `0..n-1` |
| `make_index` | `make_index n idxs` | `n`: bound for valid indices; `idxs`: integer array | Index-annotated integer array; identity at runtime |
| `permute` | `permute combine defaults perm_fn src` | `combine`: combining function; `defaults`: destination array; `perm_fn`: index-mapping function; `src`: source array | Permuted destination-shaped array |
| `scatter` | `scatter combine defaults idxs vals` | `combine`: combining function; `defaults`: destination array; `idxs`: index array; `vals`: values array | Destination-shaped array |
| `scatter_guarded` | `scatter_guarded combine defaults idxs vals guard` | `combine`: combining function; `defaults`: destination array; `idxs`: index array; `vals`: values array; `guard`: boolean array | Destination-shaped array |
| `gather` | `gather idxs array` | `idxs`: index array; `array`: source array | Array shaped like `idxs` |
| `index` | `index idx array` | `idx`: index value; `array`: source array | Element at `idx` |
| `check_index` | `check_index idx default array` | `idx`: index value; `default`: fallback element; `array`: source array | Element at `idx`, or `default` if out of bounds |

## Shape and array layout helpers

| Kernel | Syntax | Arguments | Result |
|---|---|---|---|
| `shape_of` | `shape_of array` | `array`: input array | Shape vector for the array |
| `replicate` | `replicate [spec] array` | `spec`: shape specification; `array`: source array | Replicated array |
| `slice` | `slice [spec] array` | `spec`: slice specification; `array`: source array | Sliced array |
| `reshape` | `reshape new_shape array` | `new_shape`: shape value; `array`: source array | Same elements with a new shape |
| `stencil` | `stencil boundary fn array` | `boundary`: `clamp`, `wrap`, `mirror`, or `(constant x)`; `fn`: accessor function; `array`: source array | Array of stencil results |

## Sparse and structured-array helpers

| Kernel | Syntax | Arguments | Result |
|---|---|---|---|
| `coo_sum_duplicates` | `coo_sum_duplicates nrows ncols nnz rows cols vals` | Scalar dimensions plus integer arrays for `rows`, `cols`, and `vals` | COO record with duplicate entries summed |
| `csr_from_sorted_coo` | `csr_from_sorted_coo nrows ncols nnz rows cols vals` | Scalar dimensions plus sorted integer arrays for `rows`, `cols`, and `vals` | CSR record |

## I/O helpers

| Kernel | Syntax | Arguments | Result |
|---|---|---|---|
| `read_array` | `read_array shape "file.csv"` | `shape`: expected shape; `file.csv`: path string | Array read from CSV |
| `read_array_float` | `read_array_float shape "file.csv"` | `shape`: expected shape; `file.csv`: path string | Float array read from CSV |
| `write_array` | `write_array array "file.csv"` | `array`: data to write; `file.csv`: path string | Unit |
| `write_array_float` | `write_array_float array "file.csv"` | `array`: float data to write; `file.csv`: path string | Unit |

## Compiler-internal derived helper

| Kernel | Syntax | Arguments | Result |
|---|---|---|---|
| `scatter_generate` | `scatter_generate combine defaults idxs fn` | `combine`: combining function; `defaults`: destination array; `idxs`: index array; `fn`: value generator | Equivalent to scattering over `generate (shape_of idxs) fn` |
