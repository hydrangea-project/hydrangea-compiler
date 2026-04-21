# Static bounds checking

Hydrangea can prove many array-safety conditions at compile time. This page is
about what *you* should write so the checker can prove your program safe.

The short version:

1. Keep sizes and indices in simple arithmetic form.
2. Reuse tracked shapes with `shape_of` and `proj`.
3. Add explicit bounds with `bound` / `[i bound n]` when the checker cannot infer them.
4. Put `where` clauses on wrapper functions, not just on the inner helper they call.

## What the checker is trying to prove

For indexing-style operations such as `index`, `gather`, `scatter`, and
`scatter_guarded`, the checker wants evidence that each index stays inside the
destination or source shape.

For example, these are the kinds of facts it proves:

- `i < 5`, so `index [i] arr` is safe for a length-5 array
- `elem idx <= dim src 0`, so `gather idx src` is safe
- `n <= dim arr 0`, so `gather (iota n) arr` is safe

## The easiest patterns to write

### Use built-in bounded index producers

These forms already carry useful bounds:

```hydrangea
let idx1 = iota n
let idx2 = make_index n raw_idx
```

- `iota n` produces values in `[0, n)`
- `make_index n raw_idx` tells the checker to treat `raw_idx` as bounded by `n`

Good:

```hydrangea
let src = generate [10] (let f [i] = i in f)
let main = gather (iota 10) src
```

Also good:

```hydrangea
let src = generate [10] (let f [i] = i in f)
let raw = generate [10] (let f [i] = i % 4 in f)
let idx = make_index 10 raw
let main = gather idx src
```

## Add explicit bounds when generating indices

If you generate an index array yourself, give the generator variable an explicit
bound whenever the output bound is not obvious from simple arithmetic.

```hydrangea
let idx = generate [10] (let f [i bound 10] = i / 2 in f)
```

The `[i bound 10]` annotation says `0 <= i < 10`, which lets the checker prove
facts about `i / 2`, `i + 1`, `i % 4`, and similar expressions.

Without the bound annotation, more complex expressions may only produce a
warning:

```hydrangea
let idx = generate [10] (let f [i] = i / 2 in f)   -- may warn
```

## Reuse shapes with `shape_of` and `proj`

Hydrangea tracks dimensions through `shape_of` and tuple projection, so prefer
reusing those values instead of copying literal sizes by hand.

```hydrangea
let arr = generate [m, n] (let f [i, j] = i + j in f)
let s = shape_of arr
let h = proj 0 s
let w = proj 1 s
let main = index [h - 1, w - 1] arr
```

This style also works well when building new arrays from existing shapes:

```hydrangea
let src = generate [4, 5] (let cell [i, j] = i + j in cell)
let s = shape_of src
let h = proj 0 s
let w = proj 1 s
let rowElem row [i] = index [row, i] src
let rowSum [row] = index () (reduce_generate (+) 0 [w] (rowElem row))
let main = generate [h] rowSum
```

## Named helpers are fine

The checker can follow bounds through named helpers, including partial
application, as long as the helper body is still simple enough to analyze.

```hydrangea
let shift x = x + 1
let shift_by k x = x + k

let src1 = generate [6] (let f [i] = i in f)
let idx1 = generate [5] (let f [i] = shift i in f)
let ok1 = gather idx1 src1

let src2 = generate [6] (let f [i] = i in f)
let idx2 = generate [5] (let f [i] = shift_by 1 i in f)
let ok2 = gather idx2 src2
```

Prefer helper bodies built from:

- variables already known to be bounded
- constants
- simple arithmetic like `+`, `-`, `*` by a positive constant, `%`, and some
  bounded `if` expressions

## Put `where` clauses on wrappers too

If a wrapper function calls another function with a precondition, the wrapper
usually needs its *own* `where` clause.

Good:

```hydrangea
let my_gather (idx, src) where elem idx <= dim src 0 = gather idx src
let forward (idx, src) where elem idx <= dim src 0 = my_gather (idx, src)
```

Not as good:

```hydrangea
let my_gather (idx, src) where elem idx <= dim src 0 = gather idx src
let forward p = my_gather p
```

The second form may compile with a warning because the wrapper hides the fact
the checker needs at the call site.

## Use `bound` in `where` clauses

For wrapper-style APIs, `bound` is often the nicest user-facing way to express
the required fact.

```hydrangea
let take (n, arr) where bound n (dim arr 0) = gather (iota n) arr
```

`bound n (dim arr 0)` means the checker can assume:

- `0 <= n`
- `n < dim arr 0`

This is stronger than a plain `n <= dim arr 0`, and it often makes delegated
calls easier to prove.

## Zero-sized arrays are allowed

The checker handles zero-sized shapes, so you do not need to special-case them
just to satisfy the solver.

```hydrangea
let idx = generate [0] (let f [i] = i in f)
let src = fill [0] 0
let main = gather idx src
```

## How to respond to a warning

If you see a warning like:

```text
note: could not verify '...'
```

usually do one of these:

1. Add a bound annotation such as `[i bound n]`
2. Rewrite the index computation into simpler arithmetic
3. Add a `where` clause to the wrapper function that actually forwards the call
4. Use `make_index n ...` if you are asserting a user-known bound

## When `make_index` is the right tool

`make_index` is for "I know this array is bounded by `n`, please treat it that
way".

```hydrangea
let raw = generate [10] (let f [i] = i % 4 in f)
let idx = make_index 10 raw
let main = gather idx src
```

Hydrangea still warns that the `make_index` assertion itself was not proved.
That is expected: `make_index` is a programmer assertion, not a runtime check.

## When the checker will still struggle

Today the checker is weakest on:

- opaque arithmetic with no explicit bound annotation
- wrappers that hide a needed `where` clause
- helper bodies that depend on facts the caller never states

If you keep sizes explicit, reuse `shape_of`/`proj`, and annotate genuinely
non-obvious bounds, most indexing-heavy code should pass without needing
`--no-solver-check`.
