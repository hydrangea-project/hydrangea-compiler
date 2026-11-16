# Static bounds checking

Hydrangea proves array accesses safe at compile time instead of inserting
runtime checks. This page describes what the checker proves and how to write
code it can check.

## What the checker does

The checker evaluates the program symbolically. Every top-level value is
evaluated, and a function is checked where it is called, with the arguments it
is called with: functions are inlined at their calls, so a helper needs no
annotations for the checker to see through it.

At every array access it records an obligation and asks an SMT solver (Z3) to
prove it:

- `index` and the reads of `gather`: each index component lies in `[0, dim)`;
- the routes of `scatter`, `scatter_guarded` and their fused forms: each route
  is in bounds for the destination (`permute` drops out-of-range targets, so
  its routes need not be);
- `slice`, `zipwith`, `append` and `reshape`: the shapes agree;
- `make_index`, `bound` annotations and `where` preconditions: the asserted
  bound holds.

Each obligation carries its path condition: the ranges of the enclosing
`generate` indices, the branches taken by enclosing `if`s, and the guards of
guarded scatters. Integer arithmetic is exact, with division and remainder
truncating as in C.

Values the checker cannot compute exactly become unknowns: array contents read
from a file, the results of reductions and scatters, loop state, and integers
derived from floats. The same expression always gives the same unknown.

## Errors and notes

If the solver finds a counterexample, the access is out of bounds for some
input and compilation fails:

```hydrangea
let src = generate [8] (fn [i] => i)
let main = generate [8] (fn [i] => index [i + 1] src)
```

```text
UnsatConstraints: index may be out of bounds
  counterexample: i = 7
```

If the counterexample involves an unknown, it may not be real, so the checker
reports a note and compiles the program:

```hydrangea
let src = generate [8] (fn [i] => i)
let idx = read_array [8] "idx.csv"
let main = gather idx src
```

```text
note: could not verify gather at 3:12 (it depends on values the checker does not track)
```

`--no-solver-check` turns the checker off.

## Writing checkable code

### Compute indices from indices and shapes

Index arithmetic built from `generate` indices, sizes and constants is checked
exactly, including `/` and `%`:

```hydrangea
let src = generate [10] (fn [i] => i * 3)
let main = generate [10] (fn [i] => index [i / 2] src)
```

Read sizes from the arrays themselves with `shape_of` and `proj` rather than
repeating them:

```hydrangea
let arr = generate [4, 5] (fn [i, j] => i + j)
let s = shape_of arr
let main = index [proj 0 s - 1, proj 1 s - 1] arr
```

### Guard accesses that depend on unknown sizes

An argument may be any value, including `0`, so reading the first or last
element of an array whose size is an argument needs a guard:

```hydrangea
let last n = let xs = generate [n] (fn [i] => i) in if n > 0 then index [n - 1] xs else 0
```

### Test data-dependent indices before using them

An index loaded from an array is an unknown. A test of it is a premise for the
access it guards, so it turns a note into a proof:

```hydrangea
let src = generate [8] (fn [i] => i)
let idx = read_array [8] "idx.csv"
let main = generate [8] (fn [i] =>
  let j = index [i] idx in
  if 0 <= j & j < 8 then index [j] src else 0)
```

For scatters, the guard of `scatter_guarded` plays the same role. `check_index`
reads with a default for out-of-range indices and needs no proof at all.

### Bounded index producers

`iota n` produces values in `[0, n)`, and `make_index n xs` asserts that the
elements of `xs` lie in `[0, n)`. The assertion is itself checked:

```hydrangea
let src = generate [10] (fn [i] => i * 3)
let raw = generate [10] (fn [i] => i % 4)
let a = gather (iota 10) src
let b = gather (make_index 10 raw) src
```

### Bound annotations and preconditions

`[i bound n]` on a generator index, `let x bound n = e`, and a `where` clause
on a function all state a bound. The checker proves it where the value is
bound or the function is called, so these document a contract rather than
supply an assumption:

```hydrangea
let pick (j, arr) where bound j (dim arr 0) = index [j] arr
let xs = generate [4] (fn [i] => i)
let main = pick (9, xs)
```

```text
UnsatConstraints: precondition of pick may be out of bounds
```

### Zero-sized arrays

Zero-sized shapes need no special handling: an access the empty array never
performs is never checked.

```hydrangea
let idx = generate [0] (fn [i] => i)
let src = fill [0] 0
let main = gather idx src
```
