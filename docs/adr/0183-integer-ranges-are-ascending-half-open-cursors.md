# Integer ranges are ascending half-open cursors

The standard library provides an ascending `u64` range through `Range::new` and
`Iterable[u64]`. It includes the start and excludes the stop. Equal or reversed
bounds produce an empty iterator. Reversed bounds never change direction.

The FizzBuzz example previously needed a mutable counter, a loop condition,
and a manual increment to visit consecutive integers. The compiler and library
also contain counted loops. This is an existing use for a standard iterator,
as required by the demonstrated-need constraint in
[#367](https://github.com/frendsick/casa/issues/367).

Half-open bounds match the library's slice bounds and express zero iterations
without a special case. Fixed ascending traversal makes reversed bounds empty
instead of turning a mistaken bound into a descending loop. Incrementing only
below the exclusive stop avoids overflow across the complete unsigned domain
without subtracting bounds or storing a separate exhaustion flag.

The cursor uses the existing `next` contract and iterator defaults. It needs
no new syntax, compiler special case, closure wrapper, or numeric trait. The
type uses `u64` to match collection lengths and indices from
[ADR-0125](0125-x86-64-casa-uses-fixed-width-size-types.md). Counted index loops
can pass their bounds and use yielded indices without numeric conversions.
Signed ranges, other widths, inclusive stops, descending traversal, and
arbitrary steps remain deferred until a concrete program needs them.

A `Range` iteration still costs more than an equivalent `while` iteration.
Fixed-bound counted loops in the compiler, language server, formatter, and
`argparse` use `Range`. Standard library primitives and loops that visit each
byte of program input keep `while`, as recorded in the
[style guide](../STYLE.md#loops).
