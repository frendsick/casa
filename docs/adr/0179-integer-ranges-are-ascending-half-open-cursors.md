# Integer ranges are ascending half-open cursors

The standard library provides an ascending `i64` range through `Range::new` and
`Iterable[i64]`. It includes the start and excludes the stop. Equal or reversed
bounds produce an empty iterator. Reversed bounds never change direction.

The FizzBuzz example previously needed a mutable counter, a loop condition,
and a manual increment to visit consecutive integers. The compiler and library
also contain counted loops. This is an existing use for a standard iterator,
as required by the demonstrated-need constraint in
[#367](https://github.com/frendsick/casa/issues/367).

Half-open bounds match the library's slice bounds and express zero iterations
without a special case. Fixed ascending traversal makes reversed bounds empty
instead of turning a mistaken bound into a descending loop. Incrementing only
below the exclusive stop avoids overflow across the complete signed domain
without subtracting bounds or storing a separate exhaustion flag.

The cursor uses the existing `next` contract and iterator defaults. It needs
no new syntax, compiler special case, closure wrapper, or numeric trait. The
initial type uses Casa's default integer type. Other widths, inclusive stops,
descending traversal, and arbitrary steps remain deferred until a concrete
program needs them.

This addition does not justify replacing hot counted loops. Iterator overhead
is tracked separately in [#829](https://github.com/frendsick/casa/issues/829).
The FizzBuzz example demonstrates the API without a compiler-wide migration.
