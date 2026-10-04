# Constant type parameters accept integers, bool, and char
status: amended by [ADR-0171](0171-constants-use-bounded-target-independent-expressions.md)
related issue: #438

Every integer width, `bool`, and `char` may now be a constant parameter's type.
Each distinct value is a distinct instantiation, and instantiations that differ
only in a constant argument do not unify. Under ADR-0069 each distinct constant
monomorphizes separately, so a body is still checked once against the symbolic
parameter before instantiation.

ADR-0171 also permits visible named constants of these kinds as arguments.
Their canonical values select instances, and integer values must fit the
parameter's declared width. Symbolic parameter forwarding remains separate
from named-constant evaluation. Layout queries and inline expressions remain
excluded from constant type arguments.

An integer argument is a contextual literal, the way ADR-0028 already treats
numeric literals. The literal carries no width of its own. It fits any integer
parameter whose range contains its value, so `3` can bind a `u8`, `u16`, or `u64` parameter.
Array lengths specifically use `u64`. An argument whose value does not fit the declared width
is rejected with a diagnostic that names the argument, the width, and the
parameter. A `bool` argument is written `true` or `false`. A `char` argument is a
char literal.

Float and `str` are rejected in constant position, each with a diagnostic that
names the type and the permitted set.

`f64` and `f32` are rejected because distinguishing instantiations needs total
equality, and ADR-0012 gives floats partial comparison only. `NaN` is unequal to
itself and `-0.0` equals `0.0`, so a float cannot identify an instantiation
without a stated bit-pattern rule. This project is not ready to commit to one, so
float constants are not allowed as type arguments.

`str` is rejected because two spellings that produce equal text must select the
same instantiation, so `str` needs a stated content-identity rule rather than the
pointer equality it would otherwise inherit. Until that rule exists, string
constants are not allowed as type arguments.

Bit-pattern identity or normalized NaN/zero identity would add a total float
rule that ADR-0012 declined. String identity would require content comparison or
interning. Both await separate decisions rather than entering through parameter
support.

## Consequences

- `[T const N:u8]`, `[const B:bool]`, and `[const C:char]` are legal constant
  parameters. `[const X:f64]` and `[const S:str]` are compile errors.
- Passing `300` to a `u8` constant parameter is a compile-time error.
- A constant parameter is usable as a value of its declared type inside the body
  that binds it.
