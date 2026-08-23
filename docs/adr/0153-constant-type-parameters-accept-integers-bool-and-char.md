# Constant type parameters accept integers, bool, and char

ADR-0152 introduced constant type parameters and accepted `u64` only, because a
`u64` array length was the sole use it needed. Because the declaration already
states the constant's type, as in `[T const N:u64]`, widening the accepted set is
a checking change rather than a syntax change.

Every integer width, `bool`, and `char` may now be a constant parameter's type.
Each distinct value is a distinct instantiation, and instantiations that differ
only in a constant argument do not unify. Under ADR-0069 each distinct constant
monomorphizes separately, so a body is still checked once against the symbolic
parameter before instantiation.

An integer argument is a contextual literal, the way ADR-0028 already treats
numeric literals. The literal carries no width of its own. It fits any integer
parameter whose range contains its value, so `array[i64 3]` binds a `u8`, `u16`,
or `u64` parameter alike. An argument whose value does not fit the declared width
is rejected with a diagnostic that names the argument, the width, and the
parameter. A `bool` argument is written `true` or `false`; a `char` argument is a
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

## Considered options

- A bit-pattern identity for floats, folding every `NaN` to one value and
  `-0.0` to `0.0`, would let floats participate. It commits the language to a
  total float equality that ADR-0012 deliberately declined, so it belongs in its
  own ADR rather than riding in on this one.
- A content-identity rule for `str`, comparing text rather than storage, would
  let strings participate. It needs interning or structural comparison at the
  type level and the same dedicated decision, so it is also deferred.
- Rejecting both with a diagnostic keeps the accepted set to kinds that already
  have an obvious total equality, and leaves each harder identity rule to a later
  ADR that can state it deliberately.

## Consequences

- `[T const N:u8]`, `[const B:bool]`, and `[const C:char]` are legal constant
  parameters. `[const X:f64]` and `[const S:str]` are compile errors.
- An integer constant argument is checked against the parameter's declared width.
  `array[i64 300]` bound to a `u8` parameter is a compile-time error that names
  the argument.
- A constant parameter is usable as a value of its declared type inside the body
  that binds it.
- Float and `str` constant parameters remain open. Each waits on its own identity
  rule, recorded as a future ADR, before it can be accepted.
