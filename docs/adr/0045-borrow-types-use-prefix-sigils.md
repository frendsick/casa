# Borrow types use prefix sigils

Casa writes an owned type as `T`, a shared borrow as `$T`, and an exclusive mutable borrow as `mut$T`. The borrow qualifier is a prefix attached directly to the complete type expression, without meaningful whitespace:

```casa
value:T
value:$T
value:mut$T

items:$List[i64]
items:mut$List[i64]
```

Method receivers infer `Self` and use the same prefixes:

```casa
self       # consuming owner
$self      # shared borrow
mut$self   # exclusive mutable borrow
```

## Considered options

- `&T` and `&mut T` are established spellings, but the mutable form contains a whitespace-separated modifier and produces awkward receiver forms such as `self:&mut self`.
- Postfix `T$` and `T$mut` fit Casa's execution style, but a trailing shared-borrow marker is easy to overlook at the end of a type.
- `$ref(T)` and `$mut(T)` group parameters clearly, but add wrappers and an unnecessary `ref` word for the common borrow.
- `$T` and `mut$T` keep the common form short, make the qualifier visually prominent, and preserve `name:Type` parameter grouping.

## Consequences

- `$` means shared borrow and `mut$` means exclusive mutable borrow. Casa adds no `ref` keyword.
- Prefix binding distinguishes `$Option[T]`, a borrow of an option, from `Option[$T]`, an owned option containing a borrow.
- Borrow qualifiers may appear in parameters, return types, generic arguments, and function types. ADR-0052 later permits user-defined structs and enums to contain borrowed fields under ordinary origin analysis.
- Nested borrow qualifiers such as `$$T` and `mut$mut$T` are rejected initially.
- The existing `&name` syntax for named function references is unchanged; `$` is borrow-type syntax, not a function-reference prefix or variable sigil.
- The formatter emits `$T` and `mut$T` without internal whitespace.
