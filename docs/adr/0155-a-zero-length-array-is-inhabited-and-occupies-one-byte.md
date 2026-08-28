# A zero-length array is inhabited and occupies one byte
status: amended by [ADR-0156](0156-owned-values-have-independent-behavior-not-address-identity.md)
related issue: #439

`array[T 0]` is an ordinary inhabited type. It has exactly one value, the empty
sequence, and ADR-0132's one-byte minimum applies to it without an exception:

```casa
size_of[array[T N]] # N size_of[T] * for N > 0
size_of[array[T 0]] # 1
```

The storage holds no elements. The byte gives a materialized value a nonzero
layout and gives an empty array element a nonzero stride when a generic container
computes `capacity size_of[T] *`. A local reserves one machine word because
local slots are word-sized. An aggregate field uses the one-byte layout
directly. These placements are not a stable contract (ADR-0127). ADR-0156 makes
addresses across independently owned values a representation detail.

This supersedes the line in ADR-0152 that gave `array[T 0]` size zero.

## Considered options

- Size zero, as ADR-0152 first stated. It reads directly from
  `N size_of[T] *` and matches how other languages describe an empty array. It
  also reintroduces the zero-sized value that ADR-0132 rejected: element stride
  becomes zero, consecutive elements share an address, and every generic
  container needs the branch ADR-0132 exists to avoid.
- Treat `array[T 0]` as uninhabited, so the question does not arise. It is
  false: `[]` is a value of the type, and code that constructs and destroys it
  must work.
- One byte, from ADR-0132's general rule, with no array-specific exception
  (chosen). The only cost is that `N size_of[T] *` describes the element
  storage rather than the whole value when `N` is zero.

## Consequences

- `size_of[array[T N]]` is `N size_of[T] *` for every `N > 0`, and 1 for `N`
  zero.
- A materialized `[]` follows the one-byte layout whether its storage is local,
  static, or shared.
- Destruction of an `array[T 0]` visits no elements, which the length in the
  type already states.
- No compiler or library path needs a zero-sized-value branch, which is what
  ADR-0132 chose to avoid.
