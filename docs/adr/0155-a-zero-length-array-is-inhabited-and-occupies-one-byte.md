# A zero-length array is inhabited and occupies one byte
status: amended by [ADR-0156](0156-owned-values-have-independent-behavior-not-address-identity.md)
related issue: #439

`array[T 0]` is an ordinary inhabited type. It has exactly one value, the empty
sequence, and ADR-0132's one-byte minimum applies to it without an exception:

```casa
size_of[array[T N]] # N size_of[T] * for N > 0
size_of[array[T 0]] # 1
```

The storage holds no elements. The byte supplies a nonzero layout and element
stride for generic containers computing `capacity size_of[T] *`. Storage placement
is compiler-owned under ADR-0127. Addresses across independent owners are a
representation detail under ADR-0156.

## Considered options

- Size zero, as ADR-0152 first stated. It reads directly from
  `N size_of[T] *` and matches how other languages describe an empty array. It
  also reintroduces the zero-sized value that ADR-0132 rejected: element stride
  becomes zero, consecutive elements share an address, and every generic
  container needs the branch ADR-0132 exists to avoid.
- Treat `array[T 0]` as uninhabited, so the question does not arise. However, `[]` is a value of the type, and code that constructs and destroys
  it must work.
- One byte, from ADR-0132's general rule, with no array-specific exception
  (chosen). The only cost is that `N size_of[T] *` describes the element
  storage rather than the whole value when `N` is zero.

## Consequences

- Destruction of an `array[T 0]` visits no elements, which the length in the
  type already states.
- No compiler or library path needs a zero-sized-value branch, which is what
  ADR-0132 chose to avoid.
