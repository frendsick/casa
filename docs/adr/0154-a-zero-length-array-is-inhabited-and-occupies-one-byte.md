# A zero-length array is inhabited and occupies one byte

`array[T 0]` is an ordinary inhabited type. It has exactly one value, the empty
sequence, and ADR-0132's one-byte minimum applies to it without an exception:

```casa
size_of[array[T N]] # N size_of[T] * for N > 0
size_of[array[T 0]] # 1
```

The storage holds no elements. The byte exists so the value has an address of
its own, so two zero-length arrays are distinct places, and so a generic
container computing `capacity size_of[T] *` gets a nonzero stride for an empty
array element the same way it does for an empty struct. Array storage is
word-granular today, so the compiler reserves a word rather than a byte; that is
layout, not contract (ADR-0127).

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
- `[]` allocates and yields a distinct address rather than a null or shared one.
- A static empty array literal still reserves storage, so its label does not
  collide with whatever follows it.
- Destruction of an `array[T 0]` visits no elements, which the length in the
  type already states.
- No compiler or library path needs a zero-sized-value branch, which is what
  ADR-0132 chose to avoid.
