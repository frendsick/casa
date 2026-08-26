# Owned values have independent behavior, not address identity
related issue: #475

Independently owned values behave independently under mutation and destruction.
Mutating one value cannot change another, and destroying one cannot invalidate or
destroy the other's contents. These rules do not require distinct storage. The
compiler may use inline storage, static storage, sharing, or copy-on-write when
those choices preserve the required behavior.

A raw pointer obtained from a borrow identifies a storage location, not an
owner. Comparing raw pointers remains safe, but pointers obtained from two
independently owned values may compare equal or unequal. Programs cannot use
either result as the identity of an owner. Address identity across independently
owned values is a representation detail.

Every materialized value still follows its type's layout. An inhabited type has
a minimum size and stride of one byte under ADR-0132. In particular,
`size_of[array[T 0]]` is 1, an `array[T 0]` field occupies one byte, and
consecutive `array[T 0]` elements have nonzero stride. This layout rule does not
require each standalone value to have a separate allocation or address.

This amends ADR-0020: storage sharing need not hide raw address equality as long
as mutation and destruction remain independent. It also amends ADR-0155's
requirement that each zero-length array allocate storage at a distinct address.

## Consequences

- Address comparison can describe a chosen representation, but not whether two
  independently owned values are the same owner.
- A compiler can share storage for immutable data and values with no elements,
  and can separate that storage before a mutation when required.
- Destroying shared representations must preserve every other live owner's
  value and must not release storage that another owner still needs.
- Zero-length arrays remain inhabited values. Their materialized size is one
  byte, their stride is nonzero, and their destruction visits no elements.
- Tests of owned-value semantics check mutation and destruction behavior. Layout
  tests check size and stride without requiring an allocation strategy or
  address relationship between independent values.
