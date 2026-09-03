# Standard trait derivation is a complete implementation
related issue: #658

Casa retains inline derivation for `Clone`, `Copy`, `Eq`, `Ord`, and
`Hashable`. Each derive request declares a complete trait implementation. The
compiler records that implementation as a conformance with structural operation
kinds. It does not create synthetic source-level functions. Completeness keeps
trait conformance coherent and prevents custom duplication semantics from
weakening Copy's predictable behavior.

A derived implementation supplies the requested trait, required supertraits,
and required methods not supplied by standard defaults. Clone visits fields or
the active payload in declaration order. Eq compares all fields, or the active
enum variant and payload, in declaration order and supplies PartialEq. Ord
compares fields lexicographically and enum variants before payloads, and supplies
total equality and partial ordering. Hashable includes the active variant and
every equality-relevant field, and supplies its required equality behavior.
Exact hash values remain runtime-local and unspecified. Copy supplies structural
Clone when the canonical Copy trait extends Clone.

An explicit implementation cannot overlap behavior supplied by a derive request
on the same type. This rule rejects a custom `eq` with derived Eq, Ord, or
Hashable, a custom `cmp` with derived Ord, a custom `hash` with derived Hashable,
and a custom Clone implementation with derived Clone or standard Copy. Standard
trait default methods remain available. Explicit implementations can coexist
with derive requests when their effective traits and methods do not overlap.

A generic derived implementation covers its complete receiver pattern and is
conditional on the requirements of stored fields and payloads. A concrete
explicit implementation cannot specialize or replace part of that pattern.
An unsatisfied concrete requirement is reported when its conformance is needed.
An invalid concrete derive names the first ineligible field or payload path.
A stored shared-borrow field in derived Clone duplicates the borrow without a
Clone requirement on the referent. An exclusive-borrow field cannot derive
Clone. A type that needs custom generic behavior omits the derive and provides
one complete generic implementation with the bounds required by its body.

User-defined structs and enums can implement Copy only with `derives Copy`.
Compiler-provided implementations for eligible built-in types remain unchanged.
The compiler accepts a user-derived Copy implementation only when the type has a
raw value representation, every field or payload is Copy, and the value contains
no exclusive borrow, owned indirection, or custom destruction. A type that needs
custom duplication implements Clone and remains non-Copy. A non-Copy type can
use `derives Clone` when structural duplication is sufficient.

## Consequences

- Existing partial overrides migrate to complete explicit implementations for
  the affected traits. Structurally valid explicit Copy implementations migrate
  to `derives Copy`. Types with custom Clone behavior lose Copy.
- Diagnostics cover unknown and duplicate derive names, ineligible fields or
  payloads, overlapping family members, and explicit Copy implementations. They
  name source declarations, not compiler-generated names.
- Tests cover observable behavior, conditional requirements, recursive owned
  values, borrow fields, enum payloads, Copy safety, explicit Copy and family
  rejection, and diagnostic locations through production compiler entry points.
- The language reference, examples, and tests change with the compiler
  migration. Until then, they continue to describe and test the implemented
  behavior.
