# Architecture decision records

An ADR without `status` metadata is accepted. It records the current decision
even when work to implement it is still open.

Use `status: amended by [ADR-NNNN](NNNN-slug.md)` when one or more successors
change part of the decision. Use `status: superseded by [ADR-NNNN](NNNN-slug.md)`
when a successor replaces the decision. Retired records remain available in git
history, with surviving destinations listed below.

Use `related issue: #NNN` when the related implementation issue is known. The
issue tracks delivery status. An open related issue does not change an accepted
ADR's status.

## Retired records

The [approved retention inventory](https://github.com/frendsick/casa/issues/665#issuecomment-5897405812)
and [maintainer approval](https://github.com/frendsick/casa/issues/665#issuecomment-5897492437)
identify the records folded or superseded in the cleanup. Their original text is
available in [the pre-cleanup snapshot](https://github.com/frendsick/casa/tree/afbdec5d52e6737e11f1e00ff126f7de5dc2c99f/docs/adr)
and git history. Surviving records keep their identifiers and filenames.
Historical issue references and pinned prototypes use the following destinations.

| Retired identifiers | Surviving decision |
| --- | --- |
| ADR-0001, ADR-0047, ADR-0048, ADR-0053, ADR-0055, ADR-0056, ADR-0057, ADR-0058, ADR-0059, ADR-0060, ADR-0151 | [ADR-0165](0165-runtime-state-is-owned-by-the-root-body.md) |
| ADR-0006, ADR-0020 | [ADR-0156](0156-owned-values-have-independent-behavior-not-address-identity.md) |
| ADR-0008 | [ADR-0173](0173-semantic-checking-owns-source-obligations.md) |
| ADR-0010 | [ADR-0168](0168-imports-expose-qualified-names-only.md) |
| ADR-0021 | [ADR-0082](0082-partial-and-total-equality-share-operator-methods.md) |
| ADR-0034, ADR-0077, ADR-0087, ADR-0088, ADR-0090, ADR-0091, ADR-0092 | [ADR-0163](0163-standard-trait-derivation-is-a-complete-implementation.md) |
| ADR-0073 | [ADR-0152](0152-array-length-is-part-of-the-array-type.md) |
| ADR-0074, ADR-0084 | [ADR-0080](0080-language-traits-use-minimum-contracts.md) |
| ADR-0078 | [ADR-0075](0075-clone-is-explicit-and-infallible.md) |
| ADR-0079 | [ADR-0158](0158-copy-requires-a-raw-value-representation.md) |
| ADR-0104 | [ADR-0164](0164-trait-default-methods-are-trait-owned-generic-bodies.md) |
| ADR-0118 | [ADR-0150](0150-shared-borrow-duplication-is-not-copy-conformance.md) |
| ADR-0124 | [ADR-0123](0123-raw-storage-uses-alloc-and-free.md) |
| ADR-0134 | [ADR-0126](0126-size-of-exposes-inline-storage-size.md) |
| ADR-0140 | [ADR-0016](0016-explicit-width-integer-names.md) |
