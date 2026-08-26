# Array literals have owned value semantics
status: amended by [ADR-0156](0156-owned-values-have-independent-behavior-not-address-identity.md)

Each evaluation of an array literal produces an independent owned value. Mutating one result cannot affect another evaluation of the same literal. The compiler may use static backing, sharing, stack placement, or copy-on-write when mutation and destruction still behave independently. Raw address equality is a representation detail under ADR-0156.

This supersedes ADR-0006's shared writable `.data` semantics. Sharing that changes mutation or destruction behavior conflicts with affine ownership. String literals may share read-only static backing only when mutation first promotes that backing to uniquely owned storage, so their owned behavior remains independent.
