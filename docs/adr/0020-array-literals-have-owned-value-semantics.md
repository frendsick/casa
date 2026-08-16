# Array literals have owned value semantics

Each evaluation of an array literal produces an independent owned value. Mutating one result cannot affect another evaluation of the same literal. The compiler may use static backing, sharing, stack placement, or copy-on-write only when the choice is unobservable to safe code.

This supersedes ADR-0006's shared writable `.data` semantics. Observable sharing conflicts with affine ownership and makes storage placement change program behavior. String literals may share read-only static backing only when mutation first promotes that backing to uniquely owned storage, so the sharing remains unobservable.
