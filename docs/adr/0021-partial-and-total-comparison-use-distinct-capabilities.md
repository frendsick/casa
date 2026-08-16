# Partial and total comparison use distinct capabilities

Casa uses explicit trait conformance while distinguishing partial comparison from lawful total comparison. ADR-0082 later makes PartialEq and Eq share the `eq` and `ne` hooks: Eq is the explicit total-equality refinement rather than a second method family. ADR-0083 applies the same inherited-hook model to PartialOrd and Ord.

Equality operators accept PartialEq, and ordering operators accept PartialOrd; floats therefore retain IEEE comparison. Hashing requires Eq. Derived Eq and Ord generate the required partial and total conformances, while types with partial semantics implement only the partial traits. Explicit conformance, rather than distinct equality method names, records the stronger semantic promise.
