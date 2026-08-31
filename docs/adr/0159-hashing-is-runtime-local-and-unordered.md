# Hashing is runtime-local and unordered
related issue: #368

A lawful `Hashable` implementation returns the same hash for values that `Eq`
considers equal. Repeated hashing must also return the same value while the
equality-relevant state is unchanged. Unequal values may share a hash. The
compiler validates the method contract, not these semantic laws.

Hash values are runtime aids, not durable data. Casa does not guarantee that an
exact hash repeats across processes, builds, compiler or standard library
releases, or target platforms. Programs must not persist or exchange hashes as
identifiers, checksums, or protocol values.

`Map` and `Set` preserve correctness under collisions by comparing same-hash
keys with `Eq`. Unequal colliding keys stay distinct. Collisions may reduce an
operation to linear time. Traversal order is unspecified and may change after a
mutation or resize, and between processes, builds, releases, and targets.

The standard hashes are unkeyed, and the standard `Map` and `Set` do not defend
against adversarial collision attacks. Programs must bound or validate
untrusted key sets or use a specialized collection. Casa does not expose a hash
algorithm, seed, bucket layout, or traversal order as part of the language or
standard library contract.

## Consequences

- Tests compare collection contents instead of traversal order.
- Collision tests use unequal keys with the same hash and require independent
  lookup, replacement, and removal behavior.
- The standard library may change its hash algorithms and collection layout
  without a compatibility mechanism.
