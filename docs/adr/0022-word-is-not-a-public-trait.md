# Word is not a public trait

Casa removes the public `Word` trait. Whether a value fits in one machine or stack slot is a compiler representation detail, while language contracts describe ownership and semantic types.

`Hashable` and `Display` no longer inherit `Word`. Syscalls, foreign declarations, and unsafe memory operations use exact fixed-width integer and pointer types instead of a representation-size bound. The compiler may retain internal layout classifications without exposing them as traits.
