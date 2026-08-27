# Memory Efficiency

## Review

- Trace each affected allocation to one owner and reclamation path. Cover
  success, error, and early-return paths.
- Classify duplication. Copy uses representation duplication with zero backing
  allocations. Clone can allocate, traverse owned data, and run user code.
- Prefer ownership transfer, borrowing, an existing returned reference, or a
  narrower query. Clone when the caller requires independent ownership.
- Match raw allocation with compatible reclamation and typed destruction.
- Give caches and interned data an explicit lifetime or capacity.

## Measurement

Measurement applies when a change:

- changes allocation, destruction, collection growth, cache retention, or
  compiler-pass ownership
- adds or removes Clone or collection duplication in a hot path
- addresses a measured regression or claims a memory or performance improvement
- changes Copy lowering or representation

For allocation lifetime changes, use a bounded-live-set workload that plateaus
after warm-up. Performance comparisons use a representative workload and the
median of three baseline and three changed runs. Compiler-wide comparisons use
self-compilation.

Interpret live allocations, reusable allocator bytes, heap high-water, and RSS
together.
