# Editor snapshot lifetime

Measured on 2026-09-29 with the #713 implementation and the v1.53.0 compiler.
The workload performs 630 analyses in 30 batches. Each batch replaces an owned
snapshot 20 times, alternating an unsaved import with a damaged function body.
An owned hover answer stays alive across replacement and is read afterward.
An unused source override is supplied on every imported request.

```sh
./casac -L lib docs/benchmarks/editor-snapshots/lifetime.casa -o /tmp/casa-713-lifetime
python3 docs/benchmarks/compiler-reductions/lifetime.py /tmp/casa-713-lifetime docs/benchmarks/editor-snapshots/lifetime.json
```

The existing allocator sampler stops the process at initialization and after
each batch. All 31 checkpoints had zero live allocations and zero live payload
bytes. After five warm-up batches, all measurements were identical:

| Measurement | Value |
| --- | ---: |
| Live allocations | 0 |
| Live payload bytes | 0 |
| Reusable allocations | 1,125 |
| Reusable payload bytes | 57,056 |
| Mapped heap bytes | 67,108,864 |
| RSS and peak RSS | 4,060 KiB |

The allocator retains reusable storage in its mapped chunk. The bounded
workload establishes release after replacements and a memory plateau. It does
not measure peak memory while both snapshots are live, compiler-source query
latency, workspace discovery, or workspace rename.
