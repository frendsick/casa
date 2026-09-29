# Workspace snapshot queries

Measured on 2026-09-29 with the #714 implementation and the v1.53.0 compiler.
The measurements are observations on this machine, not latency gates or a
comparison against the previous implementation.

## Small workspace

Three runs use one declaration file and two unopened importers. A later request
adds an unsaved edit. Rejected requests cover lexical, syntax, import, type, and
ownership failures in another root. Each request discovers files and builds fresh
snapshots. There is no debounce delay or snapshot cache. “Warm” means a repeated
request in the same server process.

| Operation | Median |
|---|---:|
| Discovery, one scan averaged over 1,000 scans | 0.017 ms |
| Cold references | 2.482 ms |
| Warm references | 2.306 ms |
| Rename with candidate validation | 4.459 ms |
| Open edit and references | 2.661 ms |
| Rejected rename by failure class | 2.407–3.911 ms |

`timing.json` retains all three measurements for each operation. Discovery is
measured with the existing timer library over three real files. The other times
include the LSP request and response transport.

## Repeated replacements

The lifetime workload runs 30 batches of 10 edit cycles. Each cycle retains a
valid reference answer while adding an invalid root and requesting another
answer. It checks an accepted rename and an incomplete-coverage refusal.

All 31 allocator checkpoints report zero live allocations and zero live payload
bytes. After five warm-up batches, the measurements are identical:

| Measurement | Value |
|---|---:|
| Reusable allocations | 975 |
| Reusable payload bytes | 54,680 |
| Mapped heap bytes | 67,108,864 |
| RSS and peak RSS | 4,184 KiB |

This establishes release and a memory plateau for the bounded workload. Mapped
storage remains available to the allocator. It does not establish a bound for
arbitrarily large workspaces.

## Compiler source

One run analyzes `casa.casa` with its imported sources, performs 100 reference
queries against the retained index, and releases the snapshot. It retains
1,661,065 source bytes. `index.json` records the observation:

| Phase | Time |
|---|---:|
| Source loading and analysis, including index construction | 316.742 s |
| 100 queries against the built index | 4.828 s |
| Release | 169.422 ms |

RSS and peak RSS reached 837,044 KiB and remained there after release. RSS alone
cannot distinguish live index storage from reusable allocator storage. This
large case uses RSS sampling because walking each allocator block through
`/proc` adds minutes of observation overhead. Allocation-level release evidence
comes from the bounded workload above.

Large compiler-source analysis is too slow for interactive workspace requests.
Each workspace request currently reanalyzes roots, so this cost compounds across
importers. This measurement does not establish a latency gate, an exact retained
index size, or peak memory during a large snapshot replacement.

## Reproduction

Run from the repository root:

```sh
./casac -L lib lsp.casa -o /tmp/casa-714-lsp
./casac -L lib docs/benchmarks/workspace-snapshots/discovery.casa -o /tmp/casa-714-discovery
python3 docs/benchmarks/workspace-snapshots/measure.py /tmp/casa-714-lsp docs/benchmarks/workspace-snapshots/timing.json /tmp/casa-714-discovery
./casac -L lib docs/benchmarks/workspace-snapshots/lifetime.casa -o /tmp/casa-714-lifetime
python3 docs/benchmarks/compiler-reductions/lifetime.py /tmp/casa-714-lifetime docs/benchmarks/workspace-snapshots/lifetime.json
./casac -L lib docs/benchmarks/workspace-snapshots/index.casa -o /tmp/casa-714-index
python3 docs/benchmarks/workspace-snapshots/index.py /tmp/casa-714-index docs/benchmarks/workspace-snapshots/index.json
```
