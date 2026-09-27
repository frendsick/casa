# Checked function body ownership prototype

Issue: [#689](https://github.com/frendsick/casa/issues/689). Run date: 2026-09-27. This is an experiment based on `main` at `549c68ca9a99c5596409cec0793750737dbe840d`. The compiler code remains on an evidence branch. Production adoption is a separate decision.

## Change and ownership

`SymbolStore::take_function_for_typecheck` already leaves a declaration in the store for recursive lookup. The prototype keeps the removed definition as the only owner of its changing body during checking. Analysis moves checked operations into that definition. Scheduling restores the owned result to the store and moves diagnostics into the phase accumulator. The direct semantic analysis API still makes independent copies for its returned `FunctionSemantics` and its session snapshot, as required by ADR-0167. Trait storage and type representation are unchanged.

The changed paths remove these whole-body copies:

- Scheduling clones only a declaration to inspect pending work, instead of cloning its body before checking. It no longer clones the returned checked function before committing it.
- Analysis moves checked operations into their function and transfers that function to the store on the scheduling path. It no longer copies checked operations into the function or copies the function back into the store.
- Checking an owned function from the store no longer clones that body again. The fallback that must copy a borrowed function now copies its body once, instead of first copying it through `clone_for_import`.
- Callable return-summary analysis clones only a declaration before selecting the owned stored function. Result fields from the checker, function analysis, and root analysis move into their next owner.

These are source-level copy paths, not a runtime count of individual `Op` clones. The snapshot API still copies its checked function and operations because both returned products must remain independently owned.

## Native comparison

Both compilers were built from stable Casa v1.50.0 to native stage 2 and stage 3. Each compiler's stage 2 and stage 3 assembly hashes match. The measured executable is stage 2. Each study used one warm-up for each compiler, then three serial alternating measured pairs. Whole builds used `-L lib casa.casa -o <output> --keep-asm --verbose`. Time is Linux `CLOCK_MONOTONIC_RAW`. Peak RSS is GNU `time` maximum RSS. The machine was an AMD Ryzen 7 3700X with 16 logical CPUs under Linux x86-64. [Raw evidence](evidence.json) includes source hashes, compiler hashes, all samples, assembly hashes, and 31 lifetime checkpoints per compiler.

| Input | Compiler | Warm-up s | Pair 1 s | Pair 2 s | Pair 3 s | Median s | Range s | Median peak RSS KiB |
| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| Own source | `main` | 18.767 | 18.306 | 18.782 | 20.757 | 18.782 | 2.451 | 296,060 |
| Own source | Prototype | 18.089 | 17.830 | 18.810 | 19.584 | 18.810 | 1.754 | 297,212 |
| Identical prototype source | `main` | 20.620 | 20.138 | 19.623 | 19.652 | 19.652 | 0.515 | 297,596 |
| Identical prototype source | Prototype | 19.704 | 19.616 | 19.654 | 19.180 | 19.616 | 0.474 | 297,212 |

The own-source median is 0.15% slower for the prototype. Its median peak RSS is 1,152 KiB higher. On identical input, the prototype median is 0.18% faster and median peak RSS is 384 KiB lower. Two identical-input pairs favor the prototype by 0.47 to 0.52 seconds. One favors `main` by 0.03 seconds. The measured ranges exceed the median difference, so this study does not establish a reliable whole-build speed gain. All eight identical-input outputs have the same assembly hash. Every own-source output matches its compiler's fixed-point assembly hash.

The prototype adds 28 net production source lines and 23 net test lines. The stage 2 binary grows by 1,192 bytes, including 7,242 bytes more `.text`. Its generated assembly grows by 33,758 bytes. The retained code uses the existing store lifecycle and adds no new body state protocol.

## Correctness and reclamation

`CASA_COMPILER=/tmp/casa-689-branch-final-build/stage2 tests/test_compiler.sh typechecker underflow_messages semantics` passed all four selected test files. They cover scheduler checking of an unused function, retained checked calls and inferred return type, recursive bodies, diagnostics and source location, semantic snapshot isolation, generic specialization, and ownership metadata. `tests/test_all.sh` passed all 14 shards, including fixed-point bootstrap, examples, error fixtures, and formatter checks. A fresh native stage 2/3 build of this exact source also reached a fixed point.

The existing `compiler-reductions/lifetime.casa` workload makes one successful generic cleanup request and one diagnostic-producing request 30 times in one process. Each result is destroyed before sampling. Both compilers returned to zero live allocator blocks at all 25 checkpoints after five warm-up pairs. Both retained one reusable 64 MiB heap mapping. Prototype reusable payload plateaued at 100,192 bytes, compared with 100,664 bytes for `main`. Both plateaued at 4,364 KiB RSS on this run. This establishes reclamation for this workload only.

## Reproduce

Use the two worktrees and the release compiler from `casa-release.env`. The commands below create the fixed-point compilers, comparison configs, and lifetime binaries. Run them from the prototype worktree. Change the first two paths if the worktrees are elsewhere.

```sh
main=/home/ai-agent/git/casa
prototype=/home/ai-agent/git/casa-689
out=$(mktemp -d /tmp/casa-689-repro.XXXXXX)
python3 docs/benchmarks/compiler-reductions/bootstrap.py "$prototype/casac" "$main" "$out/main"
python3 docs/benchmarks/compiler-reductions/bootstrap.py "$prototype/casac" "$prototype" "$out/prototype"
cat > "$out/own.json" <<EOF_CONFIG
[
  {"name":"main","compiler":"$out/main/stage2","source":"$main"},
  {"name":"prototype","compiler":"$out/prototype/stage2","source":"$prototype"}
]
EOF_CONFIG
cat > "$out/identical.json" <<EOF_CONFIG
[
  {"name":"main","compiler":"$out/main/stage2","source":"$prototype"},
  {"name":"prototype","compiler":"$out/prototype/stage2","source":"$prototype"}
]
EOF_CONFIG
python3 docs/benchmarks/compiler-reductions/compare.py "$out/own.json" "$out/own-measures"
python3 docs/benchmarks/compiler-reductions/compare.py "$out/identical.json" "$out/identical-measures"
(cd "$main" && "$out/main/stage2" -L lib docs/benchmarks/compiler-reductions/lifetime.casa -o "$out/main-lifetime")
(cd "$prototype" && "$out/prototype/stage2" -L lib docs/benchmarks/compiler-reductions/lifetime.casa -o "$out/prototype-lifetime")
python3 docs/benchmarks/compiler-reductions/lifetime.py "$out/main-lifetime" "$out/main-lifetime.json"
python3 docs/benchmarks/compiler-reductions/lifetime.py "$out/prototype-lifetime" "$out/prototype-lifetime.json"
CASA_COMPILER="$out/prototype/stage2" tests/test_compiler.sh typechecker underflow_messages semantics
tests/test_all.sh
```

Source file hashes and compiler binary hashes in `evidence.json` identify the exact measured inputs and executables.

## Recommendation

Keep this implementation as an experimental branch. The ownership path is simpler, but the measured whole-build gain is below observed variation and does not justify adding 28 net production lines to `main` now. Revisit it if a larger compiler representation change can use the same transfer path or a focused workload demonstrates a material gain.
