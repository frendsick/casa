# Bytecode control-flow target benchmark

Run date: 2026-08-30

This benchmark compares `origin/main` at `f414a6e` with the control-flow target
index in this document's commit. The benchmark builds nested `if` operation
sequences and measures bytecode lowering only. Each result is the median of
three runs.

## Environment

- Linux 6.18.33.2 under WSL2
- AMD Ryzen 7 3700X, 8 cores and 16 logical CPUs
- Stable v1.41.0 compiler used to build both benchmark executables

## Commands

Run these commands from the branch worktree:

```sh
git worktree add --detach /tmp/casa-450-main f414a6e
benchmark_release=$(sed -n 's/^CASAC_RELEASE_TAG=//p' casa-release.env)
mkdir /tmp/casa-450-release
gh release download "$benchmark_release" --pattern casac --dir /tmp/casa-450-release
chmod u+x /tmp/casa-450-release/casac

/tmp/casa-450-release/casac \
    -L /tmp/casa-450-main/compiler \
    -L /tmp/casa-450-main/lib \
    docs/benchmarks/control-flow-targets.casa \
    -o /tmp/casa-450-bench-main
/tmp/casa-450-release/casac \
    -L compiler \
    -L lib \
    docs/benchmarks/control-flow-targets.casa \
    -o /tmp/casa-450-bench-branch

for run in 1 2 3; do /tmp/casa-450-bench-main; done
for run in 1 2 3; do /tmp/casa-450-bench-branch; done

# Add a benchmark-only entry point to a temporary compiler copy. The production
# target table and prepass stay private.
memory_compiler=$(mktemp -d)
cp -a compiler/. "$memory_compiler"/
awk '
/^pub fn compile_typechecked$/ && !inserted {
    print "pub fn benchmark_control_flow_targets ops:$std::List[common::Op] {"
    print "    ops precompute_control_flow_targets match"
    print "        std::Result::Error(failure) => failure drop"
    print "        std::Result::Ok(targets) => targets drop"
    print "    end"
    print "}"
    print ""
    inserted = 1
}
{ print }
' compiler/bytecode.casa > "$memory_compiler/bytecode.casa"

/tmp/casa-450-release/casac \
    -L "$memory_compiler" \
    -L lib \
    docs/benchmarks/control-flow-target-memory.casa \
    -o /tmp/casa-450-memory
for run in 1 2 3; do
    gdb -q -batch \
        -x docs/benchmarks/control-flow-targets.gdb \
        --args /tmp/casa-450-memory
done
```

## Results

| Nested blocks | Main median | Branch median | Main doubling ratio | Branch doubling ratio |
|---:|---:|---:|---:|---:|
| 500 | 0.051 s | 0.003 s | n/a | n/a |
| 1,000 | 0.192 s | 0.006 s | 3.76 | 2.00 |
| 2,000 | 0.743 s | 0.014 s | 3.87 | 2.33 |
| 4,000 | 2.980 s | 0.031 s | 4.01 | 2.21 |

The main implementation approaches quadratic growth. The target index keeps
growth near linear and lowers the 4,000-block case from 2.980 seconds to 0.031
seconds.

Raw samples in execution order:

| Nested blocks | Main samples, seconds | Branch samples, seconds |
|---:|---|---|
| 500 | 0.052, 0.050, 0.051 | 0.003, 0.003, 0.004 |
| 1,000 | 0.198, 0.191, 0.192 | 0.006, 0.006, 0.007 |
| 2,000 | 0.751, 0.743, 0.741 | 0.014, 0.013, 0.015 |
| 4,000 | 2.941, 3.064, 2.980 | 0.028, 0.032, 0.031 |

## Memory

The memory benchmark repeats the private prepass on one 500-block sequence. A
temporary source copy exposes only the benchmark entry point. Production code
keeps the target table private. The source operations stay live for the full
run.

The GDB probe reads the Casa runtime allocator at prepass entry. Heap high-water
is the bump allocator offset. Reusable bytes are blocks in the large and
size-segregated free lists. Live or padding bytes are the difference. RSS is
the median of three runs.

| Completed prepasses | Heap high-water | Reusable bytes | Live or padding bytes | Free blocks | RSS |
|---:|---:|---:|---:|---:|---:|
| 0 | 700,536 B | 98,192 B | 602,344 B | 14 | 744 KiB |
| 1 | 748,656 B | 146,312 B | 602,344 B | 19 | 752 KiB |
| 10 | 748,656 B | 146,312 B | 602,344 B | 19 | 752 KiB |
| 100 | 748,656 B | 146,312 B | 602,344 B | 19 | 752 KiB |

The first prepass adds 48,120 bytes to heap high-water. All added bytes are
reusable after the call. Every allocator and RSS metric then stays fixed
through 100 completed prepasses, so the target index has a bounded live set.
