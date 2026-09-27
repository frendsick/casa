# Compiler cost attribution

Experimental evidence for [#683](https://github.com/frendsick/casa/issues/683).
The source revision is `549c68ca9a99c5596409cec0793750737dbe840d`.
This branch contains measurement tools and records. It does not implement the
production emitter change.

Replacing `rep stosq` with direct stores for function frames of 1–8 words
reduces the paired median from 18.410 to 13.928 seconds on the recorded host.
All eight outputs have identical assembly. See [evidence.json](evidence.json)
for results, limitations and the remaining candidates.

## Retained records

- [comparison.json](records/comparison.json) contains all eight measurement
  records, clocks, commands, logs, source manifests and output hashes.
- [control-builds.json](records/control-builds.json) records the three native
  build stages. Stage 2 and stage 3 assembly match. Stage 3 is the control.
- [counts.json](records/counts.json) contains every counter as `[name, value]`
  in its original manifest order, plus the manifest's `frame_bytes` map.
  Encoding its values as little-endian unsigned 64-bit integers reconstructs
  the counter output whose hash appears in `evidence.json`.
- [control-profile.json](records/control-profile.json) and
  [short-frame-profile.json](records/short-frame-profile.json) retain every
  profile sample. They use the existing fixed-point report's compact schema:
  samples are `[elapsed_raw_ns, pc, stack_index]`, stacks contain indices into
  `functions`, and `inclusive` and `leaf` contain `[function_index, count]`.
  `sampled_instructions` maps each sampled PC to its disassembled instruction.
  Packing was checked by reconstructing and comparing the complete original
  JSON objects. The profile hashes in `evidence.json` refer to those original,
  unpacked files.
- [control-profile-provenance.json](records/control-profile-provenance.json)
  confirms that the retained control profile executes the measured control's
  identical `.text` section with local labels kept for sampling.
- [verification.json](records/verification.json) records output equality for
  the counter and profile runs and probe regeneration checks. The comparison
  records retain output equality evidence for all eight timed runs.

The raw records use compact JSON to avoid repeated formatting overhead.
Original absolute command paths identify the measured runs. The commands below
use fresh output paths. ELF hashes can depend on object filenames, so compare
assembly for fixed-point and output equivalence.

## Reproduce

Use Linux x86-64, Python 3, GNU binutils and a C linker. Run from a clean source
worktree at the pinned revision, with these evidence tools available. Install
the pinned stable compiler with `./install.sh`. Run compilation, measurements
and profiles serially, without other compiler workloads.

```sh
root=$PWD
out=$(mktemp -d /tmp/casa-costs.XXXXXX)
study="$root/docs/benchmarks/compiler-cost-attribution"
baseline="$root/docs/benchmarks/fixed-point-compilation"
./casac -L "$root/lib" "$root/casa.casa" -o "$out/stage1" --keep-asm
"$out/stage1" -L "$root/lib" "$root/casa.casa" -o "$out/stage2" --keep-asm
"$out/stage2" -L "$root/lib" "$root/casa.casa" -o "$out/stage3" --keep-asm
cmp "$out/stage2.s" "$out/stage3.s"

python3 "$study/probe.py" short-frames "$out/stage3.s" \
  "$out/short-frame.s" "$out/short-frame-changes.json"
as -L "$out/short-frame.s" -o "$out/short-frame.o"
cc -nostdlib -no-pie -Wl,-e,_start -Wl,-z,noexecstack \
  -o "$out/short-frame" "$out/short-frame.o"
python3 - "$root" "$out" <<'PY'
import json, sys
from pathlib import Path
root, out = map(Path, sys.argv[1:])
variants = [{'name': name, 'compiler': str(out / binary), 'source': str(root)}
            for name, binary in [('control', 'stage3'), ('short-frame', 'short-frame')]]
(out / 'variants.json').write_text(json.dumps(variants))
PY
python3 "$root/docs/benchmarks/compiler-reductions/compare.py" \
  "$out/variants.json" "$out/comparison"
for result in "$out/comparison/"*.s; do
  cmp "$out/stage3.s" "$result"
done
```

Collect counters separately. Their runtime is not a benchmark.

```sh
python3 "$study/probe.py" counts "$out/stage3.s" \
  "$out/counted.s" "$out/counters.json"
as -L "$out/counted.s" -o "$out/counted.o"
cc -nostdlib -no-pie -Wl,-e,_start -Wl,-z,noexecstack \
  -o "$out/counted" "$out/counted.o"
"$out/counted" -L "$root/lib" "$root/casa.casa" \
  -o "$out/counted-output" --keep-asm 3> "$out/counts.bin"
cmp "$out/stage3.s" "$out/counted-output.s"
python3 - "$out" <<'PY'
import json, struct, sys
from pathlib import Path
out = Path(sys.argv[1])
manifest = json.loads((out / 'counters.json').read_text())
values = [v for (v,) in struct.iter_unpack('<Q', (out / 'counts.bin').read_bytes())]
assert len(values) == len(manifest['counters'])
counts = dict(zip(manifest['counters'], values))
for category, entry in [('allocation', 'heap_alloc'), ('memcpy', 'fn___casa_std__memcpy'),
                        ('fn___casa_std__str_hash', 'fn___casa_std__str_hash'),
                        ('fn___casa_std__String__from_str', 'fn___casa_std__String__from_str')]:
    assert sum(v for k, v in counts.items() if k.startswith(f'sizes:{category}:')) == counts[f'entry:{entry}']
(out / 'counts-decoded.json').write_text(json.dumps(counts, indent=2) + '\n')
PY
```

Collect profiles separately with local labels retained. The saved control
profile was reused from the earlier identity investigation. Its source and
native code match this control. A fresh comparison can profile both:

```sh
as -L "$out/stage3.s" -o "$out/control-profile.o"
cc -nostdlib -no-pie -Wl,-e,_start -Wl,-z,noexecstack \
  -o "$out/control-profile" "$out/control-profile.o"
objcopy -O binary --only-section=.text "$out/stage3" "$out/control.text"
objcopy -O binary --only-section=.text "$out/control-profile" "$out/profile.text"
cmp "$out/control.text" "$out/profile.text"
python3 "$baseline/sample.py" "$out/stage3.s" "$out/control-profile.json" \
  "$out/control-profile" -L "$root/lib" "$root/casa.casa" \
  -o "$out/control-profile-output" --keep-asm
python3 "$baseline/sample.py" "$out/short-frame.s" "$out/short-frame-profile.json" \
  "$out/short-frame" -L "$root/lib" "$root/casa.casa" \
  -o "$out/short-frame-profile-output" --keep-asm
cmp "$out/stage3.s" "$out/control-profile-output.s"
cmp "$out/stage3.s" "$out/short-frame-profile-output.s"
```

The sampler uses wall samples and reconstructed Casa call stacks. Inclusive
shares overlap and must not be added. Instruction attribution uses the sampled
PC directly, without relying on reconstructed callers. To recover the reported
`rep stosq` counts from the retained records:

```sh
python3 - "$study/records" <<'PY'
import json, sys
from pathlib import Path
for name in ['control', 'short-frame']:
    record = json.loads((Path(sys.argv[1]) / f'{name}-profile.json').read_text())
    instructions = dict(record['sampled_instructions'])
    count = sum(instructions[pc].startswith('rep stos ')
                for _, pc, _ in record['samples'])
    print(name, count, '/', record['sample_count'])
PY
```

For fresh samples, disassemble each corresponding executable with
`objdump -d --no-show-raw-insn`. Map each sampled PC to the instruction at that
address, or the immediately preceding instruction address when stopped inside
an instruction. Do not apply the control's address map to the modified binary.
No full compiler test suite was run for this diagnostic-only branch. Production
adoption needs source implementation, normal validation and a new fixed point.
