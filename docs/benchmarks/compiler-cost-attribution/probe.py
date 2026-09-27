#!/usr/bin/env python3
"""Build count or small-frame probes from an existing native compiler assembly.

Entry and callsite probes preserve registers, flags and both Casa stacks.
The successful exit writes little-endian u64 counters to fd 3. Compile clean
sources with the instrumented binary and compare the emitted assembly with the
unmodified compiler before using the counts.
"""
import json
from pathlib import Path
import re
import sys


def instrument(source, target, manifest):
    lines = source.read_text().splitlines(keepends=True)
    counters = []
    indices = {}
    frames = {}
    selected = {
        "heap_alloc", "heap_free", "fn_str__at", "fn_str__eq",
        "fn___casa_std__str_hash", "fn___casa_std__memcpy",
        "fn___casa_std__String__from_str", "fn___casa_std__String__append",
        "fn___casa_std__String__reserve",
    }

    def slot(name):
        if name not in indices:
            indices[name] = len(counters)
            counters.append(name)
        return f"cost_counts+{indices[name] * 8}(%rip)"

    def add(name, value=None):
        instruction = f"addq {value}, {slot(name)}" if value else f"incq {slot(name)}"
        return f"    {instruction}\n"

    def histogram(name, expression):
        # Save the only scratch register. expressions using rsp account for both pushes.
        result = "    pushfq\n    pushq %rax\n" + expression
        result += add(f"bytes:{name}", "%rax")
        marker = len(counters)
        limits = [0, 8, 16, 24, 32, 64, 128, 256, 512, 4096]
        for limit in limits:
            result += f"    cmpq ${limit}, %rax\n    jbe .Lcost_{marker}_{limit}\n"
        result += add(f"sizes:{name}:over4096") + f"    jmp .Lcost_{marker}_done\n"
        for limit in limits:
            result += f".Lcost_{marker}_{limit}:\n" + add(f"sizes:{name}:le{limit}")
            result += f"    jmp .Lcost_{marker}_done\n"
        return result + f".Lcost_{marker}_done:\n    popq %rax\n    popfq\n"

    output = []
    current = "runtime"
    for index, line in enumerate(lines):
        label = re.fullmatch(r"(\w+):\n", line)
        if label:
            name = label[1]
            current = name
            output.append(line)
            if name.startswith("fn_") or name in {"heap_alloc", "heap_free"}:
                output.append("    pushfq\n" + add(f"entry:{name}") + "    popfq\n")
                prologue = "".join(lines[index + 1:index + 12])
                frame = re.search(r"addq \$(\d+), %r14\n.*?rep stosq", prologue, re.S)
                if frame:
                    frames[name] = int(frame[1])
            if name == "heap_alloc":
                output.append(histogram("allocation", "    movq %rdi, %rax\n"))
            elif name == "fn___casa_std__memcpy":
                output.append(histogram("memcpy", "    movq 32(%rsp), %rax\n"))
            elif name in {"fn___casa_std__str_hash", "fn___casa_std__String__from_str"}:
                output.append(histogram(name, "    movq 16(%rsp), %rax\n    movq (%rax), %rax\n"))
            continue
        local = re.fullmatch(r"(\.Lheap_alloc_(?:large|search|bump|map|reuse)):\n", line)
        if local:
            output.append(line)
            output.append("    pushfq\n" + add(f"path:{local[1]}") + "    popfq\n")
            continue
        jump = re.fullmatch(r"\s+jmp (\w+)\n", line)
        if jump and jump[1] in selected:
            name = jump[1]
            probe = add(f"caller:{name}:{current}")
            if name == "heap_alloc":
                probe += add(f"allocated_bytes:{current}", "%rdi")
            output.append("    pushfq\n" + probe + "    popfq\n")
        output.append(line)
    assembly = "".join(output)
    exit_sequence = "    movq $60, %rax\n    xorq %rdi, %rdi\n    syscall\n"
    assert assembly.count(exit_sequence) == 1
    dump = ("    movq $1, %rax\n    movq $3, %rdi\n"
            "    leaq cost_counts(%rip), %rsi\n"
            f"    movq ${len(counters) * 8}, %rdx\n    syscall\n")
    assembly = assembly.replace(exit_sequence, dump + exit_sequence)
    assembly += f"\n.bss\n.align 8\ncost_counts: .skip {len(counters) * 8}\n"
    target.write_text(assembly)
    manifest.write_text(json.dumps({"counters": counters, "frame_bytes": frames}, indent=2) + "\n")
    print(f"Inserted {len(counters)} counters, recorded {len(frames)} function frames")


def short_frames(source, target, manifest):
    """Replace only 1-8 word frame fills, preserving registers and flags.

    Capacity checks and r14 reservations stay intact. Larger fills keep rep
    stosq. The input compiler never sets the direction flag, so forward stores
    reproduce its initialized bytes. This changes the profiling executable,
    not the assembly that compiler emits for its source input.
    """
    assembly = source.read_text()
    assert not re.search(r"^\s+std\s*$", assembly, re.M)
    pattern = re.compile(r"    movq \$(\d+), %rcx\n    xorq %rax, %rax\n    rep stosq\n")
    changed = []

    def replace(match):
        count = int(match[1])
        if not 1 <= count <= 8:
            return match[0]
        preceding = assembly[max(0, match.start() - 80):match.start()]
        assert preceding.endswith(f"    addq ${count * 8}, %r14\n")
        changed.append(count)
        stores = "".join(f"    movq %rax, {index * 8}(%rdi)\n" for index in range(count))
        return ("    xorq %rax, %rax\n" + stores +
                f"    leaq {count * 8}(%rdi), %rdi\n    movq $0, %rcx\n")

    target.write_text(pattern.sub(replace, assembly))
    assert changed, "No short frame fills found"
    manifest.write_text(json.dumps({
        "max_slots": 8, "changed_sites": len(changed),
        "slot_counts": {str(count): changed.count(count) for count in sorted(set(changed))},
    }, indent=2) + "\n")
    print(f"Replaced {len(changed)} short frame fills")


if __name__ == "__main__":
    mode, *paths = sys.argv[1:]
    {"counts": instrument, "short-frames": short_frames}[mode](*map(Path, paths))
