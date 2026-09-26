#!/usr/bin/env python3
"""Add entry counters to generated assembly. Run the result with fd 3 redirected.

Only the profiling executable changes. Recompiling clean sources must reproduce
the uninstrumented assembly. Counters preserve flags and the value stack.
"""
import json
from pathlib import Path
import re
import sys

source, target, manifest = map(Path, sys.argv[1:])
assembly = source.read_text()
labels = re.findall(r"^(\w+):$", assembly, re.M)
selected = [name for name in labels if
            re.match(r"fn___casa_module_(7|8|15)__", name) or
            re.search(r"__(?:derived_clone|_derived_clone|clone)(?:__mono_\d+)?$", name) or
            name in {"heap_alloc", "heap_free"}]
selected += [".Lheap_alloc_large", ".Lheap_alloc_search", ".Lheap_alloc_map"]
for index, name in enumerate(selected):
    old = f"\n{name}:\n"
    assert assembly.count(old) == 1, name
    assembly = assembly.replace(old, old +
        f"    pushfq\n    incq profile_counts+{index * 8}(%rip)\n    popfq\n")

# Count requested bytes, distinct from live bytes, mapped capacity, or RSS.
selected.append("requested_allocation_bytes")
assembly = assembly.replace("\nheap_alloc:\n", "\nheap_alloc:\n" +
    f"    pushfq\n    addq %rdi, profile_counts+{(len(selected)-1)*8}(%rip)\n    popfq\n")

# The final root return is followed directly by the process-exit syscall.
needle = "    movq $60, %rax\n    xorq %rdi, %rdi\n    syscall\n"
assert assembly.count(needle) == 1
assembly = assembly.replace(needle,
    f"    movq $1, %rax\n    movq $3, %rdi\n    leaq profile_counts(%rip), %rsi\n"
    f"    movq ${len(selected)*8}, %rdx\n    syscall\n" + needle)
assembly += f"\n.bss\n.align 8\nprofile_counts: .skip {len(selected)*8}\n"
target.write_text(assembly)
manifest.write_text(json.dumps(selected, indent=2) + "\n")
print(f"Added {len(selected)} counters")
