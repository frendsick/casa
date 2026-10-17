#!/usr/bin/env python3
"""Check source attribution in assembly and the linked ELF line table."""
import json
from pathlib import Path
import re
import subprocess
import sys
import tempfile


def run(*arguments):
    return subprocess.check_output(arguments, text=True, timeout=30)


def decode_specialization(symbol):
    encoded = symbol.removeprefix("fn_.specialized_")
    return re.sub(r"_(\d+)_", lambda match: chr(int(match[1])), encoded)


def test_traceability(compiler, root):
    module = root / 'module "quoted" \\ café.casa'
    module.write_text('''pub fn identity [T] value:T -> T { value }
pub fn selected [const N:u64] -> u64 { N }
pub fn recursive value:i64 -> i64 {
    if value 0 == then 0 return fi
    value 1 - recursive
}
pub fn mask value:u64 -> u64 { value 1 & }
''')
    source = root / "main.casa"
    source.write_text(f'''import {json.dumps(str(module), ensure_ascii=False)} as sample
42 sample::identity print
true sample::identity print
&sample::selected[7] exec print
&sample::selected[9] exec print
2 sample::recursive print
3 = bits:u64 bits sample::mask print
''')
    binary = root / "app"
    run(compiler, str(source), "-o", str(binary), "--keep-asm")
    assembly = Path(str(binary) + ".s").read_text()
    assert run(str(binary)) == "42true7901"
    assert ".generic_" not in assembly
    # A record followed directly by another record would describe no instruction.
    assert not re.search(r"^\.loc .*\n\.loc ", assembly, re.MULTILINE)
    # Repeated requests use the same file numbering, names, and line directives.
    run(compiler, str(source), "-o", str(root / "repeated"), "--keep-asm")
    assert assembly == (root / "repeated.s").read_text()

    functions = {}
    for line in run("nm", "-S", "--defined-only", str(binary)).splitlines():
        fields = line.split()
        if len(fields) == 4 and fields[2] in ("t", "T"):
            address, size, _, name = fields
            functions[name] = (int(address, 16), int(size, 16))
    instances = {decode_specialization(name): name for name in functions
                 if name.startswith("fn_.specialized_")}
    assert set(instances) == {
        "__casa_0__identity__mono_ 1:T=3:i64;",
        "__casa_0__identity__mono_ 1:T=4:bool;",
        "__casa_0__selected__mono_ 1:N=1:7;",
        "__casa_0__selected__mono_ 1:N=1:9;",
    }, instances
    for identity, name in instances.items():
        address, size = functions[name]
        assert size > 0
        expected_line = 1 if "identity" in identity else 2
        assert run("addr2line", "-e", str(binary), hex(address)).strip() == f"{module}:{expected_line}"

    # Query every instruction so inline expansions and the following caller
    # operations must both retain their own file and line.
    addresses = [int(match[1], 16) for match in re.finditer(
        r"^\s*([0-9a-f]+):\s", run("objdump", "-d", str(binary)), re.MULTILINE)]
    mapped = run("addr2line", "-e", str(binary), *(hex(address) for address in addresses)).splitlines()
    start, size = functions["_start"]
    root_lines = {location for address, location in zip(addresses, mapped)
                  if start <= address < start + size}
    assert f"{module}:7" in root_lines, root_lines
    for line in range(2, 8):
        assert f"{source}:{line}" in root_lines, root_lines
    recursive, size = functions["fn___casa_0__recursive"]
    recursive_lines = {location for address, location in zip(addresses, mapped)
                       if recursive <= address < recursive + size}
    assert {f"{module}:{line}" for line in (3, 4, 5)} <= recursive_lines, recursive_lines
    runtime = next(line.split()[0] for line in run("nm", str(binary)).splitlines()
                   if line.endswith(" heap_alloc"))
    assert "<casa-generated>:1" in run("addr2line", "-e", str(binary), "0x" + runtime)

    # Metadata survives ordinary builds that remove the intermediate assembly.
    run(compiler, str(source), "-o", str(root / "ordinary"))
    assert not (root / "ordinary.s").exists()
    assert ".debug_line" in run("readelf", "-S", str(root / "ordinary"))
    empty = root / "empty.casa"
    empty.write_text("")
    run(compiler, str(empty), "-o", str(root / "empty"))
    assert run(str(root / "empty")) == ""


if __name__ == "__main__":
    with tempfile.TemporaryDirectory(prefix="casa source traceability ") as temporary:
        test_traceability(str(Path(sys.argv[1]).resolve()), Path(temporary))
