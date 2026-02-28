"""Assembler and linker integration for producing x86_64 binaries."""

import os
import pathlib
import subprocess
import sys


def run_cmd(cmd: list[str]) -> None:
    """Run a shell command and exit on failure."""
    result = subprocess.run(cmd, capture_output=True, check=False)
    if result.returncode != 0:
        print(result.stderr.decode(), file=sys.stderr)
        sys.exit(1)


def compile_binary(asm_source: str, output_name: str, keep_asm: bool) -> None:
    """Assemble and link an assembly source string into an executable."""
    asm_file = f"{output_name}.s"
    obj_file = f"{output_name}.o"

    pathlib.Path(asm_file).write_text(asm_source, encoding="utf-8")

    run_cmd(["as", "-o", obj_file, asm_file])
    run_cmd(["ld", "-o", output_name, obj_file])

    # Clean build artifacts
    if not keep_asm:
        os.remove(asm_file)
    os.remove(obj_file)
