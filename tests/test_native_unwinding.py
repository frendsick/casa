"""Check native backtraces through runtime failures, including saved registers."""
from pathlib import Path
import re
import subprocess
import sys
import tempfile


def check_backtrace(compiler, root, name, source, expected, setup=(), stop="break *write_all"):
    path = root / f"{name}.casa"
    path.write_text(source)
    output = root / name
    subprocess.run([compiler, "-L", "lib", str(path), "-o", str(output)],
                   check=True, timeout=30)
    commands = ["set pagination off", "set disable-randomization off"]
    commands.extend(setup)
    commands.extend([stop, "continue" if setup else "run", "backtrace", "disable breakpoints", "continue"])
    arguments = ["gdb", "-nx", "-batch"]
    for command in commands:
        arguments.extend(["-ex", command])
    result = subprocess.run([*arguments, str(output)], capture_output=True, text=True, timeout=20)
    assert result.returncode == 0, result.stdout + result.stderr
    frames = re.findall(r"^#\d+\s+.*", result.stdout, re.MULTILINE)
    assert len(frames) == len(expected), result.stdout + result.stderr
    for frame, symbol in zip(frames, expected):
        assert re.search(rf"\b{symbol}\b", frame), result.stdout + result.stderr
    assert "exited with code 01" in result.stdout, result.stdout + result.stderr


def main(compiler):
    compiler = str(Path(compiler).resolve())
    with tempfile.TemporaryDirectory(prefix="casa unwind ") as temporary:
        root = Path(temporary)
        for name, declaration, value in (
            ("cached", "fn fail value:i64 -> i64 { value 1 + }", "9223372036854775807"),
            ("early_return", "fn fail value:i64 -> i64 { if value 0 < then value return fi value 1 + }", "9223372036854775807"),
            ("division", "fn fail value:i64 -> i64 { value 0 / }", "1"),
            ("conversion", "fn fail value:f64 -> i64 { value i64::trunc_from }", "1e30"),
        ):
            source = (f"import \"std\" as std\n{declaration}\n"
                      f"fn outer {{ {value} fail print }} outer")
            check_backtrace(compiler, root, name, source,
                            ["write_all", "arithmetic_error", "fn_fail", "fn_outer", "_start"])

        # These fail while aligning the size, and in heap_grow with four saved
        # words before and after mmap. None allocates a large block of physical
        # memory.
        for size, frames in (
            (18446744073709551615, ["heap_alloc"]),
            (18446744073709551608, ["heap_grow", "heap_alloc"]),
            (9223372036854775808, ["heap_grow", "heap_alloc"]),
        ):
            source = ("fn fail size:u64 { unsafe { size alloc free } }\n"
                      f"fn outer {{ {size} fail }} outer")
            check_backtrace(compiler, root, f"allocation_{size}", source,
                            ["write_all", "allocation_failure", *frames, "fn_fail", "fn_outer", "_start"])

        source = ("fn increment value:i64 -> i64 { value 1 + }\n"
                  "fn fail value:i64 -> i64 { value &increment exec }\n"
                  "fn outer { 1 fail print } outer")
        check_backtrace(compiler, root, "environment", source,
                        ["write_all", "env_stack_overflow", "fn_fail", "fn_outer", "_start"],
                        ["break *fn_fail", "run", "set {long long}&env_sp = 1048576"])

        # Inject an allocation failure during primitive formatting, after the
        # runtime saved its source pointer and length on the native stack.
        source = 'fn fail { f"{123}" print } fn outer { fail } outer'
        check_backtrace(compiler, root, "formatting", source,
                        ["write_all", "allocation_failure", "heap_alloc", "primitive_to_str", "fn_fail", "fn_outer", "_start"],
                        ["break *heap_alloc", "run", "set $rdi = -1"])

        # Reject sigaltstack while guard initialization still owns its scratch
        # frame. The failure reports directly through the write syscall.
        check_backtrace(compiler, root, "guard_setup", "",
                        ["return_stack_guard_init", "_start"],
                        ["catch syscall 131", "run", "set $orig_rax = -1", "delete breakpoints"],
                        "catch syscall 1")


if __name__ == "__main__":
    main(sys.argv[1])
