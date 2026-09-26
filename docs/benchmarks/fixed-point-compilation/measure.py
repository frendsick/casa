#!/usr/bin/env python3
"""Measure one native self-build. Keep clocks, logs, output hashes and GNU time."""
import ctypes
import hashlib
import json
from pathlib import Path
import subprocess
import sys
import time


def clocks():
    """Cross-check Python's vDSO clock against direct Linux x86-64 syscalls."""
    readings = {"python_monotonic": time.monotonic_ns()}
    for name, clock_id in [("realtime", 0), ("monotonic", 1), ("monotonic_raw", 4)]:
        stamp = (ctypes.c_long * 2)()
        if LIBC.syscall(228, clock_id, ctypes.byref(stamp)) != 0:
            raise OSError(ctypes.get_errno(), "clock_gettime")
        readings[name] = stamp[0] * 1_000_000_000 + stamp[1]
    return readings


def sha256(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


if __name__ == "__main__":
    LIBC = ctypes.CDLL(None, use_errno=True)
    label, compiler_arg, root_arg, output_arg = sys.argv[1:]
    compiler, root, output = map(Path, (compiler_arg, root_arg, output_arg))
    compiler, root, output = compiler.resolve(), root.resolve(), output.resolve()
    output.mkdir(parents=True, exist_ok=True)
    binary = output / label
    if binary.exists():
        raise SystemExit(f"Output already exists: {binary}")
    command = [str(compiler), "-L", str(root / "lib"), str(root / "casa.casa"),
               "-o", str(binary), "--keep-asm", "--verbose"]
    timed_command = ["/usr/bin/time", "-f", "%e %M %U %S", "-o",
                     str(output / f"{label}.time"), *command]
    with (output / f"{label}.log").open("w") as log:
        before = clocks()
        process = subprocess.run(timed_command, cwd=root, stdout=log, stderr=log)
        after = clocks()
    metrics = (output / f"{label}.time").read_text().splitlines()[-1].split()
    result = {
        "label": label, "command": timed_command, "cwd": str(root),
        "compiler_sha256": sha256(compiler), "exit_code": process.returncode,
        "clock_before_ns": before, "clock_after_ns": after,
        "elapsed_seconds": {key: (after[key] - before[key]) / 1e9 for key in before},
        "gnu_time": dict(zip(["wall_seconds", "peak_rss_kib", "user_seconds", "system_seconds"],
                             [float(metrics[0]), int(metrics[1]), float(metrics[2]), float(metrics[3])])),
        "log": (output / f"{label}.log").read_text(),
    }
    if process.returncode == 0:
        result["binary_sha256"] = sha256(binary)
        result["assembly_sha256"] = sha256(str(binary) + ".s")
    (output / f"{label}.json").write_text(json.dumps(result, indent=2) + "\n")
    print(json.dumps(result, indent=2), flush=True)
    sys.exit(process.returncode)
