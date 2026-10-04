#!/usr/bin/env python3
"""Linux x86-64 wall sampling for Casa's separate return stack.

Native call return addresses distinguish frames from locals and function values
on the same stack. Legacy explicit return labels require `as -L`. The child runs
unchanged instructions. Stops add overhead, so this is attribution only.
"""
from bisect import bisect_right
from collections import Counter
import ctypes
import json
import os
from pathlib import Path
import re
import signal
import struct
import subprocess
import sys
import time


class Registers(ctypes.Structure):
    _fields_ = [(name, ctypes.c_ulonglong) for name in (
        "r15 r14 r13 r12 rbp rbx r11 r10 r9 r8 rax rcx rdx rsi rdi orig_rax "
        "rip cs eflags rsp ss fs_base gs_base ds es fs gs").split()]


def ptrace(request, pid, address=0, data=0):
    result = LIBC.ptrace(request, pid, address, data)
    if result == -1:
        raise OSError(ctypes.get_errno(), "ptrace")
    return result


def symbols(binary):
    output = subprocess.check_output(["nm", "-n", binary], text=True)
    return {name: int(address, 16) for address, _, name in
            (line.split() for line in output.splitlines() if len(line.split()) == 3)}


if __name__ == "__main__":
    assembly, result_file, *command = sys.argv[1:]
    LIBC = ctypes.CDLL(None, use_errno=True)
    names = symbols(command[0])
    code = Path(assembly).read_text()
    returns = set(re.findall(
        r"leaq (\.L\w+)\(%rip\), %(?:rax|rcx)\n\s*movq %(?:rax|rcx), -8\(%r14\)", code))
    return_addresses = {names[name] for name in returns}
    disassembly = subprocess.check_output(
        ["objdump", "-d", "--no-show-raw-insn", command[0]], text=True)
    instructions = re.findall(r"^\s*([0-9a-f]+):\s+([^\n]+)$", disassembly, re.MULTILINE)
    return_addresses.update(
        int(following[0], 16)
        for current, following in zip(instructions, instructions[1:])
        if re.match(r"callq?\s", current[1]))
    entries = sorted((address, name) for name, address in names.items()
                     if name.startswith("fn_") or name in {
                         "heap_alloc", "heap_free", "heap_alloc_native", "heap_free_native",
                         "print_int", "print_uint", "print_str",
                         "encode_char", "primitive_to_str", "str_concat", "_start",
                         "write_all", "arithmetic_error", "return_stack_overflow",
                         "env_stack_overflow"})
    addresses = [address for address, name in entries]

    def function(address):
        index = bisect_right(addresses, address) - 1
        return entries[index][1] if index >= 0 else "unknown"

    pid = os.fork()
    if pid == 0:
        ptrace(0, 0)  # PTRACE_TRACEME
        os.kill(os.getpid(), signal.SIGSTOP)
        os.execv(command[0], command)
    samples = []
    alive = True
    started = time.clock_gettime_ns(time.CLOCK_MONOTONIC_RAW)
    try:
        os.waitpid(pid, 0)
        ptrace(7, pid)  # PTRACE_CONT, then wait for the exec trap.
        _, status = os.waitpid(pid, 0)
        if not os.WIFSTOPPED(status) or os.WSTOPSIG(status) != signal.SIGTRAP:
            raise RuntimeError(f"Expected exec trap, got {status}")
        with open(f"/proc/{pid}/mem", "rb", buffering=0) as memory:
            ptrace(7, pid)
            while True:
                time.sleep(0.01)
                done, status = os.waitpid(pid, os.WNOHANG)
                if done:
                    if os.WIFSTOPPED(status):
                        ptrace(7, pid, 0, os.WSTOPSIG(status))
                        continue
                    break
                os.kill(pid, signal.SIGSTOP)
                _, status = os.waitpid(pid, 0)
                while os.WIFSTOPPED(status) and os.WSTOPSIG(status) != signal.SIGSTOP:
                    ptrace(7, pid, 0, os.WSTOPSIG(status))
                    _, status = os.waitpid(pid, 0)
                if not os.WIFSTOPPED(status):
                    break
                registers = Registers()
                ptrace(12, pid, 0, ctypes.byref(registers))  # PTRACE_GETREGS
                stack = []
                size = registers.r14 - names["return_stack"]
                if 0 <= size <= 1024 * 1024 and size % 8 == 0:
                    data = os.pread(memory.fileno(), size, names["return_stack"])
                    stack = [function(word) for (word,) in struct.iter_unpack("<Q", data)
                             if word in return_addresses]
                if function(registers.rip) in {"heap_alloc_native", "heap_free_native"}:
                    # Native allocator calls leave their return address on rsp.
                    # The mmap path can save four registers above that address.
                    native_stack = os.pread(memory.fileno(), 40, registers.rsp)
                    for (word,) in struct.iter_unpack("<Q", native_stack):
                        if word in return_addresses:
                            stack.append(function(word))
                            break
                stack.append(function(registers.rip))
                samples.append({"elapsed_raw_ns": time.clock_gettime_ns(time.CLOCK_MONOTONIC_RAW) - started,
                                "pc": registers.rip, "stack": stack})
                ptrace(7, pid)
        exit_code = os.waitstatus_to_exitcode(status)
        alive = False
    finally:
        # Do not leave a traced process stopped after a sampler failure.
        if alive:
            try:
                os.kill(pid, signal.SIGKILL)
                os.waitpid(pid, 0)
            except (ProcessLookupError, ChildProcessError):
                pass
    inclusive, leaves = Counter(), Counter()
    for sample in samples:
        inclusive.update(set(sample["stack"]))
        leaves.update(sample["stack"][-1:])
    result = {"command": command, "interval_seconds": 0.01, "exit_code": exit_code,
              "elapsed_raw_seconds": (time.clock_gettime_ns(time.CLOCK_MONOTONIC_RAW) - started) / 1e9,
              "return_addresses": len(return_addresses), "sample_count": len(samples),
              "inclusive": inclusive.most_common(), "leaf": leaves.most_common(), "samples": samples}
    Path(result_file).write_text(json.dumps(result, separators=(",", ":")) + "\n")
    print(json.dumps({key: value for key, value in result.items() if key != "samples"}, indent=2))
    sys.exit(exit_code)
