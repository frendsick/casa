#!/usr/bin/env python3
"""Measure compiler-source analysis, retained-index queries, and release."""
import json
import os
from pathlib import Path
import signal
import subprocess
import sys
import time


def measure(binary):
    process = subprocess.Popen([binary], stdout=subprocess.PIPE)
    samples = []
    intervals = []
    start = time.perf_counter()
    try:
        while True:
            _, status = os.waitpid(process.pid, os.WUNTRACED)
            elapsed = time.perf_counter() - start
            if not os.WIFSTOPPED(status):
                process.returncode = os.waitstatus_to_exitcode(status)
                break
            assert os.WSTOPSIG(status) == signal.SIGSTOP
            memory = {}
            for line in Path(f"/proc/{process.pid}/status").read_text().splitlines():
                if line.startswith(("VmRSS:", "VmHWM:")):
                    memory[line.split(":")[0]] = int(line.split()[1])
            samples.append(memory)
            intervals.append(elapsed * 1000)
            start = time.perf_counter()
            os.kill(process.pid, signal.SIGCONT)
        assert process.returncode == 0 and len(samples) == 4
        return {"analysis_ms": intervals[1], "query_100_ms": intervals[2],
                "release_ms": intervals[3], "source_bytes": int(process.stdout.read()), "rss_kib": samples}
    finally:
        if process.returncode is None:
            process.kill()
            process.wait()


if __name__ == "__main__":
    Path(sys.argv[2]).write_text(json.dumps(measure(sys.argv[1]), indent=2) + "\n")
