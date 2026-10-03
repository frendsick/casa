#!/usr/bin/env python3
"""Sample Casa allocator blocks and RSS at explicit SIGSTOP boundaries on Linux."""
import hashlib
import json
import os
from pathlib import Path
import signal
import struct
import subprocess
import sys


def sample(pid, names):
    with open(f'/proc/{pid}/mem', 'rb', buffering=0) as memory:
        def word(address):
            return struct.unpack('<Q', os.pread(memory.fileno(), 8, address))[0]

        free = set()
        heads = [word(names['free_list'])]
        heads.extend(word(names['small_free_lists'] + offset) for offset in range(0, 512, 8))
        for block in heads:
            while block:
                assert block not in free, 'Duplicate or cyclic free block'
                free.add(block)
                block = word(block + 8)

        blocks = {}
        mapped = 0
        chunk = word(names['heap_chunks'])
        while chunk:
            end = word(chunk + 8)
            mapped += end - chunk
            block = chunk + 16
            while block + 8 <= end:
                size = word(block)
                if size == 0:
                    break
                assert size % 8 == 0 and block + 8 + size <= end
                blocks[block] = size
                block += 8 + size
            chunk = word(chunk)
        assert free <= blocks.keys()
        live = blocks.keys() - free
        status = Path(f'/proc/{pid}/status').read_text().splitlines()
        rss = {line.split(':')[0]: int(line.split()[1]) for line in status
               if line.startswith(('VmRSS:', 'VmHWM:'))}
        return {'live_blocks': len(live), 'live_payload_bytes': sum(blocks[b] for b in live),
                'reusable_blocks': len(free), 'reusable_payload_bytes': sum(blocks[b] for b in free),
                'mapped_heap_bytes': mapped, 'rss_kib': rss['VmRSS'], 'peak_rss_kib': rss['VmHWM']}


if __name__ == '__main__':
    binary, output = map(Path, sys.argv[1:])
    binary = binary.resolve()
    symbols = subprocess.check_output(['nm', '-n', str(binary)], text=True)
    names = {parts[2]: int(parts[0], 16) for line in symbols.splitlines()
             if len(parts := line.split()) == 3}
    process = subprocess.Popen([str(binary)])
    samples = []
    try:
        while True:
            _, status = os.waitpid(process.pid, os.WUNTRACED)
            if not os.WIFSTOPPED(status):
                process.returncode = os.waitstatus_to_exitcode(status)
                break
            assert os.WSTOPSIG(status) == signal.SIGSTOP
            samples.append(sample(process.pid, names))
            os.kill(process.pid, signal.SIGCONT)
    finally:
        if process.returncode is None:
            process.kill()
            process.wait()
    result = {'command': [str(binary)], 'binary_sha256': hashlib.sha256(binary.read_bytes()).hexdigest(),
              'exit_code': process.returncode, 'samples': samples}
    output.write_text(json.dumps(result, indent=2) + '\n')
    assert process.returncode == 0 and len(samples) == 31
    # Ignore initialization and five warm-up requests.
    assert all(item == samples[6] for item in samples[6:]), 'Memory did not plateau'
    print(json.dumps({'initial': samples[0], 'plateau': samples[6], 'samples': len(samples)}, indent=2))
