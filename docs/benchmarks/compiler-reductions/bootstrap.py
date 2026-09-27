#!/usr/bin/env python3
"""Build three native stages and retain fixed-point hashes and commands."""
import hashlib
import json
from pathlib import Path
import subprocess
import sys

compiler, root, output = map(lambda value: Path(value).resolve(), sys.argv[1:])
output.mkdir(parents=True, exist_ok=True)
records = []
for stage in range(1, 4):
    target = output / f'stage{stage}'
    command = [str(compiler), '-L', str(root / 'lib'), str(root / 'casa.casa'),
               '-o', str(target), '--keep-asm', '--verbose']
    print(f'Building {target}', flush=True)
    with (output / f'stage{stage}.log').open('w') as log:
        subprocess.run(command, stdout=log, stderr=log, check=True)
    records.append({'command': command,
                    'compiler_sha256': hashlib.sha256(compiler.read_bytes()).hexdigest(),
                    'binary_sha256': hashlib.sha256(target.read_bytes()).hexdigest(),
                    'assembly_sha256': hashlib.sha256(Path(str(target) + '.s').read_bytes()).hexdigest()})
    compiler = target
assert records[1]['assembly_sha256'] == records[2]['assembly_sha256'], 'Fixed point failed'
(output / 'builds.json').write_text(json.dumps(records, indent=2) + '\n')
print('Fixed point verified', flush=True)
