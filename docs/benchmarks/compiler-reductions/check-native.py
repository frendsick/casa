#!/usr/bin/env python3
"""Compare arithmetic traps and the leaf return-stack boundary across variants."""
import hashlib
import json
from pathlib import Path
import subprocess
import sys


if __name__ == '__main__':
    config, output = map(Path, sys.argv[1:])
    output = output.resolve()
    output.mkdir(parents=True, exist_ok=True)
    boundary = Path(__file__).resolve().parents[3] / 'tests/compiler/runtime_errors/leaf_return_stack_overflow.casa'
    cases = {}
    for integer, maximum, minimum in [('u64', 2**64 - 1, 0), ('i64', 2**63 - 1, -(2**63))]:
        for name, value, operand, operator in [('add', maximum, 1, '+'), ('sub', minimum, 1, '-'), ('mul', maximum, 2, '*')]:
            cases[f'{integer}_{name}'] = f'{value} = value:{integer} value {operand} {operator} drop\n'
            cases[f'{integer}_{name}_leaf'] = (
                f'fn calculate value:{integer} -> {integer} {{ value {operand} {operator} }}\n'
                f'{value} calculate drop\n')
    cases['leaf_return_stack_overflow'] = boundary.read_text()
    for mode in ['direct', 'indirect']:
        name = f'leaf_arithmetic_boundary_{mode}'
        cases[name] = boundary.with_name(name + '.casa').read_text()
    variants = json.loads(config.read_text())
    records = []
    for variant in variants:
        compiler = Path(variant['compiler']).resolve()
        root = Path(variant['source']).resolve()
        compiler_hash = hashlib.sha256(compiler.read_bytes()).hexdigest()
        for name, source in cases.items():
            path = output / f'{name}.casa'
            path.write_text(source)
            target = output / f"{variant['name']}-{name}"
            command = [str(compiler), '-L', str(root / 'lib'), str(path), '-o', str(target)]
            built = subprocess.run(command, capture_output=True, text=True)
            assert built.returncode == 0, (command, built.stderr)
            ran = subprocess.run([str(target)], capture_output=True, text=True)
            assert ran.returncode == 1, (name, ran.returncode, ran.stderr)
            if name == 'leaf_return_stack_overflow':
                assert ran.stderr == 'leaf boundary returned: error: return stack overflow\n', ran.stderr
            elif name.startswith('leaf_arithmetic_boundary_'):
                assert ran.stderr == 'error: return stack overflow\n', ran.stderr
            else:
                assert 'arithmetic' in ran.stderr, ran.stderr
            records.append({'variant': variant['name'], 'case': name, 'source': source,
                            'command': command, 'compiler_sha256': compiler_hash,
                            'exit_code': ran.returncode, 'stdout': ran.stdout, 'stderr': ran.stderr})
    for name in cases:
        results = {(row['exit_code'], row['stdout'], row['stderr'])
                   for row in records if row['case'] == name}
        assert len(results) == 1, (name, results)
    (output / 'checks.json').write_text(json.dumps(records, indent=2) + '\n')
    print(f'{len(records)} executions agree with the control')
