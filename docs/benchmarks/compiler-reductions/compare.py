#!/usr/bin/env python3
"""Compare incremental native variants with shared, alternating paired runs."""
import hashlib
import json
from pathlib import Path
import statistics
import subprocess
import sys


def source_hashes(root):
    paths = [root / 'casa.casa', *sorted((root / 'compiler').glob('*.casa')),
             *sorted((root / 'lib').rglob('*.casa'))]
    return {str(path.relative_to(root)): hashlib.sha256(path.read_bytes()).hexdigest()
            for path in paths}


if __name__ == '__main__':
    config, destination = map(Path, sys.argv[1:])
    builds = {item['name']: (Path(item['compiler']).resolve(), Path(item['source']).resolve())
              for item in json.loads(config.read_text())}
    destination = destination.resolve()
    destination.mkdir(parents=True, exist_ok=True)
    measure = Path(__file__).resolve().parents[1] / 'fixed-point-compilation' / 'measure.py'
    sources = {name: source_hashes(root) for name, (_, root) in builds.items()}
    samples = []
    names = list(builds)
    order = [('warmup', names), ('pair1', names),
             ('pair2', list(reversed(names))), ('pair3', names)]
    for pair, names in order:
        for name in names:
            compiler, root = builds[name]
            label = f'{pair}-{name}'
            print(f'Starting {label}', flush=True)
            with (destination / f'{label}.measurement.log').open('w') as log:
                subprocess.run([sys.executable, str(measure), label, str(compiler),
                                str(root), str(destination)], stdout=log, stderr=log, check=True)
            result = json.loads((destination / f'{label}.json').read_text())
            result.update({'variant': name, 'pair': pair})
            samples.append(result)
            print(f"{label}: {result['elapsed_seconds']['monotonic_raw']:.3f} s, "
                  f"{result['gnu_time']['peak_rss_kib']} KiB", flush=True)
    for name, (_, root) in builds.items():
        assert source_hashes(root) == sources[name], f'{name} source changed during measurement'
        assert len({item['assembly_sha256'] for item in samples
                    if item['variant'] == name}) == 1, f'{name} output was not deterministic'
        assert len({item['compiler_sha256'] for item in samples
                    if item['variant'] == name}) == 1, f'{name} compiler changed'
    medians = {}
    for name in builds:
        runs = [item for item in samples if item['variant'] == name and item['pair'] != 'warmup']
        medians[name] = {
            'raw_seconds': statistics.median(item['elapsed_seconds']['monotonic_raw'] for item in runs),
            'peak_rss_kib': statistics.median(item['gnu_time']['peak_rss_kib'] for item in runs),
        }
    result = {'sources': sources, 'order': order, 'samples': samples, 'medians': medians}
    (destination / 'comparison.json').write_text(json.dumps(result, indent=2) + '\n')
    print(json.dumps(medians, indent=2), flush=True)
