#!/usr/bin/env python3
"""Check assembly preservation, cleanup, and overlapping native driver calls."""
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import time


def test_preservation(compiler, root):
    source = root / "empty.casa"
    source.write_text("")
    for kind in ("absent", "dangling", "directory", "file", "long_name", "readonly", "symlink"):
        folder = root / kind
        folder.mkdir()
        output = folder / ("a" * 250 if kind == "long_name" else "app")
        assembly = Path(str(output) + ".s")
        target = folder / "target"
        if kind == "directory":
            assembly.mkdir()
            (assembly / "unrelated").write_bytes(b"keep this file\n")
        elif kind in ("file", "readonly"):
            assembly.write_bytes(b"keep this file\n")
            if kind == "readonly":
                assembly.chmod(0o444)
        elif kind in ("symlink", "dangling"):
            if kind == "symlink":
                target.write_bytes(b"keep this file\n")
            assembly.symlink_to(target)
        original_entries = set(folder.iterdir())
        for fail in (False, True):
            command = [compiler, str(source), "-o", str(output)]
            if fail:
                command += ["-l", "casa_missing_native_library"]
            result = subprocess.run(command, capture_output=True, text=True, timeout=20)
            assert (result.returncode != 0) == fail, result.stderr
            if fail:
                assert "native build failed with exit code" in result.stderr, result.stderr
            else:
                subprocess.run([output], check=True, timeout=10)
                output.unlink()
            assert set(folder.iterdir()) == original_entries, list(folder.iterdir())
            if kind in ("absent", "long_name"):
                assert not assembly.exists()
            elif kind == "directory":
                assert (assembly / "unrelated").read_bytes() == b"keep this file\n"
            elif kind in ("file", "readonly"):
                assert assembly.read_bytes() == b"keep this file\n"
                if kind == "readonly":
                    assert assembly.stat().st_mode & 0o222 == 0
            else:
                assert assembly.is_symlink() and assembly.readlink() == target
                if kind == "symlink":
                    assert target.read_bytes() == b"keep this file\n"
                else:
                    assert not target.exists()


def test_concurrent_builds(compiler, root):
    driver = root / "native-driver"
    driver.write_text(f"#!{sys.executable}\n" + '''
from pathlib import Path
import os
import sys
import time
root = Path(__file__).parent
assembly = Path(sys.argv[-1])
record = root / f"record.{os.getpid()}"
pending = root / f"pending.{os.getpid()}"
pending.write_text(str(assembly))
pending.replace(record)
release = root / f"release.{os.getpid()}"
deadline = time.monotonic() + 20
while not release.exists():
    if time.monotonic() > deadline:
        sys.exit(2)
    time.sleep(0.01)
assert assembly.read_text() == "complete assembly\\n"
''')
    driver.chmod(0o700)
    repository = Path(__file__).resolve().parent.parent
    source = root / "driver.casa"
    output = root / "shared"
    source.write_text(f'''
import "std" as std
import "os" as os
import {json.dumps(str(repository / "compiler/build.casa"))} as build
import {json.dumps(str(repository / "compiler/products.casa"))} as products
products::Target::LinuxX86_64 "complete assembly\\n".to_str products::AssemblySource::new = source
std::List[std::String]::new = libraries
{json.dumps(str(driver))}.to_str build::NativeDriver = driver
# Reserve the first candidate to exercise collision handling without changing it.
# SAFETY: getpid has no pointer arguments.
unsafe {{ 39 syscall0 = pid }}
f"{root}/.casa-assembly.{{pid}}.0" = occupied
448 occupied.as_str.as_cstr.unwrap dir::create.unwrap drop
"unrelated" std::Bytes::from_str f"{{occupied}}/marker".as_str.as_cstr.unwrap file::write_all.unwrap drop
false libraries {json.dumps(str(output))} source driver.compile_binary.unwrap drop
''')
    harness = root / "harness"
    subprocess.run([compiler, "-L", str(repository / "lib"), str(source), "-o", str(harness)],
                   check=True, timeout=30)
    processes = [subprocess.Popen([harness]) for _ in range(2)]
    try:
        deadline = time.monotonic() + 10
        while len(list(root.glob("record.*"))) != 2:
            assert time.monotonic() < deadline, "native drivers did not overlap"
            assert all(process.poll() is None for process in processes)
            time.sleep(0.01)
        records = sorted(root.glob("record.*"))
        assemblies = [Path(record.read_text()) for record in records]
        assert assemblies[0] != assemblies[1], assemblies
        for assembly in assemblies:
            assert assembly.is_file()
            assert assembly.parent.stat().st_mode & 0o777 == 0o700
        (root / records[0].name.replace("record.", "release.")).touch()
        deadline = time.monotonic() + 10
        while assemblies[0].parent.exists():
            assert time.monotonic() < deadline, "first build did not clean up"
            time.sleep(0.01)
        assert assemblies[1].read_text() == "complete assembly\n"
        (root / records[1].name.replace("record.", "release.")).touch()
        for process in processes:
            assert process.wait(timeout=10) == 0
            occupied = root / f".casa-assembly.{process.pid}.0"
            assert (occupied / "marker").read_text() == "unrelated"
        assert all(not assembly.parent.exists() for assembly in assemblies)
    finally:
        for process in processes:
            if process.poll() is None:
                process.kill()
            process.wait(timeout=10)


def main(compiler):
    compiler = str(Path(compiler).resolve())
    with tempfile.TemporaryDirectory(prefix="casa native assembly ") as temporary:
        root = Path(temporary)
        test_preservation(compiler, root)
        test_concurrent_builds(compiler, root)


if __name__ == "__main__":
    main(sys.argv[1])
