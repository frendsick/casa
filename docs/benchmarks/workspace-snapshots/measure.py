#!/usr/bin/env python3
"""Measure end-to-end workspace requests with two unopened importers."""
import json
from pathlib import Path
import statistics
import subprocess
import sys
import tempfile
import time

sys.dont_write_bytecode = True
sys.path.insert(0, str(Path(__file__).resolve().parents[3] / "tests"))
from test_lsp_workspace import Server


def measure(binary, discovery):
    failures = {
        "lexical": 'fn broken { "unterminated',
        "syntax": 'fn broken {',
        "import": 'import "missing.casa"',
        "type": 'fn broken { true 1 + drop }',
        "ownership": 'struct Resource { value:str } fn broken { Resource { value: "owned" } = owner owner = moved { owner.value } drop moved drop }',
    }
    timings = {name: [] for name in ("cold_references_ms", "warm_references_ms", "rename_ms", "edit_and_references_ms", "rejected_rename_ms")}
    timings.update({f"rejected_{kind}_ms": [] for kind in failures})
    timings["discovery_ms"] = []
    with tempfile.TemporaryDirectory(prefix="casa-workspace-cost-") as temporary:
        root = Path(temporary)
        shared = root / "shared.casa"
        source = "pub fn greet { }\ngreet\n"
        shared.write_text(source)
        for name in ("first", "second"):
            (root / f"{name}.casa").write_text('import "shared.casa" as peer\npeer::greet\n')
        for trial in range(3):
            elapsed_ns = int(subprocess.check_output([discovery], text=True))
            timings["discovery_ms"].append(elapsed_ns / 1000 / 1_000_000)
            server = Server(binary, root)
            try:
                for name, method, extra in (
                    ("cold_references_ms", "references", {}),
                    ("warm_references_ms", "references", {}),
                    ("rename_ms", "rename", {"newName": "hello"}),
                    ("edit_and_references_ms", "references", {}),
                    ("rejected_rename_ms", "rename", {"newName": "hello"}),
                ):
                    if name == "rejected_rename_ms":
                        (root / "broken.casa").write_text('import "missing.casa"\n')
                    start = time.perf_counter()
                    if name == "edit_and_references_ms":
                        server.open(shared, source + "greet\n", trial + 1)
                    response = server.query(method, shared, 0, 7, **extra)
                    timings[name].append((time.perf_counter() - start) * 1000)
                    assert ("error" in response) == (name == "rejected_rename_ms"), response
                for kind, failure in failures.items():
                    (root / "broken.casa").write_text(failure)
                    start = time.perf_counter()
                    response = server.query("rename", shared, 0, 7, newName="hello")
                    timings[f"rejected_{kind}_ms"].append((time.perf_counter() - start) * 1000)
                    assert "error" in response, (kind, response)
            finally:
                server.close()
                (root / "broken.casa").unlink(missing_ok=True)
    return {name: {"runs": values, "median": statistics.median(values)} for name, values in timings.items()}


if __name__ == "__main__":
    Path(sys.argv[2]).write_text(json.dumps(measure(sys.argv[1], sys.argv[3]), indent=2) + "\n")
