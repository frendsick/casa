#!/usr/bin/env python3
"""Run the throwaway slice's evidence checks and paired measurements.

Usage: python3 compiler/blueprint_prototype/run.py --control /path/to/control
Outputs are written to the requested directory, never to production paths.
"""
from dataclasses import asdict, replace
from pathlib import Path
import argparse
import gc
import hashlib
import json
import os
import platform
import shutil
import statistics
import subprocess
import sys
import time
import tracemalloc
import weakref
import zipapp

import capsule as c

HERE = Path(__file__).resolve().parent
EXPECTED = {"common": "4117193232931991", "ownership": "7787879192", "native": "45140"}


def command(arguments, **kwargs):
    result = subprocess.run([str(arg) for arg in arguments], capture_output=True, text=True, **kwargs)
    if result.returncode:
        raise RuntimeError(f"{arguments}: {result.returncode}\n{result.stderr}")
    return result.stdout


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def checks(directory, control):
    outcomes = {}
    command(["cc", "-c", HERE / "native.c", "-o", directory / "native.o"])
    command(["ar", "rcs", directory / "libcapsule651.a", directory / "native.o"])
    environment = dict(os.environ, LIBRARY_PATH=str(directory))
    for name, expected in EXPECTED.items():
        source = (HERE / (name + ".casa")).read_text()
        result = c.assembly(source)
        assert isinstance(result, c.AssemblyResult) and result.source, result
        output = directory / (name + "-candidate")
        libraries = ["capsule651"] if name == "native" else []
        previous = os.environ.get("LIBRARY_PATH")
        os.environ["LIBRARY_PATH"] = str(directory)
        try:
            assert c.build(result.source, output, libraries, keep_asm=True) is None
        finally:
            if previous is None:
                del os.environ["LIBRARY_PATH"]
            else:
                os.environ["LIBRARY_PATH"] = previous
        assert command([output]) == expected
        assert Path(str(output) + ".s").read_text() == result.source.text
        control_output = directory / (name + "-control")
        arguments = [control, HERE / (name + ".casa"), "--keep-asm", "-o", control_output]
        for library in libraries:
            arguments.extend(["-l", library])
        command(arguments, env=environment)
        assert command([control_output]) == expected
        outcomes[name] = {"stdout": expected, "candidate_bytes": output.stat().st_size,
                          "control_bytes": control_output.stat().st_size,
                          "candidate_assembly_bytes": len(result.source.text.encode()),
                          "control_assembly_bytes": Path(str(control_output) + ".s").stat().st_size}

    rejected = {
        "source_type": "true 1 + drop",
        "use_after_move": "struct R { id:i64 } fn f { 1 R = owner owner drop owner drop } f",
        "local_borrow_escape": "struct R { id:i64 } fn view value:$R -> $R { value } fn bad -> $R { 1 R = owner owner view }",
        "borrowed_result_keeps_owner_live": "struct R { id:i64 } fn view[T] value:$T -> $T { value } fn bad { 1 R = owner owner view = loan owner drop loan drop }",
        "branch_stack": "if true then 1 else false fi drop",
        "unsafe_extern": "extern fn external value:i64 -> i64 1 external drop",
        "owned_extern": "struct R { id:i64 } extern fn external value:R 1 R unsafe { external }",
        "scope_borrow_escape": "struct R { id:i64 } fn view value:$R -> $R { value } fn read value:$R -> i64 { value.id } unsafe { 1 R = owner owner view } read print",
        "borrow_reassignment": "struct R { id:i64 } fn view value:$R -> $R { value } fn bad { 1 R = a 2 R = b a view = loan b view = loan b drop loan.id print }",
        "closure_not_executed": "{ 1 print }",
        "shared_cannot_become_exclusive": "struct R { id:i64 } fn mutate value:mut$R { } fn invalid value:$R { value mutate }",
        "unsupported_native_layout": "extern fn flag value:bool -> bool unsafe { true flag drop }",
        "stack_borrow_keeps_owner_live": "struct R { id:i64 } fn view value:$R -> $R { value } fn invalid { 1 R = owner owner view owner drop drop }",
        "temporary_owner_borrow": "struct R { id:i64 } fn read value:$R -> i64 { value.id } 7 R read print",
        "temporary_owner_projection": "struct R { id:i64 } 7 R.id print",
        "nested_owner": "struct R { id:i64 } struct Outer { inner:R } 7 R Outer drop",
        "ordinary_copy": "struct R derives __casa_std__Copy { id:i64 }",
        "drop_signature": "struct R { id:i64 } impl R { fn drop self:mut$R other:i64 { other print } } fn run { 7 R = owner } run",
        "borrow_is_not_an_owner": "struct R { id:i64 } fn take value:R { } fn invalid value:$R { value take }",
        "unknown_signature": "fn unused value:Unknown { }",
        "duplicate_parameter": "fn invalid value:i64 value:i64 { }",
    }
    for name, source in rejected.items():
        result = c.assembly(source)
        assert isinstance(result, c.AssemblyResult) and result.source is None, (name, result)
        assert result.report.source == source and result.report.diagnostics
        outcomes[name] = [d.message for d in result.report.diagnostics]

    damaged = "# ö retains byte positions\nfn broken { unknown }\nfn intact -> i64 { 42 }\nintact drop\n"
    snapshot = c.analyze(damaged)
    assert isinstance(snapshot, c.AnalysisSnapshot)
    intact = len(damaged[:damaged.rfind("intact")].encode())
    assert c.hover(snapshot, intact).availability == "known"
    assert c.definition(snapshot, intact).availability == "known"
    broken = len(damaged[:damaged.index("unknown")].encode())
    assert c.hover(snapshot, broken).availability == "unavailable"
    assert c.hover(snapshot, len(damaged.encode()) + 1).availability == "unavailable"
    assert not any(isinstance(value, (c.Recipe, c.Declaration, c.Group, c.Value)) for value in c.walk(snapshot))
    answer = c.definition(snapshot, intact)
    retained = weakref.ref(snapshot)
    del snapshot
    gc.collect()
    assert retained() is None and isinstance(answer.payload, c.Span)
    outcomes["editor"] = "verified unaffected definition, unavailable failed region, UTF-8 byte ranges, owned answer, no retained bodies"

    checker = c.Checker(c.syntax((HERE / "ownership.casa").read_text()))
    assert not checker.run().diagnostics
    program = c.commit(checker)
    identity = checker.names["identity"]
    instances = [key for key, _ in program.instances if key[0] == identity]
    assert instances == [(identity, ("Resource",))], instances
    assert len(checker.recipes) == len(checker.declarations)
    try:
        c.CheckedProgram(None, (), (), ())
    except c.InvariantError:
        pass
    else:
        raise AssertionError("unsealed product accepted")
    root = checker.recipes[0]
    checker.recipes[0] = replace(root, body=(c.Call(999, (), (), ()),))
    try:
        c.commit(checker)
    except c.InvariantError:
        pass
    else:
        raise AssertionError("invalid call reference accepted")
    checker.recipes[0] = root
    called = checker.names["read"]
    checker.recipes[0] = replace(root, body=(c.Call(called, (), (), (c.Value(999, "i64"),)),))
    try:
        c.commit(checker)
    except c.InvariantError:
        pass
    else:
        raise AssertionError("wrong call arity accepted")
    checker.recipes[0] = root
    del checker.recipes[identity]
    try:
        c.commit(checker)
    except c.InvariantError:
        pass
    else:
        raise AssertionError("unfinished recipe accepted")
    outcomes["commit"] = "one Resource specialization, invalid seal/reference/arity/unfinished recipe rejected"

    good = c.assembly("1 print").source
    assert c.build(good, directory / "missing", driver=str(directory / "missing-driver")).stage == "launch"
    assert c.build(good, directory / "failed", driver="/usr/bin/false").stage == "build"
    assert c.build(good, directory / "absent" / "failed", keep_asm=True).stage == "write"
    assert c.build(c.AssemblySource("invalid assembly\n"), directory / "invalid").stage == "build"
    with open("/dev/full", "wb") as full:
        result = subprocess.run([directory / "common-candidate"], stdout=full, stderr=subprocess.PIPE)
        assert result.returncode != 0
    overflow = c.assembly("9223372036854775807 1 + print")
    assert c.build(overflow.source, directory / "overflow") is None
    result = subprocess.run([directory / "overflow"], capture_output=True, text=True)
    assert result.returncode == 1 and "integer arithmetic failed" in result.stderr
    recursion = c.assembly("fn recurse { recurse } recurse")
    assert recursion.source and c.build(recursion.source, directory / "return-stack") is None
    result = subprocess.run([directory / "return-stack"], capture_output=True, text=True)
    assert result.returncode == 1 and "return stack overflow" in result.stderr
    outcomes["failures"] = "native launch, native nonzero, write, assembler diagnostics, output failure, checked integer overflow, recursive return-stack overflow"

    package = directory / "package"
    package.mkdir(exist_ok=True)
    for filename in ("capsule.py", "runtime.py"):
        shutil.copyfile(HERE / filename, package / filename)
    (package / "__main__.py").write_text("from capsule import main\nraise SystemExit(main())\n")
    installed = directory / "installed"
    installed.mkdir(exist_ok=True)
    executable = installed / "capsule"
    zipapp.create_archive(package, executable, interpreter="/usr/bin/env python3", compressed=True)
    executable.chmod(0o755)
    shutil.rmtree(package)
    shutil.copyfile(HERE / "common.casa", installed / "input.casa")
    command([executable, "input.casa", "--keep-asm", "-o", "output"], cwd=installed)
    assert command([installed / "output"]) == EXPECTED["common"]
    assert c.RUNTIME in (installed / "output.s").read_text()
    outcomes["distribution"] = "one Python executable archive, outside checkout, no external runtime asset, full assembly retained"
    return outcomes, executable


def snapshots(source):
    # Keep one old snapshot while its replacement is constructed, like an LSP.
    for _ in range(150):
        c.analyze(source)
    samples = [0] * 6
    product_bytes = [0] * 6
    tracemalloc.start()
    gc.collect()
    current = c.analyze(source)
    first_bytes = tracemalloc.get_traced_memory()[0]
    for request in range(120):
        replacement = c.analyze(source + f"\n# edit {request:03d}\n")
        c.hover(replacement, source.encode().find(b"add_one"))
        current = replacement
        if request % 20 == 19:
            gc.collect()
            samples[request // 20] = tracemalloc.get_traced_memory()[0]
            objects = {id(value): value for value in c.walk(current)}
            product_bytes[request // 20] = sum(
                sys.getsizeof(value) + (sys.getsizeof(vars(value)) if c.is_dataclass(value) else 0)
                for value in objects.values())
            del objects
    current_bytes, peak_bytes = tracemalloc.get_traced_memory()
    del current, replacement
    gc.collect()
    released_bytes = tracemalloc.get_traced_memory()[0]
    tracemalloc.stop()
    durations = []
    for request in range(120):
        start = time.perf_counter_ns()
        current = c.analyze(source + f"\n# edit {request:03d}\n")
        c.hover(current, source.encode().find(b"add_one"))
        durations.append((time.perf_counter_ns() - start) / 1e6)
    return {"requests": 120, "first_snapshot_bytes": first_bytes, "retained_every_20": samples,
            "reachable_product_bytes_every_20": product_bytes,
            "retained_final_bytes": current_bytes, "peak_bytes": peak_bytes,
            "after_release_bytes": released_bytes, "median_ms": statistics.median(durations),
            "range_ms": [min(durations), max(durations)]}


def profile(source):
    stages = []
    started = time.perf_counter_ns()
    checker = c.Checker(c.syntax(source))
    stages.append({"name": "Source", "milliseconds": (time.perf_counter_ns() - started) / 1e6,
                   "state": {"tokens_including_trivia": len(checker.parsed.tokens),
                             "function_declarations": len(checker.declarations), "sources": 1}})
    started = time.perf_counter_ns()
    report = checker.run()
    assert not report.diagnostics
    stages.append({"name": "Checking", "milliseconds": (time.perf_counter_ns() - started) / 1e6,
                   "state": {"checked_recipes": len(checker.recipes), "editor_facts": len(checker.facts),
                             "diagnostics": len(report.diagnostics)}})
    started = time.perf_counter_ns()
    program = c.commit(checker)
    stages.append({"name": "Concrete program", "milliseconds": (time.perf_counter_ns() - started) / 1e6,
                   "state": {"concrete_instances": len(program.instances),
                             "ordinary_pointer_shapes": sum(not s.external for s in program.shapes)}})
    del checker
    started = time.perf_counter_ns()
    assembly = c.emit(program)
    stages.append({"name": "Target and rendering", "milliseconds": (time.perf_counter_ns() - started) / 1e6,
                   "state": {"target": assembly.target, "assembly_bytes": len(assembly.text.encode()),
                             "embedded_runtime_bytes": len(c.RUNTIME.encode())}})
    return stages


def write_explorer(report, path):
    payload = json.dumps(report).replace("<", "\\u003c")
    page = '''<!doctype html><html lang="en"><meta charset="utf-8">
<meta name="viewport" content="width=device-width,initial-scale=1">
<title>Compiler Capsule evidence</title>
<style>body{max-width:960px;margin:3rem auto;padding:0 1rem;font:17px/1.6 system-ui;color:#172b38;background:#f8fafb}
h1{line-height:1.2}button,select{font:inherit;padding:.5rem .8rem;margin:.3rem;border:1px solid #a6b4bb;border-radius:5px;background:white;cursor:pointer}
button[aria-pressed=true]{background:#164b68;color:white}section{padding:1rem 1.5rem;background:white;border:1px solid #dbe3e8;border-radius:8px;margin:1rem 0}
dt{font-weight:600}dd{margin:0 0 .5rem}table{width:100%;border-collapse:collapse}th,td{text-align:left;border-bottom:1px solid #ddd;padding:.4rem}small{color:#536672}</style>
<h1>Compiler Capsule evidence</h1>
<p>Can one compiler request own checking and emission while editor queries retain only source facts?</p>
<p>These are captured native executions and measured compiler runs. The page does not compile code or simulate a successful result.</p>
<label>Workload <select id="workload"></select></label><div id="steps"></div>
<button id="previous">Previous stage</button><button id="next">Next stage</button>
<section><h2 id="stage"></h2><dl id="state"></dl></section>
<section><h2>Native execution</h2><p id="native"></p><small>Control and candidate must produce identical output before measurement.</small></section>
<section><h2>Paired compilation</h2><table><thead><tr><th>Compiler</th><th>Median seconds</th><th>Observed range</th><th>Peak RSS KiB</th></tr></thead><tbody id="measurements"></tbody></table>
<p>One warm-up and three alternating measured pairs. Candidate figures include Python and executable-archive startup. These are not production performance forecasts.</p></section>
<section><h2>Editor lifetime</h2><p id="memory"></p><p>Snapshot-reachable bytes and traced process allocations are different measurements. Full workspace behavior remains unimplemented.</p></section>
<section><h2>Acceptance remains open</h2><p>The blueprint needs maintainer acceptance and explicit future time and peak-RSS tolerances. This slice cannot establish self-hosting, fixed point, complete ownership, full ABI coverage or full-workspace tooling.</p></section>
<script>const evidence=PAYLOAD;
const names=Object.keys(evidence.profiles);let state={workload:names[0],stage:0};
function transition(current,action){return action.workload?{workload:action.workload,stage:0}:{...current,stage:Math.max(0,Math.min(3,action.stage))};}
function update(action){state=transition(state,action);render();}
const selector=document.getElementById('workload');for(const name of names){const option=document.createElement('option');option.textContent=name;selector.append(option);}selector.onchange=()=>update({workload:selector.value});
document.getElementById('previous').onclick=()=>update({stage:state.stage-1});document.getElementById('next').onclick=()=>update({stage:state.stage+1});
function render(){const stages=evidence.profiles[state.workload],stage=stages[state.stage];
const steps=document.getElementById('steps');steps.replaceChildren();stages.forEach((value,index)=>{const button=document.createElement('button');button.textContent=value.name;button.setAttribute('aria-pressed',index===state.stage);button.onclick=()=>update({stage:index});steps.append(button);});
document.getElementById('stage').textContent=stage.name+' ('+stage.milliseconds.toFixed(3)+' ms, one instrumented run)';
const detail=document.getElementById('state');detail.replaceChildren();for(const [key,value] of Object.entries(stage.state)){const term=document.createElement('dt'),definition=document.createElement('dd');term.textContent=key.replaceAll('_',' ');definition.textContent=value;detail.append(term,definition);}
document.getElementById('native').textContent='Verified stdout: '+evidence.checks[state.workload].stdout;
const body=document.getElementById('measurements');body.replaceChildren();for(const [name,summary] of Object.entries(evidence.paired[state.workload].summary)){const row=document.createElement('tr');for(const value of [name,summary.seconds.median.toFixed(4),summary.seconds.range.map(v=>v.toFixed(4)).join(' to '),summary.peak_rss_kib.median]){const cell=document.createElement('td');cell.textContent=value;row.append(cell);}body.append(row);}
const memory=evidence.snapshots;document.getElementById('memory').textContent=memory.requests+' requests. Median analysis plus query '+memory.median_ms.toFixed(3)+' ms. Snapshot-reachable samples: '+memory.reachable_product_bytes_every_20.join(', ')+' bytes. Traced retained samples: '+memory.retained_every_20.join(', ')+' bytes.';}
render();</script></html>'''
    path.write_text(page.replace("PAYLOAD", payload))


def timed(arguments, sample_file, environment):
    start = time.perf_counter_ns()
    command(["/usr/bin/time", "-f", "%M", "-o", sample_file, *arguments], env=environment)
    elapsed = (time.perf_counter_ns() - start) / 1e9
    return {"seconds": elapsed, "peak_rss_kib": int(sample_file.read_text().strip())}


def measurements(directory, control, candidate):
    environment = dict(os.environ, LIBRARY_PATH=str(directory))
    results = {}
    workloads = [HERE / (name + ".casa") for name in EXPECTED]
    heavy = directory / "generic-heavy.casa"
    heavy.write_text("fn identity[T] value:$T -> $T { value }\nfn inspect value:$i64 -> i64 { value copy }\nfn run {\n0 = value\n" +
                     "value identity inspect drop\n" * 300 + "}\nrun\n")
    workloads.append(heavy)
    for source in workloads:
        output = directory / "paired-output"
        suffix = [str(source), "--keep-asm", "-o", str(output)]
        if source.stem == "native":
            suffix += ["-l", "capsule651"]
        commands = {"control": [str(control), *suffix], "candidate": [str(candidate), *suffix]}
        for arguments in commands.values():
            command(arguments, env=environment)
        samples = {name: [] for name in commands}
        sizes = {}
        for pair in range(3):
            for name in ("control", "candidate"):
                samples[name].append(timed(commands[name], directory / "rss.txt", environment))
                sizes[name] = {"executable_bytes": output.stat().st_size,
                               "assembly_bytes": Path(str(output) + ".s").stat().st_size}
        summary = {}
        for name, values in samples.items():
            summary[name] = {measure: {"median": statistics.median(v[measure] for v in values),
                                      "range": [min(v[measure] for v in values), max(v[measure] for v in values)]}
                             for measure in ("seconds", "peak_rss_kib")}
        results[source.stem] = {"sha256": digest(source), "commands": commands,
                               "samples": samples, "summary": summary, "sizes": sizes}
    return results


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--control", type=Path, required=True)
    parser.add_argument("--output", type=Path, default=Path("/tmp/casa-651-evidence"))
    args = parser.parse_args()
    directory = args.output.resolve()
    directory.mkdir(parents=True, exist_ok=True)
    control = args.control.resolve()
    outcomes, candidate = checks(directory, control)
    report = {"source_commit": command(["git", "rev-parse", "HEAD"]).strip(),
              "control_sha256": digest(control), "candidate_sha256": digest(candidate),
              "python": sys.version, "platform": platform.platform(),
              "cc": command(["cc", "--version"]).splitlines()[0],
              "source_sha256": {p.name: digest(p) for p in HERE.iterdir() if p.suffix in (".py", ".casa", ".c")},
              "checks": outcomes, "snapshots": snapshots((HERE / "common.casa").read_text()),
              "profiles": {name: profile((HERE / (name + ".casa")).read_text()) for name in EXPECTED},
              "paired": measurements(directory, control, candidate)}
    (directory / "evidence.json").write_text(json.dumps(report, indent=2) + "\n")
    write_explorer(report, directory / "evidence.html")
    print(json.dumps({"checks": list(outcomes), "snapshots": report["snapshots"],
                      "paired": {name: entry["summary"] for name, entry in report["paired"].items()},
                      "report": str(directory / "evidence.json")}, indent=2))


if __name__ == "__main__":
    main()
