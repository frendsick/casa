"""Source from GDB to count body-analysis contexts in the pinned compiler.

Offsets follow the pinned generated getters and List/Map entry layout. This
records concrete runtime inputs, not a proposed semantic cache identity.
"""
from collections import Counter
import json
import os
from pathlib import Path
import struct
import time
import gdb

contexts = Counter()
events = []
errors = []
started = time.clock_gettime_ns(time.CLOCK_MONOTONIC_RAW)


def word(address):
    return struct.unpack("<Q", gdb.selected_inferior().read_memory(address, 8))[0]


def text(address):
    length = word(address)
    if length > 10000:
        raise ValueError(f"Invalid text length: {length}")
    return bytes(gdb.selected_inferior().read_memory(address + 8, length)).decode()


def entries(address):
    buckets, size, capacity = (word(address + offset) for offset in (0, 8, 16))
    if capacity > 100000 or size > 100000:
        raise ValueError("Invalid map header")
    found = []
    for index in range(capacity):
        entry = word(buckets + index * 8)
        while entry:
            found.append(entry)
            if len(found) > size:
                raise ValueError("Map chain exceeds size")
            entry = word(entry + 16)
    if len(found) != size:
        raise ValueError("Map size differs from entry count")
    return found


def string_set(address):
    return tuple(sorted(text(word(entry)) for entry in entries(word(address))))


class Body(gdb.Breakpoint):
    def stop(self):
        try:
            stack = int(gdb.parse_and_eval("$rsp"))
            if self.location.endswith("__analyze_ops"):
                optional = word(stack + 8)
                tag = word(optional)
                if tag not in (0, 1):
                    raise ValueError("Invalid Option tag")
                function = word(optional + 8) if tag else 0
                argument_offset = 40
                kind = "body"
            else:
                function = word(stack)
                argument_offset = 16
                kind = "summary_or_scheduled"
            name = text(word(word(function))) if function else "<root>"
            bindings = tuple(sorted(
                (text(word(entry)), string_set(entry + 8))
                for entry in entries(word(stack + argument_offset))))
            active = string_set(word(stack + argument_offset + 8))
            clone_body = word(stack + argument_offset + 16)
            infer_parameters = word(stack + argument_offset + 24)
            if clone_body not in (0, 1) or infer_parameters not in (0, 1):
                raise ValueError("Invalid boolean argument")
            contexts[(kind, name, bindings, active, clone_body, infer_parameters)] += 1
        except Exception as error:
            errors.append(str(error))
            return True
        return False


class Phase(gdb.Breakpoint):
    def stop(self):
        events.append({"function": self.location,
                       "elapsed_raw_seconds": (time.clock_gettime_ns(time.CLOCK_MONOTONIC_RAW) - started) / 1e9})
        return False


Body("fn___casa_module_15__analyze_function_semantics_in_store", internal=True)
Body("fn___casa_module_15__analyze_ops", internal=True)
for function in ["fn___casa_module_7__parse_and_resolve", "fn___casa_module_8__type_check",
                 "fn___casa_module_8__schedule_functions", "fn___casa_module_15__check_trait_impls",
                 "fn___casa_module_15__validate_generic_cycles",
                 "fn___casa_module_15__monomorphize_checked_generics",
                 "fn___casa_module_10__compile_typechecked", "fn___casa_module_11__emit",
                 "fn___casa_module_12__compile_binary"]:
    Phase(function, internal=True)
gdb.execute("run")
try:
    exit_code = int(gdb.parse_and_eval("$_exitcode"))
except (gdb.error, ValueError):
    exit_code = None
if exit_code != 0:
    errors.append(f"Compiler did not exit successfully: {exit_code}")
result = {"events": events, "errors": errors, "exit_code": exit_code,
          "contexts": [{"kind": key[0], "name": key[1], "callable_bindings": key[2], "active_calls": key[3],
                        "clone_body": key[4], "infer_parameters": key[5], "calls": count}
                       for key, count in contexts.most_common()]}
Path(os.environ["CASA_PROFILE_OUTPUT"]).write_text(json.dumps(result, indent=2) + "\n")
if errors:
    raise RuntimeError(errors)
