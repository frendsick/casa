#!/usr/bin/env sh
set -eu

. "$(dirname "$0")/test-lib.sh"

ROOT_DIR=$(cd "$(dirname "$0")/.." && pwd)
cd "$ROOT_DIR"

CLI_TMP=$(mktemp -d "${TMPDIR:-/tmp}/casa_cli_tests.XXXXXX")
trap 'rm -rf "$CLI_TMP"' EXIT

select_tool "${CASA_COMPILER:-}" "$ROOT_DIR/casac" "${1:-}"
COMPILER=$TEST_TOOL
if [ "$TEST_TOOL_ARG" = true ]; then
    shift
fi

version=$(sed -n 's/^pub const CASAC_VERSION "\([^"]*\)"/\1/p' casa.casa)
expected="casac v$version"
matched=false

if matches_filter version "$@"; then
    matched=true
    [ "$($COMPILER --version /does/not/exist.casa)" = "$expected" ]
    if $COMPILER --version --unknown >/tmp/casa_cli_version_out 2>&1; then
        echo "version accepted an unknown argument" >&2
        exit 1
    fi
    grep -q 'unrecognized arguments: --unknown' /tmp/casa_cli_version_out
fi

if matches_filter verbose "$@"; then
    matched=true
    for flag in -v --verbose; do
        "$COMPILER" "$flag" examples/hello_world.casa -o "$CLI_TMP/hello_world" \
            >"$CLI_TMP/stdout" 2>"$CLI_TMP/progress"
        [ ! -s "$CLI_TMP/stdout" ]
        stages=$(sed 's/^\[INFO\] [0-9][0-9.]*s //' "$CLI_TMP/progress")
        [ "$stages" = "Reading examples/hello_world.casa
Lexing root source
Parsing and resolving imports
Checking extern declarations
Checking root types and ownership
Checking function types and ownership
Validating declarations
Specializing reachable functions
Preparing checked program
Planning and emitting assembly
Assembling and linking $CLI_TMP/hello_world
Finished $CLI_TMP/hello_world" ]
        [ "$("$CLI_TMP/hello_world")" = 'Hello world!' ]
    done
    output=$("$COMPILER" examples/hello_world.casa -o "$CLI_TMP/quiet" 2>&1)
    [ -z "$output" ]
    printf '"bad" 1 *\n' >"$CLI_TMP/invalid.casa"
    if "$COMPILER" --verbose "$CLI_TMP/invalid.casa" -o "$CLI_TMP/invalid" \
        >"$CLI_TMP/stdout" 2>"$CLI_TMP/rejected"; then
        echo "verbose compilation accepted invalid source" >&2
        exit 1
    fi
    grep -q 'Checking root types and ownership' "$CLI_TMP/rejected"
    if grep -Eq 'Specializing|Preparing checked program|Planning and emitting|Assembling and linking|Finished' "$CLI_TMP/rejected"; then
        echo "verbose compilation reported stages after source rejection" >&2
        exit 1
    fi
fi

if matches_filter missing_import "$@"; then
    matched=true
    if "$COMPILER" casa.casa -o "$CLI_TMP/missing_import" >"$CLI_TMP/import.out" 2>&1; then
        echo "compilation accepted a missing library search path" >&2
        exit 1
    fi
    grep -q 'module `std` not found, searched:' "$CLI_TMP/import.out"
    grep -q 'compiler/common.casa:2:' "$CLI_TMP/import.out"
    if grep -q 'called unwrap on error' "$CLI_TMP/import.out"; then
        echo "missing import caused an unwrap failure" >&2
        exit 1
    fi
    [ ! -e "$CLI_TMP/missing_import" ]
fi

if matches_filter stdin_buffered_input "$@"; then
    matched=true
    "$COMPILER" -L lib tests/compiler/fixtures/stdin_buffered_input.casa -o "$CLI_TMP/stdin_buffered_input"
    printf 'ab' >"$CLI_TMP/stdin_bytes"
    "$CLI_TMP/stdin_buffered_input" <"$CLI_TMP/stdin_bytes"
fi

if matches_filter process_exit "$@"; then
    matched=true
    process_exit_binary="$CLI_TMP/process_exit"
    "$COMPILER" -L lib tests/compiler/fixtures/process_exit.casa -o "$process_exit_binary"
    set +e
    "$process_exit_binary"
    actual_status=$?
    set -e
    if [ "$actual_status" -ne 7 ]; then
        echo "std::process::exit returned status $actual_status, expected 7" >&2
        exit 1
    fi
fi

if matches_filter unrelated_fault "$@"; then
    matched=true
    # The return stack guard handler must keep the default action for other faults.
    printf 'unsafe { 1 ptr::null store8 }\n' >"$CLI_TMP/unrelated_fault.casa"
    "$COMPILER" "$CLI_TMP/unrelated_fault.casa" -o "$CLI_TMP/unrelated_fault"
    set +e
    "$CLI_TMP/unrelated_fault" 2>"$CLI_TMP/unrelated_fault.err"
    actual_status=$?
    set -e
    if [ "$actual_status" -ne 139 ]; then
        echo "null write exited with status $actual_status, expected SIGSEGV" >&2
        exit 1
    fi
    if grep -q 'return stack overflow' "$CLI_TMP/unrelated_fault.err"; then
        echo "null write was reported as return stack overflow" >&2
        exit 1
    fi
fi

if matches_filter sealed_scalar "$@"; then
    matched=true
    scalar_binary="$CLI_TMP/sealed_scalar"
    "$COMPILER" tests/compiler/fixtures/sealed_scalar.casa -o "$scalar_binary" --keep-asm
    [ "$("$scalar_binary")" = "120:9:7:5:15:14:13:-1:true:true:0:true
1:0:-128:41:0:-1:0" ]
    grep -q 'call fn_factorial' "$scalar_binary.s"
    grep -q 'return_stack_overflow:' "$scalar_binary.s"
    grep -q '.cfi_startproc' "$scalar_binary.s"
fi

if matches_filter extern_target "$@"; then
    matched=true
    cat >"$CLI_TMP/unsupported.casa" <<'CASA'
extern struct Huge { values:array[u64 2305843009213693952] }
extern fn make -> Huge
unsafe { make drop }
CASA
    if "$COMPILER" "$CLI_TMP/unsupported.casa" -o "$CLI_TMP/unsupported" --keep-asm >"$CLI_TMP/target.out" 2>&1; then
        echo "assembly accepted an unsupported extern layout" >&2
        exit 1
    fi
    grep -q 'Extern return type `Huge` has no supported Linux x86-64 ABI layout' "$CLI_TMP/target.out"
    grep -q 'unsupported.casa:2:' "$CLI_TMP/target.out"
    if grep -q 'internal compiler error' "$CLI_TMP/target.out"; then
        echo "target rejection was reported as an internal failure" >&2
        exit 1
    fi
    [ ! -e "$CLI_TMP/unsupported.s" ]
    [ ! -e "$CLI_TMP/unsupported" ]
fi

if matches_filter installed_native "$@"; then
    matched=true
    cp "$COMPILER" "$CLI_TMP/casac"
    cp tests/compiler/fixtures/installed_native.casa "$CLI_TMP/program.casa"
    (
        cd "$CLI_TMP"
        ./casac program.casa -o program -l m --keep-asm
        [ "$(./program)" = "true:42" ]
        [ ! -e program.o ]
        grep -q 'heap_alloc:' program.s
        grep -q 'write_all:' program.s
        grep -q 'return_stack_overflow:' program.s
        grep -q 'call sqrt' program.s
        /usr/bin/cc -nostdlib -no-pie -Wl,-e,_start -Wl,-z,noexecstack \
            -o rebuilt program.s -l m
        [ "$(./rebuilt)" = "true:42" ]
        ./casac program.casa -o clean -l m
        [ ! -e clean.s ]
        [ ! -e clean.o ]
        if ./casac program.casa -o missing/output -l m >write.out 2>&1; then
            echo "native build accepted an unwritable assembly path" >&2
            exit 1
        fi
        grep -q 'error: cannot write missing/output.s: not found' write.out
        printf 'existing source\n' >readonly.s
        chmod a-w readonly.s
        # Root can write read-only files. Run this check only when access is denied.
        if [ ! -w readonly.s ]; then
            if ./casac program.casa -o readonly -l m --keep-asm >readonly.out 2>&1; then
                echo "native build accepted a read-only assembly file" >&2
                exit 1
            fi
            grep -q 'error: cannot write readonly.s: permission denied' readonly.out
            [ "$(cat readonly.s)" = "existing source" ]
        fi
        chmod u+w readonly.s
        if ./casac program.casa -o failed -l casa_missing_native_library >link.out 2>&1; then
            echo "native build accepted a missing library" >&2
            exit 1
        fi
        grep -q 'casa_missing_native_library' link.out
        grep -q 'error: native build failed with exit code' link.out
        [ ! -e failed.s ]
        [ ! -e failed.o ]
    )
fi

if matches_filter default_output "$@"; then
    matched=true
    compiler_path=$(cd "$(dirname "$COMPILER")" && pwd)/$(basename "$COMPILER")
    mkdir -p "$CLI_TMP/default_output/dir.v2" "$CLI_TMP/default_output/.hidden/nested"
    (
        cd "$CLI_TMP/default_output"
        for source in one.casa dir.v2/two.casa .hidden/nested/three.casa; do
            printf 'import "std"\n"ok" std::println\n' >"$source"
        done
        for input in ./one.casa ../default_output/one.casa dir.v2/two.casa \
            .hidden/nested/three.casa "$PWD/one.casa"; do
            "$compiler_path" -L "$ROOT_DIR/lib" "$input"
            # The binary replaces the extension and the source stays text.
            [ "$("${input%.casa}")" = ok ]
            grep -q '^import "std"$' "$input"
        done
        [ ! -e dir ]
    )
fi

if matches_filter default_output_rejected "$@"; then
    matched=true
    compiler_path=$(cd "$(dirname "$COMPILER")" && pwd)/$(basename "$COMPILER")
    mkdir -p "$CLI_TMP/default_output_rejected/dir.v2"
    (
        cd "$CLI_TMP/default_output_rejected"
        for source in script .script dir.v2/script dir.v2/.script trailing.; do
            printf 'import "std"\n"ok" std::println\n' >"$source"
            set +e
            "$compiler_path" -L "$ROOT_DIR/lib" "$source" >reject.out 2>&1
            actual_status=$?
            set -e
            [ "$actual_status" -eq 1 ]
            grep -q "error: cannot derive an output path from $source, pass -o" reject.out
            # The source stays text.
            grep -q '^import "std"$' "$source"
        done
        "$compiler_path" -L "$ROOT_DIR/lib" script -o script_out
        [ "$(./script_out)" = ok ]
    )
fi

if matches_filter run_absolute "$@"; then
    matched=true
    compiler_path=$(cd "$(dirname "$COMPILER")" && pwd)/$(basename "$COMPILER")
    printf 'import "std"\n"ran" std::println\n3 std::process::exit\n' >"$CLI_TMP/run_absolute.casa"
    for args in "$CLI_TMP/run_absolute.casa" "$CLI_TMP/run_absolute.casa -o $CLI_TMP/run_absolute_out"; do
        set +e
        # shellcheck disable=SC2086
        "$COMPILER" -L lib -r $args >"$CLI_TMP/run.out" 2>&1
        actual_status=$?
        set -e
        [ "$actual_status" -eq 3 ]
        [ "$(cat "$CLI_TMP/run.out")" = ran ]
    done
    (
        cd "$CLI_TMP"
        set +e
        "$compiler_path" -L "$ROOT_DIR/lib" -r run_absolute.casa -o run_relative >run.out 2>&1
        actual_status=$?
        set -e
        [ "$actual_status" -eq 3 ]
        [ "$(cat run.out)" = ran ]
    )
fi

if matches_filter run_failures "$@"; then
    matched=true
    # A signal exits with 128 plus the signal number, as a shell reports it.
    printf 'import "std"\nunsafe { 1 ptr::null store8 }\n' >"$CLI_TMP/run_signal.casa"
    set +e
    "$COMPILER" -L lib -r "$CLI_TMP/run_signal.casa" >"$CLI_TMP/run.out" 2>&1
    actual_status=$?
    set -e
    [ "$actual_status" -eq 139 ]
    # An execve failure names the path and errno instead of exiting silently.
    printf 'import "std"\nstd::List[std::Bytes]::new = command\n"/nonexistent/program" command.push_str\ncommand std::run_command = status\nstatus u8::wrapping_from std::process::exit\n' >"$CLI_TMP/run_exec.casa"
    set +e
    "$COMPILER" -L lib -r "$CLI_TMP/run_exec.casa" >"$CLI_TMP/run.out" 2>&1
    actual_status=$?
    set -e
    [ "$actual_status" -eq 127 ]
    grep -q '^error: cannot execute /nonexistent/program: errno 2$' "$CLI_TMP/run.out"
fi

if matches_filter symlink_imports "$@"; then
    matched=true
    python3 tests/test_symlink_imports.py "$COMPILER"
fi

if matches_filter source_traceability "$@"; then
    matched=true
    python3 tests/test_source_traceability.py "$COMPILER"
fi

if matches_filter native_assembly "$@"; then
    matched=true
    python3 tests/test_native_assembly.py "$COMPILER"
fi

if matches_filter unsigned_division "$@"; then
    matched=true
    python3 tests/test_unsigned_division.py "$COMPILER"
fi

if matches_filter comparison_branches "$@"; then
    matched=true
    python3 tests/test_comparison_branches.py "$COMPILER"
fi

if matches_filter lsp_workspace "$@"; then
    matched=true
    "$COMPILER" -L lib lsp.casa -o "$CLI_TMP/lsp"
    python3 tests/test_lsp_workspace.py "$CLI_TMP/lsp"
fi

report_no_matches "$matched" "$@"
echo "CLI tests passed"
