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

if matches_filter process_exit "$@"; then
    matched=true
    process_exit_binary="$CLI_TMP/process_exit"
    "$COMPILER" -L lib tests/compiler/fixtures/process_exit.casa -o "$process_exit_binary"
    set +e
    "$process_exit_binary"
    actual_status=$?
    set -e
    if [ "$actual_status" -ne 7 ]; then
        echo "process::exit returned status $actual_status, expected 7" >&2
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
    grep -q 'popq -8(%r14)' "$scalar_binary.s"
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
            if ./casac program.casa -o readonly -l m >readonly.out 2>&1; then
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

if matches_filter lsp_workspace "$@"; then
    matched=true
    "$COMPILER" -L lib lsp.casa -o "$CLI_TMP/lsp"
    python3 tests/test_lsp_workspace.py "$CLI_TMP/lsp"
fi

report_no_matches "$matched" "$@"
echo "CLI tests passed"
