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
    [ "$($COMPILER -v)" = "$expected" ]
    if $COMPILER --version --unknown >/tmp/casa_cli_version_out 2>&1; then
        echo "version accepted an unknown argument" >&2
        exit 1
    fi
    grep -q 'unrecognized arguments: --unknown' /tmp/casa_cli_version_out
fi

if matches_filter verbose "$@"; then
    matched=true
    output=$($COMPILER --verbose examples/hello_world.casa -o /tmp/casa_cli_test 2>&1)
    printf '%s\n' "$output" | grep -q 'Analyzing examples/hello_world.casa'
    rm -f /tmp/casa_cli_test
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

report_no_matches "$matched" "$@"
echo "CLI tests passed"
