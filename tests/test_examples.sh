#!/usr/bin/env sh
set -eu

. "$(dirname "$0")/test-lib.sh"

ROOT_DIR=$(cd "$(dirname "$0")/.." && pwd)

select_tool "${CASA_COMPILER:-}" "$ROOT_DIR/casac" "${1:-}"
COMPILER=$TEST_TOOL
if [ "$TEST_TOOL_ARG" = true ]; then
    shift
fi

# Run from repo root so error messages use relative paths matching the
# checked-in .err fixtures.
cd "$ROOT_DIR"
EXAMPLES_DIR="examples"

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
RESET='\033[0m'

pass=0
fail=0
matched=false

for f in "$EXAMPLES_DIR"/*.casa; do
    base=$(basename "$f" .casa)

    if ! matches_filter "$base" "$@"; then
        continue
    fi
    matched=true

    out_file="$EXAMPLES_DIR/outputs/$base.out"
    err_file="$EXAMPLES_DIR/outputs/$base.err"
    binary="/tmp/casa_test_$base"

    echo "Running test: $base"

    # Examples with .err files are expected to fail compilation
    if [ -f "$err_file" ]; then
        error_output=$("$COMPILER" -L "$ROOT_DIR/lib" "$f" -o "$binary" 2>&1 || true)
        if echo "$error_output" | diff -u - "$err_file"; then
            echo "${GREEN}[OK]${RESET} Passed: $base (expected error)"
            pass=$((pass+1))
        else
            echo "${RED}[X]${RESET}  Failed: $base (error output mismatch)"
            fail=$((fail+1))
        fi
        rm -f "$binary"
        continue
    fi

    # Compile
    if [ "$base" = foreign_function ]; then
        "$COMPILER" -L "$ROOT_DIR/lib" -l c "$f" -o "$binary"
    elif [ "$base" = raylib ]; then
        raylib_object="/tmp/casa_test_raylib_$$.o"
        raylib_library_name="casa_raylib_fixture_$$"
        raylib_library="/tmp/lib$raylib_library_name.a"
        cc -std=c11 -Wall -Wextra -Werror \
            -c "$ROOT_DIR/tests/examples/raylib.c" -o "$raylib_object"
        ar rcs "$raylib_library" "$raylib_object"
        LIBRARY_PATH="/tmp${LIBRARY_PATH:+:$LIBRARY_PATH}" \
            "$COMPILER" -L "$ROOT_DIR/lib" -l "$raylib_library_name" -l c \
            "$f" -o "$binary"
    else
        "$COMPILER" -L "$ROOT_DIR/lib" "$f" -o "$binary"
    fi

    # Run and capture output (1s timeout for interactive examples)
    output=$(timeout 1 "$binary") || true

    # Clean up binary
    rm -f "$binary"
    if [ "$base" = raylib ]; then
        rm -f "$raylib_object" "$raylib_library"
    fi

    if [ -f "$out_file" ]; then
        if echo "$output" | diff -u - "$out_file"; then
            echo "${GREEN}[OK]${RESET} Passed: $base"
            pass=$((pass+1))
        else
            echo "${RED}[X]${RESET}  Failed: $base"
            fail=$((fail+1))
        fi
    else
        echo "${YELLOW}[!]${RESET}  Missing expected output: $base"
        echo "$output" > "$out_file"
        echo "${YELLOW}[+]${RESET}  Generated $out_file"
    fi
done

report_no_matches "$matched" "$@"
echo
echo "Summary: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
