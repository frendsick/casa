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
EXAMPLES_TEST_TMP=$(mktemp -d "${TMPDIR:-/tmp}/casa_examples.XXXXXX")
trap 'rm -rf "$EXAMPLES_TEST_TMP"' EXIT

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
    binary="$EXAMPLES_TEST_TMP/$base"

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
    elif [ "$base" = game_of_life ]; then
        raylib_object="$EXAMPLES_TEST_TMP/raylib.o"
        raylib_library_name="casa_raylib_fixture"
        raylib_library="$EXAMPLES_TEST_TMP/lib$raylib_library_name.a"
        cc -std=c11 -Wall -Wextra -Werror \
            -c "$ROOT_DIR/tests/examples/raylib.c" -o "$raylib_object"
        ar rcs "$raylib_library" "$raylib_object"
        LIBRARY_PATH="$EXAMPLES_TEST_TMP${LIBRARY_PATH:+:$LIBRARY_PATH}" \
            "$COMPILER" -L "$ROOT_DIR/lib" -l "$raylib_library_name" -l c \
            "$f" -o "$binary"
    else
        "$COMPILER" -L "$ROOT_DIR/lib" "$f" -o "$binary"
    fi

    # The raylib fixture must finish successfully, including its cleanup assertions.
    if [ "$base" = game_of_life ]; then
        if output=$(timeout 5 "$binary"); then
            :
        else
            echo "${RED}[X]${RESET}  Failed: $base (runtime failure)"
            fail=$((fail+1))
            continue
        fi
        for stage in window image texture resize_image resize_texture; do
            case "$stage" in
                window) expected="Could not open the window." ;;
                *) expected="Could not create or update the grid graphics.
raylib stub ok" ;;
            esac
            status=0
            failure_output=$(CASA_RAYLIB_FAILURE="$stage" timeout 5 "$binary") || status=$?
            if [ "$status" -eq 1 ] && [ "$failure_output" = "$expected" ]; then
                pass=$((pass+1))
                echo "${GREEN}[OK]${RESET} Passed: game_of_life ($stage failure cleanup)"
            else
                echo "${RED}[X]${RESET}  Failed: game_of_life ($stage failure cleanup)"
                fail=$((fail+1))
            fi
        done
    else
        # Interactive terminal examples can run until the timeout.
        output=$(timeout 1 "$binary") || true
    fi

    # Clean up binary
    rm -f "$binary"

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
