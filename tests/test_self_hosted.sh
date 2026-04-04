#!/usr/bin/env sh
set -eu

ROOT_DIR=$(git rev-parse --show-toplevel)
TESTS_DIR="$ROOT_DIR/tests/self_hosted"

RED='\033[0;31m'
GREEN='\033[0;32m'
RESET='\033[0m'

pass=0
fail=0

for f in "$TESTS_DIR"/test_*.casa; do
    base=$(basename "$f" .casa)
    binary="/tmp/casa_${base}"

    printf "Running: %s ... " "$base"

    if ! python3 "$ROOT_DIR/casa.py" "$f" -o "$binary" 2>/tmp/casa_compile_err; then
        printf "${RED}COMPILE FAIL${RESET}\n"
        cat /tmp/casa_compile_err
        fail=$((fail+1))
        rm -f "$binary"
        continue
    fi

    output=$("$binary" 2>&1) || {
        printf "${RED}RUNTIME FAIL${RESET}\n"
        echo "$output"
        fail=$((fail+1))
        rm -f "$binary"
        continue
    }

    printf "${GREEN}OK${RESET} %s\n" "$(echo "$output" | tail -1)"
    pass=$((pass+1))
    rm -f "$binary"
done

# Self-compilation test: stage1 (Python-compiled) compiles itself to stage2
printf "Running: self_compilation ... "

stage1="/tmp/casa_stage1"
stage2="/tmp/casa_stage2"
stage2_test_bin="/tmp/casa_stage2_test"

if ! python3 "$ROOT_DIR/casa.py" "$ROOT_DIR/self_hosted/casa.casa" -o "$stage1" 2>/tmp/casa_compile_err; then
    printf "${RED}STAGE1 COMPILE FAIL${RESET}\n"
    cat /tmp/casa_compile_err
    fail=$((fail+1))
else
    if ! "$stage1" "$ROOT_DIR/self_hosted/casa.casa" -o "$stage2" 2>/tmp/casa_compile_err; then
        printf "${RED}STAGE2 COMPILE FAIL${RESET}\n"
        cat /tmp/casa_compile_err
        fail=$((fail+1))
    else
        # Verify stage2 can compile and run a program
        if ! "$stage2" "$ROOT_DIR/examples/hello_world.casa" -o "$stage2_test_bin" 2>/tmp/casa_compile_err; then
            printf "${RED}STAGE2 COMPILE OUTPUT FAIL${RESET}\n"
            cat /tmp/casa_compile_err
            fail=$((fail+1))
        else
            stage2_output=$("$stage2_test_bin" 2>&1) || true
            expected="Hello world!"
            if [ "$stage2_output" = "$expected" ]; then
                printf "${GREEN}OK${RESET}\n"
                pass=$((pass+1))
            else
                printf "${RED}STAGE2 RUNTIME FAIL${RESET}\n"
                echo "  expected: $expected"
                echo "  got:      $stage2_output"
                fail=$((fail+1))
            fi
        fi
    fi
fi
rm -f "$stage1" "$stage2" "$stage2_test_bin"

echo
echo "Summary: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
