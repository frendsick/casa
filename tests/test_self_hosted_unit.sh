#!/usr/bin/env sh
set -eu

ROOT_DIR=$(git rev-parse --show-toplevel)

RED='\033[0;31m'
GREEN='\033[0;32m'
RESET='\033[0m'

pass=0
fail=0

run_unit_test() {
    name="$1"
    source="$2"
    binary="/tmp/casa_unit_test_$name"

    printf "Compiling: %s... " "$name"
    if ! python3 "$ROOT_DIR/casa.py" "$source" -o "$binary" 2>&1; then
        printf "${RED}[X]${RESET}  Compile failed: %s\n" "$name"
        fail=$((fail+1))
        return
    fi

    set +e
    output=$("$binary" 2>&1)
    exit_code=$?
    set -e
    rm -f "$binary"

    if [ "$exit_code" -eq 0 ]; then
        printf "${GREEN}[OK]${RESET} %s - %s\n" "$name" "$output"
        pass=$((pass+1))
    else
        printf "${RED}[X]${RESET}  %s\n" "$name"
        printf "%s\n" "$output"
        fail=$((fail+1))
    fi
}

run_unit_test "lexer" "$ROOT_DIR/tests/self_hosted/test_lexer.casa"
run_unit_test "parser" "$ROOT_DIR/tests/self_hosted/test_parser.casa"
run_unit_test "bytecode" "$ROOT_DIR/tests/self_hosted/test_bytecode.casa"
run_unit_test "emitter" "$ROOT_DIR/tests/self_hosted/test_emitter.casa"

echo
echo "Summary: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
