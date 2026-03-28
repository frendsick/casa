#!/usr/bin/env sh
set -eu

ROOT_DIR=$(git rev-parse --show-toplevel)

RED='\033[0;31m'
GREEN='\033[0;32m'
RESET='\033[0m'

pass=0
fail=0

COMPILER="/tmp/casa_self_hosted_compiler"

# Compile the self-hosted compiler
python3 "$ROOT_DIR/casa.py" "$ROOT_DIR/self_hosted/casa.casa" -o "$COMPILER"

run_test() {
    name="$1"
    source="$2"
    expected="$3"
    binary="/tmp/casa_self_hosted_test_$name"

    echo "Running test: $name"
    "$COMPILER" "$source" -o "$binary" 2>&1
    output=$("$binary")
    rm -f "$binary"

    if [ "$output" = "$expected" ]; then
        echo "${GREEN}[OK]${RESET} Passed: $name"
        pass=$((pass+1))
    else
        echo "${RED}[X]${RESET}  Failed: $name"
        echo "  Expected: $expected"
        echo "  Got:      $output"
        fail=$((fail+1))
    fi
}

# Write test source files
tmp_add="/tmp/casa_sh_add.casa"
echo '34 35 + print' > "$tmp_add"
run_test "addition" "$tmp_add" "69"

tmp_neg="/tmp/casa_sh_neg.casa"
echo '-10 3 + print' > "$tmp_neg"
run_test "negative_literal" "$tmp_neg" "-7"

tmp_zero="/tmp/casa_sh_zero.casa"
echo '0 print' > "$tmp_zero"
run_test "zero" "$tmp_zero" "0"

tmp_single="/tmp/casa_sh_single.casa"
echo '42 print' > "$tmp_single"
run_test "single_int" "$tmp_single" "42"

tmp_chain="/tmp/casa_sh_chain.casa"
echo '1 2 + 3 + print' > "$tmp_chain"
run_test "chained_addition" "$tmp_chain" "6"

# Clean up
rm -f "$COMPILER" "$tmp_add" "$tmp_neg" "$tmp_zero" "$tmp_single" "$tmp_chain"

echo
echo "Summary: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
