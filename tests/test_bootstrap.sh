#!/usr/bin/env sh
set -eu

ROOT_DIR=$(cd "$(dirname "$0")/.." && pwd)
LIB_DIR="$ROOT_DIR/lib"
cd "$ROOT_DIR"

# Compiler: env var > positional path (backward compat) > default
if [ -n "${CASA_COMPILER:-}" ]; then
    COMPILER="$CASA_COMPILER"
elif [ $# -ge 1 ] && echo "$1" | grep -q '/'; then
    COMPILER="$1"
    shift
else
    COMPILER="$ROOT_DIR/casac"
fi

RED='\033[0;31m'
GREEN='\033[0;32m'
RESET='\033[0m'

pass=0
fail=0

# Self-compilation test: stage1 (released casac) compiles itself to stage2
printf "Running: self_compilation ... "

stage1="/tmp/casa_stage1"
stage2="/tmp/casa_stage2"
stage2_test_bin="/tmp/casa_stage2_test"

if ! $COMPILER -L "$LIB_DIR" "$ROOT_DIR/casa.casa" -o "$stage1" 2>/tmp/casa_compile_err; then
    printf "${RED}STAGE1 COMPILE FAIL${RESET}\n"
    cat /tmp/casa_compile_err
    fail=$((fail+1))
else
    if ! "$stage1" -L "$LIB_DIR" "$ROOT_DIR/casa.casa" -o "$stage2" 2>/tmp/casa_compile_err; then
        printf "${RED}STAGE2 COMPILE FAIL${RESET}\n"
        cat /tmp/casa_compile_err
        fail=$((fail+1))
    else
        # Verify stage2 can compile and run a program
        if ! "$stage2" -L "$LIB_DIR" "$ROOT_DIR/examples/hello_world.casa" -o "$stage2_test_bin" 2>/tmp/casa_compile_err; then
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

# Fixed-point verification: stage2.s == stage3.s
printf "Running: fixed_point ... "

stage1="/tmp/casa_fp_stage1"
stage2="/tmp/casa_fp_stage2"
stage3="/tmp/casa_fp_stage3"

if ! $COMPILER -L "$LIB_DIR" "$ROOT_DIR/casa.casa" -o "$stage1" 2>/tmp/casa_compile_err; then
    printf "${RED}STAGE1 COMPILE FAIL${RESET}\n"
    cat /tmp/casa_compile_err
    fail=$((fail+1))
else
    if ! "$stage1" -L "$LIB_DIR" "$ROOT_DIR/casa.casa" -o "$stage2" --keep-asm 2>/tmp/casa_compile_err; then
        printf "${RED}STAGE2 COMPILE FAIL${RESET}\n"
        cat /tmp/casa_compile_err
        fail=$((fail+1))
    else
        if ! "$stage2" -L "$LIB_DIR" "$ROOT_DIR/casa.casa" -o "$stage3" --keep-asm 2>/tmp/casa_compile_err; then
            printf "${RED}STAGE3 COMPILE FAIL${RESET}\n"
            cat /tmp/casa_compile_err
            fail=$((fail+1))
        else
            if diff "$stage2.s" "$stage3.s" > /dev/null 2>&1; then
                printf "${GREEN}OK${RESET}\n"
                pass=$((pass+1))
            else
                printf "${RED}STAGE2/STAGE3 ASSEMBLY DIFFERS${RESET}\n"
                diff "$stage2.s" "$stage3.s" | head -20
                fail=$((fail+1))
            fi
        fi
    fi
fi
rm -f "$stage1" "$stage2" "$stage3" "$stage2.s" "$stage3.s"

echo
echo "Summary: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
