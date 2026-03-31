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

# Subtraction
tmp_sub="/tmp/casa_sh_sub.casa"
echo '10 3 - print' > "$tmp_sub"
run_test "subtraction" "$tmp_sub" "7"

# Multiplication
tmp_mul="/tmp/casa_sh_mul.casa"
echo '6 7 * print' > "$tmp_mul"
run_test "multiplication" "$tmp_mul" "42"

# Division
tmp_div="/tmp/casa_sh_div.casa"
echo '20 4 / print' > "$tmp_div"
run_test "division" "$tmp_div" "5"

# Modulo
tmp_mod="/tmp/casa_sh_mod.casa"
echo '17 5 % print' > "$tmp_mod"
run_test "modulo" "$tmp_mod" "2"

# Combined arithmetic
tmp_arith="/tmp/casa_sh_arith.casa"
echo '3 4 * 2 + print' > "$tmp_arith"
run_test "combined_arithmetic" "$tmp_arith" "14"

# Bitwise AND
tmp_band="/tmp/casa_sh_band.casa"
echo '12 10 & print' > "$tmp_band"
run_test "bitwise_and" "$tmp_band" "8"

# Bitwise OR
tmp_bor="/tmp/casa_sh_bor.casa"
echo '12 10 | print' > "$tmp_bor"
run_test "bitwise_or" "$tmp_bor" "14"

# Bitwise XOR
tmp_bxor="/tmp/casa_sh_bxor.casa"
echo '12 10 ^ print' > "$tmp_bxor"
run_test "bitwise_xor" "$tmp_bxor" "6"

# Bitwise NOT
tmp_bnot="/tmp/casa_sh_bnot.casa"
echo '0 ~ print' > "$tmp_bnot"
run_test "bitwise_not" "$tmp_bnot" "-1"

# Shift left
tmp_shl="/tmp/casa_sh_shl.casa"
echo '1 4 << print' > "$tmp_shl"
run_test "shift_left" "$tmp_shl" "16"

# Shift right
tmp_shr="/tmp/casa_sh_shr.casa"
echo '16 2 >> print' > "$tmp_shr"
run_test "shift_right" "$tmp_shr" "4"

# Comparisons
tmp_eq="/tmp/casa_sh_eq.casa"
echo '5 5 == print' > "$tmp_eq"
run_test "equal_true" "$tmp_eq" "1"

tmp_eq2="/tmp/casa_sh_eq2.casa"
echo '5 3 == print' > "$tmp_eq2"
run_test "equal_false" "$tmp_eq2" "0"

tmp_ne="/tmp/casa_sh_ne.casa"
echo '5 3 != print' > "$tmp_ne"
run_test "not_equal_true" "$tmp_ne" "1"

tmp_lt="/tmp/casa_sh_lt.casa"
echo '5 3 < print' > "$tmp_lt"
run_test "less_than_true" "$tmp_lt" "1"

tmp_gt="/tmp/casa_sh_gt.casa"
echo '3 5 > print' > "$tmp_gt"
run_test "greater_than_true" "$tmp_gt" "1"

tmp_le="/tmp/casa_sh_le.casa"
echo '5 5 <= print' > "$tmp_le"
run_test "less_equal_true" "$tmp_le" "1"

tmp_ge="/tmp/casa_sh_ge.casa"
echo '5 5 >= print' > "$tmp_ge"
run_test "greater_equal_true" "$tmp_ge" "1"

# Boolean
tmp_and="/tmp/casa_sh_and.casa"
echo '1 1 && print' > "$tmp_and"
run_test "boolean_and_true" "$tmp_and" "1"

tmp_and2="/tmp/casa_sh_and2.casa"
echo '1 0 && print' > "$tmp_and2"
run_test "boolean_and_false" "$tmp_and2" "0"

tmp_or="/tmp/casa_sh_or.casa"
echo '0 1 || print' > "$tmp_or"
run_test "boolean_or_true" "$tmp_or" "1"

tmp_not="/tmp/casa_sh_not.casa"
echo '0 ! print' > "$tmp_not"
run_test "boolean_not_true" "$tmp_not" "1"

tmp_not2="/tmp/casa_sh_not2.casa"
echo '1 ! print' > "$tmp_not2"
run_test "boolean_not_false" "$tmp_not2" "0"

# Stack intrinsics
tmp_drop="/tmp/casa_sh_drop.casa"
echo '1 2 drop print' > "$tmp_drop"
run_test "drop" "$tmp_drop" "1"

tmp_dup="/tmp/casa_sh_dup.casa"
echo '42 dup + print' > "$tmp_dup"
run_test "dup" "$tmp_dup" "84"

tmp_swap="/tmp/casa_sh_swap.casa"
echo '1 2 swap - print' > "$tmp_swap"
run_test "swap" "$tmp_swap" "1"

tmp_over="/tmp/casa_sh_over.casa"
echo '1 2 over + print' > "$tmp_over"
run_test "over" "$tmp_over" "3"

tmp_rot="/tmp/casa_sh_rot.casa"
echo '1 2 3 rot print drop drop' > "$tmp_rot"
run_test "rot" "$tmp_rot" "1"

# Variables
tmp_var="/tmp/casa_sh_var.casa"
echo '10 = x x print' > "$tmp_var"
run_test "variable" "$tmp_var" "10"

tmp_var_inc="/tmp/casa_sh_var_inc.casa"
echo '5 = x 3 += x x print' > "$tmp_var_inc"
run_test "variable_increment" "$tmp_var_inc" "8"

# If/fi
tmp_if="/tmp/casa_sh_if.casa"
echo '1 if 1 == then 42 print fi' > "$tmp_if"
run_test "if_true" "$tmp_if" "42"

tmp_if_false="/tmp/casa_sh_if_false.casa"
echo '0 if 1 == then 42 print fi' > "$tmp_if_false"
run_test "if_false" "$tmp_if_false" ""

# If/else/fi
tmp_ifelse="/tmp/casa_sh_ifelse.casa"
printf '1 if 1 == then 10 print else 20 print fi' > "$tmp_ifelse"
run_test "if_else_true" "$tmp_ifelse" "10"

tmp_ifelse2="/tmp/casa_sh_ifelse2.casa"
printf '0 if 1 == then 10 print else 20 print fi' > "$tmp_ifelse2"
run_test "if_else_false" "$tmp_ifelse2" "20"

# Elif chain
tmp_elif="/tmp/casa_sh_elif.casa"
cat > "$tmp_elif" << 'CASA'
3 = x
if x 1 == then
    10 print
elif x 2 == then
    20 print
elif x 3 == then
    30 print
else
    40 print
fi
CASA
run_test "elif_chain" "$tmp_elif" "30"

# While loop
tmp_while="/tmp/casa_sh_while.casa"
cat > "$tmp_while" << 'CASA'
0 = i
while 5 i < do
    i print
    i 1 + = i
done
CASA
run_test "while_loop" "$tmp_while" "01234"

# Break
tmp_break="/tmp/casa_sh_break.casa"
cat > "$tmp_break" << 'CASA'
0 = i
while 10 i < do
    if i 3 == then break fi
    i print
    i 1 + = i
done
CASA
run_test "break" "$tmp_break" "012"

# Continue
tmp_continue="/tmp/casa_sh_continue.casa"
cat > "$tmp_continue" << 'CASA'
0 = i
while 5 i < do
    i 1 + = i
    if i 3 == then continue fi
    i print
done
CASA
run_test "continue" "$tmp_continue" "1245"

# Nested if in while
tmp_nested_if="/tmp/casa_sh_nested_if.casa"
cat > "$tmp_nested_if" << 'CASA'
0 = i
while 6 i < do
    if i 2 % 0 == then
        i print
    fi
    i 1 + = i
done
CASA
run_test "nested_if_in_while" "$tmp_nested_if" "024"

# Nested while in while
tmp_nested_while="/tmp/casa_sh_nested_while.casa"
cat > "$tmp_nested_while" << 'CASA'
0 = i
while 3 i < do
    0 = j
    while 2 j < do
        i j + print
        j 1 + = j
    done
    i 1 + = i
done
CASA
run_test "nested_while" "$tmp_nested_while" "011223"

# -------------------------------------------------------
# Structs
# -------------------------------------------------------

tmp_struct="/tmp/casa_sh_struct.casa"
cat > "$tmp_struct" << 'CASA'
struct Point {
    x: int
    y: int
}
20 10 Point = p
p Point::x print
p Point::y print
CASA
run_test "struct_construct_access" "$tmp_struct" "1020"

tmp_struct_set="/tmp/casa_sh_struct_set.casa"
cat > "$tmp_struct_set" << 'CASA'
struct Pair {
    a: int
    b: int
}
2 1 Pair = pair
pair Pair::a print
42 pair Pair::set_b
pair Pair::b print
CASA
run_test "struct_setter" "$tmp_struct_set" "142"

tmp_struct_three="/tmp/casa_sh_struct_three.casa"
cat > "$tmp_struct_three" << 'CASA'
struct Triple {
    x: int
    y: int
    z: int
}
30 20 10 Triple = t
t Triple::x print
t Triple::y print
t Triple::z print
CASA
run_test "struct_three_fields" "$tmp_struct_three" "102030"

# -------------------------------------------------------
# Enums
# -------------------------------------------------------

tmp_enum="/tmp/casa_sh_enum.casa"
cat > "$tmp_enum" << 'CASA'
enum Color { Red Green Blue }
Color::Red print
Color::Green print
Color::Blue print
CASA
run_test "enum_variants" "$tmp_enum" "012"

tmp_enum_eq="/tmp/casa_sh_enum_eq.casa"
cat > "$tmp_enum_eq" << 'CASA'
enum Dir { North South East West }
Dir::South = d
if d Dir::South == then 1 print else 0 print fi
if d Dir::North == then 1 print else 0 print fi
CASA
run_test "enum_compare" "$tmp_enum_eq" "10"

# -------------------------------------------------------
# Match
# -------------------------------------------------------

tmp_match="/tmp/casa_sh_match.casa"
cat > "$tmp_match" << 'CASA'
enum Color { Red Green Blue }
Color::Green = c
c match
    Color::Red => 1 print
    Color::Green => 2 print
    Color::Blue => 3 print
end
CASA
run_test "match_enum" "$tmp_match" "2"

tmp_match_wild="/tmp/casa_sh_match_wild.casa"
cat > "$tmp_match_wild" << 'CASA'
enum Color { Red Green Blue }
Color::Blue = c
c match
    Color::Red => 1 print
    _ => 9 print
end
CASA
run_test "match_wildcard" "$tmp_match_wild" "9"

tmp_match_brace="/tmp/casa_sh_match_brace.casa"
cat > "$tmp_match_brace" << 'CASA'
enum Op { Add Sub Mul }
Op::Mul = op
10 = a
3 = b
op match
    Op::Add => { a b + print }
    Op::Sub => { a b - print }
    Op::Mul => { a b * print }
end
CASA
run_test "match_braced_body" "$tmp_match_brace" "30"

tmp_match_fn="/tmp/casa_sh_match_fn.casa"
cat > "$tmp_match_fn" << 'CASA'
enum Fruit { Apple Banana Cherry }
fn describe fruit:Fruit {
    fruit match
        Fruit::Apple => 1 print
        Fruit::Banana => 2 print
        Fruit::Cherry => 3 print
    end
}
Fruit::Banana describe
Fruit::Cherry describe
Fruit::Apple describe
CASA
run_test "match_in_function" "$tmp_match_fn" "231"

tmp_match_first="/tmp/casa_sh_match_first.casa"
cat > "$tmp_match_first" << 'CASA'
enum Color { Red Green Blue }
Color::Red = c
c match
    Color::Red => 1 print
    Color::Green => 2 print
    Color::Blue => 3 print
end
CASA
run_test "match_first_arm" "$tmp_match_first" "1"

tmp_match_last="/tmp/casa_sh_match_last.casa"
cat > "$tmp_match_last" << 'CASA'
enum Color { Red Green Blue }
Color::Blue = c
c match
    Color::Red => 1 print
    Color::Green => 2 print
    Color::Blue => 3 print
end
CASA
run_test "match_last_arm" "$tmp_match_last" "3"

tmp_nested_match="/tmp/casa_sh_nested_match.casa"
cat > "$tmp_nested_match" << 'CASA'
enum Outer { A B }
enum Inner { X Y }
Outer::B = o
Inner::X = i
o match
    Outer::A => {
        i match
            Inner::X => 1 print
            Inner::Y => 2 print
        end
    }
    Outer::B => {
        i match
            Inner::X => 3 print
            Inner::Y => 4 print
        end
    }
end
CASA
run_test "nested_match" "$tmp_nested_match" "3"

tmp_struct_fn="/tmp/casa_sh_struct_fn.casa"
cat > "$tmp_struct_fn" << 'CASA'
struct Rect {
    w: int
    h: int
}
fn area rect:Rect -> int {
    rect Rect::w rect Rect::h *
}
fn make_rect -> Rect {
    4 3 Rect
}
make_rect = r
r area print
CASA
run_test "struct_in_function" "$tmp_struct_fn" "12"

# Clean up
rm -f "$COMPILER"
rm -f "$tmp_add" "$tmp_neg" "$tmp_zero" "$tmp_single" "$tmp_chain"
rm -f "$tmp_sub" "$tmp_mul" "$tmp_div" "$tmp_mod" "$tmp_arith"
rm -f "$tmp_band" "$tmp_bor" "$tmp_bxor" "$tmp_bnot" "$tmp_shl" "$tmp_shr"
rm -f "$tmp_eq" "$tmp_eq2" "$tmp_ne" "$tmp_lt" "$tmp_gt" "$tmp_le" "$tmp_ge"
rm -f "$tmp_and" "$tmp_and2" "$tmp_or" "$tmp_not" "$tmp_not2"
rm -f "$tmp_drop" "$tmp_dup" "$tmp_swap" "$tmp_over" "$tmp_rot"
rm -f "$tmp_var" "$tmp_var_inc"
rm -f "$tmp_if" "$tmp_if_false" "$tmp_ifelse" "$tmp_ifelse2" "$tmp_elif"
rm -f "$tmp_while" "$tmp_break" "$tmp_continue"
rm -f "$tmp_nested_if" "$tmp_nested_while"
rm -f "$tmp_struct" "$tmp_struct_set" "$tmp_struct_three"
rm -f "$tmp_enum" "$tmp_enum_eq"
rm -f "$tmp_match" "$tmp_match_wild" "$tmp_match_brace" "$tmp_match_fn"
rm -f "$tmp_match_first" "$tmp_match_last" "$tmp_nested_match"
rm -f "$tmp_struct_fn"

echo
echo "Summary: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
