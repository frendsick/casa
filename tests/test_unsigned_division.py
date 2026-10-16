from pathlib import Path
import re
import subprocess
import sys
import tempfile


def compile_program(compiler, root, name, source):
    path = root / f"{name}.casa"
    path.write_text(source)
    output = root / name
    subprocess.run([compiler, str(path), "-o", str(output), "--keep-asm"],
                   check=True, timeout=30)
    assembly = output.with_suffix(".s").read_text()
    return output, assembly.split(".equ CASA_RETURN_STACK_SIZE,", 1)[0]


def test_constant_powers(compiler, root):
    declarations = []
    calls = []
    expected = []
    for width in (8, 16, 32, 64):
        maximum = (1 << width) - 1
        for exponent in range(width):
            divisor = 1 << exponent
            name = f"u{width}_{exponent}"
            declarations.extend([
                f"fn quotient_{name} value:u{width} -> u{width} {{ value {divisor} / }}",
                f"fn remainder_{name} value:u{width} -> u{width} {{ value {divisor} % }}",
            ])
            values = sorted({0, 1, divisor - 1, divisor, divisor + 1, maximum})
            for value in values:
                calls.append(f'{value} quotient_{name} print ":" print '
                             f'{value} remainder_{name} print "\\n" print')
                expected.append(f"{value // divisor}:{value % divisor}\n")
    # Named constants also reach the adjacent-constant path. Keep older cached
    # values live across both optimized operations to check the stack effect.
    declarations.append("const DIVISOR:u64 { 4 2 * }")
    calls.append('11 22 33 44 55 18446744073709551615 DIVISOR / '
                 '18446744073709551615 DIVISOR % '
                 'print ":" print print ":" print print ":" print print '
                 '":" print print ":" print print ":" print print "\\n" print')
    expected.append("7:2305843009213693951:55:44:33:22:11\n")
    output, assembly = compile_program(compiler, root, "powers", "\n".join(declarations + calls))
    assert not re.search(r"\b(?:divq|idivq)\b", assembly), assembly
    assert "jz arithmetic_error" not in assembly, assembly
    assert "shrq $63" in assembly, assembly
    assert "movabsq $9223372036854775807, %rax" in assembly, assembly
    result = subprocess.run([output], check=True, capture_output=True, text=True, timeout=10)
    assert result.stdout == "".join(expected), result.stdout


def test_fallback_results(compiler, root):
    source = []
    expected = []
    for width in (8, 16, 32, 64):
        maximum = (1 << width) - 1
        source.extend([
            f"fn quotient_u{width} value:u{width} divisor:u{width} -> u{width} {{ value divisor / }}",
            f"fn remainder_u{width} value:u{width} -> u{width} {{ value 3 % }}",
            f"fn quotient_three_u{width} value:u{width} -> u{width} {{ value 3 / }}",
            f"fn remainder_variable_u{width} value:u{width} divisor:u{width} -> u{width} {{ value divisor % }}",
            f'{1 << (width - 1)} {maximum} quotient_u{width} print ":" print '
            f'{maximum} remainder_u{width} print ":" print '
            f'{maximum} quotient_three_u{width} print ":" print '
            f'{1 << (width - 1)} {maximum} remainder_variable_u{width} print "\\n" print',
            f"fn quotient_i{width} value:i{width} -> i{width} {{ value 8 / }}",
            f"fn remainder_i{width} value:i{width} -> i{width} {{ value 8 % }}",
            f'-17 quotient_i{width} print ":" print -17 remainder_i{width} print "\\n" print',
        ])
        expected.extend([f"1:{maximum % 3}:{maximum // 3}:{(1 << (width - 1)) - 1}\n", "-2:-1\n"])
    output, assembly = compile_program(compiler, root, "fallback", "\n".join(source))
    assert "divq %rbx" in assembly, assembly
    assert "idivq %rbx" in assembly, assembly
    assert "jz arithmetic_error" in assembly, assembly
    result = subprocess.run([output], check=True, capture_output=True, text=True, timeout=10)
    assert result.stdout == "".join(expected), result.stdout


def test_checked_failures(compiler, root):
    for width in (8, 16, 32, 64):
        for operator, name in (("/", "division"), ("%", "remainder")):
            sources = [
                f"fn fail value:u{width} -> u{width} {{ value 0 {operator} }} 1 fail print",
                f"fn fail value:u{width} divisor:u{width} -> u{width} {{ value divisor {operator} }} 0 1 fail print",
                f"fn fail value:i{width} -> i{width} {{ value -1 {operator} }} {-1 << (width - 1)} fail print",
            ]
            for index, source in enumerate(sources):
                output, _ = compile_program(compiler, root, f"fail_{width}_{name}_{index}", source)
                result = subprocess.run([output], capture_output=True, text=True, timeout=10)
                assert result.returncode != 0, source
                assert "integer arithmetic failed" in result.stderr, (source, result.stderr)


def main(compiler):
    compiler = str(Path(compiler).resolve())
    with tempfile.TemporaryDirectory(prefix="casa unsigned division ") as temporary:
        root = Path(temporary)
        test_constant_powers(compiler, root)
        test_fallback_results(compiler, root)
        test_checked_failures(compiler, root)


if __name__ == "__main__":
    main(sys.argv[1])
