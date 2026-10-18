"""Check comparison/jump adjacency and values live across both branch paths."""
import operator
from pathlib import Path
import re
import subprocess
import sys
import tempfile


def main(compiler):
    compiler = str(Path(compiler).resolve())
    source = []
    expected = []
    comparisons = (("eq", "==", operator.eq), ("ne", "!=", operator.ne),
                   ("lt", "<", operator.lt), ("le", "<=", operator.le),
                   ("gt", ">", operator.gt), ("ge", ">=", operator.ge))
    for typ, values in (("i64", (-1, 0, 9223372036854775807)),
                        ("u64", (0, 9223372036854775808, 18446744073709551615))):
        for name, symbol, compare in comparisons:
            function = f"{name}_{typ}"
            # Wrap and permute the cache, then keep seven older values live
            # across a comparison that also needs value-stack synchronization.
            source.append(f"fn {function} left:{typ} right:{typ} {{ "
                          f"1 2 3 4 5 6 rot swap over if left right {symbol} then 1 else 0 fi "
                          'print ":" print print ":" print print ":" print print '
                          '":" print print ":" print print ":" print print ":" print print "\\n" print }')
            for left in values:
                for right in values:
                    source.append(f"{right} {left} {function}")
                    expected.append(f"{int(compare(left, right))}:4:6:4:5:3:2:1\n")
    with tempfile.TemporaryDirectory(prefix="casa comparison branches ") as temporary:
        root = Path(temporary)
        path = root / "branches.casa"
        path.write_text("\n".join(source))
        output = root / "branches"
        subprocess.run([compiler, str(path), "-o", str(output), "--keep-asm"],
                       check=True, timeout=30)
        result = subprocess.run([output], capture_output=True, text=True, check=True, timeout=10)
        assert result.stdout == "".join(expected), result.stdout
        assembly = output.with_suffix(".s").read_text().split(".equ CASA_RETURN_STACK_SIZE,", 1)[0]
        instructions = [line.strip() for line in assembly.splitlines()
                        if re.match(r"^\s+[a-z]", line)]
        found = 0
        for index, instruction in enumerate(instructions):
            if re.fullmatch(r"cmpq %r(?:8|9|10|11), %r(?:8|9|10|11)", instruction):
                following = instructions[index + 1]
                assert re.match(r"j(?:a|ae|b|be|e|g|ge|l|le|ne) ", following), following
                found += 1
        assert found == 12, found


if __name__ == "__main__":
    main(sys.argv[1])
