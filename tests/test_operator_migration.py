#!/usr/bin/env python3
"""Check source preservation and the behavior of migrated comparisons."""

from pathlib import Path
import subprocess
import sys
import tempfile


def migrate(tool, source):
    result = subprocess.run([tool], input=source, capture_output=True, timeout=10)
    assert result.returncode == 0, result.stderr
    assert not result.stderr, result.stderr
    return result.stdout


def test_source_preservation(tool):
    source = (
        '# < <= > >= == !=\r\n"λ < ==" drop\r\n'
        "'<' '>' != drop\r\n"
        'f"literal > {1 2 <} nested {f"{3 4 >=}"}" print\r\n'
        'fn less a:i64 b:i64 -> bool { a b <= }\r\n'
        '&less drop 1 2 less drop\r\n'
        '1 2 << drop 3 1 >> drop 1 = count 1 += count\r\n'
        'const ABOVE { 1 2 > }\r\n'
        'true false == drop 1 2 swap < drop\r\n'
    ).encode()
    expected = (
        '# < <= > >= == !=\r\n"λ < ==" drop\r\n'
        "'<' '>' swap != drop\r\n"
        'f"literal > {1 2 swap <} nested {f"{3 4 swap >=}"}" print\r\n'
        'fn less a:i64 b:i64 -> bool { a b swap <= }\r\n'
        '&less drop 1 2 less drop\r\n'
        '1 2 << drop 3 1 >> drop 1 = count 1 += count\r\n'
        'const ABOVE { 1 2 swap > }\r\n'
        'true false swap == drop 1 2 swap swap < drop\r\n'
    ).encode()
    assert migrate(tool, source) == expected
    assert migrate(tool, b'"no comparisons" print\n') == b'"no comparisons" print\n'
    for source in (b'"unterminated', b'1 2 <\n\xff', b'f"{1 2 <}'):
        result = subprocess.run([tool], input=source, capture_output=True, timeout=10)
        assert result.returncode != 0
        assert result.stdout == source
        assert result.stderr


def test_behavior(tool, compiler, directory):
    # Distinct methods and observable receiver values catch operator inversion,
    # reversed evaluation, and accidental symmetric treatment of eq/ne.
    methods = []
    expressions = []
    expected = []
    for operator, method in (("==", "eq"), ("!=", "ne"), ("<", "lt"),
                             ("<=", "le"), (">", "gt"), (">=", "ge")):
        methods.append(f'''fn {method} self:$Probe other:$Probe -> bool {{
            self.value print other.value print "{method}" print true
        }}''')
        expressions.append(f'1 operand 2 operand {operator} print "\\n" print')
        expected.append(f'AB21{method}true\n')
    source = '''import "std" as std
struct Probe { value:i64 text:std::String }
impl Probe: std::PartialOrd {
    fn partial_cmp self:$Probe other:$Probe -> std::Option[std::Ordering] {
        self drop other drop std::Option::None
    }
''' + '\n'.join(methods) + '''
}
fn operand value:i64 -> Probe {
    if value 1 == then "A" else "B" fi print
    "owned".to_str value Probe
}
''' + '\n'.join(expressions) + '''
const ABOVE { 1 2 > }
ABOVE print "\\n" print
10 3 < print "\\n" print
[1, 2, 3] std::List::from_array = values
2 = limit
{ limit < } values.bisect print "\\n" print
2 { 3 < } exec print "\\n" print
1 { limit > } exec print "\\n" print
'''
    expected += ['true\n', 'true\n', '2\n', 'false\n', 'true\n']
    migrated = directory / 'migrated.casa'
    migrated.write_bytes(migrate(tool, source.encode()))
    output = directory / 'migrated'
    subprocess.run([compiler, '-L', 'lib', str(migrated), '-o', str(output)],
                   check=True, capture_output=True, timeout=30)
    result = subprocess.run([output], check=True, capture_output=True, timeout=10)
    assert result.stdout.decode() == ''.join(expected), result.stdout


def main(tool, compiler):
    tool = str(Path(tool).resolve())
    compiler = str(Path(compiler).resolve())
    test_source_preservation(tool)
    with tempfile.TemporaryDirectory(prefix='casa-operator-migration-') as temporary:
        test_behavior(tool, compiler, Path(temporary))


if __name__ == '__main__':
    main(*sys.argv[1:])
