#!/usr/bin/env python3
"""Compile imports that reach one source file through symlinks."""
from pathlib import Path
import subprocess
import sys
import tempfile


def main(compiler):
    compiler = str(Path(compiler).resolve())
    with tempfile.TemporaryDirectory(prefix="casa-symlink-imports-") as temporary:
        root = Path(temporary)
        real = root / "sub"
        real.mkdir()
        (root / "alias").symlink_to("sub", target_is_directory=True)
        (root / "file.casa").symlink_to("sub/shapes.casa")
        (root / "chain.casa").symlink_to("file.casa")
        (real / "child.casa").write_text("pub const ZERO 0\n")
        (real / "shapes.casa").write_text(
            'import "child.casa" as child\n'
            'pub struct Point { pub x:i64 }\n'
            'pub fn x_of point:Point -> i64 { point.x child::ZERO + }\n'
            '"imported root must not run" print\n'
        )
        source = root / "main.casa"
        output = root / "program"

        def compile_source(text, *options):
            source.write_text(text)
            return subprocess.run([compiler, *options, str(source), "-o", str(output)],
                                  capture_output=True, text=True)

        def check_run(text, expected, *options):
            result = compile_source(text, *options)
            assert result.returncode == 0, result.stderr
            assert subprocess.check_output([str(output)], text=True) == expected

        check_run(
            'import "sub/shapes.casa" as real\n'
            'import "alias/shapes.casa" as linked\n'
            '3 real::Point linked::x_of print\n', "3")
        check_run(
            'import "sub/shapes.casa" as real\n'
            'import "alias/shapes.casa" as linked\n'
            'import "chain.casa" as file\n'
            'import "file.casa" as real\n'
            '3 real::Point linked::x_of print\n'
            '4 linked::Point file::x_of print\n', "34")
        check_run(
            'import "sub/shapes.casa" as real\n'
            'import "shapes" as searched\n'
            '5 real::Point searched::x_of print\n', "5", "-L", str(root / "alias"))
        check_run(
            f'import "{root / "chain.casa"}" as linked\n'
            'import "sub/shapes.casa" as real\n'
            '6 linked::Point real::x_of print\n', "6")

        # Resolve /.. after the symlink, rather than removing it from the spelling.
        nested = root / "nested"
        (nested / "child").mkdir(parents=True)
        (root / "jump").symlink_to("nested/child", target_is_directory=True)
        (nested / "value.casa").write_text("pub const VALUE 7\n")
        (root / "value.casa").write_text('pub const VALUE "wrong parent"\n')
        check_run('import "jump/../value.casa" as value\nvalue::VALUE print\n', "7")

        # The resolved path must grow beyond the initial readlink buffer.
        long_directory = root / ("a" * 220) / "ääkköset"
        long_directory.mkdir(parents=True)
        (long_directory / "value.casa").write_text("pub const VALUE 9\n")
        (root / "long.casa").symlink_to(long_directory / "value.casa")
        check_run('import "long.casa" as value\nvalue::VALUE print\n', "9")

        # The root's symlink identity must also take part in cycle detection.
        (root / "root.casa").symlink_to("main.casa")
        result = compile_source('import "root.casa" as root\n')
        assert result.returncode != 0, result
        assert "Module import cycle:" in result.stderr, result.stderr
        assert "called unwrap" not in result.stderr, result.stderr

        # A module-style same-directory alias of the root is skipped before -L.
        (root / "main_alias.casa").symlink_to("main.casa")
        (real / "main_alias.casa").write_text("pub const VALUE 8\n")
        check_run('import "main_alias" as library\nlibrary::VALUE print\n',
                  "8", "-L", str(real))

        (root / "broken.casa").symlink_to("missing.casa")
        result = compile_source('import "broken.casa" as broken\n')
        assert result.returncode != 0, result
        assert "called unwrap" not in result.stderr, result.stderr
    print("Symlink import tests passed")


if __name__ == "__main__":
    main(sys.argv[1])
