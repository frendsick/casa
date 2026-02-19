#!/usr/bin/env python3

import logging
import os
import pathlib
import subprocess
import sys

from casa.bytecode import compile_bytecode
from casa.cli import parse_args
from casa.emitter import emit_program
from casa.lexer import lex_file
from casa.parser import parse_ops, resolve_identifiers
from casa.typechecker import type_check_ops

logger = logging.getLogger(__name__)


def run_cmd(cmd: list[str]) -> None:
    result = subprocess.run(cmd, capture_output=True, check=False)
    if result.returncode != 0:
        print(result.stderr.decode(), file=sys.stderr)
        sys.exit(1)


def build_binary(asm_source: str, output_name: str, keep_asm: bool) -> None:
    asm_file = f"{output_name}.s"
    obj_file = f"{output_name}.o"

    pathlib.Path(asm_file).write_text(asm_source, encoding="utf-8")

    run_cmd(["as", "-o", obj_file, asm_file])
    run_cmd(["ld", "-o", output_name, obj_file])

    if not keep_asm:
        os.remove(asm_file)
    os.remove(obj_file)


def main():
    args = parse_args()

    log_level = logging.INFO if args.verbose else logging.WARNING
    logging.basicConfig(level=log_level, format="[%(levelname)s] %(message)s")

    input_file = pathlib.Path(args.input)
    output_name = args.output or input_file.stem

    logger.info("Lexing %s", input_file)
    tokens = lex_file(input_file.resolve())

    logger.info("Parsing ops")
    ops = resolve_identifiers(parse_ops(tokens))

    logger.info("Type checking ops")
    type_check_ops(ops)

    logger.info("Compiling bytecode")
    program = compile_bytecode(ops)

    logger.info("Emitting assembly")
    asm_source = emit_program(program)

    logger.info("Building %s", output_name)
    build_binary(asm_source, output_name, args.keep_asm)

    if args.run:
        logger.info("Running %s", output_name)
        result = subprocess.run([f"./{output_name}"], check=False)
        sys.exit(result.returncode)


if __name__ == "__main__":
    main()
