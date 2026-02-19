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


def main():
    args = parse_args()

    log_level = logging.INFO if args.verbose else logging.WARNING
    logging.basicConfig(level=log_level, format="[%(levelname)s] %(message)s")

    input_file = pathlib.Path(args.input)
    output_name = args.output or input_file.stem

    logger.info("Lexing %s", input_file)
    tokens = lex_file(input_file.resolve())

    logger.info("Parsing ops")
    ops = parse_ops(tokens)

    logger.info("Resolving identifiers")
    ops = resolve_identifiers(ops)

    logger.info("Type checking ops")
    type_check_ops(ops)

    logger.info("Compiling bytecode")
    program = compile_bytecode(ops)

    logger.info("Emitting assembly")
    asm_source = emit_program(program)

    asm_file = f"{output_name}.s"
    obj_file = f"{output_name}.o"

    with open(asm_file, "w", encoding="utf-8") as asm_fh:
        asm_fh.write(asm_source)

    try:
        logger.info("Assembling %s", asm_file)
        result = subprocess.run(
            ["as", "-o", obj_file, asm_file], capture_output=True, check=False
        )
        if result.returncode != 0:
            print(result.stderr.decode(), file=sys.stderr)
            sys.exit(1)

        logger.info("Linking %s", obj_file)
        result = subprocess.run(
            ["ld", "-o", output_name, obj_file], capture_output=True, check=False
        )
        if result.returncode != 0:
            print(result.stderr.decode(), file=sys.stderr)
            sys.exit(1)

        logger.info("Built %s", output_name)
    finally:
        if not args.keep_asm and os.path.exists(asm_file):
            os.remove(asm_file)
        if os.path.exists(obj_file):
            os.remove(obj_file)


if __name__ == "__main__":
    main()
