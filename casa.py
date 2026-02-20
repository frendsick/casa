#!/usr/bin/env python3

import logging
import pathlib
import subprocess
import sys

from casa.bytecode import compile_bytecode
from casa.cli import parse_args
from casa.compiler import compile_binary
from casa.emitter import emit_program
from casa.error import WARNINGS, CasaErrorCollection, report_errors, report_warnings
from casa.lexer import lex_file
from casa.parser import parse_ops, resolve_identifiers
from casa.typechecker import type_check_all_functions, type_check_ops

logger = logging.getLogger(__name__)


def main():
    args = parse_args()

    log_level = logging.INFO if args.verbose else logging.WARNING
    logging.basicConfig(level=log_level, format="[%(levelname)s] %(message)s")

    input_file = pathlib.Path(args.input)
    output_name = args.output or input_file.stem

    try:
        logger.info("Lexing %s", input_file)
        tokens = lex_file(input_file.resolve())

        logger.info("Parsing ops")
        ops = resolve_identifiers(parse_ops(tokens))

        logger.info("Type checking ops")
        type_check_ops(ops)
        type_check_all_functions()

        logger.info("Compiling bytecode")
        program = compile_bytecode(ops)
    except CasaErrorCollection as exc:
        report_errors(exc.errors)
        sys.exit(1)

    if WARNINGS:
        report_warnings()

    logger.info("Emitting assembly")
    asm_source = emit_program(program)

    logger.info("Compiling %s", output_name)
    compile_binary(asm_source, output_name, args.keep_asm)

    if args.run:
        logger.info("Running %s", output_name)
        result = subprocess.run([f"./{output_name}"], check=False)
        sys.exit(result.returncode)


if __name__ == "__main__":
    main()
