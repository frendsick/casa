#!/usr/bin/env python3

import logging
import pathlib

from casa.bytecode import compile_bytecode
from casa.cli import parse_args
from casa.interpreter import interpret_bytecode
from casa.lexer import lex_file
from casa.parser import parse_ops, resolve_identifiers
from casa.typechecker import type_check_ops

logger = logging.getLogger(__name__)


def main():
    args = parse_args()

    log_level = logging.INFO if args.verbose else logging.WARNING
    logging.basicConfig(level=log_level, format=f"[%(levelname)s] %(message)s")

    input_file = pathlib.Path(args.input)
    logger.info(f"Lexing {input_file}")
    tokens = lex_file(input_file.resolve())

    logger.info("Parsing ops")
    ops = parse_ops(tokens)

    logger.info("Resolving identifiers")
    ops = resolve_identifiers(ops)

    logger.info("Type checking ops")
    type_check_ops(ops)

    logger.info("Compiling bytecode")
    bytecode = compile_bytecode(ops)

    logger.info("Interpreting bytecode")
    interpret_bytecode(bytecode)


if __name__ == "__main__":
    main()
