#!/usr/bin/env python3

import logging
from pathlib import Path

from casa.bytecode import compile_bytecode
from casa.cli import parse_args
from casa.emitter import emit_assembly_code
from casa.interpreter import interpret_bytecode
from casa.lexer import lex_file
from casa.parser import parse_ops, resolve_identifiers
from casa.typechecker import type_check_ops

logger = logging.getLogger(__name__)


def main():
    args = parse_args()

    log_level = logging.INFO if args.verbose else logging.WARNING
    logging.basicConfig(level=log_level, format=f"[%(levelname)s] %(message)s")

    input_file = Path(args.input)
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

    asm_file: Path | None = None  # Default to temp file
    artifact_dir = Path(args.artifact_dir) if args.artifact_dir else None
    if artifact_dir:
        output_file = Path(args.output)
        asm_file = Path(f"{artifact_dir / output_file.stem}.asm")
    emit_assembly_code(bytecode, asm_file)

    logger.info("Interpreting bytecode")
    interpret_bytecode(bytecode)


if __name__ == "__main__":
    main()
