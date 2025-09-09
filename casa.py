import logging
import pathlib

from casa.cli import parse_args
from casa.compiler import compile_bytecode
from casa.interpreter import interpret_bytecode
from casa.lexer import lex_file
from casa.parser import parse_ops, resolve_identifiers
from casa.typechecker import type_check_ops

COMPILER_FOLDER = pathlib.Path(__file__).resolve().parent
CASA_SOURCE_CODE = "test.casa"

logger = logging.getLogger(__name__)


def main():
    code_file = COMPILER_FOLDER / CASA_SOURCE_CODE

    args = parse_args()

    log_level = logging.INFO if args.verbose else logging.WARNING
    logging.basicConfig(level=log_level, format=f"[%(levelname)s] %(message)s")

    logger.info(f"Lexing {CASA_SOURCE_CODE}")
    tokens = lex_file(code_file)

    logger.info("Parsing ops")
    ops = parse_ops(tokens)

    logger.info("Resolving identifiers")
    resolve_identifiers(ops)

    logger.info("Type checking ops")
    type_check_ops(ops)

    logger.info("Compiling bytecode")
    bytecode = compile_bytecode(ops, initialize_globals=True)

    logger.info("Interpreting bytecode")
    interpret_bytecode(bytecode)


if __name__ == "__main__":
    main()
