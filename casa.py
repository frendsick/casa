import pathlib

from casa.lexer import lex_file
from casa.parser import parse_ops

COMPILER_FOLDER = pathlib.Path(__file__).resolve().parent
CASA_SOURCE_CODE = "test.casa"


def main():
    code_file = COMPILER_FOLDER / CASA_SOURCE_CODE
    tokens = lex_file(code_file)
    for token in tokens:
        print(token)

    ops = parse_ops(tokens)
    for op in ops:
        print(op)


if __name__ == "__main__":
    main()
