"""Command-line argument parsing for the Casa compiler."""

import argparse


def parse_args() -> argparse.Namespace:
    """Parse and return command-line arguments."""
    parser = argparse.ArgumentParser()
    parser.add_argument("input", help="input file")
    parser.add_argument("-o", "--output", help="output binary name")
    parser.add_argument(
        "--keep-asm", action="store_true", help="keep the .s file after linking"
    )
    parser.add_argument("-r", "--run", action="store_true", help="run after compiling")
    parser.add_argument("-v", "--verbose", action="store_true", help="verbose output")
    return parser.parse_args()
