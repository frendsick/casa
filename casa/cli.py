import argparse


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("input", help="input file")
    parser.add_argument("-v", "--verbose", action="store_true", help="verbose output")
    return parser.parse_args()
