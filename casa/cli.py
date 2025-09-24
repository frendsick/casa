import argparse


# fmt: off
def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("input", help="input file")
    parser.add_argument("-o", "--output", metavar="FILE", default="a.out", help="output file")
    parser.add_argument("--artifact-dir", metavar="DIR", help="save compiler artifacts to this directory")
    parser.add_argument("-v", "--verbose", action="store_true", help="verbose output")
    return parser.parse_args()
