"""Casa compiler package initialization and version check."""

import sys

MIN_PYTHON_VERSION = (3, 11)

if sys.version_info < MIN_PYTHON_VERSION:
    major, minor = MIN_PYTHON_VERSION
    current_major = sys.version_info.major
    current_minor = sys.version_info.minor
    raise RuntimeError(
        f"Python version {major}.{minor} or newer is required."
        f" The current version is {current_major}.{current_minor}."
    )
