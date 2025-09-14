import sys

MIN_PYTHON_VERSION = (3, 11)

if sys.version_info < MIN_PYTHON_VERSION:
    raise RuntimeError(
        f"Python version {MIN_PYTHON_VERSION[0]}.{MIN_PYTHON_VERSION[1]} or newer is required. The current version is {sys.version_info.major}.{sys.version_info.minor}."
    )
