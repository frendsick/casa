import logging
import os
import tempfile
from pathlib import Path

from casa.common import Bytecode

logger = logging.getLogger(__name__)


class GasEmitter:
    """GNU assembly emitter"""

    def __init__(self, out_file: Path | None) -> None:
        if out_file:
            self.is_tempfile = False
            out_file.parent.mkdir(parents=True, exist_ok=True)
            self.path = out_file
        else:
            self.is_tempfile = True
            file_name = tempfile.mkstemp(suffix=".asm", text=True)[1]
            self.path = Path(file_name)
        self.file = open(self.path, "a", encoding="utf-8")

    def __del__(self):
        if not self.file.closed:
            self.file.close()
        if self.is_tempfile:
            os.remove(self.path)

    def write(self, data: str):
        self.file.write(data)

    def emit(self, bytecode: Bytecode):
        logger.info(f"Emitting assembly code to {self.path}")
        for instruction in bytecode:
            print(instruction)


def emit_assembly_code(bytecode: Bytecode, out_file: Path | None):
    emitter = GasEmitter(out_file)
    emitter.emit(bytecode)
