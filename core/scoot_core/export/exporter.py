import sys
from typing import IO, TextIO

from sqlalchemy import Dialect

from scoot_core.exceptions import ScootExportFormatError
from scoot_core.model import ResultSet, TableModel

from .format.stream import StreamFormatter
from .registry import get_export_format


class FormatExportAdapter:
    def __init__(self, formatter: StreamFormatter, stream: IO):
        self.formatter = formatter
        self.stream = stream

    def start(self):
        self.formatter.start(self.stream)

    def end(self):
        self.formatter.end(self.stream)

    def table(self, table: TableModel):
        self.formatter.table(self.stream, table)

    def rows(
        self, dialect: Dialect, tables: list[TableModel], resultset: ResultSet
    ):
        self.formatter.rows(self.stream, dialect, tables, resultset)


class Exporter:
    def __init__(
        self, format: str, to_file: str | None = None, mode: str | None = None
    ):
        self.format = format
        self.to_file: str | None = to_file
        self.mode = mode
        self.stream: IO = (
            open(self.to_file, self.mode or "w")
            if self.to_file is not None
            else sys.stdout
        )

    def __enter__(self):
        fmt = get_export_format(self.format)
        if fmt is None:
            raise ScootExportFormatError(self.format)

        return FormatExportAdapter(fmt, self.stream)

    def __exit__(self, exc_type, exc_val, exc_tb):
        if self.stream and self.to_file is not None:
            self.stream.close()

        return False
