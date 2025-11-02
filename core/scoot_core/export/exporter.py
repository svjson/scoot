import sys
from typing import TextIO

from sqlalchemy import Dialect

from scoot_core.exceptions import ScootExportFormatError
from scoot_core.model import ResultSet, TableModel

from .format.stream import StreamFormatter
from .registry import get_export_format


class FormatExportAdapter:
    def __init__(self, formatter: StreamFormatter, stream: TextIO):
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
    def __init__(self, format, output, mode):
        self.format = format
        self.output = output
        self.mode = mode
        self.stream = None

    def __enter__(self):
        self.stream = open(self.output, self.mode) if self.output else sys.stdout
        fmt = get_export_format(self.format)
        if fmt is None:
            raise ScootExportFormatError(self.format)

        return FormatExportAdapter(fmt, self.stream)

    def __exit__(self, exc_type, exc_val, exc_tb):
        if self.stream and not self.output:
            self.stream.close()

        return False
