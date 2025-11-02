from typing import IO, override

from scoot_cli.output import AsciiTable
from scoot_core.export.format.stream import StreamFormatter
from ...model import ResultSet, TableModel
from .. import register_formatter
from sqlalchemy import Dialect


@register_formatter("ascii")
class AsciiFormatter(StreamFormatter):

    @override
    def table(self, stream: IO, table_model: TableModel):
        AsciiTable.from_table_model(table_model).dump(
            lambda line: stream.write(line + "\n")
        )

    @override
    def rows(
        self,
        stream: IO,
        dialect: Dialect,
        table_models: list[TableModel],
        resultset: ResultSet,
    ) -> None:
        AsciiTable.from_result_set(resultset).dump(
            lambda line: stream.write(line + "\n")
        )
