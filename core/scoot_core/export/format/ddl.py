from typing import IO, override

from scoot_core.export.format.stream import StreamFormatter
from ...model import ResultSet, TableModel
from .. import register_formatter
from sqlalchemy import Dialect, insert


@register_formatter("ddl")
class DDLFormatter(StreamFormatter):
    @override
    def table(self, stream, table: TableModel):
        stream.write(str(table.create_stmt))

    @override
    def row(
        self, stream: IO, dialect: Dialect, table_model: TableModel, record: dict
    ):
        table = self._verify_table(table_model)
        stmt = insert(table).values(**record)
        stream.write(str(stmt.compile(dialect=dialect)))

    @override
    def rows(
        self,
        stream: IO,
        dialect: Dialect,
        table_models: list[TableModel],
        resultset: ResultSet,
    ):
        """Generates INSERT statements for each rows of the argument
        ResultSet.

        FIXME: While all participating tables should be accounted for
        in the argument `table_models` the current implementation does
        not sort out values in the correct table.
        """
        tables = self._verify_tables(table_models)
        stmt = insert(tables[0]).values(
            [dict(zip(resultset.columns, row)) for row in resultset.rows]
        )
        stream.write(
            str(
                stmt.compile(
                    dialect=dialect, compile_kwargs={"literal_binds": True}
                )
            )
        )

    def end(self, stream):
        stream.write("\n")
        pass
