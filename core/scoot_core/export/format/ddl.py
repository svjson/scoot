from typing import TextIO
from ...model import ResultSet, TableModel
from .. import register_formatter
from sqlalchemy import Dialect, Table, insert


@register_formatter("ddl")
class DDLFormatter:
    def start(self, stream):
        pass

    def table(self, stream, table: TableModel):
        stream.write(str(table.create_stmt))

    def row(self, stream: TextIO, dialect: Dialect, table: Table, record: dict):
        stmt = insert(table).values(**record)
        stream.write(str(stmt.compile(dialect=dialect)))

    def rows(self, stream, dialect, table, resultset: ResultSet):
        stmt = insert(table).values(
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
        pass
