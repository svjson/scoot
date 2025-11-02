from typing import IO, override

from scoot_core.export.format.stream import StreamFormatter
from ...model import ResultSet, TableModel
from .. import register_formatter
from sqlalchemy import Dialect, insert


def _format_insert(sql: str) -> str:
    """Format a SQL INSERT statement by splitting value tuples onto new lines.

    Works safely as long as parentheses are balanced within VALUES(...).
    """
    prefix, values_part = sql.split("VALUES", 1)
    tuples = []
    depth = 0
    current = []
    indent = 2

    i = 0
    while i < len(values_part):
        ch = values_part[i]
        current.append(ch)
        if ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
            if depth == 0:
                j = i + 1
                while j < len(values_part) and values_part[j] in " \t\n,":
                    if values_part[j] == ",":
                        current.append(values_part[j])
                        break
                    j += 1
                tuples.append("".join(current).strip())
                current = []
                i = j
        i += 1
    indent_str = " " * indent
    formatted = "\n".join(f"{indent_str}{t}" for t in tuples)
    return f"{prefix}VALUES\n{formatted}\n"


@register_formatter("ddl")
class DDLFormatter(StreamFormatter):
    @override
    def table(self, stream: IO, table_model: TableModel):
        stream.write(str(table_model.create_stmt))

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
            _format_insert(
                str(
                    stmt.compile(
                        dialect=dialect, compile_kwargs={"literal_binds": True}
                    )
                )
            )
        )

    def end(self, stream):
        stream.write("\n")
        pass
