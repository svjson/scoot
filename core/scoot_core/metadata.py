from typing import Optional

from sqlalchemy import inspect, Table, MetaData
from sqlalchemy.schema import CreateTable
from sqlalchemy.exc import NoSuchTableError
import sqlglot
from sqlglot import expressions as sge

from scoot_core import query
from scoot_core.connection import Connection
from scoot_core.model import TableModel
from scoot_core.exceptions import ScootSchemaException


def list_schemas(conn: Connection) -> list[str]:
    """Return a list of schema names."""
    inspector = inspect(conn.engine)
    schemas = inspector.get_schema_names()
    return schemas


def list_tables(conn: Connection) -> list[str]:
    """Return a list of table names."""
    inspector = inspect(conn.engine)
    tables = inspector.get_table_names()
    return tables


def list_databases(conn: Connection) -> list[str]:
    """Return a list of database names."""
    dialect = conn.engine.dialect.name

    if dialect == "mssql":
        resultset = query.execute(
            conn, "SELECT name FROM sys.databases WHERE HAS_DBACCESS(name) = 1"
        )
        return [str(row[0]) for row in resultset.rows]

    raise NotImplementedError(
        f"list databases is not currently supported for dialect '{dialect}'"
    )


def describe_table(conn: Connection, table_name: str) -> Optional[TableModel]:
    """Describe the structure of a database table"""
    inspector = inspect(conn.engine)

    try:
        table = Table(table_name, MetaData(), autoload_with=conn.engine)
        inspector.reflect_table(table, include_columns=None)

        table_model = TableModel.from_sqlalchemy(
            table, create_stmt=str(CreateTable(table).compile(conn.engine))
        )
        return table_model
    except NoSuchTableError:
        raise ScootSchemaException("table", table_name)


def resolve_query_metadata(conn: Connection, sql: str):

    expr = sqlglot.parse_one(sql)

    expr_tables = list(expr.find_all(sge.Table))
    known_tables = {}
    for tbl in expr_tables:
        known_tables[tbl.name] = describe_table(conn, tbl.name)

    columns = []

    for e in expr.expressions:
        name = e.alias_or_name
        table = getattr(e, "table", None)
        column = getattr(e, "name", None)

        if isinstance(e, sge.Star):
            if table is None and len(expr_tables) == 1:
                table = expr_tables[0].name
                table_model = known_tables.get(table)
                if table_model:
                    for c in table_model.columns:
                        columns.append(
                            {"name": c.name, "table": table, "column": c.name}
                        )
            continue
        elif table == "":
            if len(expr_tables) == 1:
                table = expr_tables[0].name

        columns.append({"name": name, "table": table, "column": column})

    columns = [
        (
            table_model.get_column(c.get("column")).to_dict() | c
            if (table_model := known_tables.get(c.get("table")))
            else c
        )
        for c in columns
    ]

    return {"columns": columns}
