from sqlalchemy import inspect, Table, MetaData
from sqlalchemy.exc import NoSuchTableError
from typing import Optional

from scoot_core import query
from scoot_core.connection import Connection
from scoot_core.model import TableModel


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
        return TableModel.from_sqlalchemy(table)
    except NoSuchTableError:
        print(f"No such table: {table_name}")
        return None
