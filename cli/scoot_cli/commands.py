import json
from pprint import pprint

from scoot_core import metadata, query
from scoot_core.connection import Connection
from scoot_core.model import ListDataAdapter
from scoot_cli.output import AsciiTable


def _dump_single_column_table(header: str, values: list[str]):
    """Create a printable single-row ascii table from a header name and list of values"""
    ascii_table = AsciiTable(ListDataAdapter([header], [[v] for v in values]))
    ascii_table.dump(print)


def list_tables(conn: Connection) -> None:
    """List available tables."""
    tables = metadata.list_tables(conn)
    _dump_single_column_table("Table Name", sorted(tables))


def list_databases(conn: Connection) -> None:
    """List available databases."""
    databases = metadata.list_databases(conn)
    _dump_single_column_table("Database Name", databases)


def list_schemas(conn: Connection) -> None:
    """List available schemas."""
    schemas = metadata.list_schemas(conn)
    _dump_single_column_table("Schema Name", schemas)


def describe_table(conn: Connection, table_name: str) -> None:
    """Describe a named table"""
    table = metadata.describe_table(conn, table_name)

    ascii_table = AsciiTable.from_table_model(table)
    ascii_table.dump(print)


def execute_query(conn: Connection, query_str: str) -> None:
    """Execute a query"""
    result = query.execute(conn, query_str)

    ascii_table = AsciiTable.from_result_set(result)
    ascii_table.dump(print)
