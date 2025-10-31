import json
from pprint import pprint

from scoot_core import metadata, query, OperationContext
from scoot_core.connection import Connection
from scoot_core.model import ListDataAdapter
from scoot_cli.output import AsciiTable


def _dump_single_column_table(header: str, values: list[str]):
    """Create a printable single-row ascii table from a header name and list of values"""
    ascii_table = AsciiTable(ListDataAdapter([header], [[v] for v in values]))
    ascii_table.dump(print)


def list_tables(ctx: OperationContext) -> None:
    """List available tables."""
    tables = metadata.list_tables(ctx)
    _dump_single_column_table("Table Name", sorted(tables))


def list_databases(ctx: OperationContext) -> None:
    """List available databases."""
    databases = metadata.list_databases(ctx)
    _dump_single_column_table("Database Name", databases)


def list_schemas(ctx: OperationContext) -> None:
    """List available schemas."""
    schemas = metadata.list_schemas(ctx)
    _dump_single_column_table("Schema Name", schemas)


def describe_table(ctx: OperationContext, table_name: str) -> None:
    """Describe a named table"""
    table = metadata.describe_table(ctx, table_name)

    ascii_table = AsciiTable.from_table_model(table)
    ascii_table.dump(print)


def execute_query(ctx: OperationContext, query_str: str) -> None:
    """Execute a query"""
    result = query.execute(ctx, query_str)

    ascii_table = AsciiTable.from_result_set(result)
    ascii_table.dump(print)

def export_table(ctx: OperationContext, table_name: str) -> None:
    """Export table"""
    fmt = get_export_format("ddl")

    table = metadata.describe_table(ctx, table_name)

