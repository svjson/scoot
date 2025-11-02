from scoot_core import config, metadata, query, OperationContext
from scoot_core.model import ListDataAdapter
from scoot_core.export import exporter
from scoot_cli.output import AsciiTable
from sqlalchemy import select


def _dump_single_column_table(header: str, values: list[str]):
    """Create a printable single-row ascii table from a header name and list of values"""
    ascii_table = AsciiTable(ListDataAdapter([header], [[v] for v in values]))
    ascii_table.dump(print)


def list_connections(context_name: str | None):
    ctx = config.Context.load(context_name)
    _dump_single_column_table("Connections", ctx.list_connection_names())


def list_contexts():
    _dump_single_column_table("Contexts", config.Context.list())


def use_context(context_name: str):
    config.Context.use(context_name)
    print(f"Using context '{context_name}'")


def set_default_connection(context_name: str | None, conn_name: str) -> None:
    """Set the default configuration `conn_name`."""
    ctx = config.Context.load(context_name)
    ctx.set_default_connection(conn_name)
    ctx.persist()
    print(f"Set default connection to '{conn_name}' for context '{ctx.name}")


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


def execute_query(ctx: OperationContext, query_str: str, **kwargs) -> None:
    """Execute a query"""
    result = query.execute(ctx, query_str)

    mode = "w"
    to_file = None
    format = kwargs.get("output_format") or "ascii"
    dialect = ctx.connection.engine.dialect

    if format != "ascii":
        result.metadata = metadata.resolve_query_metadata(ctx, query_str)

        tables = [
            metadata.describe_table(ctx, table_name)
            for table_name in result.get_table_names()
        ]

        with exporter(format, to_file, mode) as exp:
            exp.start()
            exp.rows(dialect, tables, result)
            exp.end()

    else:
        ascii_table = AsciiTable.from_result_set(result)
        ascii_table.dump(print)


def export_table(ctx: OperationContext, table_name: str, **kwargs) -> None:
    """Export table"""
    format = kwargs.get("format") or "ddl"
    mode = "w"
    output = kwargs.get("to_file", None)
    include_create = kwargs.get("include_create", True)
    include_data = kwargs.get("include_data", False)

    with exporter(format, output, mode) as exp:
        table = metadata.describe_table(ctx, table_name)

        exp.start()
        if include_create:
            exp.table(table)

        if include_data and table.sa_table is not None:
            stmt = select(table.sa_table)
            query_str = stmt.compile(dialect=ctx.connection.engine.dialect)
            result = query.execute(ctx, str(query_str))
            exp.rows(ctx.connection.engine.dialect, [table], result)

        exp.end()
