from scoot_core import OperationEnv, config, metadata, query
from scoot_core.export import exporter
from scoot_core.model import ListDataAdapter
from sqlalchemy import select

from scoot_cli.output import AsciiTable


def _dump_single_column_table(header: str, values: list[str]):
    """Create a printable single-row ascii table from a header name and list of values"""
    ascii_table = AsciiTable(ListDataAdapter([header], [[v] for v in values]))
    ascii_table.dump(print)


def list_connections(context_name: str | None):
    ctx = config.Context.load(context_name)
    _dump_single_column_table("Connections", ctx.list_connection_names())


def list_contexts():
    _dump_single_column_table("Contexts", config.Context.list())


def show_current_context():
    ctx = config.Context.load(None)
    print(f"Current context: {ctx.name}")
    print(f"Default connection: {ctx.get_default_connection_name()}")


def use_context(context_name: str):
    config.Context.use(context_name)
    print(f"Using context '{context_name}'")


def set_default_connection(context_name: str | None, conn_name: str) -> None:
    """Set the default configuration `conn_name`."""
    ctx = config.Context.load(context_name)
    ctx.set_default_connection(conn_name)
    ctx.persist()
    print(f"Set default connection to '{conn_name}' for context '{ctx.name}")


def list_tables(op_env: OperationEnv) -> None:
    """List available tables."""
    tables = metadata.list_tables(op_env)
    _dump_single_column_table(
        "Table Name", sorted([table[0] for table in tables["tables"]])
    )


def list_databases(op_env: OperationEnv) -> None:
    """List available databases."""
    databases = metadata.list_databases(op_env)
    _dump_single_column_table("Database Name", databases)


def list_schemas(op_env: OperationEnv) -> None:
    """List available schemas."""
    schemas = metadata.list_schemas(op_env)
    _dump_single_column_table("Schema Name", schemas)


def describe_table(op_env: OperationEnv, table_name: str, **kwargs) -> None:
    """Describe a named table"""
    table = metadata.describe_table(op_env, table_name)

    format = kwargs.get("output_format") or "ascii"

    with exporter(format) as exp:
        exp.start()
        exp.table(table)
        exp.end()


def execute_query(op_env: OperationEnv, query_str: str, **kwargs) -> None:
    """Execute a query"""
    result = query.execute(op_env, query_str)

    mode = "w"
    to_file = None
    format = kwargs.get("output_format") or "ascii"
    dialect = op_env.connection.engine.dialect

    result.metadata = metadata.resolve_query_metadata(op_env, query_str)

    tables = [
        metadata.describe_table(op_env, table_name)
        for table_name in result.get_table_names()
    ]

    with exporter(format, to_file, mode) as exp:
        exp.start()
        exp.rows(dialect, tables, result)
        exp.end()


def export_table(op_env: OperationEnv, table_name: str, **kwargs) -> None:
    """Export table"""
    format = kwargs.get("output_format") or "ddl"
    mode = "w"
    to_file = kwargs.get("to_file", None)
    include_create = kwargs.get("include_create", True)
    include_data = kwargs.get("include_data", False)

    with exporter(format, to_file, mode) as exp:
        table = metadata.describe_table(op_env, table_name)

        exp.start()
        if include_create:
            exp.table(table)

        if include_data and table.sa_table is not None:
            stmt = select(table.sa_table)
            query_str = stmt.compile(dialect=op_env.connection.engine.dialect)
            result = query.execute(op_env, str(query_str))
            exp.rows(op_env.connection.engine.dialect, [table], result)

        exp.end()
