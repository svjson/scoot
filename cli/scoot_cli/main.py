from types import FunctionType, LambdaType
from scoot_core.connection import Connection
from scoot_core import config
from scoot_core import OperationEnv

from . import commands
from .clibuilder import ScootCLI, require


def define_db(scoot: ScootCLI):
    # Resource - db
    db = (
        scoot.resource("db").require_connection().description("Database operations")
    )

    # Verb - list
    db.verb("list").command(
        lambda op_env, _: commands.list_databases(require(op_env))
    )


def define_connection(scoot: ScootCLI):
    # Resource - connection
    connection = scoot.resource("connection").description("Connection operations")

    # Verb - list
    connection.verb("list").command(
        lambda _1, args: commands.list_connections(getattr(args, "c", None))
    )

    # Verb - set-default
    connection.verb("set-default").argument("connection_name").command(
        lambda _, args: commands.set_default_connection(
            getattr(args, "c", None), args.connection_name
        )
    )


def define_context(scoot: ScootCLI):
    # Resource - context
    context = scoot.resource("context").description("Context operations")

    # Verb - list
    context.verb("list").command(lambda _1, _2: commands.list_contexts())

    # Verb - use
    context.verb("use").argument("context_name").command(
        lambda _, args: commands.use_context(args.context_name)
    )


def define_query(scoot: ScootCLI):
    # Verb - query
    scoot.verb("query").require_connection().description(
        "Query execution"
    ).argument("sql", "The SQL query to execute").option("-o").command(
        lambda op_env, args: commands.execute_query(
            require(op_env), args.sql, **{"output_format": args.o}
        )
    )


def define_schema(scoot: ScootCLI):
    # Resource - schema
    schema = (
        scoot.resource("schema")
        .require_connection()
        .description("Schema operations")
    )

    # Verb - list
    schema.verb("list").command(
        lambda op_env, _: commands.list_schemas(require(op_env))
    )


def define_table(scoot: ScootCLI):
    # Resource - table
    table = (
        scoot.resource("table").require_connection().description("Table operations")
    )

    # Verb - list
    table.verb("list").command(
        lambda op_env, _: commands.list_tables(require(op_env))
    )

    # Verb - describe
    table.verb("describe").argument("table_name").option("-o").command(
        lambda op_env, args: commands.describe_table(
            require(op_env), args.table_name, **{"output_format": args.o}
        )
    )

    # Verb - export
    table.verb("export").argument("table_name").flag("--include-data").option(
        "-o"
    ).option("-f").command(
        lambda op_env, args: commands.export_table(
            require(op_env),
            args.table_name,
            **{
                "include_data": args.include_data,
                "output_format": args.o,
                "to_file": args.f,
            },
        )
    )


def make_operation_env(scoot, args, requires_conn):
    """Create an OperationEnv if required by the command."""
    if requires_conn:
        url = args.url

        if url is None:
            context = config.Context.load(args.c)
            connection = context.get_default_connection()
            if connection:
                url = connection.get("url", None)

        if url is None:
            scoot.error(
                "No connection URL provided and no default connection configured."
            )

        conn = Connection(url)
        return OperationEnv(conn)


def main():
    """Main entry point for the CLI application."""
    scoot = ScootCLI()

    define_connection(scoot)
    define_context(scoot)
    define_db(scoot)
    define_schema(scoot)
    define_table(scoot)
    define_query(scoot)

    args, command, requires_conn = scoot.parse()

    op_env = make_operation_env(scoot, args, requires_conn)

    if command:
        command(op_env, args)


if __name__ == "__main__":
    main()
