from types import FunctionType, LambdaType
from scoot_core.connection import Connection
from scoot_core import config
from scoot_core.opcontext import OperationContext

from . import commands
from .clibuilder import ScootCLI

handlers = {
    "connection": {
        "list": lambda _1, args: commands.list_connections(
            getattr(args, "c", None)
        ),
        "set-default": lambda _, args: commands.set_default_connection(
            getattr(args, "c", None), args.connection_name
        ),
    },
    "context": {
        "list": lambda _1, _2: commands.list_contexts(),
        "use": lambda _, args: commands.use_context(args.context_name),
    },
    "table": {
        "list": lambda ctx, _: commands.list_tables(ctx),
        "describe": lambda ctx, args: commands.describe_table(ctx, args.table_name),
        "export": lambda ctx, args: commands.export_table(
            ctx,
            args.table_name,
            **{"include_data": args.include_data, "to_file": args.o},
        ),
    },
    "db": {"list": lambda ctx, _: commands.list_databases(ctx)},
    "schema": {"list": lambda ctx, _: commands.list_schemas(ctx)},
    "query": lambda ctx, args: commands.execute_query(ctx, args.sql),
}


def run_command(scoot: ScootCLI, ctx, args):
    handler = None
    if hasattr(args, "resource"):
        handler = handlers.get(args.resource)

    if hasattr(args, "verb"):
        handler = (
            handler.get(args.verb)
            if handler is not None
            else handlers.get(args.verb)
        )

    if not isinstance(handler, (FunctionType, LambdaType)):
        scoot.error(f"No handler for {args.resource} {args.verb}")

    handler(ctx, args)


def main():

    scoot = ScootCLI()

    connection = scoot.resource("connection").description("Connection operations")
    connection.verb("list")
    connection.verb("set-default").argument("connection_name")

    context = scoot.resource("context").description("Context operations")
    context.verb("list")
    context.verb("use").argument("context_name")

    table = (
        scoot.resource("table").require_connection().description("Table operations")
    )
    table.verb("list")
    table.verb("describe").argument("table_name")
    table.verb("export").argument("table_name").flag("--include-data")

    db = (
        scoot.resource("db").require_connection().description("Database operations")
    )
    db.verb("list")

    schema = (
        scoot.resource("schema")
        .require_connection()
        .description("Schema operations")
    )
    schema.verb("list")

    scoot.verb("query").require_connection().description(
        "Query execution"
    ).argument("sql", "The SQL query to execute")

    args = scoot.parse()

    opctx: OperationContext | None = None

    if args.resource != "connection" and args.resource != "context":
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
        opctx = OperationContext(conn)

    run_command(scoot, opctx, args)


if __name__ == "__main__":
    main()
