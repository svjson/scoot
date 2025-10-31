import argparse
from scoot_cli import commands
from scoot_core.connection import Connection
from scoot_core import config
from scoot_core.opcontext import OperationContext

handlers = {
    "table": {
        "list": lambda ctx, _: commands.list_tables(ctx),
        "describe": lambda ctx, args: commands.describe_table(
            ctx, args.table_name
        ),
    },
    "db": {"list": lambda ctx, _: commands.list_databases(ctx)},
    "schema": {"list": lambda ctx, _: commands.list_schemas(ctx)},
    "query": lambda ctx, args: commands.execute_query(ctx, args.sql),
}


def main():
    # Root parser for the Scoot CLI tool, allowing us to capture non-command
    # specific flags either before or after the main command args
    root_parser = argparse.ArgumentParser(prog="scoot", add_help=False)
    root_parser.add_argument("--url", help="Database URL", required=False)
    root_parser.add_argument("-c", help="Configuration name", required=False)

    root_args, remaining_args = root_parser.parse_known_args()

    # Actual argument parser for the main command
    parser = argparse.ArgumentParser(prog="scoot", parents=[root_parser])
    subparsers = parser.add_subparsers(dest="command", required=True)

    # Sub-parser for 'table' commands
    table_parser = subparsers.add_parser("table", help="Table operations")
    table_action_parser = table_parser.add_subparsers(dest="action")

    table_action_parser.add_parser("list")
    describe_parser = table_action_parser.add_parser("describe")
    describe_parser.add_argument("table_name")

    # Sub-parser for 'db' commands
    db_parser = subparsers.add_parser("db", help="Database operations")
    db_action_parser = db_parser.add_subparsers(dest="action")
    db_action_parser.add_parser("list")

    # Sub-parser for 'schema' commands
    schema_parser = subparsers.add_parser("schema", help="Schema operations")
    schema_action_parser = schema_parser.add_subparsers(dest="action")
    schema_action_parser.add_parser("list")

    # Sub-parser for 'query' commands
    query_parser = subparsers.add_parser("query", help="Query execution")
    query_parser.add_argument("sql", help="The SQL query to execute")

    # Parse the remaining arguments not captured by the root parser
    args = parser.parse_args(remaining_args)

    url = root_args.url
    cfg_name = root_args.c or config.default_config_name
    config.configure(cfg_name)

    if url is None:
        default_conn = config.app_config.connections.get("default", None)
        if default_conn:
            url = default_conn["url"]
        else:
            parser.error(
                "No connection URL provided and no default connection configured."
            )

    conn = Connection(url)
    opctx = OperationContext(conn)

    base_handler = handlers.get(args.command, {})

    if hasattr(args, "action"):
        action_handler = base_handler.get(args.action)
        if action_handler is None:
            parser.error("No handler for {args.command} {args.action}")
        action_handler(opctx, args)
    else:
        base_handler(opctx, args)


if __name__ == "__main__":
    main()
