import traceback
import logging
from decimal import Decimal
from functools import wraps

import orjson
from flask import Flask, Response, current_app, request, g
from scoot_core import config, metadata, query, ScootErrorType
from .context import RequestContext, ServerOperation
from scoot_core.exceptions import (
    ScootApplicationError,
    ScootConnectionException,
    ScootDriverException,
    ScootError,
)

from scoot_server import connmgr

app = Flask(__name__)

logging.basicConfig(
    level=logging.INFO, format="%(asctime)s [%(levelname)s] %(message)s"
)

config.is_server = True


def default_serializer(obj):
    if isinstance(obj, Decimal):
        return float(obj)

    print(f"Type {type(obj)} is not serializable, defaulting to str: \"{obj}\"")
    return str(obj)


def json_response(data, status=200):
    return Response(
        response=orjson.dumps(data, default=default_serializer),
        status=status,
        content_type="application/json",
    )


def error_response(scoot_error, status: int | None = None):
    if status is None:
        if scoot_error.type == ScootErrorType.NOT_FOUND:
            status = 404
        else:
            status = 500

    return json_response(
        {
            "status": "error",
            "error": scoot_error.error,
            "message": scoot_error.message,
        }
        | scoot_error.additional,
        status,
    )


def with_error_handler(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        try:
            return func(*args, **kwargs)
        except ScootError as se:
            return error_response(se)
        except ModuleNotFoundError as mnfe:
            return error_response(
                ScootDriverException(
                    "missing-driver",
                    f"Required module not installed: {mnfe.name}",
                    mnfe.name,
                )
            )
        except Exception as e:
            traceback.print_exc()
            return error_response(ScootApplicationError(e), 500)

    return wrapper


def with_connection(func):
    @wraps(func)
    def wrapper(conn, *args, **kwargs):
        connection = connmgr.get_connection(conn)
        if connection is None:
            configured_conn = config.app_config.connections.get(conn, None)
            if configured_conn is None:
                raise ScootConnectionException(
                    "unknown-connection", f"Unknown connection: '{conn}'", conn
                )
            conn_url = configured_conn["url"]
            print(f"Creating connection using url: {conn_url}")

            connection = connmgr.create_connection(conn, configured_conn["url"])

        return func(connection, *args, **kwargs)

    return wrapper


def with_request_context(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        ctx = RequestContext(request.path)
        g.reqctx = ctx
        try:
            result = func(ctx, *args, **kwargs)
        finally:
            ctx.root_span.stop()
            lines = ctx.format_tree()
            current_app.logger.info("\n".join(lines))
        return result

    return wrapper


@app.route("/ping", methods=["GET"])
def ping():
    return json_response({"status": "ok"})


@app.route("/api/contexts", methods=["GET"])
@with_error_handler
def get_contexts():
    return json_response(
        {
            "contexts": {
                ctx_name: {
                    "connections": {
                        conn_name: {}
                        for conn_name in config.Context.load(
                            ctx_name
                        ).list_connection_names()
                    }
                }
                for ctx_name in config.Context.list()
            }
        }
    )


@app.route("/api/connection", methods=["POST"])
@with_error_handler
def create_connection():
    data = request.get_json()
    url = data.get("url", None)
    name = data.get("name", "default")
    persist = data.get("persist", False)

    if url is None:
        return error_response(
            ScootConnectionException(
                "no-connection-details", "No connection url provided", name
            )
        )

    conn = connmgr.create_connection(name, url)

    if persist:
        config.app_config.connections[name] = {"url": url}
        config.persist()

    return json_response({"status": "ok", "connection": conn.to_dict()})


@app.route("/api/connection", methods=["GET"])
@with_error_handler
def get_connections():
    result = {
        "connections": {
            n: {"status": "inactive"}
            for n, _ in config.app_config.connections.items()
        }
    }

    for n, c in connmgr.connections.items():
        result["connections"][n] = c.to_dict()

    return json_response(result)


@app.route("/api/<string:conn>/tables", strict_slashes=False, methods=["GET"])
@with_error_handler
@with_connection
@with_request_context
def list_tables(ctx, connection):
    opctx = ServerOperation(ctx, connection)
    return json_response({"tables": metadata.list_tables(opctx)})


@app.route("/api/<string:conn>/tables/<string:table_name>", methods=["GET"])
@with_error_handler
@with_connection
@with_request_context
def describe_table(ctx, connection, table_name):
    opctx = ServerOperation(ctx, connection)
    return json_response(metadata.describe_table(opctx, table_name).to_dict())


@app.route("/api/<string:conn>/databases", methods=["GET"])
@with_error_handler
@with_connection
@with_request_context
def list_databases(ctx, connection):
    opctx = ServerOperation(ctx, connection)
    return json_response({"databases": metadata.list_databases(opctx)})


@app.route("/api/<string:conn>/schemas", methods=["GET"])
@with_error_handler
@with_connection
@with_request_context
def list_schemas(ctx, connection):
    opctx = ServerOperation(ctx, connection)
    return json_response({"schemas": metadata.list_schemas(opctx)})


@app.route("/api/<string:conn>/query", methods=["POST"])
@with_error_handler
@with_connection
@with_request_context
def query_operation(ctx, connection):

    opctx = ServerOperation(ctx, connection)
    data = request.get_json()

    with ctx.span("parse request"):
        sql = data.get("sql")
        include_metadata = data.get("metadata", False)
        action = data.get("action", {"action": "execute"})

    result = query.perform_action(opctx, sql, action)

    with ctx.span("resolve_metadata"):
        if result.metadata and (new_stmt := result.metadata.get("stmt")):
            sql = new_stmt

        query_metadata = (
            metadata.resolve_query_metadata(opctx, sql)
            if include_metadata
            else None
        )

        if query_metadata:
            result.metadata = (result.metadata or {}) | query_metadata

    with ctx.span("serialize"):
        response = json_response(result.to_dict())

    return response
