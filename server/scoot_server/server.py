import traceback
import logging
from decimal import Decimal
from functools import wraps

import orjson
from flask import Flask, Response, current_app, request, g
from scoot_core import Cache, config, metadata, query, ScootErrorType
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


_cache: dict[str, dict[str, dict[str, Cache]]] = {}


def get_cache(ctx, conn) -> dict[str, Cache]:
    ctx_cache = _cache.get(ctx, None)
    if not ctx_cache:
        ctx_cache = {}
        _cache[ctx] = ctx_cache
    conn_cache = ctx_cache.get(conn, None)
    if not conn_cache:
        conn_cache = {}
        ctx_cache[conn] = conn_cache

    return conn_cache


def get_connection(ctx, conn):
    connection = connmgr.get_connection(ctx, conn)
    if connection is None:
        context = (
            config.Context.load(ctx)
            if config.Context.exists(ctx)
            else config.Context(ctx, {"connections": {}})
        )
        configured_conn = context.get_connection(conn)
        if configured_conn is None:
            raise ScootConnectionException(
                "unknown-connection", f"Unknown connection: '{conn}'", conn
            )
        conn_url = configured_conn["url"]
        connection = connmgr.create_connection(ctx, conn, conn_url)
        print(f"Opened connection {ctx}/{conn}: {str(connection)}")

    return connection


def with_op_env(func):
    @wraps(func)
    def wrapper(ctx: str, conn: str, *args, **kwargs):
        connection = get_connection(ctx, conn)
        req_env = RequestContext(request.path)
        cache = get_cache(ctx, conn)
        op_env = ServerOperation(req_env, connection, cache)
        try:
            result = func(op_env=op_env, *args, **kwargs)
        finally:
            op_env.end_operations()
            lines = op_env.reqenv.format_tree()
            current_app.logger.info("\n".join(lines))
        return result

    return wrapper


@app.route("/ping", methods=["GET"])
def ping():
    return json_response({"status": "ok"})


@app.route("/api/connection-manager", methods=["GET"])
@with_error_handler
def get_connections():
    return json_response({"contexts": connmgr.get_connections()})


@app.route("/api/contexts", methods=["GET"])
@with_error_handler
def get_contexts():
    return json_response({"contexts": config.Context.load_manifest()})


@app.route("/api/contexts/<string:ctx>/connections", methods=["POST"])
@with_error_handler
def create_connection(ctx: str):
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

    conn = connmgr.create_connection(ctx, name, url)

    if persist:
        config.app_config.connections[name] = {"url": url}
        config.persist()

    return json_response({"status": "ok", "connection": conn.to_dict()})


@app.route(
    "/api/contexts/<string:ctx>/connections/<string:conn>/tables",
    strict_slashes=False,
    methods=["GET"],
)
@with_error_handler
@with_op_env
def list_tables(op_env):
    return json_response({"tables": metadata.list_tables(op_env)})


@app.route(
    "/api/contexts/<string:ctx>/connections/<string:conn>/tables/<string:table_name>",
    methods=["GET"],
)
@with_error_handler
@with_op_env
def describe_table(op_env, table_name):
    return json_response(metadata.describe_table(op_env, table_name).to_dict())


@app.route(
    "/api/contexts/<string:ctx>/connections/<string:conn>/databases",
    methods=["GET"],
)
@with_error_handler
@with_op_env
def list_databases(op_env):
    return json_response({"databases": metadata.list_databases(op_env)})


@app.route(
    "/api/contexts/<string:ctx>/connections/<string:conn>/schemas", methods=["GET"]
)
@with_error_handler
@with_op_env
def list_schemas(op_env):
    return json_response({"schemas": metadata.list_schemas(op_env)})


@app.route(
    "/api/contexts/<string:ctx>/connections/<string:conn>/query", methods=["POST"]
)
@with_error_handler
@with_op_env
def query_operation(op_env: ServerOperation):

    with op_env.operation("parse request"):
        data = request.get_json()
        sql = data.get("sql")
        include_metadata = data.get("metadata", False)
        action = data.get("action", {"action": "execute"})

    result = query.perform_action(op_env, sql, action)

    with op_env.operation("resolve_metadata"):
        if result.metadata and (new_stmt := result.metadata.get("stmt")):
            sql = new_stmt

        query_metadata = (
            metadata.resolve_query_metadata(op_env, sql)
            if include_metadata
            else None
        )

        if query_metadata:
            result.metadata = (result.metadata or {}) | query_metadata

    with op_env.operation("serialize"):
        response = json_response(result.to_dict())

    return response
