import traceback
from decimal import Decimal
from functools import wraps

import orjson
from flask import Flask, Response, request
from scoot_core import config, metadata, query
from scoot_core.exceptions import (
    ScootApplicationError,
    ScootConnectionException,
    ScootDriverException,
    ScootError,
)

from scoot_server import connmgr

app = Flask(__name__)


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


def error_response(scoot_error, status=400):
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


@app.route("/ping", methods=["GET"])
def ping():
    return json_response({"status": "ok"})


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
def list_tables(connection):
    return json_response({"tables": metadata.list_tables(connection)})


@app.route("/api/<string:conn>/tables/<string:table_name>", methods=["GET"])
@with_error_handler
@with_connection
def describe_table(connection, table_name):
    return json_response(metadata.describe_table(connection, table_name).to_dict())


@app.route("/api/<string:conn>/databases", methods=["GET"])
@with_error_handler
@with_connection
def list_databases(connection):
    return json_response({"databases": metadata.list_databases(connection)})


@app.route("/api/<string:conn>/schemas", methods=["GET"])
@with_error_handler
@with_connection
def list_schemas(connection):
    return json_response({"schemas": metadata.list_schemas(connection)})


@app.route("/api/<string:conn>/query", methods=["POST"])
@with_error_handler
@with_connection
def query_operation(connection):
    data = request.get_json()
    sql = data.get("sql")
    include_metadata = data.get("metadata", False)
    action = data.get("action", {"action": "execute"})

    result = query.perform_action(connection, sql, action)

    if result.metadata and (new_stmt := result.metadata.get("stmt")):
        sql = new_stmt

    query_metadata = (
        metadata.resolve_query_metadata(connection, sql)
        if include_metadata
        else None
    )

    if query_metadata:
        result.metadata = (result.metadata or {}) | query_metadata

    return json_response(result.to_dict())
