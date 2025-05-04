import orjson
import traceback
from functools import wraps
from flask import Flask, request, Response

from scoot_server import connmgr
from scoot_core import metadata, query, config

app = Flask(__name__)


def json_response(data, status=200):
    return Response(
        response=orjson.dumps(data), status=status, content_type="application/json"
    )


def error_response(msg, status=400, error_context={}):
    return json_response(
        {"status": "error", "message": msg} | error_context, status
    )


def module_not_found_error_response(module_name):
    msg = f"Required module not installed: {module_name}"
    print(msg)
    return error_response(
        msg, 503, {"error": "missing-driver", "driver": module_name}
    )


def with_connection(func):
    @wraps(func)
    def wrapper(conn, *args, **kwargs):
        try:
            connection = connmgr.get_connection(conn)
            if connection is None:
                configured_conn = config.app_config.connections[conn]
                if configured_conn is None:
                    return error_response(f"Unknown connection: '{conn}'")
                connection = connmgr.create_connection(conn, configured_conn["url"])
            return func(connection, *args, **kwargs)
        except ModuleNotFoundError as mnfe:
            return module_not_found_error_response(mnfe.name)
        except Exception as e:
            traceback.print_exc()
            return error_response(f"{e}")

    return wrapper


@app.route("/ping", methods=["GET"])
def ping():
    return json_response({"status": "ok"})


@app.route("/api/connection", methods=["POST"])
def create_connection():
    data = request.get_json()
    url = data.get("url", None)
    name = data.get("name", "default")
    persist = data.get("persist", False)

    if url is None:
        return error_response("No connection url provided")
    try:
        connmgr.create_connection(name, url)

        if persist:
            config.app_config.connections[name] = {"url": url}
            config.persist()

        return json_response({"status": "ok"})
    except ModuleNotFoundError as mnfe:
        return module_not_found_error_response(mnfe.name)
    except Exception as e:
        traceback.print_exc()
        return error_response(f"{e}")


@app.route("/api/connection", methods=["GET"])
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


@app.route("/api/<string:conn>/tables", methods=["GET"])
@with_connection
def list_tables(connection):
    return json_response({"tables": metadata.list_tables(connection)})


@app.route("/api/<string:conn>/tables/<string:table_name>", methods=["GET"])
@with_connection
def describe_table(connection, table_name):
    return json_response(metadata.describe_table(connection, table_name).to_dict())


@app.route("/api/<string:conn>/databases", methods=["GET"])
@with_connection
def list_databases(connection):
    return json_response({"databases": metadata.list_databases(connection)})


@app.route("/api/<string:conn>/schemas", methods=["GET"])
@with_connection
def list_schemas(connection):
    return json_response({"schemas": metadata.list_schemas(connection)})


@app.route("/api/<string:conn>/query", methods=["POST"])
@with_connection
def run_query(connection):
    data = request.get_json()
    sql = data.get("sql")
    include_metadata = data.get("metadata", False)

    result = query.execute(connection, sql)
    result.metadata = (
        metadata.resolve_query_metadata(connection, sql)
        if include_metadata
        else None
    )

    return json_response(result.to_dict())
