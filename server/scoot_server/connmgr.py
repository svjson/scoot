from scoot_core.connection import Connection
from typing import Any, Optional
from scoot_core.config import Context

connections: dict[str, dict[str, Connection]] = {}


def _get_connection_context(ctx: str):
    global connections
    context = connections.get(ctx, None)
    if not context:
        context = {}
        connections[ctx] = context
    return context


def create_connection(ctx: str, name: str, url: str) -> Connection:
    conn = Connection(url)
    global connections
    context = _get_connection_context(ctx)
    if context.get(name, None):
        context[name].close()
    context[name] = conn

    return conn


def get_connection(ctx: str, name: str) -> Optional[Connection]:
    global connections
    context = _get_connection_context(ctx)
    return context.get(name, None)


def get_connections() -> dict[str, dict[str, dict[str, Any]]]:
    global connections
    manifest = Context.load_manifest()
    result = {
        ctx_name: {
            "connections": {
                conn_name: {
                    "status": ("active" if conn else "inactive"),
                    **(conn.to_dict() if conn else {}),
                }
                for conn_name in ctx.get("connections", {}).keys()
                if (conn := get_connection(ctx_name, conn_name)) or True
            }
        }
        for ctx_name, ctx in manifest.items()
    }

    for ctx_name in connections.keys():
        result_ctx = result.get(ctx_name)
        if not result_ctx:
            result_ctx = {"connections": {}}
            result[ctx_name] = result_ctx
        result_ctx_conns = result_ctx.get("connections", {})
        ctx_conns = connections[ctx_name]
        for conn_name, conn in ctx_conns.items():
            result_conn = result_ctx_conns.get(conn_name)
            if not result_conn:
                result_ctx_conns[conn_name] = {"status": "active", **conn.to_dict()}

    return result
