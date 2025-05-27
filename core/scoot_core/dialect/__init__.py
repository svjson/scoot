import sqlalchemy

from . import registry
from ..connection import Connection


def sqlglot_dialect(dialect_name: str):
    if dialect_name == "postgresql":
        return "postgres"
    elif dialect_name == "mssql":
        return "tsql"

    return dialect_name


def find_and_apply_additional_constraints(
    conn: Connection, table: sqlalchemy.Table
):
    return registry.find_and_apply_additional_constraints(conn, table)
