from types import ModuleType
from typing import Optional

import sqlalchemy
from sqlalchemy.sql.type_api import TypeEngine
from sqlglot import exp

from .. import types
from ..connection import Connection
from . import mssql, mysql, oracle, postgres
from .sqlglot import sqlglot_dialect

_dialect_module: dict[str, ModuleType] = {
    "mariadb": mysql,
    "mssql": mssql,
    "mysql": mysql,
    "oracle": oracle,
    "postgresql": postgres,
}


def _module(dialect: str) -> ModuleType:
    module = _dialect_module.get(dialect, None)
    if not module:
        raise ValueError(f"Dialect '{dialect}' is not supported.")
    return module


def resolve_type(
    dialect: str,
    type: types.TypeAdapter | TypeEngine | exp.DataType | exp.ColumnDef,
) -> tuple[Optional[types.Type], Optional[str], Optional[str]]:
    module = _module(dialect)
    return module.type_mapper.resolve_type(
        types.type_adapter(type, sqlglot_dialect(dialect))
    )


def find_and_apply_additional_constraints(conn: Connection, table: sqlalchemy.Table):
    dialect = conn.get_dialect()
    module = _module(dialect)

    module.find_and_apply_additional_constraints(conn, table)
