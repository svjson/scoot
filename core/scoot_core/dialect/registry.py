from types import ModuleType
from typing import Optional, Type

from sqlalchemy import sql
import sqlalchemy
from sqlalchemy.sql.type_api import TypeEngine

from .mapper import TypeMapper
from . import mssql, mysql, oracle, postgres
from .. import types
from ..connection import Connection

_dialect_module: dict[str, ModuleType] = {
    "mariadb": mysql,
    "mssql": mssql,
    "mysql": mysql,
    "oracle": oracle,
    "postgresql": postgres,
}

_dialect_mapper_type_inst: dict[str, TypeMapper] = {}


def resolve_type(
    dialect: str, sqatype: TypeEngine
) -> tuple[Optional[types.Type], Optional[str], Optional[str]]:
    mapper = _dialect_mapper_type_inst.get(dialect, None)
    if not mapper:
        module = _dialect_module.get(dialect, None)
        if not module or not module.TypeMapperImpl:
            raise ValueError(f"Dialect '{dialect}' is not supported.")
        else:
            mapper = module.TypeMapperImpl()
            _dialect_mapper_type_inst[dialect] = mapper

    return mapper.resolve_type(sqatype)


def find_and_apply_additional_constraints(
    conn: Connection, table: sqlalchemy.Table
):
    dialect = conn.get_dialect()
    module = _dialect_module.get(dialect, None)
    if not module:
        raise ValueError(f"Dialect '{dialect}' is not supported.")

    module.find_and_apply_additional_constraints(conn, table)
