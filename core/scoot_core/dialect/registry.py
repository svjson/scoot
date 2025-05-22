from typing import Optional, Type

from sqlalchemy.sql.type_api import TypeEngine

from .mapper import TypeMapper
from .mssql import MSSQLTypeMapper
from .mysql import MySQLTypeMapper
from .oracle import OracleTypeMapper
from .postgres import PostgresTypeMapper

from .. import types

_dialect_type_impl: dict[str, Type[TypeMapper]] = {
    "mariadb": MSSQLTypeMapper,
    "mssql": MSSQLTypeMapper,
    "mysql": MySQLTypeMapper,
    "oracle": OracleTypeMapper,
    "postgresql": PostgresTypeMapper,
}

_dialect_type_inst: dict[str, TypeMapper] = {}


def resolve_type(
    dialect: str, sqatype: TypeEngine
) -> tuple[Optional[types.Type], Optional[str], Optional[str]]:
    mapper = _dialect_type_inst.get(dialect, None)
    if not mapper:
        mapper_cls = _dialect_type_impl.get(dialect, None)
        if not mapper_cls:
            raise ValueError(f"Dialect '{dialect}' is not supported.")
        else:
            mapper = mapper_cls()
            _dialect_type_inst[dialect] = mapper

    return mapper.resolve_type(sqatype)
