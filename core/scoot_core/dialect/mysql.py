from typing import Optional, override

import sqlalchemy
from sqlalchemy.dialects.mysql import dialect as mysql_dialect

from .. import types
from ..connection import Connection
from ..types import SIGNED, UNSIGNED
from .mapper import (
    DECIMALConverter,
    TemporalConverter,
    TypeConverter,
    TypeMapper,
    VARCHARConverter,
)


class MySQLTypeMapper(TypeMapper):
    dialect = mysql_dialect()

    def __init__(self):
        self.conversion_map = {
            "BOOLEAN": types.Boolean(),
            "DATE": types.Temporal(
                date=types.Date(
                    min=(1000, 1, 1), max=(9999, 12, 31), calendar="Gregorian"
                )
            ),
            "DATETIME": TemporalConverter(
                default=types.Temporal(
                    date=types.Date(
                        min=(1000, 1, 1), max=(9999, 12, 31), calendar="gregorian"
                    ),
                    time=types.Time(clock="24", fsp=6),
                )
            ),
            "DECIMAL": DECIMALConverter(10, 0),
            "TEXT": types.String(
                max_len=None, encoding="utf-8", collation=None, lob=True
            ),
            "TIME": TemporalConverter(
                default=types.Temporal(time=types.Time(clock="24", fsp=6))
            ),
            "TINYINT": types.Integer(1, UNSIGNED),
            "INTEGER": types.Integer(64, SIGNED),
            "VARCHAR": VARCHARConverter(lambda _: None),
        }

    @override
    def resolve_type(
        self, type: types.TypeAdapter
    ) -> tuple[Optional[types.Type], Optional[str], Optional[str]]:
        type_expr = str(type)
        base_type, _, _ = self.parse_sql_type(type_expr)

        mapped = self.conversion_map.get(base_type, None)
        scoot_type = mapped if isinstance(mapped, types.Type) else None

        if isinstance(mapped, TypeConverter):
            scoot_type = mapped.convert(type)

        if scoot_type is None:
            raise TypeError(f"Unable to resolve type of: '{type}'")

        native_type = type.native_expression(self.dialect)

        return scoot_type, type_expr, native_type


TypeMapperImpl = MySQLTypeMapper
type_mapper = MySQLTypeMapper()


def find_and_apply_additional_constraints(conn: Connection, table: sqlalchemy.Table):
    return []
