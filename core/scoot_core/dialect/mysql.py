from typing import Optional, override

from sqlalchemy import sql
import sqlalchemy
from .mapper import (
    TypeConverter,
    TypeMapper,
    DECIMALConverter,
    VARCHARConverter,
    TemporalConverter,
)
from sqlalchemy.dialects.mysql import dialect as mysql_dialect
from sqlalchemy.sql.type_api import TypeEngine

from .. import types
from ..types import SIGNED, UNSIGNED
from ..connection import Connection


class MySQLTypeMapper(TypeMapper):

    dialect = mysql_dialect()

    def __init__(self):
        self.conversion_map = {
            "TINYINT": types.Integer(1, UNSIGNED),
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
            "INTEGER": types.Integer(64, SIGNED),
            "VARCHAR": VARCHARConverter(lambda _: None),
        }

    @override
    def resolve_type(
        self, alchemy_type: TypeEngine
    ) -> tuple[Optional[types.Type], Optional[str], Optional[str]]:
        type_expr = str(alchemy_type)
        base_type, _, _ = self.parse_sql_type(type_expr)

        mapped = self.conversion_map.get(base_type, None)
        scoot_type = mapped if isinstance(mapped, types.Type) else None

        if isinstance(mapped, TypeConverter):
            scoot_type = mapped.convert(alchemy_type)

        native_type = alchemy_type.compile(self.dialect)

        return scoot_type, type_expr, native_type


TypeMapperImpl = MySQLTypeMapper
type_mapper = MySQLTypeMapper()


def find_and_apply_additional_constraints(
    conn: Connection, table: sqlalchemy.Table
):
    return []
