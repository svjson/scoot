from typing import Optional, override

import sqlalchemy

from scoot_core.connection import Connection
from .mapper import TypeConverter, TypeMapper, VARCHARConverter
from sqlalchemy.dialects.oracle import dialect as oracle_dialect
from sqlalchemy.sql.type_api import TypeEngine

from .. import types


class OracleTypeMapper(TypeMapper):

    date_min = (-4712, 1, 1)
    date_max = (9999, 12, 31)

    NUMBER = types.Decimal(38, 0)

    def __init__(self):
        self.dialect = oracle_dialect()
        self.conversion_map = {
            "CLOB": types.String(
                max_len=None, encoding="utf-8", collation=None, lob=True
            ),
            "DATE": types.Temporal(
                date=types.Date(
                    min=self.date_min,
                    max=self.date_min,
                    calendar="proleptic gregorian",
                ),
                time=types.Time(clock="24"),
            ),
            "SMALLINT": self.NUMBER,
            "INTEGER": self.NUMBER,
            "NUMBER": self.NUMBER,
            "VARCHAR2": VARCHARConverter(lambda _: None),
        }

    @override
    def resolve_type(
        self, alchemy_type: TypeEngine
    ) -> tuple[Optional[types.Type], Optional[str], Optional[str]]:
        type_expr = str(alchemy_type)
        base_type, args, _ = self.parse_sql_type(type_expr)

        if base_type == 'VARCHAR':
            base_type = 'VARCHAR2'

        mapped = self.conversion_map.get(base_type, None)

        if base_type == "NUMBER" and len(args) > 0:
            scoot_type = self.parse_decimal(args)
        else:
            scoot_type = mapped if isinstance(mapped, types.Type) else None

        if not scoot_type and isinstance(mapped, TypeConverter):
            scoot_type = mapped.convert(alchemy_type)

        native_type = alchemy_type.compile(self.dialect)

        return scoot_type, type_expr, native_type

    def parse_decimal(self, args):
        int_args = [int(a) for a in args]
        return types.Decimal(*int_args)


TypeMapperImpl = OracleTypeMapper


def find_and_apply_additional_constraints(
    conn: Connection, table: sqlalchemy.Table
):
    return []
