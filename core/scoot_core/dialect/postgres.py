from typing import Optional, override
from .mapper import TypeConverter, TypeMapper, DECIMALConverter, VARCHARConverter
from sqlalchemy.dialects.postgresql import dialect as postgres_dialect
from sqlalchemy.sql.type_api import TypeEngine

from .. import types


class PostgresTypeMapper(TypeMapper):

    def __init__(self):
        self.dialect = postgres_dialect()
        self.conversion_map = {
            "INTEGER": types.Integer(64, True),
            "TIMESTAMPTZ": types.Temporal(
                date=types.Date(
                    min=(-4712, 1, 1),
                    max=(9999, 12, 31),
                    calendar="proleptic gregorian",
                ),
                time=types.Time(
                    clock="24",
                    fsp=10,
                    timezone=types.TimeZone(offset="t", zone="t"),
                ),
            ),
            "NUMERIC": DECIMALConverter(default_precision=None, default_scale=None),
            "VARCHAR": VARCHARConverter(lambda _: None),
            "TEXT": types.String(
                max_len=None, encoding="utf-8", collation=None, lob=True
            ),
            "BOOLEAN": types.Boolean(
                true_literals=["true", "yes", "on", "1"],
                false_literals=["false", "no", "off", "0"],
            ),
        }

    @override
    def resolve_type(
        self, alchemy_type: TypeEngine
    ) -> tuple[Optional[types.Type], Optional[str], Optional[str]]:
        type_expr = str(alchemy_type)
        base_type, _, _ = self.parse_sql_type(type_expr)

        if (
            type_expr == "TIMESTAMP WITH TIME ZONE"
            or type_expr == "TIMESTAMP"
            and getattr(alchemy_type, "timezone")
        ):
            base_type = "TIMESTAMPTZ"

        mapped = self.conversion_map.get(base_type, None)
        scoot_type = mapped if isinstance(mapped, types.Type) else None

        if isinstance(mapped, TypeConverter):
            scoot_type = mapped.convert(alchemy_type)

        native_type = alchemy_type.compile(self.dialect)

        return scoot_type, type_expr, native_type
