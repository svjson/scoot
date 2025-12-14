from typing import Optional, override

import sqlalchemy
from sqlalchemy.dialects.mssql import dialect as mssql_dialect
from sqlalchemy.sql.expression import text
from sqlalchemy.sql.schema import CheckConstraint

from scoot_core.connection import Connection

from .. import types
from ..types import SIGNED, UNSIGNED
from .mapper import (
    BinaryConverter,
    DECIMALConverter,
    TemporalConverter,
    TypeConverter,
    TypeMapper,
    VARCHARConverter,
    subst_leading,
)


class MSSQLTypeMapper(TypeMapper):
    def __init__(self):
        self.conversion_map: dict[str, types.Type | TypeConverter] = {
            "BINARY": BinaryConverter(),
            "BIT": types.Integer(1, UNSIGNED),
            "BOOLEAN": types.Boolean(),
            "DATETIME": TemporalConverter(
                default=types.Temporal(
                    date=types.Date(
                        min=(1, 1, 1), max=(9999, 12, 31), calendar="gregorian"
                    ),
                    time=types.Time(clock="24", fsp=8),
                )
            ),
            "DATETIMEOFFSET": TemporalConverter(
                default=types.Temporal(
                    date=types.Date(
                        min=(1, 1, 1), max=(9999, 12, 31), calendar="gregorian"
                    ),
                    time=types.Time(
                        clock="24", fsp=8, timezone=types.TimeZone(offset="t")
                    ),
                )
            ),
            "DECIMAL": DECIMALConverter(18, 0),
            "INTEGER": types.Integer(64, SIGNED),
            "NTEXT": types.String(2 * 1024 * 1024 * 1024, "utf-16"),
            "NVARCHAR": VARCHARConverter(self.parse_collation),
            "TEXT": types.String(),
            "VARBINARY": BinaryConverter(),
            "VARCHAR": VARCHARConverter(self.parse_collation),
        }

    native_translation_map = {
        "BIT": str.lower,
        "DATETIME": str.lower,
        "DATETIMEOFFSET": str.lower,
        "DECIMAL": str.lower,
        "INTEGER": lambda _: "int",
        "VARCHAR": subst_leading("varchar"),
        "NVARCHAR": subst_leading("nvarchar"),
    }

    dialect = mssql_dialect()

    @override
    def resolve_type(
        self, type: types.TypeAdapter
    ) -> tuple[Optional[types.Type], Optional[str], Optional[str]]:
        type_expr = str(type)
        base_type, _, _ = self.parse_sql_type(type_expr)

        mapped: types.Type | TypeConverter | None = self.conversion_map.get(
            base_type, None
        )
        scoot_type = mapped if isinstance(mapped, types.Type) else None

        if isinstance(mapped, TypeConverter):
            scoot_type = mapped.convert(type)

        if scoot_type is None:
            raise TypeError(f"Unable to resolve type of: '{type}'")

        scoot_type.source_type = type

        native_type = type.native_expression(self.dialect)
        n = native_type.find("(")
        native_base_type = native_type[:n] if n != -1 else native_type
        translator = self.native_translation_map.get(native_base_type, None)

        native_type = translator(native_type) if translator else native_type

        return (scoot_type, type_expr, native_type)

    def parse_collation(self, collation: str) -> Optional[types.Collation]:
        # Split on underscores, last two are always sensitivity flags
        parts = collation.split("_")
        if len(parts) < 4:
            return None

        *locale_parts, cs_flag, as_flag = parts

        if locale_parts[0] == "SQL":
            locale_parts = locale_parts[1:]

        return types.Collation(
            locale="_".join(locale_parts),
            case_sensitive=cs_flag.upper() == "CS",
            accent_sensitive=as_flag.upper() == "AS",
        )


TypeMapperImpl = MSSQLTypeMapper
type_mapper = MSSQLTypeMapper()


def find_and_apply_additional_constraints(conn: Connection, table: sqlalchemy.Table):
    schema = table.schema
    if schema:
        schema = f"'${schema}'"
    else:
        schema = "SCHEMA_NAME()"

    tbl_con = conn.execute(
        f"""
          SELECT
            cc.constraint_name constraint_name, cc.check_clause check_sql
          FROM
            information_schema.check_constraints cc
          INNER JOIN
            information_schema.constraint_column_usage cu
          ON
            cc.constraint_name = cu.constraint_name
          WHERE
            cu.table_name = '{table.name}'
          AND
            cu.table_schema = {schema}
        """
    )

    for row in tbl_con.rows:
        if row[1] is not None:
            table.constraints.add(
                CheckConstraint(text(row[1]), name=row[0], table=table)
            )

    return []
