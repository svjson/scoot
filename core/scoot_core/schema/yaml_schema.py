from typing import Any

from scoot_core import types
from scoot_core.schema.ir import ColumnIR, ForeignKeyIR, TableIR
from scoot_core.schema.reader import SchemaReader


def to_integer(bits=64, signed=True, source_type=None):
    return types.Integer(bits, signed, source_type=source_type)


def to_string(max_len: int | None = None, encoding: str = "utf-8", source_type=None):
    return types.String(max_len, encoding, source_type=source_type)


def to_zoned_date_time(source_type=None):
    temp = types.Temporal(
        date=types.Date(min=(1, 1, 1), max=(9999, 12, 31), calendar="gregorian"),
        time=types.Time(clock="24", fsp=8, timezone=types.TimeZone(offset="t")),
        source_type=source_type,
    )
    return temp


def to_decimal(precision=None, scale=None, source_type=None):
    return types.Decimal(precision, scale, source_type=source_type)


scoot_types = {
    "integer": to_integer,
    "varchar": to_string,
    "boolean": types.Boolean,
    "text": to_string,
    "decimal": to_decimal,
    "timestamp": to_zoned_date_time,
}


def to_scoot_type(type_str: str):
    if "(" in type_str and ")" in type_str:
        base_type, params = type_str.split("(")
        params = params.rstrip(")").split(",")
        return scoot_types[base_type.strip()](*map(int, params), source_type=type_str)
    else:
        return scoot_types.get(type_str, to_string)(source_type=type_str)


class YamlSchemaReader(SchemaReader):
    """
    Reads a table schema from the Scoot YAML Schema format, that is used
    to define the backend-agnostic system test database schema, into the
    intermediate TableIR format.
    """

    def __init__(self, table_schema: dict[str, Any] | None = None):
        if table_schema is None:
            raise TypeError("No table schema")
        self.table_schema: dict[str, Any] = table_schema

    def read_table(self) -> TableIR:
        table_name = self.table_schema["name"]
        columns = []

        for col in self.table_schema["columns"]:
            columns.append(self.read_column(col))

        return TableIR(table_name, columns)

    def read_column(self, col: dict[str, Any]):
        col_name = col["name"]
        col_type = to_scoot_type(col["type"].lower())
        is_primary_key = col.get("primary_key", False) is True
        default_value = col.get("default", None)
        nullable = is_primary_key is False and col.get("nullable", True)
        unique = col.get("unique", False) or is_primary_key
        check = col.get("check")
        references = col.get("references", None)
        foreign_key = None
        if references:
            ref_table, ref_column = references.rstrip(")").split("(")
            foreign_key = ForeignKeyIR(ref_table, ref_column)

        return ColumnIR(
            name=col_name,
            type=col_type,
            nullable=nullable,
            primary_key=is_primary_key,
            foreign_key=foreign_key,
            default=default_value,
            unique=unique,
            check_constraints=[check] if check else [],
        )
