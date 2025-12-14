from sqlalchemy.sql.sqltypes import TypeEngine
from sqlglot import exp

from .adapter import TypeAdapter, Types
from .definition import (
    SIGNED,
    UNSIGNED,
    Binary,
    Boolean,
    Collation,
    Date,
    Decimal,
    Integer,
    String,
    Temporal,
    Time,
    TimeZone,
    Type,
)
from .sqlalchemy_adapter import SqlAlchemyType
from .sqlglot_adapter import SqlGlotType


def type_adapter(
    type_inst: TypeAdapter | TypeEngine | exp.DataType | exp.ColumnDef,
    dialect: str | None = None,
) -> "TypeAdapter":
    """
    Factory method to create a TypeAdapter instance from various type
    representations.

    Args:
        type_inst (TypeAdapter | TypeEngine | exp.DataType | exp.ColumnDef):
            The type representation to adapt.
        dialect (str | None): The SQL dialect name, used for sqlglot types.

    Returns:
        TypeAdapter: An instance of a TypeAdapter subclass.

    Raises:
        TypeError: If the provided type_inst cannot be adapted.
    """
    if isinstance(type_inst, TypeEngine):
        return SqlAlchemyType.from_alchemy_type(type_inst)
    if isinstance(type_inst, exp.DataType) or isinstance(type_inst, exp.ColumnDef):
        return SqlGlotType.from_datatype(type_inst, dialect)
    if isinstance(type_inst, TypeAdapter):
        return type_inst


__all__ = [
    "SIGNED",
    "UNSIGNED",
    "Binary",
    "Boolean",
    "Collation",
    "Date",
    "Decimal",
    "Integer",
    "SqlAlchemyType",
    "SqlGlotType",
    "String",
    "Temporal",
    "Time",
    "TimeZone",
    "Type",
    "Types",
    "TypeAdapter",
    "type_adapter",
]
