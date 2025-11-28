from dataclasses import dataclass

from scoot_core.types import Type


@dataclass
class ColumnIR:
    """
    Intermediate representation of a database table column.
    """

    name: str
    type: Type
    native_type: str | None
    nullable: bool
    primary_key: bool
    default: str | None = None


@dataclass
class TableIR:
    """
    Intermediate representation of a database table.
    """

    name: str
    columns: list[ColumnIR]
