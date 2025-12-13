from dataclasses import dataclass, field

from scoot_core.types import Type


@dataclass
class ForeignKeyIR:
    """
    Intermadiate representation of a foreign key constraint.
    """

    table: str
    column: str

    def to_dict(self):
        return {"table": self.table, "column": self.column}


@dataclass
class ColumnIR:
    """
    Intermediate representation of a database table column.
    """

    name: str
    type: Type
    nullable: bool = True
    primary_key: bool = False
    foreign_key: ForeignKeyIR | None = None
    default: str | None = None
    unique: bool = False
    check_constraints: list[str] = field(default_factory=list)

    def to_dict(self):
        cdict = {
            "name": self.name,
            "type": self.type.to_dict(),
            "source_type": (
                str(self.type.source_type) if self.type.source_type else None
            ),
            "nullable": self.nullable,
            "primary_key": self.primary_key,
            "default": self.default,
            "unique": self.unique,
        }
        if self.foreign_key:
            cdict["foreign_key"] = self.foreign_key.to_dict()
        if len(self.check_constraints) > 0:
            cdict["check_constraints"] = self.check_constraints

        return cdict


@dataclass
class TableIR:
    """
    Intermediate representation of a database table.
    """

    name: str
    columns: list[ColumnIR]

    def to_dict(self):
        return {
            "name": self.name,
            "columns": [col.to_dict() for col in self.columns],
        }
