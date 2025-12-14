from typing import Tuple

from sqlalchemy.schema import (
    CheckConstraint,
    Column,
    Constraint,
    ForeignKey,
    MetaData,
    Table,
)

from scoot_core.schema.emitter import SchemaEmitter
from scoot_core.schema.ir import ColumnIR, TableIR


class SqlAlchemyTableEmitter(SchemaEmitter[Table]):
    """
    Emits a sqlalchemy Table schema from a TableIR instance.

    Args:
       SchemaEmitter (SchemaEmitter[Table]): The base schema emitter class.

    Returns:
       Table: The emitted sqlalchemy Table schema.
    """

    def emit_table(self, table: TableIR) -> Table:
        columns = []
        constraints: list[Constraint] = []
        primary_key = []

        for col in table.columns:
            column, col_constraints = self.emit_column(col)
            columns.append(column)
            constraints += col_constraints
            if col.primary_key:
                primary_key.append(column)

        return Table(table.name, MetaData(), *columns, *constraints, *primary_key)

    def emit_column(self, col: ColumnIR) -> Tuple[Column, list[Constraint]]:
        column = Column(
            col.name,
            col.type.to_sqlalchemy_type(),
            # Primary key is implicitly unique, and attempting to create a table
            # from a table model with unique AND primary key set will cause oracle_23c
            # to throw a fit.
            unique=col.unique and not col.primary_key,
            primary_key=col.primary_key,
            nullable=col.nullable,
        )

        if col.foreign_key:
            column.append_foreign_key(
                ForeignKey(f"{col.foreign_key.table}.{col.foreign_key.column}")
            )

        return (column, [CheckConstraint(chk) for chk in col.check_constraints])
