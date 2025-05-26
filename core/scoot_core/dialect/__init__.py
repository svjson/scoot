from typing import Any, cast

from sqlalchemy import Table, Column, ForeignKeyConstraint
from sqlalchemy.schema import CreateTable

from scoot_core.connection import Connection
from scoot_core.model import TableModel, ColumnModel
from .registry import resolve_type as resolve_scoot_type


def make_table_model(conn: Connection, table: Table) -> TableModel:
    tbl = TableModel(name=table.name, schema=table.schema)
    for sa_column in table.columns:
        tbl.add_column(make_table_column(conn, sa_column))
    for sa_con in table.constraints:
        if isinstance(sa_con, ForeignKeyConstraint):
            tbl.constraints.append(
                {
                    "name": sa_con.name,
                    "type": "fk",
                    "columns": [ck for ck in sa_con.column_keys],
                    "reference": {
                        "table": sa_con.referred_table.name,
                        "columns": [fk.column.name for fk in sa_con.elements],
                    },
                }
            )
    tbl.create_stmt = str(CreateTable(table).compile(dialect=conn.engine.dialect))
    return tbl


def make_table_column(conn: Connection, column: Column):
    scoot_type, driver_type, native_type = resolve_scoot_type(
        conn.get_dialect(), column.type
    )
    # print(f"{column.name}:")
    # print(
    #     "-" * len(column.name) + f" {type(column.type).__qualname__}: {column.name}"
    # )
    # print(scoot_type)
    # print(f"SQLAlchemy: {driver_type}")
    # print(f"Native: {native_type}")
    # print("/" * len(column.name))
    # print("")

    return ColumnModel(
        name=column.name,
        type_=scoot_type,
        native_type=native_type,
        nullable=column.nullable,
        primary_key=column.primary_key,
        default=(
            str((cast(Any, column.default).arg))
            if column.default and column.default
            else None
        ),
    )
