import traceback
from typing import Any, Optional, cast

from sqlalchemy import (
    inspect,
    Table,
    Column,
    Constraint,
    MetaData,
    CheckConstraint,
    ForeignKeyConstraint,
)
from sqlalchemy.schema import CreateTable
from sqlalchemy.exc import NoSuchTableError
import sqlglot
from sqlglot import expressions as sge

from .dialect import sqlglot_dialect

from .connection import Connection
from .model import TableModel, ColumnModel
from .exceptions import ScootSchemaException
from .dialect.registry import (
    resolve_type as resolve_scoot_type,
    find_and_apply_additional_constraints,
)
from . import expression


def list_schemas(conn: Connection) -> list[str]:
    """Return a list of schema names."""
    inspector = inspect(conn.engine)
    schemas = inspector.get_schema_names()
    return schemas


def list_tables(conn: Connection) -> list[str]:
    """Return a list of table names."""
    inspector = inspect(conn.engine)
    tables = inspector.get_table_names()
    return tables


def list_databases(conn: Connection) -> list[str]:
    """Return a list of database names."""
    dialect = conn.engine.dialect.name

    if dialect == "mssql":
        resultset = conn.execute(
            "SELECT name FROM sys.databases WHERE HAS_DBACCESS(name) = 1"
        )
        return [str(row[0]) for row in resultset.rows]
    elif dialect == "mysql":
        resultset = conn.execute("SHOW DATABASES;")
        return [str(row[0]) for row in resultset.rows]
    elif dialect == "postgresql":
        c = conn.connect()
        conn.disconnect(c)

    raise NotImplementedError(
        f"list databases is not currently supported for dialect '{dialect}'"
    )


def make_table_column(conn: Connection, column: Column):
    scoot_type, driver_type, native_type = resolve_scoot_type(
        conn.get_dialect(), column.type
    )

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


def parse_constraint(conn: Connection, sa_con: Constraint):
    if isinstance(sa_con, ForeignKeyConstraint):
        return {
            "name": sa_con.name,
            "type": "fk",
            "columns": [ck for ck in sa_con.column_keys],
            "reference": {
                "table": sa_con.referred_table.name,
                "columns": [fk.column.name for fk in sa_con.elements],
            },
        }
    elif isinstance(sa_con, CheckConstraint):
        try:
            expr = sqlglot.parse_one(
                str(sa_con.sqltext), read=sqlglot_dialect(conn.get_dialect())
            )
            return {
                "name": sa_con.name,
                "type": "chk",
            } | expression.parse_conditional(expr)
        except Exception:
            traceback.print_exc()
            return None
    else:
        print(f"- Ignoring Table constraint {sa_con.__class__.__name__}.")


def make_table_model(conn: Connection, table: Table) -> TableModel:
    tbl = TableModel(name=table.name, schema=table.schema)
    for sa_column in table.columns:
        tbl.add_column(make_table_column(conn, sa_column))
    for sa_con in table.constraints:
        constraint = parse_constraint(conn, sa_con)
        if constraint:
            tbl.constraints.append(constraint)

    tbl.create_stmt = str(CreateTable(table).compile(dialect=conn.engine.dialect))
    return tbl


def get_identifier_name(identifier):
    """
    Extracts the string value from a sqlglot Identifier object.
    If the value is None, returns None.
    """
    if identifier is None:
        return None
    return (
        identifier.this
        if isinstance(identifier, sge.Identifier)
        else str(identifier)
    )


def parse_table_name(table_name) -> tuple[Optional[str], Optional[str], str]:
    """
    Parses a fully qualified table name using sqlglot.
    """
    expr = sqlglot.parse_one(f"SELECT * FROM {table_name}")
    table_expr = expr.find(sge.Table)

    if not table_expr:
        raise ValueError(f"Could not parse table name: '{table_name}'.")

    return (
        get_identifier_name(table_expr.args.get("catalog")),
        get_identifier_name(table_expr.args.get("db")),
        cast(str, get_identifier_name(table_expr.args.get("this"))),
    )


def describe_table(
    conn: Connection, table_expression: str, ignore_failure=False
) -> Optional[TableModel]:
    """Describe the structure of a database table.

    FIXME: Some system tables and constructs that appear to be tables in certain
    RDBMS systems will not be found and reflected by SQLAlchemy's MetaData, meaning
    that we will need to allow failure in these cases - until a workaround is
    devised.

    This pertains to SQL Servers sys.* views, for example."""
    inspector = inspect(conn.engine)

    table_name = None
    schema_name = None

    try:
        _, schema_name, table_name = parse_table_name(table_expression)

        md = MetaData()
        md.reflect(bind=conn.engine)
        table = Table(table_name, md, schema=schema_name, autoload_with=conn.engine)

        inspector.reflect_table(table, include_columns=None)

        find_and_apply_additional_constraints(conn, table)

        return make_table_model(conn, table)
    except NoSuchTableError as e:
        if ignore_failure:
            print(
                f"describe_table: {table_expression} [{schema_name} {table_name}] {e}"
            )
            return None
        else:
            raise ScootSchemaException("table", table_expression)


def resolve_query_metadata(conn: Connection, sql: str):
    expr = sqlglot.parse_one(sql)

    expr_tables = list(expr.find_all(sge.Table))
    known_tables = {}
    for tbl in expr_tables:
        known_tables[tbl.sql()] = describe_table(
            conn, tbl.sql(), ignore_failure=True
        )

    columns = []

    for e in expr.expressions:
        name = e.alias_or_name
        table: str | None = getattr(e, "table", None)
        column: str | None = getattr(e, "name", None)
        constraints = []
        table_model: TableModel | None = known_tables.get(table, None)

        if e.alias:
            if e.this:
                if e.this.__class__.__name__ == "Column":
                    column = e.this.name

        if isinstance(e, sge.Star):
            if table is None and len(expr_tables) == 1:
                for tbl in expr_tables:
                    tbl_name = tbl.name
                    table_model = known_tables.get(tbl.sql())
                    if table_model:
                        for c in table_model.columns:
                            columns.append(
                                {
                                    "name": c.name,
                                    "table": tbl.sql(),
                                    "column": c.name,
                                    "constraints": table_model.get_constraints_for_column(
                                        c.name
                                    ),
                                }
                            )
            continue

        if column and table is None or table == "":
            for tbl_name, tbl_model in known_tables.items():
                if tbl_model:
                    for c in tbl_model.columns:
                        if c.name == column:
                            table = tbl_name
                            break
                if table:
                    break

        if table_model:
            constraints = table_model.get_constraints_for_column(column)

        columns.append(
            {
                "name": name,
                "table": table,
                "column": column,
                "constraints": constraints,
            }
        )

    columns = [
        (
            table_model.get_column(c.get("column")).to_dict() | c
            if (table_model := known_tables.get(c.get("table")))
            else c
        )
        for c in columns
    ]

    return {"columns": columns}
