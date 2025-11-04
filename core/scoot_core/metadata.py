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
import sqlglot.expressions as exp

from .openv import OperationEnv

from .dialect import sqlglot_dialect

from .config import is_server
from .connection import Connection
from .model import TableModel, ColumnModel
from .exceptions import ScootQueryException, ScootSchemaException
from .dialect.registry import (
    resolve_type as resolve_scoot_type,
    find_and_apply_additional_constraints,
)
from . import expression


def list_schemas(ctx: OperationEnv) -> list[str]:
    """Return a list of schema names."""
    inspector = inspect(ctx.connection.engine)
    schemas = inspector.get_schema_names()
    return schemas


def list_tables(ctx: OperationEnv) -> list[str]:
    """Return a list of table names."""
    inspector = inspect(ctx.connection.engine)
    tables = inspector.get_table_names()
    return tables


def list_databases(ctx: OperationEnv) -> list[str]:
    """Return a list of database names."""
    conn = ctx.connection
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
    scoot_type, _, native_type = resolve_scoot_type(conn.get_dialect(), column.type)

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
    elif is_server:
        print(f"- Ignoring Table constraint {sa_con.__class__.__name__}.")


def make_table_model(conn: Connection, table: Table) -> TableModel:
    tbl = TableModel(name=table.name, schema=table.schema, sa_table=table)
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


def describe_table(op_env: OperationEnv, table_expression: str) -> TableModel:
    """Describe the structure of a database table.

    FIXME: Some system tables and constructs that appear to be tables in certain
    RDBMS systems will not be found and reflected by SQLAlchemy's MetaData, meaning
    that we will need to allow failure in these cases - until a workaround is
    devised.

    This pertains to SQL Servers sys.* views, for example."""
    with op_env.operation("describe_table"):
        conn = op_env.connection
        inspector = inspect(conn.engine)

        table_name = None
        schema_name = None

        try:
            _, schema_name, table_name = parse_table_name(table_expression)

            if not table_name:
                raise ScootQueryException(
                    f"Failed to resolve table from expression: {table_expression}"
                )

            if table_name:
                cached = op_env.table_model_cache(conn.default_schema(), table_name)
                if cached:
                    return cached

            md = MetaData()
            #            with ctx.operation("md.reflect"):
            #                md.reflect(bind=conn.engine)
            with op_env.operation("Table()"):
                table = Table(
                    table_name, md, schema=schema_name, autoload_with=conn.engine
                )

            with op_env.operation("reflect_table"):
                inspector.reflect_table(table, include_columns=None)

            with op_env.operation("find_additional_constraints"):
                find_and_apply_additional_constraints(conn, table)

            return op_env.cache_table_model(
                conn.default_schema(), table_name, make_table_model(conn, table)
            )
        except NoSuchTableError:
            raise ScootSchemaException("table", table_expression)


def try_describe_table(
    ctx: OperationEnv, table_expression: str
) -> Optional[TableModel]:
    try:
        return describe_table(ctx, table_expression)
    except ScootSchemaException:
        return None


class ColumnMeta:
    def __init__(self, e):
        self.expr = e
        self.name = e.alias_or_name
        self.table: str | None = getattr(e, "table", None)
        self.column: str | None = getattr(e, "name", None)
        self.constraints = []
        self.table_model: TableModel | None = None

    def to_dict(self):
        return {
            "name": self.name,
            "table": self.table,
            "column": self.column,
            "constraints": self.constraints,
        }


def resolve_query_metadata(ctx: OperationEnv, sql: str):
    with ctx.operation("resolve_query_metadata"):
        expr = sqlglot.parse_one(sql)

        with ctx.operation("Enumerating known tables"):
            expr_tables = list(expr.find_all(sge.Table))
            known_tables: dict[str, TableModel] = {}
            for tbl in expr_tables:
                tbl_meta = try_describe_table(ctx, tbl.name)
                if tbl_meta:
                    known_tables[tbl_meta.name] = tbl_meta

        columns = []

        def is_anonymous(e) -> bool:
            """Return True if the expression is an anonymous (generic) function call,
            e.g. COUNT(*), MAX(x), LOWER(name), etc.
            """
            return isinstance(e, (exp.Anonymous, exp.Func))

        for e in expr.expressions:

            with ctx.operation("inspect expression"):

                colmeta = ColumnMeta(e)
                if colmeta.table:
                    colmeta.table_model = known_tables.get(
                        colmeta.table.casefold(), None
                    )

                if e.alias:
                    if e.this:
                        if e.this.__class__.__name__ == "Column":
                            colmeta.column = str(e.this.name)

                if isinstance(e, sge.Star):
                    if colmeta.table is None and len(expr_tables) == 1:
                        for tbl in expr_tables:
                            tbl_name = tbl.name
                            table_model = known_tables.get(tbl.name)
                            if table_model:
                                for c in table_model.columns:
                                    columns.append(
                                        {
                                            "name": c.name,
                                            "table": tbl.name,
                                            "column": c.name,
                                            "constraints": table_model.get_constraints_for_column(
                                                c.name
                                            ),
                                        }
                                    )
                    continue
                elif is_anonymous(e):
                    columns.append(
                        {
                            "name": str(e),
                            "table": None,
                            "column": None,
                            "constraints": [],
                        }
                    )
                    continue

                if colmeta.column is not None and (
                    colmeta.table is None or colmeta.table == ""
                ):
                    for tbl_name, tbl_model in known_tables.items():
                        if tbl_model:
                            for c in tbl_model.columns:
                                if c.name.lower() == colmeta.column.lower():
                                    colmeta.table = tbl_name
                                    colmeta.table_model = tbl_model
                                    break
                        if colmeta.table:
                            break

                if colmeta.table_model and colmeta.column:
                    colmeta.constraints = (
                        colmeta.table_model.get_constraints_for_column(
                            colmeta.column
                        )
                    )

                columns.append(colmeta.to_dict())

        columns = [
            (
                (
                    (
                        (c | col.to_dict() if isinstance(col, ColumnModel) else c)
                        if (col := table_model.get_column(c.get("column")))
                        else c
                    )
                    if (table_model := known_tables.get(tbl_name.casefold()))
                    else c
                )
                if (tbl_name := c.get("table"))
                else c
            )
            for c in columns
        ]

        return {"columns": columns}
