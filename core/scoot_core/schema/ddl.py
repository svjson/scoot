from scoot_core.dialect import sqlglot_dialect
from scoot_core.dialect.registry import resolve_type
from scoot_core.openv import OperationEnv
from scoot_core.schema.ir import ColumnIR, TableIR
from .reader import SchemaReader
from sqlglot import parse_one, exp


class DDLReader(SchemaReader):
    """
    Reads a table schema from a DDL CREATE TABLE statement into the
    intermediate TableIR format.

    Attributes:
        op_env (OperationEnv): The operation environment containing dialect information.
        dialect (str): The SQL dialect used for parsing.
        ddl (str): The DDL CREATE TABLE statement to be parsed.
    """

    def __init__(self, op_env: OperationEnv, ddl: str):
        """
        Initializes the DDLReader with the given operation environment and DDL statement.

        Args:
            op_env (OperationEnv): The operation environment containing dialect information.
            ddl (str): The DDL CREATE TABLE statement to be parsed.
        """
        self.op_env = op_env
        self.dialect = op_env.get_dialect()
        self.glot_dialect = sqlglot_dialect(self.dialect)
        self.ddl = ddl

    def read_table(self) -> TableIR:
        """
        Reads the DDL CREATE TABLE statement and converts it into a TableIR.

        This will attempt to resolve the dialect/database backend specific
        type specifications using the `dialect` package.

        Returns:
            TableIR: The intermediate representation of the table schema.
        Raises:
            TypeError: If the DDL statement is not a CREATE TABLE statement
                       or if a column type cannot be resolved.
        """
        expr = parse_one(self.ddl, read=sqlglot_dialect(self.op_env.get_dialect()))

        if not isinstance(expr, exp.Create):
            raise TypeError("Expression is not a CREATE TABLE statement")

        table_expr = expr.this
        table_name = table_expr.this.name

        columns: list[ColumnIR] = []

        for column_expr in table_expr.expressions:
            if isinstance(column_expr, exp.ColumnDef):
                columns.append(self.read_column(table_expr, column_expr))

        return TableIR(table_name, columns)

    def read_column(
        self, table_expr: exp.Table, column_expr: exp.ColumnDef
    ) -> ColumnIR:
        """
        Reads a column definition from the DDL and converts it into a ColumnIR.

        Args:
            table_expr (exp.Table): The table expression containing the column.
            column_expr (exp.ColumnDef): The column definition expression.

        Returns:
            ColumnIR: The intermediate representation of the column schema.
        Raises:
            TypeError: If the column type cannot be resolved.
        """
        column_name = column_expr.name
        scoot_type, _, native_type = resolve_type(self.dialect, column_expr)
        if not scoot_type:
            raise TypeError(
                f"Could not resolve type of {column_expr} ({native_type})"
            )

        nullable = False
        if null_constr := column_expr.find(exp.NotNullColumnConstraint):
            nullable = null_constr.args.get("allow_null") or False

        pk_constraints = [
            constraint
            for constraint in table_expr.find_all(exp.Constraint)
            if constraint.find(exp.PrimaryKeyColumnConstraint)
        ]

        is_primary_key = any(
            (col := c.find(exp.Column)) and col.this.name == column_name
            for c in pk_constraints
        )

        return ColumnIR(
            name=column_name,
            type=scoot_type,
            native_type=native_type,
            nullable=nullable,
            primary_key=is_primary_key,
            default=None,
        )
