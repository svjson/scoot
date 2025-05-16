from functools import wraps
import re

from sqlalchemy import text, exc
from scoot_core.exceptions import ScootQueryException
from scoot_core.connection import Connection
from scoot_core.model import ResultSet

from sqlglot import parse_one, exp


class SQLQueryModifier:
    def __init__(self, sql):
        self.ast = parse_one(sql)

    def add_where_condition(self, condition):
        where_clause = self.ast.find(exp.Where)

        where_node = parse_one(f"SELECT * FROM x WHERE {condition}").find(exp.Where)
        new_condition = where_node.this if where_node else None

        if where_clause:
            where_clause.set(
                "this", exp.And(this=where_clause.this, expression=new_condition)
            )
        else:
            self.ast.set("where", exp.Where(this=new_condition))

    def remove_where_condition(self, condition):
        where_clause = self.ast.find(exp.Where)

        if not where_clause:
            return

        # Parse the condition as an expression
        where_node = parse_one(f"SELECT * FROM x WHERE {condition}").find(exp.Where)
        if where_node is None:
            return

        target_expr = where_node.this

        # Recursively search and remove the target condition
        def remove_expr(node):
            if isinstance(node, exp.And):
                left = remove_expr(node.this)
                right = remove_expr(node.expression)

                # If both sides are None, the entire AND can be removed
                if left is None and right is None:
                    return None
                # If only one side is None, return the other side
                elif left is None:
                    return right
                elif right is None:
                    return left
                # Otherwise, update the AND node
                node.set("this", left)
                node.set("expression", right)
                return node

            # Direct comparison for leaf nodes
            if node == target_expr:
                return None

            return node
            if isinstance(node, exp.And):
                if node.this == target_expr:
                    return node.expression
                elif node.expression == target_expr:
                    return node.this
                else:
                    node.set("this", remove_expr(node.this))
                    node.set("expression", remove_expr(node.expression))
                    return node
            return node

        new_where = remove_expr(where_clause.this)
        if isinstance(new_where, exp.And):
            self.ast.set("where", exp.Where(this=new_where))
        elif new_where:
            self.ast.set("where", exp.Where(this=new_where))
        else:
            self.ast.set("where", None)

    def get_sql(self):
        return self.ast.sql()


def handler_generic(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        return func(*args, **kwargs)

    return wrapper


def handler_pyodbc(func):

    def extract_sql_message(exc):
        if len(exc.args) > 1:
            message = exc.args[1]
            # Extract only the main error message
            match = re.search(r"(\[SQL Server\]\s*.*?)(?:\s*\(\d+\).*)?$", message)
            if match:
                return match.group(1).strip()
            return str(exc)

    @wraps(func)
    def wrapper(*args, **kwargs):
        import pyodbc

        try:
            return func(*args, **kwargs)
        except exc.ProgrammingError as pe:
            if isinstance(pe.__cause__, pyodbc.ProgrammingError):
                root_exc: pyodbc.ProgrammingError = pe.__cause__
                raise ScootQueryException(extract_sql_message(root_exc))
            else:
                raise pe

    return wrapper


def get_dialect_error_handler(driver_name):
    handlers = {"pyodbc": handler_pyodbc}

    return handlers.get(driver_name, handler_generic)


def execute(connection: Connection, sql: str) -> ResultSet:
    """Execute a raw SQL query.

    Wraps execution in a driver-specific exception handler, attempting
    to produce meaningful error messages.

    Returns:
      columns: list of column names
      rows: list of rows (each row is a list of values)
    """
    error_handler = get_dialect_error_handler(connection.engine.driver)

    @error_handler
    def do_execute(connection: Connection, sql: str):

        with connection.engine.connect().execution_options(
            isolation_level="AUTOCOMMIT"
        ) as conn:
            result = conn.execute(text(sql))
            columns = list(result.keys())
            rows = [list(row) for row in result]

            return ResultSet(columns, rows)

    return do_execute(connection, sql)


def modify(connection: Connection, sql: str, action_instr: dict) -> ResultSet:
    target = action_instr.get("target")
    operation = action_instr.get("operation")
    conditions = action_instr.get("conditions", [])

    result = SQLQueryModifier(sql)

    if target == "WHERE":
        for cond in conditions:
            lhs = cond.get("lhs")
            cmp = cond.get("cmp")
            rhs = cond.get("rhs")

            expr = f"{lhs} {cmp} {rhs}"

            if operation == "add":
                result.add_where_condition(expr)
            elif operation == "remove":
                result.remove_where_condition(expr)
            else:
                raise ValueError(f"Unknown operation: {operation}")

    modified_sql = result.get_sql()

    result_set = execute(connection, result.get_sql())
    result_set.metadata = (result_set.metadata or {}) | {"stmt": modified_sql}
    return result_set


def perform_action(connection: Connection, sql: str, action_instr: dict):
    """Perform an action on an SQL query.

    Args:
            connection (Connection): The database connection.
            sql (str): The SQL query to execute.
            action (dict): The action to perform on the SQL query.
    """
    action = action_instr.get("action")

    if action == "execute":
        return execute(connection, sql)
    elif action == "modify":
        return modify(connection, sql, action_instr)
    else:
        raise ValueError(f"Unknown action: {action}")
