from sqlalchemy import text
from scoot_core.connection import Connection
from scoot_core.model import ResultSet


def execute(connection: Connection, sql: str) -> ResultSet:
    """Execute a raw SQL query.

    Returns:
      columns: list of column names
      rows: list of rows (each row is a list of values)
    """
    with connection.engine.connect() as conn:
        result = conn.execute(text(sql))
        columns = list(result.keys())
        rows = [list(row) for row in result]
    return ResultSet(columns, rows)
