import sqlalchemy
from sqlalchemy import Engine, create_engine, text

from scoot_core.model import ResultSet

from .error import dialect_error_handler


class Connection:
    """Basic abstraction for a logical AND physical Connection to
    a database backend.

    Currenly just a paper-thin wrapper around ``sqlalchemy.engine.Engine``."""

    def __init__(self, url):
        self.url = url
        self.engine: Engine = create_engine(url)
        self.active_connections = set[sqlalchemy.Connection]()

    def get_dialect(self) -> str:
        return self.engine.dialect.name

    def connect(self) -> sqlalchemy.Connection:
        conn = self.engine.connect()
        self.active_connections.add(conn)
        return conn

    def disconnect(self, conn: sqlalchemy.Connection):
        self.active_connections.remove(conn)
        conn.close()

    def reconfigure(self, url):
        """Reconfigure this connection to use a different connection url/string."""

        self.close()
        self.engine = create_engine(url)

    def default_schema(self):
        return self.engine.url.database

    def execute(self, sql: str) -> ResultSet:
        """Execute a raw SQL query.

        Wraps execution in a driver-specific exception handler, attempting
        to produce meaningful error messages.

        Returns:
          columns: list of column names
          rows: list of rows (each row is a list of values)
        """
        error_handler = dialect_error_handler(self.engine.driver)

        @error_handler
        def do_execute(connection: Connection, sql: str):
            with connection.engine.connect().execution_options(
                isolation_level="AUTOCOMMIT"
            ) as conn:
                result = conn.execute(text(sql))
                columns = list(result.keys())
                rows = [list(row) for row in result]

                return ResultSet(columns, rows)

        return do_execute(self, sql)

    def close(self):
        """Close the physical connection. sqlalchemy.Engine and its pool
        implementations does not close any outstanding connections, so we
        will attempt to take down any active stray connections before we
        dispose of the Engine."""
        for connection in self.active_connections:
            if not connection.closed:
                try:
                    print(f"Attempting to close stray connection {connection}")
                    connection.close()
                except Exception as e:
                    print(f"Connection {connection} failed to close gracefully. {e}")

        self.engine.dispose()

    def to_dict(self):
        url = self.engine.url
        return {
            "dialect": url.get_dialect().name,
            "host": url.host,
            "port": url.port,
            "username": url.username,
            "database": url.database,
            "driver": url.drivername,
        }

    def __str__(self):
        url = self.engine.url
        return f"{url.drivername}//{url.username}:xxxxx@${url.host}:{url.port}/{url.database}"
