from sqlalchemy import create_engine
from sqlalchemy.engine import Engine


class Connection:
    """Basic abstraction for a logical AND physical Connection to
    a database backend.

    Currenly just a paper-thin wrapper around ``sqlalchemy.engine.Engine``."""

    def __init__(self, url):
        self.url = url
        self.engine: Engine = create_engine(url)

    def reconfigure(self, url):
        """Reconfigure this connection to use a different connection url/string."""

        self.close()
        self.engine = create_engine(url)

    def close(self):
        """Close the physical connection. This does not close any outstanding
        connections from the pool."""
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
