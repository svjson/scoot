from sqlalchemy import create_engine
from sqlalchemy.engine import Engine
from typing import Optional


def connect(url: str) -> None:
    return create_engine(url)


class Connection:
    def __init__(self, url):
        self.url = url
        self.engine = connect(url)

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

    def close(self):
        self.engine.dispose()
