from scoot_core.connection import Connection
from typing import Optional

connections = {}


def create_connection(name: str, url: str) -> Connection:
    conn = Connection(url)
    global connections
    if connections.get(name):
        connections[name].close()
    connections[name] = conn

    return conn


def get_connection(name: str) -> Optional[Connection]:
    global connections
    return connections.get(name)
