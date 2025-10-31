# from abc import abstractmethod

from .connection import Connection


class OperationContext:

    def __init__(self, connection: Connection):
        self.connection = connection
        pass

    def get_dialect(self) -> str:
        return self.connection.get_dialect()

    def operation(self, name):
        return Operation(name)


class Operation:
    def __init__(self, name: str):
        self.name = name

    def __enter__(self):
        pass

    def __exit__(self, _exc_type, _exc, _tb):
        pass
