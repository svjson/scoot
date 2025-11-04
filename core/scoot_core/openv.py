from scoot_core.model import TableModel
from .cache import Cache
from .connection import Connection


class OperationEnv:

    def __init__(
        self, connection: Connection, cache: dict[str, Cache] | None = None
    ):
        self.connection = connection
        self.cache = cache if cache is not None else {}
        pass

    def get_dialect(self) -> str:
        return self.connection.get_dialect()

    def _schema_cache(self, schema_name: str):
        cache = self.cache.get(schema_name)
        if not cache:
            cache = Cache()
            self.cache[schema_name] = cache
        return cache

    def table_model_cache(
        self, schema_name: str | None, table_name: str
    ) -> TableModel | None:
        if schema_name:
            schema_name = schema_name.lower()
            schema_cache = self._schema_cache(schema_name)
            return schema_cache.get_table_model(table_name)

    def cache_table_model(
        self, schema_name: str | None, table_name: str, table_model: TableModel
    ) -> TableModel:
        if schema_name:
            schema_name = schema_name.lower()
            self._schema_cache(schema_name).put_table_model(table_name, table_model)
        return table_model

    def operation(self, name):
        return Operation(name)


class Operation:
    def __init__(self, name: str):
        self.name = name

    def __enter__(self):
        return self

    def __exit__(self, _exc_type, _exc, _tb):
        pass
