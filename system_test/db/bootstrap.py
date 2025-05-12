from sqlalchemy import Connection
from sqlalchemy import text, Table
from sqlalchemy.schema import CreateTable
from typing import Optional, override

from .log import log
from .schema_parser import DbSchema


class DbBootstrapper:
    def __init__(self, connection, backend, db_schema: DbSchema):
        self.backend = backend
        self.db_schema = db_schema
        self.connection = connection
        self.engine = connection.engine

    def _execute(self, statement):
        try:
            conn: Connection = self.connection.engine.connect().execution_options(
                isolation_level="AUTOCOMMIT"
            )

            result = conn.execute(text(statement))
            conn.close()
            return result
        except Exception as e:
            log.error(f"Error executing '{statement}': {e}")
            raise e

    def bootstrap(self):
        log.info("Creating container...")
        self.create_container()
        log.info("Creating schemas...")
        for schema in self.db_schema.schemas:
            log.info(f"Creating schema '{schema.name}'")
            self.create_schema(schema)
            for table in schema.tables:
                log.info(f"Creating table '{table.name}'")
                self._execute(str(CreateTable(table).compile(self.engine)))

    def create_container(self):
        pass

    def create_schema(self, schema):
        del schema
        pass


class MariaDBBootstrapper(DbBootstrapper):
    @override
    def create_container(self):
        # no op
        pass

    def create_schema(self, schema):
        self._execute(f"CREATE DATABASE {schema.name};")
        self._execute(f"USE {schema.name};")


class MSSQLBootstrapper(DbBootstrapper):
    @override
    def create_container(self):
        self._execute(f"CREATE DATABASE {self.db_schema.name};")

    @override
    def create_schema(self, schema):
        self._execute(f"CREATE SCHEMA {schema.name};")


class PostgresBootstrapper(DbBootstrapper):
    @override
    def create_schema(self, schema):
        self._execute(f"CREATE SCHEMA {schema.name};")

    @override
    def create_container(self):
        self._execute(f"CREATE DATABASE {self.db_schema.name};")


class MySQLBootstrapper(DbBootstrapper):
    @override
    def create_container(self):
        # no op
        pass

    @override
    def create_schema(self, schema):
        # No direct CREATE SCHEMA equivalent in MySQL. Default to CREATE DATABASE.
        self._execute(f"CREATE DATABASE {schema.name};")
        self._execute(f"USE {schema.name};")


class Oracle11gBootstrapper(DbBootstrapper):
    @override
    def create_container(self):
        # no op
        pass

    def create_schema(self, schema):
        self._execute(f"CREATE USER {schema.name} IDENTIFIED BY password")
        self._execute(f"GRANT CONNECT, RESOURCE TO {schema.name}")
        self._execute(f"ALTER USER {schema.name} QUOTA UNLIMITED ON USERS")


class Oracle23cBootstrapper(DbBootstrapper):
    @override
    def create_container(self):
        # no op - use freepdb1 for now
        pass

    def create_schema(self, schema):
        # no op - use container-provide table space for now
        pass


def get_bootstrapper(connection, backend, db_schema):
    bootstrap_impl = {
        "mssql": MSSQLBootstrapper,
        "postgres": PostgresBootstrapper,
        "mysql": MySQLBootstrapper,
        "mariadb": MariaDBBootstrapper,
        "oracle_11g": Oracle11gBootstrapper,
        "oracle_23c": Oracle23cBootstrapper,
    }
    return bootstrap_impl.get(backend.get("name"))(connection, backend, db_schema)
