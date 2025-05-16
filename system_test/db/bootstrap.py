from sqlalchemy import Connection
from sqlalchemy import text
from sqlalchemy.schema import CreateTable
from typing import override, Type

from .log import log
from .schema_parser import DbSchema, SchemaParser
from .service import BackendService, Prop


class DbBootstrapper:
    def __init__(self, backend: BackendService, db_schema: DbSchema):
        self.backend = backend
        self.db_schema = db_schema
        self.connection = backend.connection

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

    def _reconfigure(self):
        self.connection.close()
        self.connection.reconfigure(self.backend.get_active_connection_url())

    def bootstrap(self):
        log.info("Creating container...")
        self.create_container()
        log.info("Creating schemas...")
        for schema in self.db_schema.schemas:
            log.info(f"Creating schema '{schema.name}'")
            self.create_schema(schema)
            for table in schema.tables:
                log.info(f"Creating table '{table.fullname}'")
                table.metadata.reflect(bind=self.connection.engine)
                self._execute(
                    str(CreateTable(table).compile(self.connection.engine))
                )

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
        self._reconfigure()


class MSSQLBootstrapper(DbBootstrapper):
    @override
    def create_container(self):
        self._execute(f"CREATE DATABASE {self.db_schema.name};")
        self._execute(f"USE {self.db_schema.name};")
        pass

    @override
    def create_schema(self, schema):

        self._execute(f"CREATE SCHEMA {schema.name};")
        self._reconfigure()


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
        self._reconfigure()


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
        # no op - schema user is created during container initialization
        assert schema is not None
        pass


bootstrap_impl: dict[Prop, Type[DbBootstrapper]] = {
    "mssql": MSSQLBootstrapper,
    "postgres": PostgresBootstrapper,
    "mysql": MySQLBootstrapper,
    "mariadb": MariaDBBootstrapper,
    "oracle_11g": Oracle11gBootstrapper,
    "oracle_23c": Oracle23cBootstrapper,
}


def get_bootstrapper(backend: BackendService, db_schema) -> DbBootstrapper:
    Bootstrapper_cls = bootstrap_impl.get(backend.name or "")
    assert Bootstrapper_cls is not None
    return Bootstrapper_cls(backend, db_schema)


def bootstrap_database(backend: BackendService) -> None:
    schema_parser = SchemaParser.from_file(
        "system_test/db/schema/nexartrade_v1.yaml"
    )
    db_schema = schema_parser.parse()
    bootstrapper = get_bootstrapper(backend, db_schema)
    bootstrapper.bootstrap()
