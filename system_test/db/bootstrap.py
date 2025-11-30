import csv
import os
from datetime import datetime
from typing import Type, override

import sqlalchemy
from sqlalchemy import Connection, Table, inspect, text
from sqlalchemy.schema import CreateTable
from sqlalchemy.sql import insert

from .log import log
from .schema_parser import DbSchema, SchemaParser
from .service import BackendService, Prop


class DbBootstrapper:
    def __init__(self, backend: BackendService, db_schema: DbSchema):
        self.backend = backend
        self.db_schema = db_schema
        self.connection = backend.connection

    def _execute(self, statement):
        conn = self.backend.connect().execution_options(isolation_level="AUTOCOMMIT")
        try:
            result = conn.execute(text(statement))
            return result
        except Exception as e:
            log.error(f"Error executing '{statement}': {e}")
            raise e
        finally:
            self.backend.disconnect(conn)

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
                self.create_table(table)
                log.info(f"Creating table '{table.fullname}'")

    def create_container(self):
        pass

    def create_schema(self, schema):
        del schema
        pass

    def create_table(self, table):
        table.metadata.reflect(bind=self.connection.engine)
        self._execute(str(CreateTable(table).compile(self.connection.engine)))


class MariaDBBootstrapper(DbBootstrapper):
    @override
    def create_schema(self, schema):
        self._execute(f"CREATE DATABASE {schema.name};")
        self._reconfigure()


class MSSQLBootstrapper(DbBootstrapper):
    @override
    def create_container(self):
        self._execute(f"CREATE LOGIN nexar WITH PASSWORD = 'StrongPassword123';")
        self._execute(f"ALTER LOGIN nexar ENABLE;")
        self._execute(f"CREATE USER nexar FOR LOGIN nexar;")
        self._execute(f"CREATE DATABASE {self.db_schema.name};")
        self._execute(f"USE {self.db_schema.name};")
        self._execute(f"CREATE USER nexar FOR LOGIN nexar;")
        self._execute(f"ALTER USER nexar WITH LOGIN = nexar;")
        pass

    @override
    def create_schema(self, schema):
        self._execute(f"CREATE SCHEMA {schema.name};")
        self._execute(f"ALTER USER nexar WITH DEFAULT_SCHEMA = {schema.name};")
        self._execute(
            f"GRANT ALTER, CONTROL, DELETE, EXECUTE, INSERT, SELECT, UPDATE ON SCHEMA::{schema.name} TO nexar;"
        )
        self._execute(
            f"GRANT ALTER, CREATE TABLE, CREATE VIEW, CREATE PROCEDURE, CREATE FUNCTION, CREATE SCHEMA, DELETE, EXECUTE, INSERT, SELECT, UPDATE TO nexar;"
        )
        self._reconfigure()


class PostgresBootstrapper(DbBootstrapper):
    @override
    def create_container(self):
        self._execute(f"CREATE DATABASE {self.db_schema.name};")
        self._reconfigure()

    @override
    def create_schema(self, schema):
        self._execute(f"CREATE SCHEMA {schema.name};")


class MySQLBootstrapper(DbBootstrapper):
    @override
    def create_schema(self, schema):
        # No direct CREATE SCHEMA equivalent in MySQL. Default to CREATE DATABASE.
        self._execute(f"CREATE DATABASE {schema.name};")
        self._reconfigure()


class Oracle11gBootstrapper(DbBootstrapper):
    def create_schema(self, schema):
        self._execute(f"CREATE USER {schema.name} IDENTIFIED BY password")
        self._execute(f"GRANT CONNECT, RESOURCE TO {schema.name}")
        self._execute(f"ALTER USER {schema.name} QUOTA UNLIMITED ON USERS")
        self._execute(f"ALTER USER {schema.name} DEFAULT TABLESPACE USERS")
        self._reconfigure()

    def create_table(self, table: Table):
        super().create_table(table)
        for col in table.columns:
            if col.autoincrement == "auto" and col.primary_key:
                seq_name = f"{table.name}_{col.name}_seq"
                log.info(f"Creating sequence {seq_name}...")
                self._execute(
                    f"CREATE SEQUENCE {seq_name} START WITH 1 INCREMENT BY 1 NOCACHE"
                )
                trigger_name = f"{table.name}_{col.name}_trigger"
                log.info(f"Creating trigger {trigger_name}...")
                new_col = f":NEW.{col.name}"

                statement = f"""
                    CREATE OR REPLACE TRIGGER {trigger_name}
                    BEFORE INSERT ON {table.name}
                    FOR EACH ROW
                    BEGIN
                      IF {new_col} IS NULL THEN
                        SELECT {seq_name}.NEXTVAL INTO {new_col} FROM DUAL;
                      END IF;
                    END;
                """

                conn: Connection = self.backend.connect().execution_options(
                    isolation_level="AUTOCOMMIT"
                )

                try:
                    result = conn.connection.cursor().execute(statement)
                    return result
                except Exception as e:
                    log.error(f"Error executing '{statement}': {e}")
                    raise e
                finally:
                    self.backend.disconnect(conn)


class Oracle23cBootstrapper(DbBootstrapper):
    @override
    def create_container(self):
        # no op - use freepdb1 for now
        pass

    def create_schema(self, schema):
        # no op - schema user is created during container initialization
        assert schema is not None
        pass

    def create_table(self, table: Table):
        # Temp solution using seqs and triggers for now
        # FIXME: Use IDENTITY, like so:
        # CREATE TABLE users (
        #   id NUMBER GENERATED BY DEFAULT AS IDENTITY
        #   (START WITH 1 INCREMENT BY 1 CACHE 20),

        super().create_table(table)
        for col in table.columns:
            if col.autoincrement == "auto" and col.primary_key:
                seq_name = f"{table.name}_{col.name}_seq"
                log.info(f"Creating sequence {seq_name}...")
                self._execute(
                    f"CREATE SEQUENCE {seq_name} START WITH 1 INCREMENT BY 1 NOCACHE"
                )
                trigger_name = f"{table.name}_{col.name}_trigger"
                log.info(f"Creating trigger {trigger_name}...")
                new_col = f":NEW.{col.name}"

                statement = f"""
                    CREATE OR REPLACE TRIGGER {trigger_name}
                    BEFORE INSERT ON {table.name}
                    FOR EACH ROW
                    BEGIN
                      IF {new_col} IS NULL THEN
                        SELECT {seq_name}.NEXTVAL INTO {new_col} FROM DUAL;
                      END IF;
                    END;
                """

                conn: Connection = self.backend.connect().execution_options(
                    isolation_level="AUTOCOMMIT"
                )

                try:
                    result = conn.connection.cursor().execute(statement)
                    return result
                except Exception as e:
                    log.error(f"Error executing '{statement}': {e}")
                    raise e
                finally:
                    self.backend.disconnect(conn)


def sanitize_record(table: Table, record: dict[str, str]):
    sanitized = {}
    for col_name, raw in record.items():
        column = table.columns.get(col_name)
        assert column is not None

        if column.autoincrement is True or (
            column.autoincrement == "auto" and column.primary_key
        ):
            continue

        if isinstance(column.type, sqlalchemy.Boolean) or str(column.type) == "TINYINT":
            sanitized[col_name] = raw.lower() in ["true", "1", "yes"]
        elif isinstance(column.type, sqlalchemy.types.INTEGER):
            if raw.lower() in ["true", "yes"]:
                raw = "1"
            elif raw.lower() in ["false", "no"]:
                raw = "0"
            sanitized[col_name] = int(raw)
        elif str(column.type) == "NUMBER":
            sanitized[col_name] = float(raw)
        elif str(column.type) == "DATE":
            sanitized[col_name] = datetime.strptime(raw, "%Y-%m-%d %H:%M:%S")
        else:
            sanitized[col_name] = raw
    return sanitized


def populate_table(backend: BackendService, schema: str, table: str):
    log.info(f"Populating table '{schema}.{table}'...")
    filename = f"system_test/db/schema/{schema}.{table}.csv"
    if os.path.exists(filename):
        with open(filename, "r", newline="") as file:
            conn = backend.connect()
            try:
                reader = csv.DictReader(file)

                engine = backend.connection.engine
                inspector = inspect(conn.engine)

                md = sqlalchemy.MetaData()
                md.reflect(bind=engine)
                tbl = Table(table, md, schema=schema, autoload_with=engine)
                inspector.reflect_table(tbl, include_columns=None)

                for record in reader:
                    conn.execute(insert(tbl).values(sanitize_record(tbl, record)))

            finally:
                backend.disconnect(conn)


def populate_tables(backend: BackendService, db_schema: DbSchema):
    for schema in db_schema.schemas:
        for table in schema.tables:
            populate_table(backend, schema.name, table.name)


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
    schema_parser = SchemaParser.from_file("system_test/db/schema/nexartrade_v1.yaml")
    db_schema = schema_parser.parse()
    bootstrapper = get_bootstrapper(backend, db_schema)
    bootstrapper.bootstrap()
    try:
        populate_tables(backend, db_schema)
    except Exception as e:
        log.error(f"Populating tables failed. {e}")
        raise e
