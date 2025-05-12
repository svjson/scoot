from functools import wraps
from sqlalchemy import text
import pytest
import docker
import threading
import time
from typing import Union, Optional
from system_test.db.schema_parser import SchemaParser
from system_test.db.bootstrap import get_bootstrapper
from system_test.db.log import log
from scoot_core.connection import Connection

KvProp = dict[str, str]
Prop = Union[str, int, KvProp]
BackendConfig = dict[str, Prop]


class TestBackend:
    def __init__(self, config: BackendConfig, connection: Connection):
        self.config = config
        self.connection = connection

    def teardown(self):
        self.connection.close()


BACKENDS: dict[str, BackendConfig] = {
    "mariadb": {
        "name": "mariadb",
        "image": "mariadb:10.11",
        "port": 13307,
        "target_port": 3306,
        "connection_url": "mysql+pymysql://root:password@localhost:13307/mysql",
        "env": {"MARIADB_ROOT_PASSWORD": "password"},
        "ping_query": "SELECT 1;",
    },
    "mssql": {
        "name": "mssql",
        "image": "mcr.microsoft.com/mssql/server:2022-CU5-ubuntu-20.04",
        "port": 11433,
        "target_port": 1433,
        "connection_url": "mssql+pyodbc://sa:Password123@localhost:11433/tempdb?driver=ODBC+Driver+18+for+SQL+Server&TrustServerCertificate=yes",
        "env": {"ACCEPT_EULA": "Y", "MSSQL_SA_PASSWORD": "Password123"},
        "ping_query": "SELECT 1;",
    },
    "mysql": {
        "name": "mysql",
        "image": "mysql:8.4",
        "port": 13306,
        "target_port": 3306,
        "connection_url": "mysql+pymysql://root:password@localhost:13306",
        "env": {"MYSQL_ROOT_PASSWORD": "password"},
        "ping_query": "SELECT 1;",
    },
    "oracle_11g": {
        "name": "oracle_11g",
        "image": "wnameless/oracle-xe-11g-r2",
        "port": 11521,
        "target_port": 1521,
        "connection_url": "oracle+cx_oracle://system:oracle@localhost:11521/?service_name=XE",
        "env": {"ORACLE_ENABLE_XDB": "true", "ORACLE_ALLOW_REMOTE": "true"},
        "ping_query": "SELECT 1 FROM DUAL",
    },
    "oracle_23c": {
        "name": "oracle_23c",
        "image": "gvenzl/oracle-free:23.7-slim-faststart",
        "port": 11522,
        "target_port": 1521,
        "connection_url": "oracle+cx_oracle://nexar:password@(DESCRIPTION=(ADDRESS=(PROTOCOL=TCP)(HOST=localhost)(PORT=11522))(CONNECT_DATA=(SERVICE_NAME=freepdb1)))",
        "env": {
            "ORACLE_PASSWORD": "oracle",
            "APP_USER": "nexar",
            "APP_USER_PASSWORD": "password",
        },
        "ping_query": "SELECT 1 FROM DUAL",
    },
    "postgres": {
        "name": "postgres",
        "image": "postgres:15.3",
        "port": 15432,
        "target_port": 5432,
        "connection_url": "postgresql+psycopg2://postgres:password@localhost:15432/postgres",
        "env": {"POSTGRES_PASSWORD": "password"},
        "ping_query": "SELECT 1;",
    },
}


client = docker.from_env()
lock = threading.Lock()


@pytest.fixture(scope="session")
def db_backend(request, backend):
    if not backend:
        raise Exception(f"No Backend specified.")

    backend_config = BACKENDS.get(backend)
    if not backend_config:
        raise Exception(f"Unknown backend {backend}")

    container_name = f"scoot_{backend_config['name']}_{backend_config['port']}"
    container = None
    test_backend = None

    try:
        lock.acquire()
        cleanup_existing_container(container_name)
        container = start_container(backend_config, container_name)
        wait_for_container(container)
        connection = wait_for_service(backend_config)
        bootstrap_database(connection, backend_config)

        test_backend = TestBackend(backend_config, connection)
        yield test_backend
    finally:
        try:
            log.info(f"({backend}) Stopping and removing: {container_name}")
            if test_backend:
                test_backend.teardown()
            # print("dummy")
            if container:
                container.stop()
        finally:
            if container:
                container.remove()
            ##log.info(f"({backend}) Not stopping nor removing: {container_name}")
            lock.release()


def cleanup_existing_container(container_name: str) -> None:
    """Stop and/or remove any container from previous test runs
    that may have been left over due do aborted runs or crashes."""
    for container in client.containers.list(all=True):
        if container.name == container_name:
            try:
                if container.status == "running":
                    container.stop()
                container.remove()
                log.info(f"Removed existing container: {container_name}")
            except Exception as e:
                log.error(f"Error while removing container {container_name}: {e}")


def start_container(backend_config: dict, container_name: str):
    """Start the Docker container according to the test backend specified
    in backend_config."""
    log.info(f"Starting container: {container_name}")
    return client.containers.run(
        image=backend_config["image"],
        name=container_name,
        ports={f"{backend_config["target_port"]}/tcp": backend_config["port"]},
        detach=True,
        environment=backend_config["env"],
    )


def wait_and_retry(max_attempts=30, wait_interval: float = 0.5):

    def func_wrapper(func):
        @wraps(func)
        def retry_wrapper(*args, **kwargs):
            retries = 0
            success_sem = False
            last_result = None

            while not success_sem and retries < max_attempts:

                last_result = func(*args, **kwargs)
                if last_result and not isinstance(last_result, Exception):
                    return last_result
                time.sleep(wait_interval)
                retries += 1

            raise RuntimeError(
                f"Action {func.__name__} did not complete in time. Last attempt gave: {last_result}"
            )

        return retry_wrapper

    return func_wrapper


@wait_and_retry(wait_interval=0.5)
def wait_for_container(container) -> bool:
    """Wait for a started container to reach the "running" state."""

    log.info("Waiting for container to enter state 'running'...")

    container.reload()
    if container.status == "running":
        log.info(f"Container {container.name} is now running")
        return True

    return False


@wait_and_retry(wait_interval=1.0)
def wait_for_service(backend_config) -> Optional[Connection]:
    """Wait for the databaser server to become responsive and accept requests."""

    log.info(
        f"Waiting for service at localhost:{backend_config['port']} to become responsive..."
    )

    scoot_conn = None
    conn = None

    try:
        scoot_conn = Connection(backend_config["connection_url"])
        conn = scoot_conn.engine.connect()

        conn.execute(text(backend_config["ping_query"]))
        conn.close()
        return scoot_conn
    except Exception as e:
        if conn and not conn.closed:
            conn.close()
        if scoot_conn:
            scoot_conn.close()

        return e


def bootstrap_database(connection, backend_config: dict) -> None:
    schema_parser = SchemaParser.from_file(
        "system_test/db/schema/nexartrade_v1.yaml"
    )
    db_schema = schema_parser.parse()
    bootstrapper = get_bootstrapper(connection, backend_config, db_schema)
    bootstrapper.bootstrap()
