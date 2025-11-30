import os
import threading
from typing import Optional, Union

import docker
import sqlalchemy
from docker.models.containers import Container
from scoot_core.connection import Connection
from sqlalchemy import text

from .backends import BACKENDS
from .infra import wait_and_retry
from .log import log

client = docker.from_env()

RUNNING = "running"

KvProp = dict[str, str]
Prop = Union[str, int, KvProp]
BackendConfig = dict[str, Prop]


class BackendService:
    """
    Represents a started backend service running in a Docker container.

    The responsibility of starting the service falls outside of this class,
    and it is used only wrap the state of the service once started.

    Attributes:
        config (BackendConfig): The backend configuration dictionary.
        container (Container): The Docker container instance.
        connection (Connection): The database connection instance.
        container_name (str): The name of the Docker container.
        name (str): The name of the backend service.
    """

    def __init__(
        self, config: BackendConfig, container: Container, connection: Connection
    ):
        """
        Initialize the BackendService with the given configuration,
        container, and connection.

        Args:
            config (BackendConfig): The backend configuration dictionary.
            container (Container): The Docker container instance.
            connection (Connection): The database connection instance.
        """
        self.config = config
        self.connection = connection
        self.container = container
        self.container_name = container.name
        self.name = config.get("name")

    def get_name(self):
        """
        Get the name of the backend service.
        """
        return str(self.name)

    def get_connection_url(self):
        """
        Get the base connection URL of the backend service.

        This is the connection URL used to initially connect to the database
        and before any schemas or state is applied.

        This may differ from the connection URL used to access the fully
        bootstrapped service.
        """
        return self.connection.url

    def get_active_connection_url(self):
        """
        Get the "active" connection URL of the backend service.

        This is the connection URL used to access the fully bootstrapped
        service. This may differ from the base connection URL if additional
        schemas or state have been applied to the service.
        """
        return self.config.get(
            "active_connection_url", self.config.get("connection_url")
        )

    def connect(self) -> sqlalchemy.Connection:
        """
        Create and return a new database connection with AUTOCOMMIT isolation level.
        """
        return self.connection.connect().execution_options(isolation_level="AUTOCOMMIT")

    def disconnect(self, conn: sqlalchemy.Connection):
        """
        Disconnect the given database connection.
        """
        self.connection.disconnect(conn)

    def teardown(self):
        """
        Teardown the backend service by closing the connection and
        stopping/removing the container.
        """
        try:
            log.info("Closing connections...")
            self.connection.close()
        finally:
            if self.container is not None:
                kill_container(self.name, self.container)


def start_service(backend_name: str) -> BackendService:
    """
    Start the specified backend service in a Docker container and wait for
    it to become responsive.

    Args:
        backend_name (str): The name of the backend service to start.
    Returns:
        BackendService: An instance of BackendService representing the started service.
    """
    if not backend_name:
        raise ValueError("No backend specified.")

    backend_config = BACKENDS.get(backend_name)
    if not backend_config:
        raise KeyError(f"Unrecognized backend: {backend_name}")

    container_name = f"scoot_{backend_config['name']}_{backend_config['port']}"
    log_thread = None
    container = None

    try:
        cleanup_existing_container(container_name)
        container = start_container(backend_config, container_name)
        if os.getenv("SCOOT_DEBUG_CONTAINERS"):
            log_thread = stream_container_logs(container, prefix=container_name)
        wait_for_container(container)
        connection = wait_for_service(backend_config)

        if connection is None:
            raise TimeoutError(
                f"Service '{backend_name}' did not start within the time out limit."
            )

        return BackendService(backend_config, container, connection)
    except Exception as e:
        if container:
            kill_container(backend_name, container)
        if log_thread and log_thread.is_alive():
            log_thread.join()
        raise e


def kill_container(service, container: Container):
    log.info(f"({service}) Stopping and removing: {container.name}")
    try:
        container.reload()
        if container.status == RUNNING:
            container.stop()
    finally:
        container.remove()


def cleanup_existing_container(container_name: str) -> None:
    """Stop and/or remove any container from previous test runs
    that may have been left over due do aborted runs or crashes."""
    for container in client.containers.list(all=True):
        if container.name == container_name:
            try:
                if container.status == RUNNING:
                    container.stop()
                container.remove()
                log.info(f"Removed existing container: {container_name}")
            except Exception as e:
                log.error(f"Error while removing container {container_name}: {e}")


def stream_container_logs(container, prefix=None):
    def _stream():
        for line in container.logs(stream=True, follow=True, stdout=True, stderr=True):
            msg = line.decode("utf-8", errors="replace").rstrip()
            if prefix:
                log.info(f"[{prefix}] {msg}")
            else:
                log.info(msg)

    thread = threading.Thread(target=_stream, daemon=True)
    thread.start()
    return thread


def start_container(backend_config: dict, container_name: str):
    """Start the Docker container according to the test backend specified
    in backend_config."""
    log.info(f"Starting container: {container_name}")
    container = client.containers.run(
        image=backend_config["image"],
        name=container_name,
        ports={f"{backend_config['target_port']}/tcp": backend_config["port"]},
        detach=True,
        environment=backend_config["env"],
    )
    return container


@wait_and_retry(wait_interval=0.5)
def wait_for_container(container: Container) -> bool:
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
