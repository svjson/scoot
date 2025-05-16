from typing import Union, Optional

import docker
from docker.models.containers import Container
from sqlalchemy import text

from .backends import BACKENDS
from .log import log
from .infra import wait_and_retry

from scoot_core.connection import Connection

client = docker.from_env()

RUNNING = "running"

KvProp = dict[str, str]
Prop = Union[str, int, KvProp]
BackendConfig = dict[str, Prop]


class BackendService:
    def __init__(
        self, config: BackendConfig, container: Container, connection: Connection
    ):
        self.config = config
        self.connection = connection
        self.container = container
        self.container_name = container.name
        self.name = config.get("name")

    def get_active_connection_url(self):
        return self.config.get(
            "active_connection_url", self.config.get("connection_url")
        )

    def teardown(self):
        try:
            self.connection.close()
        finally:
            if self.container is not None:
                kill_container(self.name, self.container)


def start_service(backend_name: str) -> BackendService:
    if not backend_name:
        raise ValueError(f"No backend specified.")

    backend_config = BACKENDS.get(backend_name)
    if not backend_config:
        raise KeyError(f"Unrecognized backend: {backend_name}")

    container_name = f"scoot_{backend_config['name']}_{backend_config['port']}"
    container = None

    try:
        cleanup_existing_container(container_name)
        container = start_container(backend_config, container_name)
        wait_for_container(container)
        connection = wait_for_service(backend_config)

        return BackendService(backend_config, container, connection)
    except Exception as e:
        if container:
            kill_container(backend_name, container)
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
