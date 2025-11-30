import json
import os
import threading

import pytest

from .db.backends import BACKENDS
from .db.bootstrap import bootstrap_database
from .db.service import BackendService, start_service
from .emacs_test_runner import start_emacs_daemon, start_emacs_unit_test_daemon

lock = threading.Lock()

# def pytest_sessionstart(session):
#    """
#    Initialize emacs daemon hooks and enumerate ERT tests if any
#    ERT test suites are enabled in the run
#    """
#    if not


@pytest.fixture(scope="session")
def db_backend(request, backend):
    if not backend:
        raise Exception("No Backend specified.")

    if backend not in BACKENDS.keys():
        raise Exception(f"Unknown backend {backend}")

    db_backend = None
    try:
        lock.acquire()
        db_backend = start_service(backend)
        bootstrap_database(db_backend)
        yield db_backend
    finally:
        try:
            if db_backend:
                db_backend.teardown()
        finally:
            lock.release()


@pytest.fixture(scope="session", name="emacs_unit_test_daemon")
def emacs_unit_test_fixture(request):
    if (
        "emacs" not in request.config._enabled_modules
        or "unit" not in request.config._enabled_modes
    ):
        pytest.skip("Emacs unit tests are disabled")
    emacs_unit_daemon = start_emacs_unit_test_daemon()
    yield emacs_unit_daemon
    emacs_unit_daemon.stop()


@pytest.fixture(scope="session", name="emacs_daemon")
def emacs_fixture(request, db_backend):
    if (
        "emacs" not in request.config._enabled_modules
        or "system" not in request.config._enabled_modes
    ):
        pytest.skip("Emacs tests are disabled")

    emacs_daemon = start_emacs_daemon(db_backend)
    yield emacs_daemon
    emacs_daemon.stop()


@pytest.fixture(scope="session", name="fake_home_env")
def cli_test_setup(request, tmp_path_factory, db_backend: BackendService):
    if not request.config.getoption("--cli"):
        return

    fake_home = tmp_path_factory.mktemp("fake_home")
    config_dir = fake_home / ".scoot/config"
    config_dir.mkdir(parents=True, exist_ok=True)

    config = {
        "connections": {"default": {"url": db_backend.get_active_connection_url()}}
    }

    (config_dir / "nexartrade.json").write_text(json.dumps(config, indent=2))

    return {**os.environ, "HOME": str(fake_home)}
