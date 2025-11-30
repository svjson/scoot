from typing import TypedDict

from system_test import conftest as st
from system_test.db.backends import BACKENDS


class ModuleConfig(TypedDict):
    testpath: str


MODULES: dict[str, ModuleConfig] = {
    "cli": {"testpath": "cli/cli_test"},
    "core": {"testpath": "core/core_test"},
    "server": {"testpath": "server/server_test"},
    "emacs": {"testpath": "scoot.el/test"},
}


def pytest_addoption(parser):
    """
    Add option parameters to pytest for selecting test suites.
    """

    # Module Flags
    parser.addoption("--emacs", action="store_true", help="Enable scoot.el tests")
    parser.addoption("--cli", action="store_true", help="Enable scoot-cli tests")
    parser.addoption("--core", action="store_true", help="Enable scoot-core tests")
    parser.addoption("--server", action="store_true", help="Enable scoot-server tests")

    # Mode Flags
    parser.addoption("--unit", action="store_true", help="Enable unit tests")
    parser.addoption("--system", action="store_true", help="Enable system tests")

    # Backend Selection
    parser.addoption(
        "--backend",
        action="append",
        default=[],
        help="Specify database backends to run tests against. Options: mssql, oracle, mysql, mariadb, postgres, all.",
    )


def pytest_configure(config):
    """
    Inspect the provided custom options, collect enabled modules and modes
    and store to custom fields on the pytest configuration object.

    Args:
        config: Pytest configuration object.
    """
    config.pluginmanager.register(st, name="system_test_fixtures")

    enabled_modes = {
        mode for mode in ["unit", "system"] if config.getoption(f"--{mode}")
    }

    if not enabled_modes:
        enabled_modes.add("unit")

    enabled_modules = {
        module_name for module_name in MODULES if config.getoption(f"--{module_name}")
    }

    enabled_paths = [
        f"{MODULES[module_name]['testpath']}/{mode}"
        for mode in enabled_modes
        for module_name in enabled_modules
    ]

    config._enabled_modules = enabled_modules
    config._enabled_modes = enabled_modes
    config.args = enabled_paths


def pytest_collection_modifyitems(config, items):
    """
    Inspect collected tests and deselect tests marked for a specific
    backend that doesn't match the backend parameterization.

    Args:
        config: Pytest configuration object.
        items: List of collected test items.
    """
    selected = []
    deselected = []

    def is_disabled_by_mark(item):
        for backend in {
            *[mark.args[0] for mark in item.iter_markers("backend")],
            *[
                backend
                for mark in item.iter_markers("backends")
                for backend in mark.args
            ],
        }:
            if item.callspec.params["backend"] != backend:
                return True

        return False

    for item in items:
        if is_disabled_by_mark(item):
            deselected.append(item)
            continue

        selected.append(item)

    config.hook.pytest_deselected(items=deselected)
    items[:] = selected


def pytest_generate_tests(metafunc):
    """Determines which backends to run based on the `--backend` parameter."""
    if "db_backend" in metafunc.fixturenames:
        backends = metafunc.config.getoption("backend")
        if not backends:
            print("No backend specified.")
        elif "all" in backends:
            backends = BACKENDS.keys()
        else:
            for b in backends:
                if b not in BACKENDS.keys():
                    raise Exception(f"Unsupported backend: {b}")
        metafunc.config._backends = backends
        metafunc.parametrize("backend", backends, scope="session")
