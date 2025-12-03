from typing import TypedDict

import pytest

from system_test import conftest as st
from system_test.db.backends import BACKENDS
from system_test.emacs.ert_test import enumerate_test_suite
from system_test.emacs_test_runner import start_emacs_daemon


class ModuleConfig(TypedDict):
    name: str
    testpath: str


class TestItem(TypedDict):
    test: pytest.Function
    mode: str
    module: str
    backend: str | None


MODULES: dict[str, ModuleConfig] = {
    "cli": {"name": "cli", "testpath": "cli/cli_test"},
    "core": {"name": "core", "testpath": "core/core_test"},
    "server": {"name": "server", "testpath": "server/server_test"},
    "emacs": {"name": "scoot.el", "testpath": "scoot.el/test"},
}


def pytest_addoption(parser):
    """
    Add option parameters to pytest for selecting test suites.
    """

    # Module Flags
    parser.addoption("--emacs", action="store_true", help="Enable scoot.el tests")
    parser.addoption("--cli", action="store_true", help="Enable scoot-cli tests")
    parser.addoption("--core", action="store_true", help="Enable scoot-core tests")
    parser.addoption(
        "--server", action="store_true", help="Enable scoot-server tests"
    )

    # Mode Flags
    parser.addoption("--unit", action="store_true", help="Enable unit tests")
    parser.addoption("--system", action="store_true", help="Enable system tests")

    # Backend Selection
    parser.addoption(
        "--backend",
        action="append",
        default=[],
        help="Specify database backends to run tests against. Options: mssql, oracle_11g, oracle_23c, mysql, mariadb, postgres, all.",
    )


def expand_selected_backends(config) -> list[str]:
    """
    Inspect the backends selected with --backend, if any, and expand to
    actual backend names.

    Args:
        config: Pytest configuration object.

    Returns:
        List of backend names to use for testing.
        - empty list if no --backend arguments were provided
        - a list of all available backends if "all" was provided
        - otherwise a verbatim list of provided backend names

    Raises:
        ValueError: If an unknown backend name was provided.
    """
    selected = config.getoption("--backend") or []

    if not selected:
        return []

    if "all" in selected:
        return list(BACKENDS.keys())

    for b in selected:
        if b not in BACKENDS:
            raise ValueError(f"Unknown backend: {b}")

    return list(selected)


def pytest_configure(config):
    """
    Inspect the provided custom options, collect enabled modules and modes
    and store to custom fields on the pytest configuration object.

    Args:
        config: Pytest configuration object.
    """
    config.pluginmanager.register(st, name="system_test_fixtures")

    enabled_modes = [
        mode for mode in ["unit", "system"] if config.getoption(f"--{mode}")
    ]

    if not enabled_modes:
        enabled_modes.append("unit")

    enabled_modules = {
        mod["name"]
        for module_name, mod in MODULES.items()
        if config.getoption(f"--{module_name}")
    }

    config._backends = expand_selected_backends(config)
    config._enabled_modules = enabled_modules
    config._enabled_modes = enabled_modes
    config._ert_unit_tests = []
    config._ert_system_tests = []


def pytest_sessionstart(session):
    if "scoot.el" in session.config._enabled_modules:
        emacs = start_emacs_daemon("scoot_test")
        session.config._emacs_daemon = emacs

        session.config._ert_unit_tests = enumerate_test_suite(
            emacs, "./scoot.el/test/unit", "unit"
        )
        session.config._ert_system_tests = enumerate_test_suite(
            emacs, "./scoot.el/test/system", "system"
        )


def pytest_collection_modifyitems(config, items: list[pytest.Function]):
    """
    Inspect collected tests and deselect tests marked for a specific
    backend that doesn't match the backend parameterization.

    Args:
        config: Pytest configuration object.
        items: List of collected test items.
    """
    selected: list[TestItem] = []
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
        mode = next(
            (key for key in item.keywords.keys() if key in config._enabled_modes),
            None,
        )
        module_name = next(
            (key for key in item.keywords.keys() if key in config._enabled_modules),
            None,
        )
        backend = (
            str(item.callspec.params.get("backend", None))
            if "backend" in item.fixturenames
            else None
        )

        if not mode or not module_name or is_disabled_by_mark(item):
            deselected.append(item)
            continue

        selected.append(
            {"test": item, "mode": mode, "module": module_name, "backend": backend}
        )

    selected.sort(
        key=lambda t: (
            config._enabled_modes.index(t["mode"]),
            t["backend"],
            t["module"],
        )
    )

    config.hook.pytest_deselected(items=deselected)
    items[:] = (it["test"] for it in selected)


def pytest_generate_tests(metafunc):
    if "db_backend" in metafunc.fixturenames:
        metafunc.parametrize("backend", metafunc.config._backends, scope="session")

    if "ert_unit_test" in metafunc.fixturenames:
        metafunc.parametrize(
            "ert_unit_test", metafunc.config._ert_unit_tests, scope="session"
        )

    if "ert_system_test" in metafunc.fixturenames:
        metafunc.parametrize(
            "ert_system_test", metafunc.config._ert_system_tests, scope="session"
        )
