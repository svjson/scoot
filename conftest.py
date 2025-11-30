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
        metafunc.parametrize("backend", backends, scope="session")
