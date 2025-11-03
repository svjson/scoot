import pytest
from system_test.db.backends import BACKENDS


def pytest_addoption(parser):
    parser.addoption(
        "--backend",
        action="append",
        default=[],
        help="Specify database backends to run tests against. Options: mssql, oracle, mysql, mariadb, postgres, all.",
    )
    parser.addoption(
        "--emacs", action="store_true", help="Enable emacs integration tests"
    )
    parser.addoption(
        "--cli", action="store_true", help="Enable scoot-cli integration tests"
    )
    parser.addoption(
        "--core", action="store_true", help="Enable scoot-core integration tests"
    )


def pytest_configure(config):
    config.addinivalue_line("markers", "cli: mark test as requiring --cli to run")
    config.addinivalue_line("markers", "core: mark test as requiring --core to run")


def pytest_runtest_setup(item):
    if "cli" in item.keywords and not item.config.getoption("--cli"):
        pytest.skip("need --cli option to run")
    elif "core" in item.keywords and not item.config.getoption("--core"):
        pytest.skip("need --core option to run")


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
