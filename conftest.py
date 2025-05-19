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
