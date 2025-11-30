import pytest

from system_test.db.service import BackendService
from system_test.emacs.ert_test import discover_ert_tests
from system_test.emacs_test_runner import EmacsDaemon, run_test

ERT_SYSTEM_TESTS = discover_ert_tests("./scoot.el/test/system")


@pytest.mark.parametrize("case", ERT_SYSTEM_TESTS, ids=lambda c: c["name"])
def test__scoot_system_ert(case, db_backend: BackendService, emacs_daemon: EmacsDaemon):
    """
    Run all scoot.el ERT system tests.
    """
    run_test(
        emacs_daemon,
        case.get("file"),
        case.get("name"),
        context_name=db_backend.name,
        root_path=["scoot.el", "test", "system"],
    )
