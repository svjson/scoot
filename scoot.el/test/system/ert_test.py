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
        case.get("name"),
        load_files=[case.get("file")],
        context_name=db_backend.name,
        root_path=["scoot.el", "test", "system"],
        before_test=f"""
        (progn
          (setq scoot-auto-start-server t)
          (scoot-test--set-connection (list :context "{db_backend.name}"
                                            :name "{db_backend.name}"
                                            :url "{db_backend.get_active_connection_url()}")))
        """,
    )
