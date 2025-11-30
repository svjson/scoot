import pytest

from system_test.emacs.ert_test import discover_ert_tests
from system_test.emacs_test_runner import EmacsDaemon, run_test

ERT_UNIT_TESTS = discover_ert_tests("./scoot.el/test/unit")


def test__scoot_unit_ert(ert_unit_test, emacs_daemon: EmacsDaemon):
    """
    Run all pure scoot.el ERT unit tests.
    """
    run_test(
        emacs_daemon,
        ert_unit_test,
        root_path=["scoot.el", "test", "unit"],
        before_test="(setq scoot-auto-start-server nil)",
    )
