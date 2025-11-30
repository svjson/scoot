import pytest

from system_test.emacs.ert_test import discover_ert_tests
from system_test.emacs_test_runner import EmacsDaemon, run_test

ERT_UNIT_TESTS = discover_ert_tests("./scoot.el/test/unit")


@pytest.mark.parametrize("case", ERT_UNIT_TESTS, ids=lambda c: c["name"])
def test__scoot_unit_ert(case, emacs_unit_test_daemon: EmacsDaemon):
    """
    Run all pure scoot.el ERT unit tests.
    """
    run_test(
        emacs_unit_test_daemon,
        case.get("file"),
        case.get("name"),
        root_path=["scoot.el", "test", "unit"],
    )
