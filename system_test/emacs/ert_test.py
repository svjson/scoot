import pytest
import pprint
from system_test.emacs_test_runner import EmacsDaemon, run_test


import re
from pathlib import Path

ERT_DEFTEST_RE = re.compile(r"^\s*\(ert-deftest\s+([^\s)]+)")


def discover_ert_tests(test_dir: str | Path):
    """
    Traverse all .el test files under test_dir and return a list of
    {'file': relative_file_path, 'name': test_name} dicts suitable
    for pytest parameterization.
    """
    test_dir = Path(test_dir)
    results = []
    for path in test_dir.rglob("*.el"):
        rel_path = path.relative_to(test_dir)
        with open(path, encoding="utf-8") as f:
            for line in f:
                match = ERT_DEFTEST_RE.match(line)
                if match:
                    test_name = match.group(1)
                    results.append({"file": str(rel_path), "name": test_name})
    return results


ERT_DEFTESTS = discover_ert_tests("./scoot.el/test")


@pytest.mark.parametrize("case", ERT_DEFTESTS, ids=lambda c: c["name"])
def test__scoot_unit_ert(case, emacs_unit_test_daemon: EmacsDaemon):
    """
    Run all pure scoot.el ERT unit tests.
    """
    run_test(
        emacs_unit_test_daemon,
        case.get("file"),
        case.get("name"),
        root_path=["scoot.el", "test"],
    )
