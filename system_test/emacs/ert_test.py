import re
from pathlib import Path


ERT_DEFTEST_RE = re.compile(r"^\s*\(ert-deftest\s+([^\s)]+)")


def _get_test_files(test_dir: str | Path):
    test_dir = Path(test_dir)
    return test_dir.rglob("*.el")


def discover_ert_tests(test_dir: str | Path):
    """
    Traverse all .el test files under test_dir and return a list of
    {'file': relative_file_path, 'name': test_name} dicts suitable
    for pytest parameterization.
    """
    results = []
    for path in _get_test_files(test_dir):
        with open(path, encoding="utf-8") as f:
            for line in f:
                match = ERT_DEFTEST_RE.match(line)
                if match:
                    test_name = match.group(1)
                    results.append(
                        {"file": str(path.relative_to(test_dir)), "name": test_name}
                    )
    return results

