from system_test.emacs_test_runner import run_test, EmacsDaemon
from system_test.db.service import BackendService


def test__scoot_result__verify_column_headers_with_primary_and_foreign_keys(
    db_backend: BackendService, emacs_daemon: EmacsDaemon
):
    run_test(
        emacs_daemon,
        "scoot-resultset/column-tests.el",
        "result--verify-column-headers-with-primary-and-foreign-keys",
        context_name=db_backend.name,
    )


def test__scoot_result__verify_cell_rendering__nexartrade_products(
    db_backend: BackendService, emacs_daemon: EmacsDaemon
):
    run_test(
        emacs_daemon,
        "scoot-resultset/column-tests.el",
        "result--verify-cell-rendering--nexartrade-products",
        context_name=db_backend.name,
    )
