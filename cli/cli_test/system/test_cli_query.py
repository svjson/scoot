from textwrap import dedent

from system_test.db.service import BackendService

from .run_scoot_cli import ScootCli


def test_query__nexartrade__id_and_name_from_product(
    fake_home_env, db_backend: BackendService
):
    result = ScootCli(fake_home_env).run(
        [
            "query",
            "-c",
            "nexartrade",
            "SELECT id, name FROM products WHERE id <= 2 ORDER BY id",
        ]
    )
    assert result.stderr == ""
    assert (
        result.stdout
        == dedent(
            """
          +----+--------------------------------------------+
          | id | name                                       |
          +----+--------------------------------------------+
          | 1  | Automated neutral Graphical User Interface |
          | 2  | Managed value-added Local Area Network     |
          +----+--------------------------------------------+
        """
        ).lstrip()
    )
