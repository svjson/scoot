from textwrap import dedent
import pytest
import subprocess

from system_test.db.service import BackendService
from .run_scoot_cli import ScootCli


@pytest.mark.cli
def test_table_list__nexartrade(fake_home_env, db_backend: BackendService):
    result = ScootCli(fake_home_env).run("-c nexartrade table list")

    assert (
        result.stdout
        == dedent(
            """
               +-------------+
               | Table Name  |
               +-------------+
               | order_items |
               | orders      |
               | products    |
               | users       |
               +-------------+
            """
        ).lstrip()
    )

    pass
