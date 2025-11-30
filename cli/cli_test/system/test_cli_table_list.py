from textwrap import dedent

from system_test.db.service import BackendService

from .run_scoot_cli import ScootCli


def test_table_list__nexartrade__c_arg_last(fake_home_env, db_backend: BackendService):
    result = ScootCli(fake_home_env).run("table list -c nexartrade")
    print(result.stdout)
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


def test_table_list__nexartrade__c_arg_before_verb(
    fake_home_env, db_backend: BackendService
):
    result = ScootCli(fake_home_env).run("table -c nexartrade list")
    print(result.stdout)
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


def test_table_list__nexartrade__c_arg_first(fake_home_env, db_backend: BackendService):
    result = ScootCli(fake_home_env).run("-c nexartrade table list")
    print(result.stdout)
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
