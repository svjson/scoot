from textwrap import dedent

from cli_test.system.run_scoot_cli import ScootCli
from system_test.db.service import BackendService
from system_test.util import only_for_backend, only_for_backends


@only_for_backend("mariadb")
def test_table_describe_users__nexartrade_mariadb(
    fake_home_env, db_backend: BackendService
):
    result = ScootCli(fake_home_env).run("-c nexartrade table describe users")

    assert (
        result.stdout
        == dedent(
            """
+---------------+--------------+----------+-------------+---------+
| Column Name   | Type         | Nullable | Primary Key | Default |
+---------------+--------------+----------+-------------+---------+
| id            | INTEGER(11)  | False    | True        | None    |
| username      | VARCHAR(50)  | True     | False       | None    |
| email         | VARCHAR(100) | True     | False       | None    |
| password_hash | VARCHAR(255) | True     | False       | None    |
| created_at    | DATETIME     | True     | False       | None    |
| last_login    | DATETIME     | True     | False       | None    |
| is_active     | TINYINT(1)   | True     | False       | None    |
+---------------+--------------+----------+-------------+---------+
            """
        ).lstrip()
    )


@only_for_backend("mssql")
def test_table_describe_users__nexartrade_mssql(
    fake_home_env, db_backend: BackendService
):
    result = ScootCli(fake_home_env).run("-c nexartrade table describe users")

    assert (
        result.stdout
        == dedent(
            """
+---------------+---------------------------------------------------+----------+-------------+---------+
| Column Name   | Type                                              | Nullable | Primary Key | Default |
+---------------+---------------------------------------------------+----------+-------------+---------+
| id            | int                                               | False    | True        | None    |
| username      | varchar(50) COLLATE SQL_Latin1_General_CP1_CI_AS  | True     | False       | None    |
| email         | varchar(100) COLLATE SQL_Latin1_General_CP1_CI_AS | True     | False       | None    |
| password_hash | varchar(255) COLLATE SQL_Latin1_General_CP1_CI_AS | True     | False       | None    |
| created_at    | datetimeoffset                                    | True     | False       | None    |
| last_login    | datetimeoffset                                    | True     | False       | None    |
| is_active     | bit                                               | True     | False       | None    |
+---------------+---------------------------------------------------+----------+-------------+---------+
            """
        ).lstrip()
    )


@only_for_backend("mysql")
def test_table_describe_users__nexartrade_mysql(
    fake_home_env, db_backend: BackendService
):
    result = ScootCli(fake_home_env).run("-c nexartrade table describe users")

    assert (
        result.stdout
        == dedent(
            """
+---------------+--------------+----------+-------------+---------+
| Column Name   | Type         | Nullable | Primary Key | Default |
+---------------+--------------+----------+-------------+---------+
| id            | INTEGER      | False    | True        | None    |
| username      | VARCHAR(50)  | True     | False       | None    |
| email         | VARCHAR(100) | True     | False       | None    |
| password_hash | VARCHAR(255) | True     | False       | None    |
| created_at    | DATETIME     | True     | False       | None    |
| last_login    | DATETIME     | True     | False       | None    |
| is_active     | TINYINT(1)   | True     | False       | None    |
+---------------+--------------+----------+-------------+---------+
            """
        ).lstrip()
    )


@only_for_backends("oracle_11g", "oracle_23c")
def test_table_describe_users__nexartrade_oracle(
    fake_home_env, db_backend: BackendService
):
    result = ScootCli(fake_home_env).run("-c nexartrade table describe users")

    assert (
        result.stdout
        == dedent(
            """
+---------------+-------------------+----------+-------------+---------+
| Column Name   | Type              | Nullable | Primary Key | Default |
+---------------+-------------------+----------+-------------+---------+
| id            | INTEGER           | False    | True        | None    |
| username      | VARCHAR(50 CHAR)  | True     | False       | None    |
| email         | VARCHAR(100 CHAR) | True     | False       | None    |
| password_hash | VARCHAR(255 CHAR) | True     | False       | None    |
| created_at    | DATE              | True     | False       | None    |
| last_login    | DATE              | True     | False       | None    |
| is_active     | INTEGER           | True     | False       | None    |
+---------------+-------------------+----------+-------------+---------+
            """
        ).lstrip()
    )


@only_for_backend("postgres")
def test_table_describe_users__nexartrade_postgres(
    fake_home_env, db_backend: BackendService
):
    result = ScootCli(fake_home_env).run("-c nexartrade table describe users")

    assert (
        result.stdout
        == dedent(
            """
+---------------+--------------------------+----------+-------------+---------+
| Column Name   | Type                     | Nullable | Primary Key | Default |
+---------------+--------------------------+----------+-------------+---------+
| id            | INTEGER                  | False    | True        | None    |
| username      | VARCHAR(50)              | True     | False       | None    |
| email         | VARCHAR(100)             | True     | False       | None    |
| password_hash | VARCHAR(255)             | True     | False       | None    |
| created_at    | TIMESTAMP WITH TIME ZONE | True     | False       | None    |
| last_login    | TIMESTAMP WITH TIME ZONE | True     | False       | None    |
| is_active     | BOOLEAN                  | True     | False       | None    |
+---------------+--------------------------+----------+-------------+---------+
            """
        ).lstrip()
    )
