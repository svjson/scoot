from scoot_core.query import SQLQueryModifier
from scoot_core.model import TableModel, ColumnModel
from unittest.mock import patch

from .table_model_fixture import INTEGER, VARCHAR, DATETIME, BOOLEAN


users_table = TableModel(
    "users",
    "nexartrade_staging",
    columns=[
        ColumnModel("id", INTEGER, False, True, None),
        ColumnModel("username", VARCHAR(50), True, False, None),
        ColumnModel("email", VARCHAR(100), True, False, None),
        ColumnModel("password_hash", VARCHAR(255), True, False, None),
        ColumnModel("created_at", DATETIME, True, False, None),
        ColumnModel("last_login", DATETIME, True, False, None),
        ColumnModel("is_active", BOOLEAN, True, False, None),
    ],
)


def test_remove_column__single_table__no_alias():

    # Given
    query_mod = SQLQueryModifier(
        "SELECT id, email, password_hash, created_at FROM nexartrade_staging.users",
        None,
    )

    # When
    query_mod.remove_from_select([{"name": "password_hash"}])

    # Then
    assert (
        query_mod.get_sql()
        == "SELECT id, email, created_at FROM nexartrade_staging.users"
    )


def test_remove_column__single_table__aliased_table():

    # Given
    query_mod = SQLQueryModifier(
        "SELECT u.id, u.email, u.password_hash, u.created_at FROM nexartrade_staging.users AS u",
        None,
    )

    # When
    query_mod.remove_from_select([{"name": "password_hash"}])

    # Then
    assert (
        query_mod.get_sql()
        == "SELECT u.id, u.email, u.created_at FROM nexartrade_staging.users AS u"
    )


@patch("scoot_core.query.try_describe_table", return_value=users_table)
def test_remove_column__single_table__deconstruct_star(_):

    # Given
    query_mod = SQLQueryModifier("SELECT * FROM nexartrade_staging.users", None)

    # When
    query_mod.remove_from_select([{"name": "password_hash"}])

    # Then
    assert (
        query_mod.get_sql()
        == "SELECT id, username, email, created_at, last_login, is_active FROM nexartrade_staging.users"
    )


@patch("scoot_core.query.try_describe_table", return_value=users_table)
def test_remove_column__single_aliased_table__deconstruct_star(_):

    # Given
    query_mod = SQLQueryModifier(
        "SELECT * FROM nexartrade_staging.users AS u", None
    )

    # When
    query_mod.remove_from_select([{"name": "password_hash"}])

    # Then
    assert (
        query_mod.get_sql()
        == "SELECT u.id, u.username, u.email, u.created_at, u.last_login, u.is_active FROM nexartrade_staging.users AS u"
    )
