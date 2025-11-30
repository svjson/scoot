from scoot_core.openv import OperationEnv
from scoot_core.query import SQLQueryModifier


def test_identify_items__single_table__with_regular_columns(
    dialect_op_env: OperationEnv,
):
    # Given
    query = "SELECT id, email FROM nexartrade_staging.users"

    # When
    query_mod = SQLQueryModifier(query, dialect_op_env)
    # Then
    assert query_mod._tbl_expr == [
        {"table": "users", "space": "nexartrade_staging"}
    ]

    assert query_mod._select_items == [
        {
            "column": "id",
            "sql": "id",
            "table": {"table": "users", "space": "nexartrade_staging"},
        },
        {
            "column": "email",
            "sql": "email",
            "table": {"table": "users", "space": "nexartrade_staging"},
        },
    ]


def test_identify_items__single_table__with_aliased_columns(
    dialect_op_env: OperationEnv,
):
    # Given
    query = "SELECT id as ident, email as contact FROM nexartrade_staging.users"

    # When
    query_mod = SQLQueryModifier(query, dialect_op_env)

    # Then
    assert query_mod._tbl_expr == [
        {"table": "users", "space": "nexartrade_staging"}
    ]

    assert query_mod._select_items == [
        {
            "column": "id",
            "sql": "id AS ident",
            "alias": "ident",
            "table": {"table": "users", "space": "nexartrade_staging"},
        },
        {
            "column": "email",
            "sql": "email AS contact",
            "alias": "contact",
            "table": {"table": "users", "space": "nexartrade_staging"},
        },
    ]


def test_identify_items__aliased_table(dialect_op_env: OperationEnv):

    # Given
    query = "SELECT u.id, u.email FROM nexartrade_staging.users AS u"

    # When
    query_mod = SQLQueryModifier(query, dialect_op_env)

    # Then
    assert query_mod._tbl_expr == [
        {"table": "users", "alias": "u", "space": "nexartrade_staging"}
    ]

    assert query_mod._select_items == [
        {
            "column": "id",
            "owner": "u",
            "sql": "u.id",
            "table": {
                "table": "users",
                "alias": "u",
                "space": "nexartrade_staging",
            },
        },
        {
            "column": "email",
            "owner": "u",
            "sql": "u.email",
            "table": {
                "table": "users",
                "alias": "u",
                "space": "nexartrade_staging",
            },
        },
    ]
