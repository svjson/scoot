import pytest

from system_test.core.nexartrade__users import (
    nexartrade__users_columns,
    nexartrade__users_column,
)
from system_test.core.nexartrade__orders import (
    nexartrade__orders_columns,
    nexartrade__orders_column,
)

from system_test.db.service import BackendService
from scoot_core import OperationEnv
from scoot_core import metadata


@pytest.mark.core
def test__SELECT_star_FROM_users(db_backend: BackendService):
    # Given
    op_env = OperationEnv(db_backend.connection)

    # When
    resolved = metadata.resolve_query_metadata(op_env, "SELECT * FROM users")

    # Then
    assert resolved == {"columns": nexartrade__users_columns(db_backend.get_name())}


@pytest.mark.core
def test__SELECT_star_FROM_users_u(db_backend: BackendService):
    # Given
    op_env = OperationEnv(db_backend.connection)

    # When
    resolved = metadata.resolve_query_metadata(op_env, "SELECT * FROM users u")

    # Then
    assert resolved == {"columns": nexartrade__users_columns(db_backend.get_name())}


@pytest.mark.core
def test__SELECT_id_username_FROM_users(db_backend: BackendService):
    # Given
    op_env = OperationEnv(db_backend.connection)

    # When
    resolved = metadata.resolve_query_metadata(
        op_env, "SELECT id, username FROM users"
    )

    # Then
    assert resolved == {
        "columns": nexartrade__users_columns(
            db_backend.get_name(), ["id", "username"]
        )
    }


@pytest.mark.core
def test__SELECT_COUNT_FROM_users(db_backend: BackendService):
    # Given
    op_env = OperationEnv(db_backend.connection)

    # When
    resolved = metadata.resolve_query_metadata(op_env, "SELECT COUNT(*) FROM users")

    # Then
    assert resolved == {
        "columns": [
            {"name": "COUNT(*)", "constraints": [], "column": None, "table": None}
        ]
    }


@pytest.mark.core
def test__SELECT_id__username_FROM_users_u(db_backend: BackendService):
    # Given
    op_env = OperationEnv(db_backend.connection)

    # When
    resolved = metadata.resolve_query_metadata(
        op_env, "SELECT id, username FROM users u"
    )

    # Then
    assert resolved == {
        "columns": nexartrade__users_columns(
            db_backend.get_name(), ["id", "username"]
        )
    }


@pytest.mark.core
def test__SELECT_implicit_join_FROM_users_and_orders(db_backend: BackendService):
    # Given
    op_env = OperationEnv(db_backend.connection)

    # When
    resolved = metadata.resolve_query_metadata(
        op_env,
        """
        SELECT
          users.id,
          users.username,
          orders.id,
          total_amount
        FROM
          users,
          orders
        WHERE
          users.id = orders.user_id
        """,
    )

    # Then
    assert resolved == {
        "columns": [
            nexartrade__users_column(
                db_backend.get_name(), "id", override={"name": "users.id"}
            ),
            nexartrade__users_column(
                db_backend.get_name(),
                "username",
                override={"name": "users.username"},
            ),
            nexartrade__orders_column(
                db_backend.get_name(), "id", override={"name": "orders.id"}
            ),
            nexartrade__orders_column(db_backend.get_name(), "total_amount"),
        ]
    }
