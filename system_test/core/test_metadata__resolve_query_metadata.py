import pytest

from system_test.core.nexartrade__users import nexartrade__users_columns
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
