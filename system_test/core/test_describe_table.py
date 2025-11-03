import pytest

from system_test.db.service import BackendService
from scoot_core import metadata, OperationEnv


@pytest.mark.core
def test__describe_users(db_backend: BackendService):

    opctx = OperationEnv(db_backend.connection)

    table = metadata.describe_table(opctx, "nexartrade_staging.users")

    assert [column.name for column in table.columns] == [
        "id",
        "username",
        "email",
        "password_hash",
        "created_at",
        "last_login",
        "is_active",
    ]

    id_column = table.get_column("id")
    assert id_column.primary_key
