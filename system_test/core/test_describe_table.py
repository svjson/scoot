from system_test.db.service import BackendService
from system_test.db.log import log
from scoot_core import metadata
from scoot_core.model import TableModel


def test__describe_users(db_backend: BackendService):

    table = metadata.describe_table(db_backend.connection, "users")

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
    assert id_column.primary_key == True
