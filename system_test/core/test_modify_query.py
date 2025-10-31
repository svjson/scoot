from system_test.db.service import BackendService

from scoot_core import OperationContext
from scoot_core.query import SQLQueryModifier

def test__remove_single_item_from_select_STAR(db_backend: BackendService):
    # Given
    opctx = OperationContext(db_backend.connection)
    query_mod = SQLQueryModifier("SELECT * FROM users", opctx)

    # When
    query_mod.remove_from_select([{"name": "password_hash"}])

    # Then
    assert (
        query_mod.get_sql()
        == "SELECT id, username, email, created_at, last_login, is_active FROM users"
    )
