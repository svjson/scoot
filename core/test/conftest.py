from typing import cast
import pytest
from scoot_core.openv import OperationEnv
from scoot_core.connection import Connection

DIALECTS = ["postgres", "mysql", "mssql", "oracle"]


@pytest.fixture(params=DIALECTS, ids=lambda d: f"dialect={d}")
def dialect_op_env(request):
    dialect_name = request.param

    class FakeConnection:
        def __init__(self, dialect):
            self.dialect = dialect

        def get_dialect(self):
            return self.dialect

    fake_conn = FakeConnection(dialect=dialect_name)

    op_env = OperationEnv(connection=cast(Connection, fake_conn))

    return op_env
