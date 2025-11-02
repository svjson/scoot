from scoot_core.model import TableModel, ColumnModel
from scoot_core import types

INTEGER = types.Integer(bits=64, signed=types.SIGNED)
DATETIME = types.Temporal(
    date=types.Date(min=(0000, 1, 1), max=(9999, 12, 31), calendar="gregorian"),
    time=types.Time(clock="24"),
)


def VARCHAR(size):
    types.String(max_len=size, encoding="utf-8", collation=None)


BOOLEAN = types.Boolean()


def test_get_column__case_insensitivity():

    # Given
    users_table = TableModel(
        "users",
        "nexartrade_staging",
        columns=[
            ColumnModel("Id", INTEGER, False, True, None),
            ColumnModel("UserName", VARCHAR(50), True, False, None),
            ColumnModel("email", VARCHAR(100), True, False, None),
            ColumnModel("password_hash", VARCHAR(255), True, False, None),
            ColumnModel("created_at", DATETIME, True, False, None),
            ColumnModel("last_login", DATETIME, True, False, None),
            ColumnModel("is_active", BOOLEAN, True, False, None),
        ],
    )

    assert users_table.get_column("Id")
    assert users_table.get_column("id")
    assert users_table.get_column("ID")
    assert users_table.get_column("iD")
