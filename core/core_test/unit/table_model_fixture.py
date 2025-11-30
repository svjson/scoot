from scoot_core import types

INTEGER = types.Integer(bits=64, signed=types.SIGNED)
DATETIME = types.Temporal(
    date=types.Date(min=(0000, 1, 1), max=(9999, 12, 31), calendar="gregorian"),
    time=types.Time(clock="24"),
)
BOOLEAN = types.Boolean()


def VARCHAR(size):
    types.String(max_len=size, encoding="utf-8", collation=None)
