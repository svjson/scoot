def dialect_expected(dialect_ops, dialect):
    return dialect_ops.get(dialect, dialect_ops["default"])
