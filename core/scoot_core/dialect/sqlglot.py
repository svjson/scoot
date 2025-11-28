def sqlglot_dialect(dialect_name: str):
    if dialect_name == "postgresql":
        return "postgres"
    elif dialect_name == "mssql":
        return "tsql"

    return dialect_name
