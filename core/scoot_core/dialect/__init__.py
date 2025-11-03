from .registry import resolve_type, find_and_apply_additional_constraints


def sqlglot_dialect(dialect_name: str):
    if dialect_name == "postgresql":
        return "postgres"
    elif dialect_name == "mssql":
        return "tsql"

    return dialect_name


__all__ = [
    "sqlglot_dialect",
    "resolve_type",
    "find_and_apply_additional_constraints",
]
