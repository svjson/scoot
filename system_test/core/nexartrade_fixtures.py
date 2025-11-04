import copy


def get_column(columns, col: int | str):
    if isinstance(col, str):
        return next((c for c in columns if c.get("column") == col), {})
    return columns[col]


def nexartrade__table_column(
    column_data, dialect: str, col: int | str, override: dict | None = None
):
    baseline = copy.deepcopy(get_column(column_data["baseline"], col))
    dialect_col_overrides = column_data.get(dialect, None)

    dialect_override = (
        copy.deepcopy(get_column(dialect_col_overrides, col))
        if dialect_col_overrides is not None
        else {}
    )

    if override is None:
        override = {}
    return {} | baseline | dialect_override | override


def nexartrade__table_columns(
    column_data, dialect: str, columns: list[str] | None = None
):
    for_dialect = [
        nexartrade__table_column(column_data, dialect, i)
        for i, _ in enumerate(column_data["baseline"])
    ]

    if columns is None:
        return for_dialect

    return [get_column(for_dialect, c) for c in columns]


assert get_column([{"column": "test1"}, {"column": "test2"}], "test2") == {
    "column": "test2"
}

assert get_column([{"column": "test1"}, {"column": "test2"}], 1) == {
    "column": "test2"
}

assert nexartrade__table_column(
    {"baseline": [{"column": "test1"}, {"column": "test2"}]}, "mariadb", "test2"
) == {"column": "test2"}
