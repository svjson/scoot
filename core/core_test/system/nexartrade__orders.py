from .nexartrade_fixtures import nexartrade__table_column, nexartrade__table_columns

ORDERS__COLUMNS = {
    "baseline": [
        {
            "column": "id",
            "constraints": [],
            "default": None,
            "name": "id",
            "native_type": "INTEGER(11)",
            "nullable": False,
            "primary_key": True,
            "table": "orders",
            "type": "INTEGER",
            "typespec": {"bits": 64, "signed": True, "type": "INTEGER"},
        },
        {"column": "user_id"},
        {"column": "order_number"},
        {"column": "status"},
        {"column": "shipping_address"},
        {
            "column": "total_amount",
            "constraints": [
                {
                    "columns": ["total_amount"],
                    "condition": {"lhs": "total_amount", "pred": ">=", "rhs": "0"},
                    "name": "CONSTRAINT_2",
                    "type": "chk",
                },
                {
                    "columns": ["total_amount"],
                    "condition": {"lhs": "total_amount", "pred": ">=", "rhs": "0"},
                    "name": "CONSTRAINT_2",
                    "type": "chk",
                },
            ],
            "default": None,
            "name": "total_amount",
            "native_type": "DECIMAL(10, 2)",
            "nullable": True,
            "primary_key": False,
            "table": "orders",
            "type": "DECIMAL",
            "typespec": {"precision": 10, "scale": 2, "type": "DECIMAL"},
        },
        {"column": "created_at"},
    ],
    "postgres": [
        {"column": "id", "native_type": "INTEGER"},
        {"column": "user_id"},
        {"column": "order_number"},
        {"column": "status"},
        {"column": "shipping_address"},
        {
            "column": "total_amount",
            "constraints": [
                {
                    "columns": ["total_amount"],
                    "condition": {"lhs": "total_amount", "pred": ">=", "rhs": "0"},
                    "name": "orders_total_amount_check",
                    "type": "chk",
                },
                {
                    "columns": ["total_amount"],
                    "condition": {"lhs": "total_amount", "pred": ">=", "rhs": "0"},
                    "name": "orders_total_amount_check",
                    "type": "chk",
                },
            ],
            "default": None,
            "name": "total_amount",
            "native_type": "NUMERIC(10, 2)",
            "nullable": True,
            "primary_key": False,
            "table": "orders",
            "type": "DECIMAL",
            "typespec": {"precision": 10, "scale": 2, "type": "DECIMAL"},
        },
        {"column": "created_at"},
    ],
    "mssql": [
        {"column": "id", "native_type": "int"},
        {"column": "user_id"},
        {"column": "order_number"},
        {"column": "status"},
        {"column": "shipping_address"},
        {
            "column": "total_amount",
            "constraints": [
                {
                    "columns": ["total_amount"],
                    "condition": {"lhs": "total_amount", "pred": ">=", "rhs": "0"},
                    "name": "CK__orders__total_am__4316F928",
                    "type": "chk",
                }
            ],
            "default": None,
            "name": "total_amount",
            "native_type": "decimal(10, 2)",
            "nullable": True,
            "primary_key": False,
            "table": "orders",
            "type": "DECIMAL",
            "typespec": {"precision": 10, "scale": 2, "type": "DECIMAL"},
        },
    ],
    "mysql": [
        {"column": "id", "native_type": "INTEGER"},
        {"column": "user_id", "native_type": "INTEGER"},
        {},
        {},
        {},
        {
            "column": "total_amount",
            "constraints": [
                {
                    "columns": ["total_amount"],
                    "condition": {"lhs": "total_amount", "pred": ">=", "rhs": "0"},
                    "name": "orders_chk_2",
                    "type": "chk",
                },
                {
                    "columns": ["total_amount"],
                    "condition": {"lhs": "total_amount", "pred": ">=", "rhs": "0"},
                    "name": "orders_chk_2",
                    "type": "chk",
                },
            ],
        },
        {},
    ],
    "oracle_11g": [
        {
            "column": "id",
            "native_type": "INTEGER",
            "type": "DECIMAL",
            "typespec": {"precision": 38, "scale": 0, "type": "DECIMAL"},
        },
        {"column": "user_id"},
        {"column": "order_number"},
        {"column": "status"},
        {"column": "shipping_address"},
        {
            "column": "total_amount",
            "constraints": [
                {
                    "columns": ["total_amount"],
                    "condition": {"lhs": "total_amount", "pred": ">=", "rhs": "0"},
                    "name": "sys_c006999",
                    "type": "chk",
                },
                {
                    "columns": ["total_amount"],
                    "condition": {"lhs": "total_amount", "pred": ">=", "rhs": "0"},
                    "name": "sys_c006999",
                    "type": "chk",
                },
            ],
            "default": None,
            "name": "total_amount",
            "native_type": "NUMBER(10, 2)",
            "nullable": True,
            "primary_key": False,
            "table": "orders",
            "type": "DECIMAL",
            "typespec": {"precision": 38, "scale": 0, "type": "DECIMAL"},
        },
        {"column": "created_at"},
    ],
    "oracle_23c": [
        {
            "column": "id",
            "native_type": "INTEGER",
            "type": "DECIMAL",
            "typespec": {"precision": 38, "scale": 0, "type": "DECIMAL"},
        },
        {"column": "user_id"},
        {"column": "order_number"},
        {"column": "status"},
        {"column": "shipping_address"},
        {
            "column": "total_amount",
            "constraints": [
                {
                    "columns": ["total_amount"],
                    "condition": {"lhs": "total_amount", "pred": ">=", "rhs": "0"},
                    "name": "sys_c008666",
                    "type": "chk",
                },
                {
                    "columns": ["total_amount"],
                    "condition": {"lhs": "total_amount", "pred": ">=", "rhs": "0"},
                    "name": "sys_c008666",
                    "type": "chk",
                },
            ],
            "default": None,
            "name": "total_amount",
            "native_type": "NUMBER(10, 2)",
            "nullable": True,
            "primary_key": False,
            "table": "orders",
            "type": "DECIMAL",
            "typespec": {"precision": 38, "scale": 0, "type": "DECIMAL"},
        },
        {"column": "created_at"},
    ],
}


def nexartrade__orders_columns(dialect: str, columns: list[str] | None = None):
    return nexartrade__table_columns(ORDERS__COLUMNS, dialect, columns)


def nexartrade__orders_column(
    dialect: str, col: int | str, override: dict | None = None
):
    return nexartrade__table_column(ORDERS__COLUMNS, dialect, col, override)
