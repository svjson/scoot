import sqlglot
import yaml
from scoot_core.dialect.sqlglot import sqlglot_dialect
from scoot_core.metadata import resolve_column_metadata
from scoot_core.model import TableModel
from scoot_core.openv import OperationEnv
from scoot_core.schema.tbl_model import TableModelEmitter
from scoot_core.schema.translate import translate_table_schema
from scoot_core.schema.yaml_schema import YamlSchemaReader
from sqlglot import expressions


def nexartrade_tables(dialect: str):
    return {
        "products": translate_table_schema(
            YamlSchemaReader(
                yaml.safe_load(
                    """
name: products
description: "Products available for purchase."
columns:
  - name: id
    type: integer
    primary_key: true
    auto_increment: true
  - name: name
    type: varchar(100)
    unique: true
  - name: description
    type: text
  - name: sku
    type: varchar(20)
    unique: true
  - name: price
    type: decimal(10, 2)
    check: "price >= 0"
  - name: in_stock
    type: integer
    check: "in_stock >= 0"
  - name: created_at
    type: timestamp
    default: current_timestamp
"""
                )
            ),
            TableModelEmitter(dialect),
        )
    }


def test_resolve_column_metadata__id_and_name_from_nexartrade_products(
    dialect_op_env: OperationEnv,
):
    # Given
    expr = sqlglot.parse_one(
        "SELECT id, name FROM products",
        read=sqlglot_dialect(dialect_op_env.get_dialect()),
    )
    tbl_exprs = list(expr.find_all(expressions.Table))
    known_tables = nexartrade_tables(dialect_op_env.get_dialect())
    columns = []

    # When
    for e in expr.expressions:
        columns_meta = resolve_column_metadata(
            dialect_op_env, e, known_tables, tbl_exprs
        )
        columns.extend(columns_meta)

    # Then
    assert columns == [
        {"column": "id", "constraints": [], "name": "id", "table": "products"},
        {"column": "name", "constraints": [], "name": "name", "table": "products"},
    ]


def test_resolve_column_metadata__star_from_nexartrade_products(
    dialect_op_env: OperationEnv,
):
    # Given
    expr = sqlglot.parse_one(
        "SELECT * FROM products", read=sqlglot_dialect(dialect_op_env.get_dialect())
    )
    tbl_exprs = list(expr.find_all(expressions.Table))
    known_tables = nexartrade_tables(dialect_op_env.get_dialect())
    columns = []

    # When
    for e in expr.expressions:
        columns_meta = resolve_column_metadata(
            dialect_op_env, e, known_tables, tbl_exprs
        )
        columns.extend(columns_meta)

    # Then
    assert columns == [
        {"column": "id", "constraints": [], "name": "id", "table": "products"},
        {"column": "name", "constraints": [], "name": "name", "table": "products"},
        {
            "column": "description",
            "constraints": [],
            "name": "description",
            "table": "products",
        },
        {"column": "sku", "constraints": [], "name": "sku", "table": "products"},
        {
            "column": "price",
            "constraints": [],
            "name": "price",
            "table": "products",
        },
        {
            "column": "in_stock",
            "constraints": [],
            "name": "in_stock",
            "table": "products",
        },
        {
            "column": "created_at",
            "constraints": [],
            "name": "created_at",
            "table": "products",
        },
    ]
