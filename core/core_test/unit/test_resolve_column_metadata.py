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


NEXAR_TRADE__PRODUCTS_COLUMNS = [
    {"name": "id", "column": "id", "table": "products", "constraints": []},
    {"name": "name", "column": "name", "table": "products", "constraints": []},
    {
        "name": "description",
        "column": "description",
        "table": "products",
        "constraints": [],
    },
    {"column": "sku", "constraints": [], "name": "sku", "table": "products"},
    {"column": "price", "constraints": [], "name": "price", "table": "products"},
    {
        "name": "in_stock",
        "column": "in_stock",
        "table": "products",
        "constraints": [],
    },
    {
        "name": "created_at",
        "column": "created_at",
        "table": "products",
        "constraints": [],
    },
]


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
    assert columns == NEXAR_TRADE__PRODUCTS_COLUMNS


def test_resolve_column_metadata__table_dot_star_from_nexartrade_products(
    dialect_op_env: OperationEnv,
):
    # Given
    expr = sqlglot.parse_one(
        "SELECT products.* FROM products",
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
    assert columns == NEXAR_TRADE__PRODUCTS_COLUMNS


def test_resolve_column_metadata__table_dot_star_from_two_aliased_tables(
    dialect_op_env: OperationEnv,
):
    # Given
    expr = sqlglot.parse_one(
        """
    SELECT
      o.*, oi.*
    FROM
      orders as o,
      order_items as oi
    WHERE oi.order_id = o.id
      AND o.id = 2;
    """
    )

    tbl_exprs = list(expr.find_all(expressions.Table))
    known_tables = {
        "orders": translate_table_schema(
            YamlSchemaReader(
                yaml.safe_load(
                    """
name: orders
columns:
  - name: id
    type: integer
    primary_key: true
    auto_increment: true
  - name: user_id
    type: integer
    references: users(id)
"""
                )
            ),
            TableModelEmitter(dialect_op_env.get_dialect()),
        ),
        "order_items": translate_table_schema(
            YamlSchemaReader(
                yaml.safe_load(
                    """
name: order_items
description: "Line items for orders, with product references."
columns:
  - name: id
    type: integer
    primary_key: true
    auto_increment: true
  - name: order_id
    type: integer
    references: orders(id)
  - name: product_id
    type: integer
    references: products(id)
"""
                )
            ),
            TableModelEmitter(dialect_op_env.get_dialect()),
        ),
    }
    columns = []

    # When
    for e in expr.expressions:
        columns_meta = resolve_column_metadata(
            dialect_op_env, e, known_tables, tbl_exprs
        )
        columns.extend(columns_meta)

    # Then
    assert columns == [
        {"name": "o.id", "column": "id", "table": "orders", "constraints": []},
        {
            "name": "o.user_id",
            "column": "user_id",
            "table": "orders",
            "constraints": [],
        },
        {
            "name": "oi.id",
            "column": "id",
            "table": "order_items",
            "constraints": [],
        },
        {
            "name": "oi.order_id",
            "column": "order_id",
            "table": "order_items",
            "constraints": [],
        },
        {
            "name": "oi.product_id",
            "column": "product_id",
            "table": "order_items",
            "constraints": [],
        },
    ]
