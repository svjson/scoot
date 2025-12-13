import pytest
import yaml
from scoot_core.schema.ddl import DDLReader
from scoot_core.schema.ir import TableIR
from scoot_core.schema.yaml_schema import YamlSchemaReader


def test_ddl_to_table_ir__single_non_nullable_column():
    # Given
    yaml_table = yaml.safe_load(
        """
name: mytable
columns:
  - name: id
    type: integer
    nullable: false
"""
    )

    # When
    reader = YamlSchemaReader(yaml_table)
    table_ir: TableIR = reader.read_table()

    # Then
    assert table_ir.to_dict() == {
        "name": "mytable",
        "columns": [
            {
                "name": "id",
                "type": {"bits": 64, "signed": True, "type": "INTEGER"},
                "source_type": "integer",
                "primary_key": False,
                "nullable": False,
                "unique": False,
                "default": None,
            }
        ],
    }


def test_dll_to_table_ir__nexartrade_users():
    # Given
    yaml_table = yaml.safe_load(
        """
name: users
description: "User accounts and their core attributes."
columns:
  - name: id
    type: integer
    primary_key: true
    auto_increment: true
  - name: username
    type: varchar(50)
    unique: true
  - name: email
    type: varchar(100)
    unique: true
  - name: password_hash
    type: varchar(255)
  - name: created_at
    type: timestamp
    default: current_timestamp
  - name: last_login
    type: timestamp
  - name: is_active
    type: boolean
    default: true
"""
    )

    # When
    reader = YamlSchemaReader(yaml_table)
    table_ir: TableIR = reader.read_table()

    # Then
    assert table_ir.to_dict() == {
        "name": "users",
        "columns": [
            {
                "name": "id",
                "type": {"bits": 64, "signed": True, "type": "INTEGER"},
                "source_type": "integer",
                "primary_key": True,
                "nullable": False,
                "unique": True,
                "default": None,
            },
            {
                "default": None,
                "name": "username",
                "nullable": True,
                "primary_key": False,
                "source_type": "varchar(50)",
                "unique": True,
                "type": {
                    "collation": None,
                    # "collation":
                    # {
                    #     "accent-sensitive": True,
                    #     "case-sensitive": False,
                    #     "locale": "Latin1_General_CP1",
                    # },
                    "encoding": "utf-8",
                    "lob": False,
                    "max-len": 50,
                    "type": "STRING",
                },
            },
            {
                "default": None,
                "name": "email",
                "nullable": True,
                "primary_key": False,
                "unique": True,
                "source_type": "varchar(100)",
                "type": {
                    "collation": None,
                    # "collation":
                    # {
                    #     "accent-sensitive": True,
                    #     "case-sensitive": False,
                    #     "locale": "Latin1_General_CP1",
                    # },
                    "encoding": "utf-8",
                    "lob": False,
                    "max-len": 100,
                    "type": "STRING",
                },
            },
            {
                "default": None,
                "name": "password_hash",
                "nullable": True,
                "primary_key": False,
                "unique": False,
                "source_type": "varchar(255)",
                "type": {
                    "collation": None,
                    # "collation":
                    # {
                    #     "accent-sensitive": True,
                    #     "case-sensitive": False,
                    #     "locale": "Latin1_General_CP1",
                    # },
                    "encoding": "utf-8",
                    "lob": False,
                    "max-len": 255,
                    "type": "STRING",
                },
            },
            {
                "default": "current_timestamp",
                "name": "created_at",
                "nullable": True,
                "unique": False,
                "primary_key": False,
                "source_type": "timestamp",
                "type": {
                    "date": {
                        "calendar": "gregorian",
                        "max": (9999, 12, 31),
                        "min": (1, 1, 1),
                    },
                    "time": {
                        "clock": "24",
                        "fsp": 8,
                        "precision": None,
                        "scale": None,
                        "tz": {"offset": "t", "zone": None},
                    },
                    "type": "TEMPORAL",
                },
            },
            {
                "default": None,
                "name": "last_login",
                "nullable": True,
                "primary_key": False,
                "unique": False,
                "source_type": "timestamp",
                "type": {
                    "date": {
                        "calendar": "gregorian",
                        "max": (9999, 12, 31),
                        "min": (1, 1, 1),
                    },
                    "time": {
                        "clock": "24",
                        "fsp": 8,
                        "precision": None,
                        "scale": None,
                        "tz": {"offset": "t", "zone": None},
                    },
                    "type": "TEMPORAL",
                },
            },
            {
                "default": True,
                "name": "is_active",
                "nullable": True,
                "primary_key": False,
                "type": {
                    "type": "BOOLEAN",
                    "notation": "literal",
                    "false-literals": None,
                    "true-literals": None,
                },
                "source_type": "boolean",
                "unique": False,
            },
        ],
    }


def test_dll_to_table_ir__nexartrade_order_items():
    # Given
    yaml_table = yaml.safe_load(
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
    nullable: false
  - name: product_id
    type: integer
    references: products(id)
    nullable: false
  - name: quantity
    type: integer
    check: "quantity > 0"
  - name: unit_price
    type: decimal(10, 2)
    check: "unit_price >= 0"
  - name: total_price
    type: decimal(10, 2)
    check: "total_price >= 0"
        """
    )

    # When
    reader = YamlSchemaReader(yaml_table)
    table_ir: TableIR = reader.read_table()

    # Then
    assert table_ir.to_dict() == {
        "name": "order_items",
        "columns": [
            {
                "name": "id",
                "type": {"bits": 64, "signed": True, "type": "INTEGER"},
                "source_type": "integer",
                "primary_key": True,
                "nullable": False,
                "unique": True,
                "default": None,
            },
            {
                "name": "order_id",
                "type": {"bits": 64, "signed": True, "type": "INTEGER"},
                "source_type": "integer",
                "primary_key": False,
                "foreign_key": {"table": "orders", "column": "id"},
                "nullable": False,
                "unique": False,
                "default": None,
            },
            {
                "name": "product_id",
                "type": {"bits": 64, "signed": True, "type": "INTEGER"},
                "source_type": "integer",
                "primary_key": False,
                "foreign_key": {"table": "products", "column": "id"},
                "nullable": False,
                "unique": False,
                "default": None,
            },
            {
                "name": "quantity",
                "type": {"bits": 64, "signed": True, "type": "INTEGER"},
                "source_type": "integer",
                "primary_key": False,
                "nullable": True,
                "unique": False,
                "default": None,
                "check_constraints": ["quantity > 0"],
            },
            {
                "name": "unit_price",
                "type": {"precision": 10, "scale": 2, "type": "DECIMAL"},
                "source_type": "decimal(10, 2)",
                "primary_key": False,
                "nullable": True,
                "unique": False,
                "default": None,
                "check_constraints": ["unit_price >= 0"],
            },
            {
                "name": "total_price",
                "type": {"precision": 10, "scale": 2, "type": "DECIMAL"},
                "source_type": "decimal(10, 2)",
                "primary_key": False,
                "nullable": True,
                "unique": False,
                "default": None,
                "check_constraints": ["total_price >= 0"],
            },
        ],
    }
