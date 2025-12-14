import pytest
from scoot_core.schema.ddl import DDLReader
from scoot_core.schema.sqlalchemy import SqlAlchemyTableEmitter
from scoot_core.schema.tbl_model import TableModelEmitter
from scoot_core.schema.translate import translate_table_schema
from sqlalchemy.dialects.mssql import DATETIMEOFFSET
from sqlalchemy.schema import Column, MetaData, Table
from sqlalchemy.sql.sqltypes import VARCHAR, Integer, String
from sqlalchemy.types import DateTime


def test_ddl_to_table_model__single_non_nullable_column():
    # Given
    dialect = "mssql"
    ddl = """
       CREATE TABLE mytable (
         id INTEGER NOT NULL
       )
    """
    reader = DDLReader(dialect, ddl)
    emitter = SqlAlchemyTableEmitter()

    # When
    table = translate_table_schema(reader, emitter)

    # Then
    assert table.name == "mytable"
    assert len(table.columns) == 1
    column = table.columns[0]
    assert column.name == "id"
    assert isinstance(column.type, Integer)
    assert column.nullable is False
    assert column.primary_key is False


def test_ddl_to_table_model__nexartrade_users():
    # Given
    dialect = "mssql"
    ddl = """
      CREATE TABLE users (
	id INTEGER NOT NULL IDENTITY(1,1),
	username VARCHAR(50) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	email VARCHAR(100) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	password_hash VARCHAR(255) COLLATE SQL_Latin1_General_CP1_CI_AS NULL,
	created_at DATETIMEOFFSET NULL,
	last_login DATETIMEOFFSET NULL,
 	is_active BIT NULL,
 	CONSTRAINT [PK__users__3213E83FAAF8442F] PRIMARY KEY CLUSTERED (id)
      )
    """

    reader = DDLReader(dialect, ddl)
    emitter = SqlAlchemyTableEmitter()

    # When
    table = translate_table_schema(reader, emitter)

    # Then
    assert table.name == "users"
    assert len(table.columns) == 7

    column_id = table.columns[0]
    assert column_id.name == "id"
    assert isinstance(column_id.type, Integer)
    assert column_id.primary_key is True
    assert column_id.nullable is False
    assert column_id.unique is False  # Implicitly unique via PK

    column_username = table.columns[1]
    assert column_username.name == "username"
    type_username = column_username.type
    assert isinstance(type_username, String)
    assert type_username.length == 50
    assert type_username.collation == "Latin1_General_CP1"
    assert column_username.primary_key is False
    assert column_username.nullable is True
    assert column_username.unique is False

    column_email = table.columns[2]
    assert column_email.name == "email"
    type_email = column_email.type
    assert isinstance(type_email, String)
    assert type_email.length == 100
    assert type_email.collation == "Latin1_General_CP1"
    assert column_email.primary_key is False
    assert column_email.nullable is True
    assert column_email.unique is False

    column_password = table.columns[3]
    assert column_password.name == "password_hash"
    type_password = column_password.type
    assert isinstance(type_password, String)
    assert type_password.length == 255
    assert type_password.collation == "Latin1_General_CP1"
    assert column_password.primary_key is False
    assert column_password.nullable is True
    assert column_password.unique is False

    column_created = table.columns[4]
    assert column_created.name == "created_at"
    type_created = column_created.type
    assert isinstance(type_created, DateTime)
    assert type_created.timezone is True

    {
        "name": "users",
        "schema": None,
        "columns": [
            {
                "name": "created_at",
                "type": "TEMPORAL",
                "native_type": "datetimeoffset",
                "typespec": {
                    "date": {
                        "calendar": "gregorian",
                        "max": (9999, 12, 31),
                        "min": (1, 1, 1),
                    },
                    "time": {
                        "clock": "24",
                        "fsp": None,
                        "precision": None,
                        "scale": None,
                        "tz": None,
                    },
                    "type": "TEMPORAL",
                },
                "nullable": True,
                "primary_key": False,
                "default": None,
            },
            {
                "name": "last_login",
                "type": "TEMPORAL",
                "native_type": "datetimeoffset",
                "typespec": {
                    "date": {
                        "calendar": "gregorian",
                        "max": (9999, 12, 31),
                        "min": (1, 1, 1),
                    },
                    "time": {
                        "clock": "24",
                        "fsp": None,
                        "precision": None,
                        "scale": None,
                        "tz": None,
                    },
                    "type": "TEMPORAL",
                },
                "nullable": True,
                "primary_key": False,
                "default": None,
            },
            {
                "name": "is_active",
                "type": "INTEGER",
                "typespec": {"bits": 1, "signed": False, "type": "INTEGER"},
                "native_type": "bit",
                "nullable": True,
                "primary_key": False,
                "default": None,
            },
        ],
        "constraints": [],
        "create_stmt": None,
    }
