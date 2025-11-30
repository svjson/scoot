from types import MethodType
from scoot_core.connection import Connection
from scoot_core.openv import OperationEnv
from scoot_core.schema.ddl import DDLReader
from scoot_core.schema.tbl_model import TableModelEmitter
from scoot_core.schema.translate import translate_table_schema


def make_conn(dialect):
    dummy_connection = Connection.__new__(Connection)

    def get_dialect(self):
        return dialect

    dummy_connection.get_dialect = MethodType(get_dialect, dummy_connection)

    return dummy_connection


def make_op_env(dialect):
    return OperationEnv(make_conn(dialect))


def test_ddl_to_table_model__single_non_nullable_column():
    # Given
    op_env = make_op_env("mssql")
    ddl = """
       CREATE TABLE mytable (
         id INTEGER NOT NULL
       )
    """
    reader = DDLReader(op_env, ddl)
    emitter = TableModelEmitter()

    # When
    model = translate_table_schema(reader, emitter)

    # Then
    assert model.to_dict() == {
        "name": "mytable",
        "schema": None,
        "columns": [
            {
                "name": "id",
                "type": "INTEGER",
                "native_type": "int",
                "typespec": {"bits": 64, "signed": True, "type": "INTEGER"},
                "nullable": False,
                "primary_key": False,
                "default": None,
            }
        ],
        "constraints": [],
        "create_stmt": None,
    }


def test_ddl_to_table_model__nexartrade_users():
    # Given
    op_env = make_op_env("mssql")
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

    reader = DDLReader(op_env, ddl)
    emitter = TableModelEmitter()

    # When
    model = translate_table_schema(reader, emitter)

    # Then
    assert model.to_dict() == {
        "name": "users",
        "schema": None,
        "columns": [
            {
                "name": "id",
                "type": "INTEGER",
                "native_type": "int",
                "typespec": {"bits": 64, "signed": True, "type": "INTEGER"},
                "nullable": False,
                "primary_key": True,
                "default": None,
            },
            {
                "name": "username",
                "type": "STRING",
                "native_type": "varchar(50) COLLATE SQL_Latin1_General_CP1_CI_AS",
                "typespec": {
                    "collation": {
                        "accent-sensitive": True,
                        "case-sensitive": False,
                        "locale": "Latin1_General_CP1",
                    },
                    "encoding": "utf-8",
                    "lob": False,
                    "max-len": 50,
                    "type": "STRING",
                },
                "nullable": True,
                "primary_key": False,
                "default": None,
            },
            {
                "name": "email",
                "type": "STRING",
                "native_type": "varchar(100) COLLATE SQL_Latin1_General_CP1_CI_AS",
                "typespec": {
                    "collation": {
                        "accent-sensitive": True,
                        "case-sensitive": False,
                        "locale": "Latin1_General_CP1",
                    },
                    "encoding": "utf-8",
                    "lob": False,
                    "max-len": 100,
                    "type": "STRING",
                },
                "nullable": True,
                "primary_key": False,
                "default": None,
            },
            {
                "name": "password_hash",
                "type": "STRING",
                "native_type": "varchar(255) COLLATE SQL_Latin1_General_CP1_CI_AS",
                "typespec": {
                    "collation": {
                        "accent-sensitive": True,
                        "case-sensitive": False,
                        "locale": "Latin1_General_CP1",
                    },
                    "encoding": "utf-8",
                    "lob": False,
                    "max-len": 255,
                    "type": "STRING",
                },
                "nullable": True,
                "primary_key": False,
                "default": None,
            },
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
