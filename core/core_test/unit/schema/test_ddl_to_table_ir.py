from scoot_core.schema.ddl import DDLReader
from scoot_core.schema.ir import TableIR


def test_ddl_to_table_ir__single_non_nullable_column():
    # Given
    dialect = "mssql"
    ddl = """
       CREATE TABLE mytable (
         id INTEGER NOT NULL
       )
    """

    # When
    reader = DDLReader(dialect, ddl)
    table_ir: TableIR = reader.read_table()

    # Then
    assert table_ir.to_dict() == {
        "name": "mytable",
        "columns": [
            {
                "name": "id",
                "type": {"bits": 64, "signed": True, "type": "INTEGER"},
                "source_type": "INTEGER",
                "unique": False,
                "primary_key": False,
                "nullable": False,
                "default": None,
            }
        ],
    }


def test_dll_to_table_ir__nexartrade_users():
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

    # When
    reader = DDLReader(dialect, ddl)
    table_ir: TableIR = reader.read_table()

    # Then
    assert table_ir.to_dict() == {
        "name": "users",
        "columns": [
            {
                "name": "id",
                "type": {"bits": 64, "signed": True, "type": "INTEGER"},
                "source_type": "INTEGER",
                "primary_key": True,
                "unique": True,
                "nullable": False,
                "default": None,
            },
            {
                "default": None,
                "name": "username",
                "nullable": True,
                "unique": False,
                "primary_key": False,
                "source_type": "VARCHAR(50)",
                "type": {
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
            },
            {
                "default": None,
                "name": "email",
                "unique": False,
                "nullable": True,
                "primary_key": False,
                "source_type": "VARCHAR(100)",
                "type": {
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
            },
            {
                "default": None,
                "name": "password_hash",
                "nullable": True,
                "primary_key": False,
                "unique": False,
                "source_type": "VARCHAR(255)",
                "type": {
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
            },
            {
                "default": None,
                "name": "created_at",
                "nullable": True,
                "unique": False,
                "primary_key": False,
                "source_type": "DATETIMEOFFSET",
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
                "unique": False,
                "primary_key": False,
                "source_type": "DATETIMEOFFSET",
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
                "name": "is_active",
                "nullable": True,
                "unique": False,
                "primary_key": False,
                "type": {"bits": 1, "signed": False, "type": "INTEGER"},
                "source_type": "BIT",
            },
        ],
    }
