import copy

from system_test.db.log import log


USERS__COLUMNS = {
    "baseline": [
        {
            'column': 'id',
            'constraints': [],
            'default': None,
            'name': 'id',
            'native_type': 'INTEGER(11)',
            'nullable': False,
            'primary_key': True,
            'table': 'users',
            'type': 'INTEGER',
            'typespec': {'bits': 64, 'signed': True, 'type': 'INTEGER'},
        },
        {
            'column': 'username',
            'constraints': [],
            'default': None,
            'name': 'username',
            'native_type': 'VARCHAR(50)',
            'nullable': True,
            'primary_key': False,
            'table': 'users',
            'type': 'STRING',
            'typespec': {
                'collation': None,
                'encoding': 'utf-8',
                'lob': False,
                'max-len': 50,
                'type': 'STRING',
            },
        },
        {
            'column': 'email',
            'constraints': [],
            'default': None,
            'name': 'email',
            'native_type': 'VARCHAR(100)',
            'nullable': True,
            'primary_key': False,
            'table': 'users',
            'type': 'STRING',
            'typespec': {
                'collation': None,
                'encoding': 'utf-8',
                'lob': False,
                'max-len': 100,
                'type': 'STRING',
            },
        },
        {
            'column': 'password_hash',
            'constraints': [],
            'default': None,
            'name': 'password_hash',
            'native_type': 'VARCHAR(255)',
            'nullable': True,
            'primary_key': False,
            'table': 'users',
            'type': 'STRING',
            'typespec': {
                'collation': None,
                'encoding': 'utf-8',
                'lob': False,
                'max-len': 255,
                'type': 'STRING',
            },
        },
        {
            'column': 'created_at',
            'constraints': [],
            'default': None,
            'name': 'created_at',
            'native_type': 'DATETIME',
            'nullable': True,
            'primary_key': False,
            'table': 'users',
            'type': 'TEMPORAL',
            'typespec': {
                'date': {
                    'calendar': 'gregorian',
                    'max': (9999, 12, 31),
                    'min': (1000, 1, 1),
                },
                'time': {
                    'clock': '24',
                    'fsp': None,
                    'precision': None,
                    'scale': None,
                    'tz': None,
                },
                'type': 'TEMPORAL',
            },
        },
        {
            'column': 'last_login',
            'constraints': [],
            'default': None,
            'name': 'last_login',
            'native_type': 'DATETIME',
            'nullable': True,
            'primary_key': False,
            'table': 'users',
            'type': 'TEMPORAL',
            'typespec': {
                'date': {
                    'calendar': 'gregorian',
                    'max': (9999, 12, 31),
                    'min': (1000, 1, 1),
                },
                'time': {
                    'clock': '24',
                    'fsp': None,
                    'precision': None,
                    'scale': None,
                    'tz': None,
                },
                'type': 'TEMPORAL',
            },
        },
        {
            'column': 'is_active',
            'constraints': [],
            'default': None,
            'name': 'is_active',
            'native_type': 'TINYINT(1)',
            'nullable': True,
            'primary_key': False,
            'table': 'users',
            'type': 'INTEGER',
            'typespec': {'bits': 1, 'signed': False, 'type': 'INTEGER'},
        },
    ],
    "mssql": [
        {'native_type': 'int'},
        {
            'native_type': 'varchar(50) COLLATE SQL_Latin1_General_CP1_CI_AS',
            'typespec': {
                'collation': {
                    'accent-sensitive': True,
                    'case-sensitive': False,
                    'locale': 'Latin1_General_CP1',
                },
                'encoding': 'utf-8',
                'lob': False,
                'max-len': 50,
                'type': 'STRING',
            },
        },
        {
            'native_type': 'varchar(100) COLLATE SQL_Latin1_General_CP1_CI_AS',
            'typespec': {
                'collation': {
                    'accent-sensitive': True,
                    'case-sensitive': False,
                    'locale': 'Latin1_General_CP1',
                },
                'encoding': 'utf-8',
                'lob': False,
                'max-len': 100,
                'type': 'STRING',
            },
        },
        {
            'native_type': 'varchar(255) COLLATE SQL_Latin1_General_CP1_CI_AS',
            'typespec': {
                'collation': {
                    'accent-sensitive': True,
                    'case-sensitive': False,
                    'locale': 'Latin1_General_CP1',
                },
                'encoding': 'utf-8',
                'lob': False,
                'max-len': 255,
                'type': 'STRING',
            },
        },
        {
            'native_type': 'datetimeoffset',
            'typespec': {
                'date': {
                    'calendar': 'gregorian',
                    'max': (9999, 12, 31),
                    'min': (1, 1, 1),
                },
                'time': {
                    'clock': '24',
                    'fsp': None,
                    'precision': None,
                    'scale': None,
                    'tz': None,
                },
                'type': 'TEMPORAL',
            },
        },
        {
            'native_type': 'datetimeoffset',
            'typespec': {
                'date': {
                    'calendar': 'gregorian',
                    'max': (9999, 12, 31),
                    'min': (1, 1, 1),
                },
                'time': {
                    'clock': '24',
                    'fsp': None,
                    'precision': None,
                    'scale': None,
                    'tz': None,
                },
                'type': 'TEMPORAL',
            },
        },
        {'native_type': 'bit'},
    ],
    "mysql": [{'native_type': 'INTEGER'}, {}, {}, {}, {}, {}, {}],
    "postgres": [
        {'native_type': 'INTEGER'},
        {},
        {},
        {},
        {
            'native_type': 'TIMESTAMP WITH TIME ZONE',
            'typespec': {
                'date': {
                    'calendar': 'proleptic gregorian',
                    'max': (9999, 12, 31),
                    'min': (-4712, 1, 1),
                },
                'time': {
                    'clock': '24',
                    'fsp': 10,
                    'precision': None,
                    'scale': None,
                    'tz': {'offset': 't', 'zone': 't'},
                },
                'type': 'TEMPORAL',
            },
        },
        {
            'native_type': 'TIMESTAMP WITH TIME ZONE',
            'typespec': {
                'date': {
                    'calendar': 'proleptic gregorian',
                    'max': (9999, 12, 31),
                    'min': (-4712, 1, 1),
                },
                'time': {
                    'clock': '24',
                    'fsp': 10,
                    'precision': None,
                    'scale': None,
                    'tz': {'offset': 't', 'zone': 't'},
                },
                'type': 'TEMPORAL',
            },
        },
        {
            'native_type': 'BOOLEAN',
            'type': 'BOOLEAN',
            'typespec': {
                'false-literals': ['false', 'no', 'off', '0'],
                'notation': 'literal',
                'true-literals': ['true', 'yes', 'on', '1'],
                'type': 'BOOLEAN',
            },
        },
    ],
    "oracle_11g": [
        {
            'native_type': 'INTEGER',
            'type': 'DECIMAL',
            'typespec': {'precision': 38, 'scale': 0, 'type': 'DECIMAL'},
        },
        {'native_type': 'VARCHAR(50 CHAR)'},
        {'native_type': 'VARCHAR(100 CHAR)'},
        {'native_type': 'VARCHAR(255 CHAR)'},
        {
            'native_type': 'DATE',
            'typespec': {
                'date': {
                    'calendar': 'proleptic gregorian',
                    'max': (-4712, 1, 1),
                    'min': (-4712, 1, 1),
                },
                'time': {
                    'clock': '24',
                    'fsp': None,
                    'precision': None,
                    'scale': None,
                    'tz': None,
                },
                'type': 'TEMPORAL',
            },
        },
        {
            'native_type': 'DATE',
            'typespec': {
                'date': {
                    'calendar': 'proleptic gregorian',
                    'max': (-4712, 1, 1),
                    'min': (-4712, 1, 1),
                },
                'time': {
                    'clock': '24',
                    'fsp': None,
                    'precision': None,
                    'scale': None,
                    'tz': None,
                },
                'type': 'TEMPORAL',
            },
        },
        {
            'native_type': 'INTEGER',
            'type': 'DECIMAL',
            'typespec': {'precision': 38, 'scale': 0, 'type': 'DECIMAL'},
        },
    ],
    "oracle_23c": [
        {
            'native_type': 'INTEGER',
            'type': 'DECIMAL',
            'typespec': {'precision': 38, 'scale': 0, 'type': 'DECIMAL'},
        },
        {'native_type': 'VARCHAR(50 CHAR)'},
        {'native_type': 'VARCHAR(100 CHAR)'},
        {'native_type': 'VARCHAR(255 CHAR)'},
        {
            'native_type': 'DATE',
            'typespec': {
                'date': {
                    'calendar': 'proleptic gregorian',
                    'max': (-4712, 1, 1),
                    'min': (-4712, 1, 1),
                },
                'time': {
                    'clock': '24',
                    'fsp': None,
                    'precision': None,
                    'scale': None,
                    'tz': None,
                },
                'type': 'TEMPORAL',
            },
        },
        {
            'native_type': 'DATE',
            'typespec': {
                'date': {
                    'calendar': 'proleptic gregorian',
                    'max': (-4712, 1, 1),
                    'min': (-4712, 1, 1),
                },
                'time': {
                    'clock': '24',
                    'fsp': None,
                    'precision': None,
                    'scale': None,
                    'tz': None,
                },
                'type': 'TEMPORAL',
            },
        },
        {
            'native_type': 'INTEGER',
            'type': 'DECIMAL',
            'typespec': {'precision': 38, 'scale': 0, 'type': 'DECIMAL'},
        },
    ],
}


def nexartrade__users_columns(dialect: str, columns: list[str] | None = None):
    for_dialect = [
        nexartrade__users_column(dialect, i)
        for i, _ in enumerate(USERS__COLUMNS["baseline"])
    ]

    if columns is None:
        return for_dialect

    result = []

    lookup = {item['column']: item for item in for_dialect}
    for column_name in columns:
        if column_name in lookup:
            result.append(lookup[column_name])

    return result


def nexartrade__users_column(dialect: str, col: int):
    baseline = copy.deepcopy(USERS__COLUMNS["baseline"][col])
    dialect_overrides = USERS__COLUMNS.get(dialect, None)

    if not dialect_overrides:
        return baseline

    return {} | baseline | copy.deepcopy(dialect_overrides[col])
