import yaml
from sqlalchemy import (
    Table,
    Column,
    ForeignKey,
    CheckConstraint,
    Integer,
    String,
    Boolean,
    Text,
    MetaData,
    DECIMAL,
)

from sqlalchemy.sql.sqltypes import DATETIME_TIMEZONE


from system_test.db.log import log

# from sqlglot.expressions import Table, Column, Index, ForeignKey, Check


class DbSchema:
    def __init__(self, name, version, schemas: list["TblSchema"]):
        self.name = name
        self.version = version
        self.schemas = schemas


class TblSchema:
    def __init__(self, name, tables: list[Table]):
        self.name = name
        self.tables = tables


class SchemaParser:
    def __init__(self, schema):
        self.schema = schema

    @classmethod
    def from_file(cls, file_path):
        with open(file_path, "r") as f:
            schema = yaml.safe_load(f)
        return SchemaParser(schema)

    def parse_entity(self, entity, metadata):
        table_name = entity["name"]
        columns = []
        checks = []
        primary_key = []

        for col in entity['columns']:
            col_name = col['name']
            col_type = col['type'].lower()
            is_pk = col.get('primary_key', False)
            references = col.get('references')
            unique = col.get('unique', False)
            check = col.get('check')

            # Map custom types to SQLAlchemy types
            sqlalchemy_type = {
                'integer': Integer,
                'varchar': lambda size: String(size),
                'boolean': Boolean,
                'text': Text,
                'decimal': lambda precision, scale: DECIMAL(precision, scale),
                'timestamp': DATETIME_TIMEZONE,
            }

            # Handle data type with parameters
            if '(' in col_type and ')' in col_type:
                base_type, params = col_type.split('(')
                params = params.rstrip(')').split(',')
                sqlalchemy_type = sqlalchemy_type[base_type.strip()](
                    *map(int, params)
                )
            else:
                sqlalchemy_type = sqlalchemy_type.get(col_type, String)

            # Define column
            column = Column(
                col_name, sqlalchemy_type, unique=unique, primary_key=is_pk
            )

            if is_pk:
                primary_key.append(column)

            if references:
                ref_table, ref_column = references.rstrip(')').split('(')
                column.append_foreign_key(ForeignKey(f'{ref_table}.{ref_column}'))

            if check:
                checks.append(CheckConstraint(check))

            columns.append(column)

        # Define table
        return Table(table_name, metadata, *columns, *checks, *primary_key)

    def parse_schema(self, schema):
        entities = schema.get("entities", [])
        metadata = MetaData(schema=schema.get("name"))
        tables = [self.parse_entity(entity, metadata) for entity in entities]
        return TblSchema(name=schema.get("name"), tables=tables)

    def parse(self):
        db_name = self.schema.get('name')
        db_version = self.schema.get('version')
        schema_models = self.schema.get("schemas", [])
        schemas = [self.parse_schema(schema) for schema in schema_models]
        return DbSchema(name=db_name, version=db_version, schemas=schemas)
