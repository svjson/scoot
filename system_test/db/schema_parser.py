import yaml
from scoot_core.schema.sqlalchemy import SqlAlchemyTableEmitter
from scoot_core.schema.translate import translate_table_schema
from scoot_core.schema.yaml_schema import YamlSchemaReader
from sqlalchemy import MetaData, Table


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
    def from_file(cls, file_path) -> "SchemaParser":
        with open(file_path, "r") as f:
            schema = yaml.safe_load(f)
        return SchemaParser(schema)

    def parse_entity(self, entity, metadata) -> Table:
        return translate_table_schema(
            YamlSchemaReader(entity), SqlAlchemyTableEmitter()
        )

    def parse_schema(self, schema) -> TblSchema:
        entities = schema.get("entities", [])
        metadata = MetaData(schema=schema.get("name"))
        tables = [self.parse_entity(entity, metadata) for entity in entities]
        return TblSchema(name=schema.get("name"), tables=tables)

    def parse(self) -> DbSchema:
        db_name = self.schema.get("name")
        db_version = self.schema.get("version")
        schema_models = self.schema.get("schemas", [])
        schemas = [self.parse_schema(schema) for schema in schema_models]
        return DbSchema(name=db_name, version=db_version, schemas=schemas)
