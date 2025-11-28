from scoot_core.schema.emitter import SchemaEmitter
from scoot_core.schema.reader import SchemaReader
from typing import TypeVar

T = TypeVar("T")


def translate_table_schema(reader: SchemaReader, emitter: SchemaEmitter[T]) -> T:
    """
    Translates a table schema from one format to another using the provided
    reader and emitter.

    Args:
        reader (SchemaReader): The schema reader to read the table schema.
        emitter (SchemaEmitter[T]): The schema emitter to emit the table schema.
    Returns:
        T: The translated table schema in the format defined by the emitter.
    """
    return emitter.emit_table(reader.read_table())
