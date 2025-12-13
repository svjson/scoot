from scoot_core.dialect.registry import resolve_type
from scoot_core.model import ColumnModel, TableModel
from scoot_core.schema.emitter import SchemaEmitter
from scoot_core.schema.ir import ColumnIR, TableIR
from scoot_core.types.adapter import ScootTypeAdapter, TypeAdapter


class TableModelEmitter(SchemaEmitter[TableModel]):
    """
    Emits a TableModel from a TableIR.

    Args:
        SchemaEmitter (SchemaEmitter[TableModel]): The base schema emitter class.

    Returns:
        TableModel: The emitted table model.
    """

    def __init__(self, dialect: str):
        self.dialect = dialect

    def emit_table(self, table: TableIR) -> TableModel:
        """
        Emit a TableModel from the given TableIR.

        Args:
            table (TableIR): The table intermediate representation to emit.

        Returns:
            TableModel: The emitted table model.
        """
        return TableModel(
            table.name,
            None,
            columns=[self.emit_table_column(column) for column in table.columns],
        )

    def emit_table_column(self, column: ColumnIR) -> ColumnModel:
        """
        Emit a ColumnModel from the given ColumnIR.

        Args:
            column (ColumnIR): The column intermediate representation to emit.

        Returns:
            ColumnModel: The emitted column model.
        """

        _, _, native_type = resolve_type(
            self.dialect,
            (
                column.type.source_type
                if isinstance(column.type.source_type, TypeAdapter)
                else column.type.to_sqlalchemy_type()
            ),
        )

        return ColumnModel(
            column.name,
            column.type,
            column.nullable,
            column.primary_key,
            default=column.default,
            native_type=native_type,
        )
