from scoot_core.model import ColumnModel, TableModel
from scoot_core.schema.emitter import SchemaEmitter
from scoot_core.schema.ir import ColumnIR, TableIR


class TableModelEmitter(SchemaEmitter[TableModel]):
    """
    Emits a TableModel from a TableIR.

    Args:
        SchemaEmitter (SchemaEmitter[TableModel]): The base schema emitter class.

    Returns:
        TableModel: The emitted table model.
    """

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
        return ColumnModel(
            column.name,
            column.type,
            column.nullable,
            column.primary_key,
            default=column.default,
            native_type=column.native_type,
        )
