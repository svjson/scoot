from scoot_core.model import (
    TableModel,
    ResultSet,
    TabularDataAdapter,
    TableAdapter,
    ResultSetAdapter,
)


class AsciiTable:

    def __init__(self, adapter: TabularDataAdapter):
        self.adapter = adapter
        self.column_names = self.adapter.get_header_labels()
        self.column_widths = self.adapter.get_column_widths()

    def _horizontal_border_row(self):
        line = "+"
        for w in self.adapter.get_column_widths():
            line += "-" * (w + 1)
            line += "-+"
        return line

    def _header_row(self):
        line = "|"
        for i, w in enumerate(self.column_widths):
            line += " "
            line += self.column_names[i]
            line += " " * (w - len(self.column_names[i]))
            line += " |"
        return line

    def _data_row(self, row_index):
        line = "|"
        for i, w in enumerate(self.column_widths):
            line += " "
            val = self.adapter.get_cell_data(row_index, i)
            line += val
            line += " " * (w - len(val))
            line += " |"
        return line

    def dump(self, target):
        divider = self._horizontal_border_row()
        target(divider)
        target(self._header_row())
        target(divider)

        for i in range(self.adapter.size()):
            target(self._data_row(i))

        target(divider)

    @classmethod
    def from_table_model(cls, table: TableModel):
        return AsciiTable(
            TableAdapter(
                [
                    ["Column Name", "name"],
                    ["Type", "native_type"],
                    ["Nullable", "nullable"],
                    ["Primary Key", "primary_key"],
                    ["Default", "default"],
                ],
                table,
            )
        )

    @classmethod
    def from_result_set(cls, result_set: ResultSet):
        return AsciiTable(ResultSetAdapter(result_set))
