from typing import Any, override


class ColumnModel:
    def __init__(self, name, type_, nullable, primary_key, default=None):
        self.name = name
        self.type = type_
        self.nullable = nullable
        self.primary_key = primary_key
        self.default = default

    @classmethod
    def from_sqlalchemy(cls, sa_column):
        return cls(
            name=sa_column.name,
            type_=str(sa_column.type),
            nullable=sa_column.nullable,
            primary_key=sa_column.primary_key,
            default=(
                str(sa_column.default.arg)
                if sa_column.default is not None
                else None
            ),
        )

    def to_dict(self):
        return {
            "name": self.name,
            "type": self.type,
            "nullable": self.nullable,
            "primary_key": self.primary_key,
            "default": self.default,
        }


class TableModel:
    def __init__(self, name, schema):
        self.name = name
        self.schema = schema
        self.columns = []
        self._column_indices = {}

    def add_column(self, column):
        self.columns.append(column)

    def get_column(self, column_name):
        col_index = self._column_indices.get(column_name, None)
        for i in range(0, len(self.columns)):
            if self.columns[i].name == column_name:
                self._column_indices[column_name] = i
                col_index = i
                break

        if col_index is None:
            return {}

        return self.columns[col_index]

    @classmethod
    def from_sqlalchemy(cls, sa_table):
        table = cls(name=sa_table.name, schema=sa_table.schema)
        for sa_column in sa_table.columns:
            table.add_column(ColumnModel.from_sqlalchemy(sa_column))
        return table

    def to_dict(self):
        return {
            "name": self.name,
            "schema": self.schema,
            "columns": [col.to_dict() for col in self.columns],
        }

    def column_max_len(self, column_field):
        ml = len(column_field)
        for col in self.columns:
            l = len(str(getattr(col, column_field)))
            if l > ml:
                ml = l
        return ml


class ResultSet:
    def __init__(self, columns, rows: list[list[Any]], metadata=None):
        self.columns = columns
        self.rows = rows
        self.metadata = metadata

    def to_dict(self):
        result = {"columns": self.columns, "rows": self.rows}
        if self.metadata is not None:
            result["metadata"] = self.metadata
        return result


class TabularDataAdapter:
    def __init__(self):
        self._column_widths = None

    def _calc_column_max_width(self, col_name) -> int:
        _ = col_name
        raise NotImplementedError(
            "Subclass must implement _calc_column_max_width()"
        )

    def size(self) -> int:
        raise NotImplementedError("Subclass must implement size()")

    def get_cell_data(self, row, column) -> str:
        _ = row
        _ = column
        raise NotImplementedError("Subclass must implement get_cell_data()")

    def get_column_names(self) -> list[str]:
        raise NotImplementedError("Subclass must implement get_column_names()")

    def get_column_widths(self) -> list[int]:
        if self._column_widths is not None:
            return self._column_widths
        self._column_widths = [
            self._calc_column_max_width(n) for n in self.get_column_names()
        ]
        return self._column_widths


class ListDataAdapter(TabularDataAdapter):
    def __init__(self, headers: list[str], data: list[list]):
        super().__init__()
        self.headers = headers
        self.data = data

    @override
    def get_column_names(self) -> list[str]:
        return self.headers

    @override
    def size(self) -> int:
        return len(self.data)

    @override
    def get_cell_data(self, row, column):
        return str(self.data[row][column])

    @override
    def _calc_column_max_width(self, col_name) -> int:
        column_index = self.headers.index(col_name)
        ml = len(col_name)
        for row in self.data:
            l = len(str(row[column_index]))
            if l > ml:
                ml = l
        return ml


class TableAdapter(TabularDataAdapter):
    def __init__(self, headers, table):
        super().__init__()
        self.headers = headers
        self.table = table

    @override
    def get_column_names(self) -> list[str]:
        return self.headers

    @override
    def size(self) -> int:
        return len(self.table.columns)

    @override
    def get_cell_data(self, row, column):
        return str(
            getattr(self.table.columns[row], self.get_column_names()[column])
        )

    @override
    def _calc_column_max_width(self, col_name) -> int:
        ml = len(col_name)
        for col in self.table.columns:
            l = len(str(getattr(col, col_name)))
            if l > ml:
                ml = l
        return ml


class ResultSetAdapter(TabularDataAdapter):
    def __init__(self, result_set):
        super().__init__()
        self.result_set = result_set

    @override
    def get_column_names(self) -> list[str]:
        return self.result_set.columns

    @override
    def size(self) -> int:
        return len(self.result_set.rows)

    @override
    def get_cell_data(self, row, column):
        return str(self.result_set.rows[row][column])

    @override
    def _calc_column_max_width(self, col_name) -> int:
        column_index = self.result_set.columns.index(col_name)
        ml = len(col_name)
        for row in self.result_set.rows:
            l = len(str(row[column_index]))
            if l > ml:
                ml = l
        return ml
