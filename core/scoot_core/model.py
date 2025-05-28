from typing import Any, override, Optional

from scoot_core import types


class ColumnModel:
    def __init__(
        self,
        name,
        type_: types.Type | None,
        nullable,
        primary_key,
        default=None,
        *,
        native_type: str | None = None,
        sqltype: str | None = None,
    ):
        self.name = name
        self.type = type_
        self.native_type = native_type
        self.sqltype = sqltype
        self.nullable = nullable
        self.primary_key = primary_key
        self.default = default

    def to_dict(self):
        typespec = self.type.to_dict() if self.type else None
        return {
            "name": self.name,
            "type": typespec.get("type") if typespec else None,
            "typespec": typespec,
            "native_type": self.native_type,
            "nullable": self.nullable,
            "primary_key": self.primary_key,
            "default": self.default,
        }


class TableModel:
    def __init__(self, name, schema, **kwargs):
        self.name = name
        self.schema = schema
        self.columns = kwargs.get("columns", [])
        self._column_indices = {}
        self.create_stmt: Optional[str] = None
        self.constraints = []

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

    def column_max_len(self, column_field):
        ml = len(column_field)
        for col in self.columns:
            l = len(str(getattr(col, column_field)))
            if l > ml:
                ml = l
        return ml

    def get_constraints_for_column(self, column_name: str):
        return [
            con for con in self.constraints if column_name in con.get("columns")
        ]

    def to_dict(self):
        return {
            "name": self.name,
            "schema": self.schema,
            "columns": [col.to_dict() for col in self.columns],
            "constraints": self.constraints,
            "create_stmt": self.create_stmt,
        }


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

    def get_header_labels(self) -> list[str]:
        raise NotImplementedError("Subclass must implement get_header_labels()")

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
    def get_header_labels(self) -> list[str]:
        return self.headers

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
        self.headers = [h[0] if isinstance(h, list) else h for h in headers]
        self.column_names = [h[1] if isinstance(h, list) else h for h in headers]
        self.table = table

    @override
    def get_header_labels(self) -> list[str]:
        return self.headers

    @override
    def get_column_names(self) -> list[str]:
        return self.column_names

    @override
    def size(self) -> int:
        return len(self.table.columns)

    @override
    def get_cell_data(self, row, column):
        return str(getattr(self.table.columns[row], self.column_names[column]))

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
