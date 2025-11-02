from abc import abstractmethod
from typing import TextIO

from sqlalchemy import Dialect, Table

from scoot_core.model import ResultSet, TableModel


class StreamFormatter:

    def _verify_table(self, table: TableModel) -> Table:
        if table.sa_table is None:
            raise TypeError("TableModel has no Alchemy Table")
        return table.sa_table

    def _verify_tables(self, tables: list[TableModel]) -> list[Table]:
        return [self._verify_table(table) for table in tables]

    def start(self, stream: TextIO) -> None:
        pass

    @abstractmethod
    def table(self, stream: TextIO, table: TableModel) -> None:
        pass

    @abstractmethod
    def row(
        self,
        stream: TextIO,
        dialect: Dialect,
        table_model: TableModel,
        record: dict,
    ) -> None:
        pass

    @abstractmethod
    def rows(
        self,
        stream: TextIO,
        dialect: Dialect,
        table_models: list[TableModel],
        resultset: ResultSet,
    ) -> None:
        pass

    def end(self, stream):
        pass
