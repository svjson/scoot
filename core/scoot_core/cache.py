from .model import TableModel


class Cache:
    def __init__(self):
        self.table_model: dict[str, TableModel] = {}

    def get_table_model(self, table_name: str) -> TableModel | None:
        return self.table_model.get(table_name.lower(), None)

    def put_table_model(self, table_name: str, table_model: TableModel):
        self.table_model[table_name.lower()] = table_model

    def __repr__(self):
        return f"<Cache({hex(id(self))}): table_model: {self.table_model}>"
