from abc import ABC, abstractmethod
from typing import Self, Type, override

from sqlalchemy.sql.type_api import TypeEngine
from sqlalchemy.sql import sqltypes as sqla
from sqlalchemy import Dialect
from sqlglot import exp

from enum import Enum, auto


class Types(Enum):
    STRING = auto()


SqlAlchemyTypes: dict[Types, Type] = {Types.STRING: sqla.String}

SqlGlotTypes: dict[Types, list[exp.DataType.Type]] = {
    Types.STRING: [exp.DataType.Type.VARCHAR]
}


class TypeAdapter(ABC):

    @abstractmethod
    def get_collation(self) -> str | None:
        raise NotImplementedError

    @abstractmethod
    def get_precision(self) -> int | None:
        raise NotImplementedError

    @abstractmethod
    def get_scale(self) -> int | None:
        raise NotImplementedError

    @abstractmethod
    def get_timezone(self) -> str | None:
        raise NotImplementedError

    @abstractmethod
    def is_type(self, type: Types) -> bool:
        raise NotImplementedError

    @abstractmethod
    def max_length(self) -> int | None:
        raise NotImplementedError

    @abstractmethod
    def native_expression(self, dialect: Dialect) -> str:
        raise NotImplementedError

    @classmethod
    def get_instance(
        cls,
        type: Self | TypeEngine | exp.DataType | exp.ColumnDef,
        dialect: str | None = None,
    ) -> "TypeAdapter":
        if isinstance(type, TypeEngine):
            return SqlAlchemyType.from_alchemy_type(type)
        if isinstance(type, exp.DataType) or isinstance(type, exp.ColumnDef):
            return SqlGlotType.from_datatype(type, dialect)
        if isinstance(type, cls):
            return type

        raise TypeError(f"Cannot create TypeAdapter instance for: {type}")


class SqlAlchemyType(TypeAdapter):

    def __init__(self, type: TypeEngine):
        self.type = type

    @override
    def get_collation(self) -> str | None:
        if isinstance(self.type, sqla.String):
            return self.type.collation

        return None

    @override
    def get_precision(self) -> int | None:
        return getattr(self.type, "precision", None)

    @override
    def get_scale(self) -> int | None:
        return getattr(self.type, "scale", None)

    @override
    def get_timezone(self) -> str | None:
        return getattr(self.type, "timezone", None)

    @override
    def is_type(self, type: Types) -> bool:
        return isinstance(self.type, SqlAlchemyTypes[type])

    @override
    def max_length(self) -> int | None:
        if isinstance(self.type, sqla.String):
            return self.type.length

        return None

    @override
    def native_expression(self, dialect: Dialect) -> str:
        return self.type.compile(dialect)

    @classmethod
    def from_alchemy_type(cls, type: TypeEngine) -> Self:
        return cls(type)

    def __str__(self):
        return str(self.type)


class SqlGlotType(TypeAdapter):

    def __init__(self, type: exp.DataType | exp.ColumnDef, dialect: str | None):
        self.column = type if isinstance(type, exp.ColumnDef) else None
        if self.column:
            datatype = self.column.args.get("kind")
            if not isinstance(datatype, exp.DataType):
                raise TypeError(f"Invalid type/column: {type}")
            self.datatype = datatype
        elif isinstance(type, exp.DataType):
            self.datatype = type
        self.dialect = dialect

    @override
    def get_collation(self) -> str | None:
        if not self.column:
            return None

        if collate := self.column.find(exp.CollateColumnConstraint):
            return str(collate.this)

        return None

    @override
    def get_precision(self) -> int | None:
        raise NotImplementedError

    @override
    def get_scale(self) -> int | None:
        raise NotImplementedError

    @override
    def get_timezone(self) -> str | None:
        return None

    @override
    def is_type(self, type: Types) -> bool:
        return any(
            self.datatype.is_type(glot_type) for glot_type in SqlGlotTypes[type]
        )

    @override
    def max_length(self) -> int | None:
        if self.is_type(Types.STRING):
            typearg = next(
                expr for expr in self.datatype.args.get("expressions", [])
            )
            if typearg:
                return int(str(typearg))
        return None

    @override
    def native_expression(self, dialect: Dialect) -> str:
        if self.column and (
            collate_constr := self.column.find(exp.CollateColumnConstraint)
        ):
            return f"{self.datatype.sql(dialect=self.dialect)} {collate_constr}"
        return self.datatype.sql(dialect=self.dialect)

    @classmethod
    def from_datatype(
        cls, type: exp.DataType | exp.ColumnDef, dialect: str | None
    ) -> Self:
        return cls(type, dialect)

    def __str__(self):
        return self.datatype.sql(dialect=self.dialect)
