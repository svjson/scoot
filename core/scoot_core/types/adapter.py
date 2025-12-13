from abc import ABC, abstractmethod
from enum import Enum, auto
from typing import Self, Type, override

from sqlalchemy import Dialect
from sqlalchemy.sql import sqltypes as sqla
from sqlalchemy.sql.type_api import TypeEngine

from . import definition as types


class Types(Enum):
    STRING = auto()


class TypeAdapter(ABC):
    """
    Abstract base class for adapting different type representations
    (e.g., SQLAlchemy types, sqlglot DataType expressions) to a simplistic
    common interface.
    """

    @abstractmethod
    def get_collation(self) -> str | None:
        """
        Get the string representation of the type collation.

        Always returns None for non-text types.

        Returns:
            str | None: The collation name if applicable, otherwise None.
        """
        raise NotImplementedError

    @abstractmethod
    def get_precision(self) -> int | None:
        """
        Get the numeric precision of the type if applicable.

        Returns:
            int | None: The precision value if applicable, otherwise None.
        """
        raise NotImplementedError

    @abstractmethod
    def get_scale(self) -> int | None:
        """
        Get the numeric scale of the type if applicable.

        Returns:
            int | None: The scale value if applicable, otherwise None.
        """
        raise NotImplementedError

    @abstractmethod
    def get_timezone(self) -> str | None:
        """
        Get the timezone information of the type if applicable.

        Returns:
            str | None: The timezone information if applicable, otherwise None.
        """
        raise NotImplementedError

    @abstractmethod
    def is_type(self, type: Types) -> bool:
        """
        Check if the adapted type matches the given Types enum.

        Args:
            type (Types): The type to check against.
        """
        raise NotImplementedError

    @abstractmethod
    def max_length(self) -> int | None:
        """
        Get the maximum length of the type if applicable.

        Returns:
            int | None: The maximum length if applicable, otherwise None.
        """
        raise NotImplementedError

    @abstractmethod
    def native_expression(self, dialect: Dialect) -> str:
        """
        Get the native type expression for the given SQL dialect.

        Native in this case refers to the syntax of the SQL dialect as
        opposed to the common internal data type nomenclature.

        Args:
            dialect (Dialect): The SQLAlchemy dialect to use for rendering.
        """
        raise NotImplementedError


class ScootTypeAdapter(TypeAdapter):
    """
    TypeAdapter implementation for the internal Scoot types.Type class
    hierarchy.
    """

    def __init__(self, type: types.Type):
        self.type = type

    @override
    def get_collation(self) -> str | None:
        raise NotImplementedError

    @override
    def get_precision(self) -> int | None:
        raise NotImplementedError

    @override
    def get_scale(self) -> int | None:
        raise NotImplementedError

    @override
    def get_timezone(self) -> str | None:
        raise NotImplementedError

    @override
    def is_type(self, type: Types) -> bool:
        raise NotImplementedError

    @override
    def max_length(self) -> int | None:
        raise NotImplementedError

    @override
    def native_expression(self, dialect: Dialect) -> str:
        raise NotImplementedError

    @override
    def __str__(self):
        return str(self.type.sqltype)
