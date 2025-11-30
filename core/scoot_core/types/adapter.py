from abc import ABC, abstractmethod
from typing import Self, Type, override

from sqlalchemy.sql.type_api import TypeEngine
from sqlalchemy.sql import sqltypes as sqla
from sqlalchemy import Dialect
from sqlglot import exp

from enum import Enum, auto


class Types(Enum):
    STRING = auto()


"""
Maps scoot_core.types.Types to SQLAlchemy TypeEngine types.
"""
SqlAlchemyTypes: dict[Types, Type] = {Types.STRING: sqla.String}

"""
Maps scoot_core.types.Types to sqlglot DataType.Type.
"""
SqlGlotTypes: dict[Types, list[exp.DataType.Type]] = {
    Types.STRING: [exp.DataType.Type.VARCHAR]
}


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

    @classmethod
    def get_instance(
        cls,
        type_inst: Self | TypeEngine | exp.DataType | exp.ColumnDef,
        dialect: str | None = None,
    ) -> "TypeAdapter":
        """
        Factory method to create a TypeAdapter instance from various type
        representations.

        Args:
            type_inst (TypeAdapter | TypeEngine | exp.DataType | exp.ColumnDef):
                The type representation to adapt.
            dialect (str | None): The SQL dialect name, used for sqlglot types.

        Returns:
            TypeAdapter: An instance of a TypeAdapter subclass.

        Raises:
            TypeError: If the provided type_inst cannot be adapted.
        """
        if isinstance(type_inst, TypeEngine):
            return SqlAlchemyType.from_alchemy_type(type_inst)
        if isinstance(type_inst, exp.DataType) or isinstance(
            type_inst, exp.ColumnDef
        ):
            return SqlGlotType.from_datatype(type_inst, dialect)
        if isinstance(type_inst, cls):
            return type_inst

        raise TypeError(
            f"Cannot create TypeAdapter instance for: {type(type_inst)} / {type_inst}"
        )


class SqlAlchemyType(TypeAdapter):
    """
    TypeAdapter implementation for SQLAlchemy TypeEngine types.
    """

    def __init__(self, type: TypeEngine):
        self.type = type

    @override
    def get_collation(self) -> str | None:
        """
        Get the collation of the SQLAlchemy type if applicable.

        Always returns None for types that are not of the `String`
        type hierarchy.

        Returns:
            str | None: The collation name if applicable, otherwise None.
        """
        if isinstance(self.type, sqla.String):
            return self.type.collation

        return None

    @override
    def get_precision(self) -> int | None:
        """
        Get the precision of the SQLAlchemy type if applicable.

        Returns:
            int | None: The precision value if applicable, otherwise None.
        """
        return getattr(self.type, "precision", None)

    @override
    def get_scale(self) -> int | None:
        """
        Get the scale of the SQLAlchemy type if applicable.

        Returns:
            int | None: The scale value if applicable, otherwise None.
        """
        return getattr(self.type, "scale", None)

    @override
    def get_timezone(self) -> str | None:
        """
        Get the timezone information of the SQLAlchemy type if applicable.

        Returns:
            str | None: The timezone information if applicable, otherwise None.
        """
        return getattr(self.type, "timezone", None)

    @override
    def is_type(self, type: Types) -> bool:
        """
        Check if the SQLAlchemy type matches the given Types enum.

        Args:
            type (Types): The type to check against.

        Returns:
            bool: True if the type matches, otherwise False.
        """
        return isinstance(self.type, SqlAlchemyTypes[type])

    @override
    def max_length(self) -> int | None:
        """
        Get the maximum length of the SQLAlchemy type if applicable.

        Returns:
            int | None: The maximum length if applicable, otherwise None.
        """
        if isinstance(self.type, sqla.String):
            return self.type.length

        return None

    @override
    def native_expression(self, dialect: Dialect) -> str:
        """
        Get the native type expression for the given SQL dialect.

        Native in this case refers to the syntax of the partiular SQL
        `dialect` as generated by SQLAlchemy.
        """
        return self.type.compile(dialect)

    @classmethod
    def from_alchemy_type(cls, type: TypeEngine) -> Self:
        """
        Create a SqlAlchemyType instance from a SQLAlchemy TypeEngine type.

        Args:
            type (TypeEngine): The SQLAlchemy type to adapt.

        Returns:
            SqlAlchemyType: An instance of SqlAlchemyType.
        """
        return cls(type)

    def __str__(self):
        """
        Get the string representation of the SQLAlchemy type.
        """
        return str(self.type)


class SqlGlotType(TypeAdapter):
    """
    TypeAdapter implementation for sqlglot DataType and/or ColumnDef
    expressions.

    This adapter may be used with a plain DataType, but without a ColumnDef
    instance certain type information will be unavailable.

    column: exp.ColumnDef | None
    datatype: exp.DataType
    dialect: str | None
    """

    def __init__(self, type: exp.DataType | exp.ColumnDef, dialect: str | None):
        """
        Initialize the SqlGlotType adapter.

        Args:
            type (exp.DataType | exp.ColumnDef): The sqlglot type expression
                to adapt.
            dialect (str | None): The SQL dialect name for rendering.
        """
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
        """
        Get the collation of the sqlglot type if applicable.

        Always returns None for types that are not considered by sqlglot
        to be a TEXT_TYPE.

        Only available when the adapter contains a ColumnDef instance.

        Returns:
            str | None: The collation name if applicable, otherwise None.
        """
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
        """
        Check if the sqlglot type matches the given Types enum.


        Args:
            type (Types): The type to check against.
        Returns:
            bool: True if the type matches, otherwise False.
        """
        return any(
            self.datatype.is_type(glot_type) for glot_type in SqlGlotTypes[type]
        )

    @override
    def max_length(self) -> int | None:
        """
        Get the maximum length of the sqlglot type if applicable.

        Returns:
            int | None: The maximum length if applicable, otherwise None.
        """
        if self.is_type(Types.STRING):
            typearg = next(
                expr for expr in self.datatype.args.get("expressions", [])
            )
            if typearg:
                return int(str(typearg))
        return None

    @override
    def native_expression(self, dialect: Dialect) -> str:
        """
        Get the native type expression for the given SQL dialect.

        Native in this case refers to the syntax of the SQL dialect as
        generated by sqlglot.

        Args:
            dialect (Dialect): The SQLAlchemy dialect to use for rendering.

        Returns:
            str: The native type expression.
        """
        if self.column and (
            collate_constr := self.column.find(exp.CollateColumnConstraint)
        ):
            return f"{self.datatype.sql(dialect=self.dialect)} {collate_constr}"
        return self.datatype.sql(dialect=self.dialect)

    @classmethod
    def from_datatype(
        cls, type: exp.DataType | exp.ColumnDef, dialect: str | None
    ) -> Self:
        """
        Create a SqlGlotType instance from a sqlglot DataType or ColumnDef.

        Args:
            type (exp.DataType | exp.ColumnDef): The sqlglot type expression
                to adapt.
            dialect (str | None): The SQL dialect name for rendering.
        """
        return cls(type, dialect)

    def __str__(self):
        """
        Get the string representation of the sqlglot type.
        """
        return self.datatype.sql(dialect=self.dialect)
