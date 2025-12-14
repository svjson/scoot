import re
from abc import abstractmethod
from typing import Callable, Optional, override

from .. import types


class TypeConverter:
    """Base class for type converters.  This class is used to convert SQLAlchemy types
    to Scoot types.  The `convert` method should be implemented by subclasses to
    perform the actual conversion.  The `__init__` method can be used to pass
    additional and backend/DBMS-specific parameters to the converter.

    The `convert` method should return a Scoot type object, the alchemy type name and
    the backend-native type name."""

    @abstractmethod
    def convert(self, type: types.TypeAdapter) -> types.Type:
        pass


class BinaryConverter(TypeConverter):
    """Converts SQLAlchmery BINARY/LargeBinary to the Scoot Binary type."""

    @override
    def convert(self, type: types.TypeAdapter):
        max_len = None
        if type.is_type(types.Types.BINARY):
            max_len = type.max_length()

        return types.Binary(max_len=max_len)


class VARCHARConverter(TypeConverter):
    """Converts SQLAlchemy VARCHAR types to Scoot String types.

    The constructor accepts a `collation_parser` function that is used to parse/decode
    backend/DBMS-system-specific collation strings in, if present"""

    def __init__(self, collation_parser: Callable[[str], Optional[types.Collation]]):
        self.collation_parser = collation_parser
        pass

    @override
    def convert(self, type: types.TypeAdapter):
        collation: Optional[types.Collation] = None
        max_len = 1
        lob = False

        if type.is_type(types.Types.STRING):
            max_len = type.max_length()
            if not max_len:
                lob = True

            if collation_str := type.get_collation():
                collation = self.collation_parser(collation_str)

        return types.String(
            max_len=max_len, encoding="utf-8", collation=collation, lob=lob
        )


class DECIMALConverter(TypeConverter):
    """Converts SQLAlchemy DECIMAL types to Scoot Decimal types."""

    def __init__(self, default_precision: int | None, default_scale: int | None):
        self.default_precision = default_precision
        self.default_scale = default_scale

    @override
    def convert(self, type: types.TypeAdapter):
        return types.Decimal(
            precision=type.get_precision() or self.default_precision,
            scale=type.get_scale() or self.default_scale,
        )


class TemporalConverter(TypeConverter):
    """Converts SQLAlchemy Temporal types to Scoot Temporal types."""

    def __init__(self, default: types.Temporal):
        self.default = default

    @override
    def convert(self, type: types.TypeAdapter):
        date = self.default.date.clone() if self.default.date else None
        time = self.default.time.clone() if self.default.time else None

        if time and not type.get_timezone():
            time.timezone = None

        return types.Temporal(date=date, time=time)


class TypeMapper:
    """Abstract base class for type mappers.

    Should be implemented by subclasses for each backend/DBMS system.
    """

    @abstractmethod
    def resolve_type(
        self, type: types.TypeAdapter
    ) -> tuple[Optional[types.Type], Optional[str], Optional[str]]:
        pass

    def parse_sql_type(self, type_str: str) -> tuple[str, list[str], Optional[str]]:
        """
        Parses a SQL type string like:
          - VARCHAR(50) COLLATE "abc"
          - DECIMAL(10,2)
          - TIMESTAMP(3) WITH TIME ZONE
        Returns:
          ParsedSQLType(base_type='VARCHAR', parameters=['50'], modifiers='COLLATE "abc"')
        """
        type_str = type_str.strip()

        type_match = re.match(r"^([A-Za-z0-9_]+)\s*(\(([^)]*)\))?(.*)$", type_str)
        if not type_match:
            raise ValueError(f"Could not parse type string: {type_str}")

        base_type = type_match.group(1).upper()
        param_str = type_match.group(3)
        modifiers = type_match.group(4).strip()

        parameters = [p.strip() for p in param_str.split(",")] if param_str else []

        return base_type, parameters, modifiers or None


def subst_leading(pattern: str, leading_len: int | None = None):
    if not leading_len:
        leading_len = len(pattern)

    return lambda type_str: pattern + type_str[leading_len:]
