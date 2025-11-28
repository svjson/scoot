from abc import abstractmethod
from typing import Any, override

SIGNED = True
UNSIGNED = False


class Type:
    """Base class for all types.

    This class hierarchy is used to define the types of a DBS, and their specifics.
    Each type has an optional sqltype, which is the native SQL type name.

    The main purpose is to provide detailed type information in an
    implementation-agnostic way, serving as an abstraction layer for client
    applications, such as scoot.el, where it provides editing capabilities with
    the information required to enforce type correctness."""

    def __init__(self, *, sqltype: str | None = None):
        self.sqltype = sqltype

    @abstractmethod
    def to_dict(self) -> dict[str, Any]:
        pass

    @override
    def __str__(self) -> str:
        return str(self.to_dict())


class Boolean(Type):
    """Represents a boolean type."""

    def __init__(
        self,
        *,
        notation="literal",
        true_literals: list[str] | None = None,
        false_literals: list[str] | None = None,
        sqltype: str | None = None,
    ):
        super().__init__(sqltype=sqltype)
        self.notation = notation
        self.true_literals = true_literals
        self.false_literals = false_literals

    @override
    def to_dict(self):
        return {
            "type": "BOOLEAN",
            "notation": self.notation,
            "true-literals": self.true_literals,
            "false-literals": self.false_literals,
        }


class Integer(Type):
    """Represents an integer type."""

    def __init__(self, bits: int, signed: bool, *, sqltype: str | None = None):
        super().__init__(sqltype=sqltype)
        self.bits = bits
        self.signed = signed

    @override
    def to_dict(self):
        return {"type": "INTEGER", "bits": self.bits, "signed": self.signed}


class Decimal(Type):
    """Represents a decimal type.

    Decimal is used to store exact numeric values with a fixed number of decimal
    places. It includes options for precision and scale.

    Precision is the total number of digits, and scale is the number of digits
    to the right of the decimal point.

    Precision and scale with values None indicate that there is no hard limit, like
    in PostgreSQL, where the type arbitrary and with unlimited scale/precision up to
    1000 digits.
    """

    def __init__(
        self,
        precision: int | None,
        scale: int | None,
        *,
        sqltype: str | None = None,
    ):
        super().__init__(sqltype=sqltype)
        self.precision = precision
        self.scale = scale

    @override
    def to_dict(self):
        return {"type": "DECIMAL", "precision": self.precision, "scale": self.scale}


class Collation:
    """Represents a collation type.

    Collation is used to define how string comparison should be done.
    It includes options for locale, case sensitivity, and accent sensitivity.

    Accent sensitivity determines whether characters with accents are treated
    as different characters. For example, "a" and "á" would be considered
    different characters if accent sensitivity is enabled.
    """

    def __init__(
        self,
        *,
        locale: str | None = None,
        case_sensitive: bool = False,
        accent_sensitive: bool = False,
    ):
        self.locale = locale
        self.case_sensitive = case_sensitive
        self.accent_sensitive = accent_sensitive

    def to_dict(self):
        return {
            "locale": self.locale,
            "case-sensitive": self.case_sensitive,
            "accent-sensitive": self.accent_sensitive,
        }


class String(Type):
    """Represents a string type.

    String is used to store variable-length character strings. It includes
    options for maximum length, encoding, and collation.

    This includes CLOB and CLOB-like types."""

    def __init__(
        self,
        max_len: int | None,
        encoding: str,
        collation: Collation | None,
        *,
        lob: bool = False,
        sqltype: str | None = None,
    ):
        super().__init__(sqltype=sqltype)
        self.max_len = max_len
        self.encoding = encoding
        self.collation = collation
        self.lob = lob

    @override
    def to_dict(self):
        return {
            "type": "STRING",
            "max-len": self.max_len,
            "encoding": self.encoding,
            "collation": self.collation.to_dict() if self.collation else None,
            "lob": self.lob,
        }


class TimeZone:
    """Represents a time zone type.

    Time zone is used to define the time zone offset and/or the time zone name."""

    def __init__(
        self, *, offset: str | None = None, zone: str | None = None, local=False
    ):
        self.offset = offset
        self.zone = zone
        self.local = local

    def clone(self):
        return TimeZone(offset=self.offset, zone=self.zone, local=self.local)

    def to_dict(self):
        return {"offset": self.offset, "zone": self.zone}


class Date:
    def __init__(
        self, min: tuple[int, int, int], max: tuple[int, int, int], calendar: str
    ):
        self.min = min
        self.max = max
        self.calendar = calendar

    def clone(self):
        return Date(min=self.min, max=self.max, calendar=self.calendar)

    def to_dict(self):
        return {"min": self.min, "max": self.max, "calendar": self.calendar}


class Time:
    def __init__(
        self,
        clock: str,
        *,
        timezone: TimeZone | None = None,
        fsp: int | None = None,
        precision: int | None = None,
        scale: int | None = None,
    ):
        self.clock = clock
        self.timezone = timezone
        self.fsp = fsp
        self.precision = precision
        self.scale = scale

    def clone(self):
        return Time(
            clock=self.clock,
            timezone=self.timezone.clone() if self.timezone else None,
        )

    def to_dict(self):
        return {
            "clock": self.clock,
            "fsp": self.fsp,
            "precision": self.precision,
            "scale": self.scale,
            "tz": self.timezone.to_dict() if self.timezone else None,
        }


class Temporal(Type):
    """Represents a temporal type, such as DATE, TIME, or TIMESTAMP.

    Temporal is used to store date and time values. It includes options for
    time zone, precision, and scale.

    unitl and units specify the largest and smallest unit of time for the
    temporal type, such as Y, M, D, H, h, m, MS, NS.

    A DATE sqltype would typically be Y/D and TIME H/S, and a full timestamp
    with ns precision Y/NS."""

    def __init__(
        self,
        *,
        date: Date | None = None,
        time: Time | None = None,
        sqltype: str | None = None,
    ):
        super().__init__(sqltype=sqltype)
        self.date = date
        self.time = time

    def to_dict(self):
        return {
            "type": "TEMPORAL",
            "date": self.date.to_dict() if self.date else None,
            "time": self.time.to_dict() if self.time else None,
        }
