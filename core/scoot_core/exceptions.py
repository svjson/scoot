from enum import Enum, auto


class ScootErrorType(Enum):
    NOT_FOUND = auto()
    VALIDATION_FAILED = auto()
    AUTH_FAILED = auto()
    CONNECTION_ERROR = auto()
    INTERNAL_ERROR = auto()


class ScootError(Exception):
    """Base exception for Scoot-related errors and exceptions."""

    def __init__(
        self, error, message, type=ScootErrorType.INTERNAL_ERROR, additional={}
    ):
        self.error = error
        self.type = type
        self.message = message
        self.additional = dict(additional)


class ScootConnectionException(ScootError):
    """Signals an error relating to a Scoot connection."""

    def __init__(self, error, message, connection_name=""):
        super().__init__(
            error,
            message,
            ScootErrorType.CONNECTION_ERROR,
            {"connection": connection_name},
        )


class ScootResourceException(ScootError):
    """Signals an error relating to a Scoot resource."""

    def __init__(self, type, name):
        super().__init__(
            "resource-error",
            f"No such {type}: '{name}'",
            ScootErrorType.NOT_FOUND,
            {"type": type, "name": name},
        )


class ScootSchemaException(ScootError):
    """Signals an attempt to use schema objects incorrectly."""

    def __init__(self, type, name):
        super().__init__(
            "schema-error",
            f"No such {type}: '{name}'",
            ScootErrorType.NOT_FOUND,
            {"type": type, "name": name},
        )


class ScootQueryException(ScootError):
    """Signals a query syntax or execution error."""

    def __init__(self, message):
        super().__init__("query-error", message)


class ScootDriverException(ScootError):
    """Signals a problem with a database driver."""

    def __init__(self, error, message, driver):
        super().__init__(
            error, message, ScootErrorType.INTERNAL_ERROR, {"driver": driver}
        )


class ScootApplicationError(ScootError):
    """Signals an unexpected and unhandled error in the application."""

    def __init__(self, exception):
        super().__init__("server-error", f"{exception}")


class ScootExportFormatError(ScootError):
    """Signals an unexpected error related to export formats."""

    def __init__(self, format: str):
        super().__init__("export-format", f"Unknown format: '{format}'")
