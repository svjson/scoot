class ScootError(Exception):
    """Base exception for Scoot-related errors and exceptions."""

    def __init__(self, error, message, additional={}):
        self.error = error
        self.message = message
        self.additional = dict(additional)


class ScootConnectionException(ScootError):
    """Signals an error relating to a Scoot connection."""

    def __init__(self, error, message, connection_name=""):
        super().__init__(error, message, {"connection": connection_name})


class ScootSchemaException(ScootError):
    """Signals an attempt to use schema objects incorrectly."""

    def __init__(self, type, name):
        super().__init__(
            "schema-error",
            f"No such {type}: '{name}'",
            {"type": type, "name": name},
        )


class ScootDriverException(ScootError):
    """Signals a problem with a database driver."""

    def __init__(self, error, message, driver):
        super().__init__(error, message, {"driver": driver})


class ScootApplicationError(ScootError):
    """Signals an unexpected and unhandled error in the application."""

    def __init__(self, exception):
        super().__init__("server-error", f"{exception}")
