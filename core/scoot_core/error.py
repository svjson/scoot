import re
from functools import wraps
import traceback

from sqlalchemy import exc

from scoot_core.exceptions import ScootQueryException


def handler_generic(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        return func(*args, **kwargs)

    return wrapper


def handler_pyodbc(func):

    def extract_sql_message(exc):
        if len(exc.args) > 1:
            message = exc.args[1]
            # Extract only the main error message
            match = re.search(r"(\[SQL Server\]\s*.*?)(?:\s*\(\d+\).*)?$", message)
            if match:
                return match.group(1).strip()
            return str(exc)

    @wraps(func)
    def wrapper(*args, **kwargs):
        import pyodbc

        try:
            return func(*args, **kwargs)
        except exc.ProgrammingError as pe:
            if isinstance(pe.__cause__, pyodbc.ProgrammingError):
                traceback.print_exc()
                root_exc: pyodbc.ProgrammingError = pe.__cause__
                raise ScootQueryException(extract_sql_message(root_exc))
            else:
                raise pe

    return wrapper


handlers = {"pyodbc": handler_pyodbc}


def dialect_error_handler(driver_name: str):
    return handlers.get(driver_name, handler_generic)
