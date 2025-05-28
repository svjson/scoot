from functools import wraps
import inspect
import pytest

from .db.service import BackendService


def only_for_backend(backend_name):
    return only_for_backends(backend_name)


def only_for_backends(*backends):
    def decorator(func):
        sig = inspect.signature(func)

        @wraps(func)
        def wrapper(*args, **kwargs):
            bound = sig.bind_partial(*args, **kwargs)
            bound.apply_defaults()

            db_backend = bound.arguments.get("db_backend", None)
            if db_backend and db_backend.name not in backends:
                pytest.skip(f"N/A for '{db_backend.name}'")

            return func(*args, **kwargs)

        return wrapper

    return decorator
