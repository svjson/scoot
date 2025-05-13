from sqlalchemy import text
import pytest
import threading

from .db.service import start_service
from .db.bootstrap import bootstrap_database
from .db.log import log
from .db.backends import BACKENDS

lock = threading.Lock()


@pytest.fixture(scope="session")
def db_backend(request, backend):
    if not backend:
        raise Exception(f"No Backend specified.")

    if backend not in BACKENDS.keys():
        raise Exception(f"Unknown backend {backend}")

    db_backend = None
    try:
        lock.acquire()
        db_backend = start_service(backend)
        bootstrap_database(db_backend)
        yield db_backend
    finally:
        try:
            if db_backend:
                db_backend.teardown()
        finally:
            lock.release()
