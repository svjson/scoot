from typing import Protocol, runtime_checkable

import sqlalchemy


@runtime_checkable
class SqlAlchemyTypeConvertible(Protocol):
    def to_sqlalchemy_type(self) -> sqlalchemy.types.TypeEngine:
        raise NotImplementedError


@runtime_checkable
class SqlGlotTypeConvertible(Protocol):
    def to_sqlglot_type(self) -> sqlalchemy.types.TypeEngine:
        raise NotImplementedError
