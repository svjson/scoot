from abc import ABC, abstractmethod
from typing import Generic, TypeVar

from .ir import TableIR


T = TypeVar("T")


class SchemaEmitter(Generic[T], ABC):
    """
    Abstract base class for emitting a intermediate format TableIR into some
    other format.
    """

    @abstractmethod
    def emit_table(self, table: TableIR) -> T:
        """
        Emit a single table model in the format defined by the concrete
        emitter instance.

        Args:
            table (TableIR): The table intermediate representation to emit.
        """
        raise NotImplementedError
