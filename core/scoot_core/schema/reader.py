from abc import ABC, abstractmethod


class SchemaReader(ABC):
    """
    Abstract base class for reading schemas from any source into the
    intermediate TableIR format.
    """

    @abstractmethod
    def read_table(self):
        """
        Read a single table schema from the source defined by the subclass.
        """
        raise NotImplementedError
