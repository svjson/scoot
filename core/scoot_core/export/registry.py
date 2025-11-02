import importlib
from typing import Type

from scoot_core.export.format.stream import StreamFormatter

_FORMATTERS: dict[str, Type[StreamFormatter]] = {}


def register_formatter(name: str):
    def decorator(cls: Type[StreamFormatter]):
        _FORMATTERS[name] = cls
        return cls

    return decorator


def get_export_format(name: str) -> StreamFormatter | None:
    if name in _FORMATTERS:
        return _FORMATTERS[name]()
    try:
        importlib.import_module(f"scoot_core.export.format.{name}")
    except ModuleNotFoundError:
        raise ValueError(f"Unknown formatter: {name}")

    if name not in _FORMATTERS:
        raise ValueError(f"Formatter {name!r} failed to register")
    return _FORMATTERS[name]()
