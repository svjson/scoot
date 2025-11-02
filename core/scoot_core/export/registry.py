import importlib
from typing import Type

from scoot_core.export.format.stream import StreamFormatter

_FORMATTERS: dict[str, Type[StreamFormatter]] = {}

_ALIASES: dict[str, str] = {"sql": "ddl"}


def register_formatter(name: str):
    def decorator(cls: Type[StreamFormatter]):
        global _FORMATTERS, _ALIASES
        _FORMATTERS[name] = cls
        return cls

    return decorator


def get_export_format(name: str) -> StreamFormatter | None:
    global _FORMATTERS, _ALIASES
    if name in _ALIASES:
        name = _ALIASES[name]
    if name in _FORMATTERS:
        return _FORMATTERS[name]()
    try:
        importlib.import_module(f"scoot_core.export.format.{name}")
    except ModuleNotFoundError:
        raise ValueError(f"Unknown formatter: {name}")

    if name not in _FORMATTERS:
        raise ValueError(f"Formatter {name!r} failed to register")
    return _FORMATTERS[name]()
