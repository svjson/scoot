import importlib

_FORMATTERS= {}

def register_formatter(name):
    def decorator(cls):
        _FORMATTERS[name] = cls
        return cls
    return decorator

def get_export_format(name):
    if name in _FORMATTERS:
        return _FORMATTERS[name]()
    try:
        importlib.import_module(f"scoot_core.export.format.{name}")
    except ModuleNotFoundError:
        raise ValueError(f"Unknown formatter: {name}")

    if name not in _FORMATTERS:
        raise ValueError(f"Formatter {name!r} failed to register")
    return _FORMATTERS[name]()
