import json
from pathlib import Path
from typing import Annotated

default_config_name: Annotated[
    str, "The configuration name used if no configuration has been specified."
] = "scoot_default"

is_server = False


class Config:
    """Represents a persistent configuration of connections"""

    def __init__(self, connections: dict, name: str):
        self.name = name
        self.connections = connections

    def to_dict(self):
        return {"connections": self.connections}


app_config: Annotated[Config, "The currently active configuration."] = Config(
    {}, default_config_name
)


def _config_path(name: str):
    """Constructs a path to a configuration file on disk based
    on the configuration name."""
    return Path.home() / ".scoot" / "config" / f"{name}.json"


def _load_config(name: str) -> dict:
    """Load a named configuration from disk."""
    config_path = _config_path(name)
    cfg = {"connections": {}}
    if config_path.exists():
        try:
            with config_path.open("r", encoding="utf-8") as f:
                cfg = json.load(f)
                if cfg["connections"] is None:
                    cfg["connections"] = {}
        except Exception as e:
            print(f"Error reading configuration file: {config_path}\n{e}")
    return cfg


def persist():
    """Persists the current state of the active configuration."""
    config_path = _config_path(app_config.name)
    try:
        if not config_path.parent.exists():
            config_path.parent.mkdir(parents=True)
        with config_path.open("w", encoding="utf-8") as f:
            json.dump(app_config.to_dict(), f, indent=2)
    except Exception as e:
        print(f"Error writing configuration: {config_path}\n{e}")


def configure(name):
    """Initialize the application configuration."""
    if name is None:
        name = default_config_name

    config_contents = _load_config(name)
    global app_config
    app_config = Config(config_contents["connections"], name)
