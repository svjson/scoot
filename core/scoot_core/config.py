import json
from pathlib import Path
from typing import Annotated

from scoot_core.exceptions import ScootConnectionException

default_config_name: Annotated[
    str, "The configuration name used if no configuration has been specified."
] = "scoot_default"

is_server = False

SCOOT_USER_DIR = Path.home() / ".scoot"

CONFIG_BASE_DIR = SCOOT_USER_DIR / "config"


class Config:
    """Represents a persistent configuration of connections"""

    def __init__(self, connections: dict[str, dict], name: str):
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
    return CONFIG_BASE_DIR / f"{name}.json"


def _load_config(path: Path, name: str) -> dict:
    """Load a named configuration from disk."""
    config_path = path / name
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

    config_contents = _load_config(CONFIG_BASE_DIR, name + ".json")
    global app_config
    app_config = Config(config_contents["connections"], name)


def list_configuration_names():
    """List all .json files in CONFIG_BASE_DIR"""
    if not CONFIG_BASE_DIR.exists():
        return []

    config_files = CONFIG_BASE_DIR.glob("*.json")
    config_names = [f.stem for f in config_files if f.is_file()]
    return config_names


def default_connection_exists():
    default_cfg_path = SCOOT_USER_DIR / "default_config.json"
    return (
        default_cfg_path
        if default_cfg_path.exists() or default_cfg_path.is_symlink()
        else None
    )


def use_default():
    config_contents = _load_config(SCOOT_USER_DIR, "default_config.json")
    global app_config
    # Get the file name stem of the file default_cfg_path symlink points to
    default_cfg_path = SCOOT_USER_DIR / "default_config"
    cfg_name = default_cfg_path.resolve().stem
    app_config = Config(config_contents["connections"], cfg_name)
    return app_config.connections.get("default", {}).get("url")


def set_default_connection(conn_name: str):
    cfg_path = _config_path(conn_name)
    if not cfg_path.exists():
        raise ScootConnectionException(
            FileNotFoundError(cfg_path),
            f"Could not find connection '{conn_name}'.",
            conn_name,
        )
    default_cfg_path = SCOOT_USER_DIR / "default_config.json"
    if default_cfg_path.exists() or default_cfg_path.is_symlink():
        default_cfg_path.unlink()
    default_cfg_path.symlink_to(cfg_path)
