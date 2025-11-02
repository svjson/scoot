import json
from pathlib import Path
from typing import Annotated, Any

from scoot_core.exceptions import ScootConnectionException, ScootResourceException

default_config_name: Annotated[
    str, "The configuration name used if no configuration has been specified."
] = "scoot_default"

is_server = False

SCOOT_USER_DIR = Path.home() / ".scoot"

CONFIG_BASE_DIR = SCOOT_USER_DIR / "config"

CURRENT_CONTEXT_PATH = SCOOT_USER_DIR / "current_context.json"


def _load_config_file(path: Path) -> dict:
    """Load a named configuration from disk."""
    cfg = {"connections": {}}
    if path.exists():
        try:
            with path.open("r", encoding="utf-8") as f:
                cfg = json.load(f)
                if cfg["connections"] is None:
                    cfg["connections"] = {}
        except Exception as e:
            print(f"Error reading configuration file: {path}\n{e}")
    return cfg


class Context:

    def __init__(self, name: str, config: dict):
        self.name = name
        self.connections: dict[str, dict[str, Any]] = config.get("connections", {})

    def list_connection_names(self) -> list[str]:
        return list(self.connections.keys())

    def get_connection(self, name: str):
        return self.connections.get(name, None)

    def get_default_connection(self):
        return self.connections.get("default", None)

    def set_default_connection(self, name: str):
        if name not in self.connections.keys():
            raise ScootResourceException("connection", name)
        self.connections["default"] = self.connections[name]

    def persist(self):
        file_path = CONFIG_BASE_DIR / f"{self.name}.json"

        try:
            if not file_path.parent.exists():
                file_path.parent.mkdir(parents=True)
            with file_path.open("w", encoding="utf-8") as f:
                json.dump({"connections": self.connections}, f, indent=2)
        except Exception as e:
            print(f"Error writing configuration: {file_path}\n{e}")
        pass

    @staticmethod
    def list() -> list[str]:
        """List all .json files in CONFIG_BASE_DIR"""
        if not CONFIG_BASE_DIR.exists():
            return []

        context_files = CONFIG_BASE_DIR.glob("*.json")
        context_names = [f.stem for f in context_files if f.is_file()]
        return context_names

    @staticmethod
    def load_manifest() -> dict[str, dict[str, dict[str, dict]]]:
        return {
            ctx_name: {
                "connections": {
                    conn_name: {}
                    for conn_name in Context.load(ctx_name).list_connection_names()
                }
            }
            for ctx_name in Context.list()
        }

    @staticmethod
    def load(context_name: str | None):
        if context_name is None:
            current_name = Context.current_context_name()
            if not current_name:
                raise ScootResourceException("context", "<current_context>")
            return Context(current_name, _load_config_file(CURRENT_CONTEXT_PATH))
        else:
            return Context(
                context_name,
                _load_config_file(CONFIG_BASE_DIR / f"{context_name}.json"),
            )

    @staticmethod
    def use(ctx_name: str):
        ctx_path = _config_path(ctx_name)
        if not ctx_path.exists():
            raise ScootConnectionException(
                FileNotFoundError(ctx_path),
                f"Could not find context '{ctx_name}'.",
                ctx_name,
            )
        if Context.current_context_name():
            CURRENT_CONTEXT_PATH.unlink()
        CURRENT_CONTEXT_PATH.symlink_to(ctx_path)

    @staticmethod
    def exists(ctx_name: str):
        ctx_path = _config_path(ctx_name)
        return ctx_path.exists()

    @staticmethod
    def current_context_name():
        return (
            CURRENT_CONTEXT_PATH.readlink().stem
            if CURRENT_CONTEXT_PATH.exists() or CURRENT_CONTEXT_PATH.is_symlink()
            else None
        )


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

    config_contents = _load_config_file(CONFIG_BASE_DIR / f"{name}.json")
    global app_config
    app_config = Config(config_contents["connections"], name)


def list_configuration_names():
    """List all .json files in CONFIG_BASE_DIR"""
    if not CONFIG_BASE_DIR.exists():
        return []

    config_files = CONFIG_BASE_DIR.glob("*.json")
    config_names = [f.stem for f in config_files if f.is_file()]
    return config_names


def use_default():
    config_contents = _load_config_file(CURRENT_CONTEXT_PATH)
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
