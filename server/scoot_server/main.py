import argparse

from scoot_server import server
from scoot_core import config


def main():
    parser = argparse.ArgumentParser(prog="scoot-server", add_help=True)
    parser.add_argument("-p", help="Server port", required=False)
    parser.add_argument("-c", help="Server configuration name", required=False)
    args = parser.parse_args()

    app = server.app

    port = getattr(args, "p")
    if port is None:
        port = 8221

    cfg_name = getattr(args, "c")
    config.configure(cfg_name)

    print(f" * Configuration: {config.app_config.name}")

    app.run(host="127.0.0.1", port=port)


if __name__ == "__main__":
    main()
