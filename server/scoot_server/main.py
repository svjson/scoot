import argparse

from scoot_server import server

def main():
    parser = argparse.ArgumentParser(prog="scoot-server", add_help=True)
    parser.add_argument("-p", help="Server port", required=False)

    args = parser.parse_args()

    app = server.app

    port = getattr(args, "p")
    if port is None:
        port = 8221

    app.run(host="127.0.0.1", port=port)

if __name__ == "__main__":
    main()
