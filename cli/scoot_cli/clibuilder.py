from argparse import ArgumentParser


def _make_connection_args_parser():
    connarg_parser = ArgumentParser(add_help=False)
    connarg_parser.add_argument("--url", help="Database URL", required=False)
    connarg_parser.add_argument(
        "-c", dest="c", help="Configuration name", required=False
    )
    return connarg_parser


class CommandBuilder:

    def __init__(self, verb: str, **kwargs):
        self._verb = verb
        self._description = kwargs.get("description", None)
        self.arguments = []
        self._require_conn = kwargs.get("require_connection", False)
        self._conn_arg: ArgumentParser | None = kwargs.get("conn_arg", None)

    def require_connection(self):
        self._require_conn = True
        return self

    def requires_connection(self):
        return self._require_conn

    def description(self, description):
        self._description = description
        return self

    def argument(self, name, description: str | None = None):
        props = {}
        if description:
            props["help"] = description
        self.arguments.append({"name": name, "def": props})
        return self

    def flag(self, name):
        self.arguments.append({"name": name, "def": {"action": "store_true"}})
        return self

    def option(self, name):
        self.arguments.append({"name": name, "def": {}})
        return self

    def build(self, subparsers):
        props = {}
        if self._description:
            props["help"] = self._description
        if self._require_conn:
            props["parents"] = [self._conn_arg]
        parser = subparsers.add_parser(self._verb, **props)

        for arg in self.arguments:
            parser.add_argument(arg.get("name"), **arg.get("def"))


class ResourceBuilder:
    def __init__(self, name: str, **kwargs):
        self._name = name
        self._description: str | None = None
        self._verbs: dict[str, CommandBuilder] = {}
        self._require_conn = False
        self._conn_arg: ArgumentParser | None = kwargs.get("conn_arg", None)

    def verb(self, verb):
        builder = CommandBuilder(
            verb, require_connection=self._require_conn, conn_arg=self._conn_arg
        )
        self._verbs[verb] = builder
        return builder

    def description(self, docstring: str):
        self._description = docstring
        return self

    def require_connection(self):
        self._require_conn = True
        return self

    def requires_connection(self):
        return self._require_conn

    def build(self, subparsers):
        props = {}
        if self._description:
            props["help"] = self._description
        if self._require_conn:
            props["parents"] = [self._conn_arg]
        parser = subparsers.add_parser(self._name, **props)
        subparsers = parser.add_subparsers(dest="verb")

        for verb in self._verbs.values():
            verb.build(subparsers)


class ScootCLI:

    def __init__(self):
        self.parser = ArgumentParser(prog="scoot")
        self.subparsers = self.parser.add_subparsers(dest="resource", required=True)
        self.connarg_parser = _make_connection_args_parser()
        self.preparser = ArgumentParser(
            parents=[self.connarg_parser], add_help=False
        )
        self._resources: dict[str, ResourceBuilder] = {}
        self._verbs: dict[str, CommandBuilder] = {}

    def resource(self, name):
        builder = ResourceBuilder(name, conn_arg=self.connarg_parser)
        self._resources[name] = builder
        return builder

    def verb(self, name, description: str | None = None):
        builder = CommandBuilder(
            name, description=description, conn_arg=self.connarg_parser
        )
        self._verbs[name] = builder
        return builder

    def error(self, message):
        self.parser.error(message)

    def requires_connection(self):
        return False

    def _build_parser(self, subparsers):
        for resource in self._resources.values():
            resource.build(subparsers)

        for verb in self._verbs.values():
            verb.build(subparsers)

    def _resolve_verb_path(self, args):
        path = []
        if hasattr(args, "resource"):
            path.append(args.resource)

        if hasattr(args, "verb"):
            path.append(args.verb)

        return path

    def _resolve_verb(self, args):
        args = args[0]
        ctx = self
        if hasattr(args, "resource"):
            ctx = self._resources.get(args.resource, None)

        if ctx is None:
            return None

        if hasattr(args, "verb"):
            ctx = ctx._verbs.get(args.verb, None)

        return ctx

    def parse(self):
        preparser_sub = self.preparser.add_subparsers(
            dest="resource", required=True
        )
        self._build_parser(preparser_sub)
        verb = self._resolve_verb(self.preparser.parse_known_args())
        glob_args, remaining = self.connarg_parser.parse_known_args()

        if verb and verb.requires_connection():
            parsed = self.preparser.parse_args(remaining)
            setattr(parsed, "c", glob_args.c)
            setattr(parsed, "url", glob_args.url)
            return parsed

        self._build_parser(self.subparsers)
        return self.parser.parse_args()
