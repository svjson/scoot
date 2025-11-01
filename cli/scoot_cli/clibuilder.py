from argparse import ArgumentParser


class CommandBuilder:

    def __init__(self, verb: str, **kwargs):
        self._verb = verb
        self._description = kwargs.get("description", None)
        self.arguments = []
        self._require_conn = kwargs.get("require_connection", False)

    def require_connection(self):
        self._require_conn = True
        return self

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

    def build(self, subparsers):
        props = {}
        if self._description:
            props["help"] = self._description
        parser = subparsers.add_parser(self._verb, **props)
        if self._require_conn:
            parser.add_argument("--url", help="Database URL", required=False)
            parser.add_argument("-c", help="Configuration name", required=False)

        for arg in self.arguments:
            parser.add_argument(arg.get("name"), **arg.get("def"))


class ResourceBuilder:
    def __init__(self, name: str):
        self._name = name
        self._description: str | None = None
        self._verbs: list[CommandBuilder] = []
        self._require_conn = False

    def verb(self, verb):
        builder = CommandBuilder(verb, require_connection=self._require_conn)
        self._verbs.append(builder)
        return builder

    def description(self, docstring: str):
        self._description = docstring
        return self

    def require_connection(self):
        self._require_conn = True
        return self

    def build(self, subparsers):
        props = {}
        if self._description:
            props["help"] = self._description
        parser = subparsers.add_parser(self._name, **props)
        subparsers = parser.add_subparsers(dest="verb")

        for verb in self._verbs:
            verb.build(subparsers)


class ScootCLI:

    def __init__(self):
        self.parser = ArgumentParser(prog="scoot")
        self.subparsers = self.parser.add_subparsers(dest="resource", required=True)
        self._resources: list[ResourceBuilder] = []
        self._verbs: list[CommandBuilder] = []

    def resource(self, name):
        builder = ResourceBuilder(name)
        self._resources.append(builder)
        return builder

    def verb(self, name, description: str | None = None):
        builder = CommandBuilder(name, description=description)
        self._verbs.append(builder)
        return builder

    def error(self, message):
        self.parser.error(message)

    def parse(self):
        for resource in self._resources:
            resource.build(self.subparsers)

        for verb in self._verbs:
            verb.build(self.subparsers)

        return self.parser.parse_args()
