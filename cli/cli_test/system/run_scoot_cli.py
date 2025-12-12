import subprocess


class ScootCli:
    def __init__(self, env):
        self.env = env

    def run(self, args: str | list[str]):
        arg_list = args if isinstance(args, list) else args.split(" ")
        return subprocess.run(
            ["scoot"] + arg_list, env=self.env, capture_output=True, text=True
        )
