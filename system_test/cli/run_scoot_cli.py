import subprocess


class ScootCli:
    def __init__(self, env):
        self.env = env

    def run(self, args: str):
        return subprocess.run(
            ["scoot"] + args.split(" "),
            env=self.env,
            capture_output=True,
            text=True,
        )
