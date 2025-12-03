import os
import selectors
import subprocess
import time
from functools import wraps

from system_test.db.log import log


def wait_and_retry(
    max_attempts=int(os.getenv("SCOOT_WAIT_TIMEOUT", 30)),
    wait_interval: float = 0.5,
):
    def func_wrapper(func):
        @wraps(func)
        def retry_wrapper(*args, **kwargs):
            retries = 0
            success_sem = False
            last_result = None

            while not success_sem and retries < max_attempts:
                last_result = func(*args, **kwargs)
                if last_result and not isinstance(last_result, Exception):
                    return last_result
                time.sleep(wait_interval)
                retries += 1

            raise RuntimeError(
                f"Action {func.__name__} did not complete in time. Last attempt gave: {last_result}"
            )

        return retry_wrapper

    return func_wrapper


def run_cmd(cmd, env_vars={}, silent=True, silent_error=None) -> str:
    env = os.environ.copy()
    env.update(env_vars)
    output_lines = []
    try:
        proc = subprocess.Popen(
            cmd,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            env=env,
            bufsize=1,
        )

        sel = selectors.DefaultSelector()
        assert proc.stdout is not None
        assert proc.stderr is not None
        kout = sel.register(proc.stdout, selectors.EVENT_READ)
        kerr = sel.register(proc.stderr, selectors.EVENT_READ)

        fd_map = {
            kout: {
                "stream": proc.stdout,
                "eof": False,
                "silent": silent,
                "log": log.info,
            },
            kerr: {
                "stream": proc.stderr,
                "eof": False,
                "silent": silent_error,
                "log": log.error,
            },
        }

        while True:
            exited = proc.poll()

            events = sel.select(timeout=0.05)

            for fd, e in events:
                fdobj = fd_map[fd]
                instr = fdobj["stream"]
                line = instr.readline()

                if not line:
                    if exited is not None:
                        fdobj["eof"] = True
                    continue

                line = line.rstrip()

                fdobj["eof"] = False
                output_lines.append(line)
                if not fdobj["silent"]:
                    fdobj["log"](line)

            if exited is not None:
                if fd_map[kout]["eof"] and fd_map[kerr]["eof"]:
                    break

        return "\n".join(output_lines)
    except subprocess.CalledProcessError as e:
        log.info(e.stdout)
        log.error(f"Command failed with return code {e.returncode}")
        log.error(e.stderr)
        return "\n".join([e.stdout, e.stderr])


def run_script(script, env_vars={}) -> str:
    return run_cmd(["bash", script], env_vars)
