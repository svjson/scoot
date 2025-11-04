import time
import os
import subprocess
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
        with subprocess.Popen(
            cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, env=env
        ) as proc:
            assert proc.stdout is not None
            assert proc.stderr is not None
            for line in proc.stdout:
                if not silent:
                    log.info(line.strip())
                output_lines.append(line.strip())

            for line in proc.stderr:
                if not silent_error:
                    log.error(line.strip())
                output_lines.append(line.strip())

            proc.wait()

        return "\n".join(output_lines)
    except subprocess.CalledProcessError as e:
        log.info(e.stdout)
        log.error(f"Command failed with return code {e.returncode}")
        log.error(e.stderr)
        return "\n".join([e.stdout, e.stderr])


def run_script(script, env_vars={}) -> str:
    return run_cmd(["bash", script], env_vars)
