import time
from functools import wraps


def wait_and_retry(max_attempts=30, wait_interval: float = 0.5):

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
