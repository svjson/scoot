import ast
import os
import re
import shutil
import tempfile
import time

from system_test.db.infra import run_cmd, run_script, wait_and_retry
from system_test.db.log import log
from system_test.db.service import BackendService


class EmacsDaemon:
    """
    Represents a running Emacs daemon instance.

    Attributes:
        name (str): The name of the Emacs daemon instance.
        pid (int): The process ID of the Emacs daemon.
    """

    def __init__(self, name, pid):
        self.name = name
        self.pid = pid

    def eval_lisp(self, elisp, silent=True, silent_error=None):
        """
        Evaluate the given elisp expression in the Emacs daemon.

        Args:
            elisp (str): The elisp expression to evaluate.
            silent (bool): If True, suppress standard output.
            silent_error (bool or None): If True, suppress error output.
                If None, error output is not suppressed.
        """
        return run_cmd(
            ["emacsclient", "-s", self.name, "-e", elisp],
            silent=silent,
            silent_error=silent_error,
        )

    def stop(self):
        log.info(f"Stopping emacs daemon '{self.name}'")
        self.eval_lisp("(kill-emacs 0)")
        time.sleep(1)
        try:
            os.kill(self.pid, 15)
        except ProcessLookupError:
            log.info("Emacs exited nicely.")


def start_emacs_daemon(db_backend: BackendService):
    """
    Configures and starts a daemonized Emacs instance to use for
    running ert tests.
    """
    tmp_emacs_home = tempfile.mkdtemp(prefix="emacs-test-")
    log.info(f"Create tmp emacs home: {tmp_emacs_home}")

    init_dest = os.path.join(tmp_emacs_home, "init.el")
    log.info(f"Placing {init_dest}...")
    shutil.copyfile(os.path.join("system_test", "emacs", "test-init.el"), init_dest)

    log.info("Starting emacs daemon...")
    backend_name = db_backend.config.get("name")
    instance_name = f"dmn_{backend_name}"

    conn_url = db_backend.get_active_connection_url()

    run_script(
        "system_test/emacs/start_emacs_daemon.sh",
        {
            "SCOOT_TEMP_EMACS_DIR": tmp_emacs_home,
            "SCOOT_EMACS_INSTANCE_NAME": instance_name,
            "SCOOT_TEST_CONNECTION": f'(list :context "{backend_name}" :name "{backend_name}" :url "{conn_url}")',
        },
    )

    pid = get_emacs_pid(tmp_emacs_home)
    daemon = EmacsDaemon(instance_name, pid)

    log.info(
        f"Emacs daemon with pid {pid} running from user-emacs-directory {tmp_emacs_home}"
    )

    if is_emacs_initialized(daemon):
        log.info("Emacs test fixtures initialized.")

    return daemon


def start_emacs_unit_test_daemon():
    """
    Configures and starts a daemonized Emacs instance to use for
    running ert tests.
    """
    tmp_emacs_home = tempfile.mkdtemp(prefix="emacs-test-")
    log.info(f"Create tmp emacs home: {tmp_emacs_home}")

    init_dest = os.path.join(tmp_emacs_home, "init.el")
    log.info(f"Placing {init_dest}...")
    shutil.copyfile(os.path.join("system_test", "emacs", "test-init.el"), init_dest)

    log.info("Starting emacs daemon...")
    instance_name = "dmn_unit_test"

    run_script(
        "system_test/emacs/start_emacs_daemon.sh",
        {
            "SCOOT_TEMP_EMACS_DIR": tmp_emacs_home,
            "SCOOT_EMACS_INSTANCE_NAME": instance_name,
            "SCOOT_TEST_CONNECTION": "nil",
        },
    )

    pid = get_emacs_pid(tmp_emacs_home)
    daemon = EmacsDaemon(instance_name, pid)
    log.info(
        f"Emacs daemon with pid {pid} running from user-emacs-directory {tmp_emacs_home}"
    )

    log.info("Setting 'scoot-auto-start-server to nil")
    daemon.eval_lisp("(setq scoot-auto-start-server nil)")

    if is_emacs_initialized(daemon):
        log.info("Emacs test fixtures initialized.")

    return daemon


@wait_and_retry(wait_interval=0.1)
def get_emacs_pid(emacs_home: str):
    """
    Look for the PID of the Emacs daemon in `emacs_home`/proc.pid, which is
    dropped by the init.el used for daemon.
    """

    pid_file = os.path.join(emacs_home, "proc.pid")

    try:
        with open(pid_file) as f:
            return int(f.read().strip())
    except FileNotFoundError:
        return False


@wait_and_retry(wait_interval=0.1)
def is_emacs_initialized(daemon: EmacsDaemon):
    """
    Check for the Scoot test-runner function.
    """

    return daemon.eval_lisp("(fboundp 'scoot-test--run-test)", True, True) == "t"


def parse_test_result(result: str):
    """
    Parse the ert test result, as formatted by the `scoot-test--run-test`
    function defined in `test-init.el`
    """

    if result[0] == '"':
        result = ast.literal_eval(result)

    sections = {}
    pattern = r"\[([^\]]+)\](.*?)\[/\1\]"

    matches = re.findall(pattern, result, re.DOTALL)

    for tag, content in matches:
        sections[tag] = content.strip()

    return sections


def run_test(
    emacs_daemon: EmacsDaemon,
    test_file,
    test_name,
    root_path=["system_test", "emacs", "tests"],
    context_name=None,
):
    """
    Run a test in `test_file` identified by the symbol `test_name`.

    Args:
        emacs_daemon (EmacsDaemon): The emacs daemon to run the test in.
        test_file (str): The file name containing the test to run, relative to
            "<project root>/system_test/emacs/tests/"
        test_name (str): The symbol name (un-quoted) that identifies the test to
            run.
        context_name (str): Optional context name to output to the log for
            tracing. Has no functional effect.
    """
    context_label = f" ({context_name})" if context_name else ""
    log.info(f"Running test{context_label}: '{test_name}")

    relative_test_file = os.path.join(*root_path, test_file)
    log.info(f"From file: '{relative_test_file}'")

    emacs_daemon.eval_lisp(f'(load-file "{relative_test_file}")', True)

    test_result = emacs_daemon.eval_lisp(f"(scoot-test--run-test '{test_name})")
    result = parse_test_result(test_result)
    success = "ert-test-passed" == result.get("Result", None)

    if success:
        log.info(f"Passed: '{test_name}")
    else:
        log.error(f"Failed: '{test_name}'")
        for section in [
            "Result",
            "Duration",
            "Messages",
            "Assertions",
            "Infos",
            "Backtrace",
            "ServerOutput",
        ]:
            log.error("==========================================================")
            content = result.get(section, "")
            if content and type(content) is str and content.strip():
                lines = content.strip().replace("\\n", "\n").split("\n")
                if len(lines) == 1:
                    log.error(f"{section}: {lines[0]}")
                else:
                    log.error(f"{section}:")
                    log.error(
                        "----------------------------------------------------------"
                    )
                    for line in lines:
                        log.error(line)
            else:
                log.error(f"{section}: N/A")
        log.error("==========================================================")

    assert success
