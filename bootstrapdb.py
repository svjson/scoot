#!.venv/bin/python

import sys
import signal
import traceback
from typing import Optional
from time import sleep

from system_test.db.log import log
from system_test.db.service import BackendService, start_service
from system_test.db.bootstrap import bootstrap_database


def main():
    if len(sys.argv) != 2:
        print("Usage: python -m bootstrapdb <backend_name>")
        return

    service: Optional[BackendService] = None

    def teardown(sig, _frame):
        del _frame
        print("Stopping...")
        if service is not None:
            service.teardown()
        print("Exiting...")
        sys.exit(sig)

    try:
        backend_name = sys.argv[1]

        service = start_service(backend_name)

        for sig in (signal.SIGINT, signal.SIGTERM, signal.SIGQUIT, signal.SIGHUP):
            signal.signal(sig, teardown)

        bootstrap_database(service)

        log.info(f"Service {service.name} startup completed...")

        try:
            while True:
                sleep(2)
        except KeyboardInterrupt:
            teardown(signal.SIGTERM, None)

    except Exception as e:
        print(str(e))
        traceback.print_exc()
        teardown(None, None)


if __name__ == '__main__':
    main()
