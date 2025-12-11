#!/bin/sh

INST=$SCOOT_EMACS_INSTANCE_NAME
EMD=$SCOOT_TEMP_EMACS_DIR

# Activate virtualenv for subprocesses
if [ -f ".venv/bin/activate" ]; then
    # Replicate virtualenv activate behavior
    export PATH="$(pwd)/.venv/bin:$PATH"
    export VIRTUAL_ENV="$(pwd)/.venv"
fi

# Start daemon and trim X11 warning on stderr
emacs --daemon=$INST --init-directory "${EMD}"  2>&1 | grep -v -A 4 -B 1 "Warning: due to a long standing Gtk+ bug"

