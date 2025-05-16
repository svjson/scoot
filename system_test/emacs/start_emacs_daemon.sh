#!/bin/sh

INST=$SCOOT_EMACS_INSTANCE_NAME
EMD=$SCOOT_TEMP_EMACS_DIR
CONN=$SCOOT_TEST_CONNECTION

# Start daemon and trim X11 warning on stderr
emacs --daemon=$INST --init-directory "${EMD}"  2>&1 | grep -v -A 4 -B 1 "Warning: due to a long standing Gtk+ bug"

# Load init.el and set connection to use for tests.
emacsclient -s $INST -e "(load-file \"${EMD}/init.el\")"
emacsclient -s $INST -e "(scoot-test--set-connection ${CONN})"
