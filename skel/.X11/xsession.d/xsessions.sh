#!/bin/sh

if [ -z "XSESSIONS_ENTER" ]; then
    export XSESSIONS_ENTER=1
    exec $SHELL $0 $@
fi
unset XSESSIONS_ENTER

: ${XSESSIONS_NAME:=${1:-$DESKTOP_SESSION}}
: ${XSESSIONS_FILE:=$HOME/.X11/xsessions.d/$XSESSIONS_NAME.sh}

export XSESSIONS_NAME
export XSESSIONS_FILE

if [ ! -f $XSESSIONS_FILE ]; then
    echo "xsessions - $(date -Isec) - ERROR: NOTFND - sx: $XSESSIONS_NAME, file: $XSESSIONS_FILE"
    exit 1
fi

echo "xsessions - $(date -Isec) - info: start - sx: $XSESSIONS_NAME, file: $XSESSIONS_FILE, ..."

set -x
. $XSESSIONS_FILE
set +x

export rc=$?

echo "xsessions - $(date -Isec) - info: start - sx: $XSESSIONS_NAME, file: $XSESSIONS_FILE, (rc=$rc) done."

exit $rc
