#!/bin/sh

case "$1" in
    --sx)
        shift
        TILIX_SESSION="$1"
        TILIX_COMMAND="tilix$TILIX_SESSION"
        shift
        ;;
    *)
        TILIX_COMMAND="$(basename $0 .sh)"
        TILIX_SESSION="${TILIX_COMMAND//tilix/}"
    ;;
esac
export TILIX_COMMAND
export TILIX_SESSION
exec tilix $(find ~/.config/tilix/sx/$TILIX_SESSION -name '*.json' | sort | sed -e 's/^/-s /' | tr '\n' ' ') $@

