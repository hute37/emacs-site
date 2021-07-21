#!/bin/sh


tilix6() { export TILIX_SESSION=6; tilix $(find ~/.config/tilix/sx/$TILIX_SESSION -name '*.json' | sort | sed -e 's/^/-s /' | tr '\n' ' '); }


