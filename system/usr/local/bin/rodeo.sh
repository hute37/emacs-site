!/bin/sh

# Should be platform neutral - at least working on Linux and Windows
USER_NAME=`basename $HOME`

# HHHOME is used to pass the HOME directory of the user running rodeo
# and is used in "start.sh" to create the same user within the container.

# Users home is mounted as home
# --rm will remove the container as soon as it ends
docker run --rm \
    -i -t \
    -v ${HOME}:/home/${USER_NAME} \
    -v /tmp/.X11-unix:/tmp/.X11-unix \
    -e DISPLAY=unix$DISPLAY \
    -e HHHOME=${HOME} \
    timcera/rodeo-desktop-ubuntu:latest

