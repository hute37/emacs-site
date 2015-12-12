#!/bin/sh
# -*- mode: shell-script;-*-

##
#  Java environment 
#

#set -x


SYS_JAVA="$(which java)"
SYS_JAVA_BIN="dirname $(which java)"
SYS_JAVA_HOME="dirname SYS_JAVA_BIN"

export SYS_JAVA 
export SYS_JAVA_BIN
export SYS_JAVA_HOME

SYSJAVACONF="/opt/java/etc/java.conf"
USRJAVACONF="$HOME/.etc/java.conf"

JAVA_CONF_SET=1

if [ -r "$USRJAVACONF" ]; then
 JAVACONF=$USRJAVACONF
elif [ -r "$SYSJAVACONF" ]; then
 JAVACONF=$SYSJAVACONF
else
 JAVA_CONF_SET=0
fi

export JAVA_CONF_SET

if [ -n "$JAVACONF" ] ; then

#set -a

test -f $JAVACONF && . $JAVACONF || true

#set +a

fi

#set +x
