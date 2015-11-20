# -*- mode: shell-script;-*-

##
#  Java environment 
#

#set -x


export SYS_JAVA="$(which java)"
export SYS_JAVA_BIN="dirname $(which java)"
export SYS_JAVA_HOME="dirname SYS_JAVA_BIN"


SYSJAVACONF="/opt/java/etc/java.conf"
USRJAVACONF="$HOME/.etc/java.conf"

JAVA_CONF_SET=1

if [ -r "$USRJAVACONF" ]; then
 JAVACONF=$USRJAVACONF
elif [ -r "$SYSJAVACONF" ]; then
 JAVACONF=$SYSJAVACONF
else
 export JAVA_CONF_SET=0
fi

if [ -n "$JAVACONF" ] ; then

#set -a

test -f $JAVACONF && . $JAVACONF || true

#set +a

fi

#set +x
