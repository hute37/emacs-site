# -*- mode: shell-script;-*-

##
#  Java environment 
#

#set -x


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
