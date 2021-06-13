# -*- mode: shell-script;-*-

##
#  Java environment 
#

#set -x


SYSJAVACONF="/opt/java/etc/java.conf"

if [ -f ~/.java-8.on ]; then 
	USRJAVACONF="$HOME/.etc/java8.conf"
else if [ -f ~/.java-7.on ]; then
	USRJAVACONF="$HOME/.etc/java7.conf"
else if [ -f ~/.java.on ]; then
	USRJAVACONF="$HOME/.etc/java.conf"
else
	USRJAVACONF=""
fi
fi
fi


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
