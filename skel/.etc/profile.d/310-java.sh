# -*- mode: shell-script;-*-

##
#  Java environment 
#

#set -x


export SYS_JAVA="$(which java)"
export SYS_JAVA_BIN="dirname $(which java)"
export SYS_JAVA_HOME="dirname SYS_JAVA_BIN"


#set +x
