# -*- mode: shell-script;-*-

##
#  Hadoop environment 
#

#set -x


if [ -n "$SPARK_HOME" ] && [ -f ~/.spark.on ]; then

    #LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$SPARK_HOME/lib
    LD_LIBRARY_PATH=$SPARK_HOME/lib:$LD_LIBRARY_PATH
    export LD_LIBRARY_PATH
 
    # PATH=$PATH:$SPARK_HOME/bin
    PATH=$SPARK_HOME/bin:$PATH
    [ -z "$JAVA_HOME" ] || PATH=$JAVA_HOME/bin:$JAVA_HOME/jre/bin:$PATH
    export PATH

fi

#set +x
