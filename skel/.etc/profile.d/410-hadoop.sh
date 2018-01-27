# -*- mode: shell-script;-*-

##
#  Hadoop environment 
#

#set -x


SPARK_DEFAULT=/opt/sc/spark

if [ -d "$SPARK_DEFAULT" ]; then

    SPARK_HOME="$SPARK_DEFAULT"

    SPARK_MAJOR_VERSION=2
    
    # PATH=$PATH:$SPARK_HOME/bin
    # PATH=$SPARK_HOME/bin:$PATH
    # [ -z "$JAVA_HOME" ] || PATH=$JAVA_HOME/bin:$JAVA_HOME/jre/bin:$PATH
    # export PATH


    # PYSPARK_PYTHON=$CONDA_HOME/bin/python3
    # PYTHONHASHSEED=0
    
    # LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$CONDA_HOME/lib
    # LD_LIBRARY_PATH=$CONDA_HOME/lib:$LD_LIBRARY_PATH
    # export LD_LIBRARY_PATH

    export SPARK_HOME
    export SPARK_MAJOR_VERSION
    
    # export PYSPARK_PYTHON
    # export PYTHONHASHSEED
    
fi



#set +x
