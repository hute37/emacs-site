#!/bin/sh
# -*- mode: shell-script;-*-

##
#  anaconda python environment
#

CONDA_DEFAULT=/opt/sc/anaconda3

if [ -d "$CONDA_DEFAULT" ]; then

    CONDA_HOME="$CONDA_DEFAULT"

    PYSPARK_PYTHON=$CONDA_HOME/bin/python3

    PYTHONHASHSEED=0
    
    # LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$CONDA_HOME/lib
    # LD_LIBRARY_PATH=$CONDA_HOME/lib:$LD_LIBRARY_PATH
    # export LD_LIBRARY_PATH
 
    # PATH=$PATH:$CONDA_HOME/bin
    # PATH=$CONDA_HOME/bin:$PATH
    # [ -z "$JAVA_HOME" ] || PATH=$JAVA_HOME/bin:$JAVA_HOME/jre/bin:$PATH
    # export PATH

    export CONDA_HOME
    export PYSPARK_PYTHON
    export PYTHONHASHSEED
    
fi

