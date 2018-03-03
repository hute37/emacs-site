# -*- mode: shell-script;-*-

##
#  anaconda python environment (first)
#

if [ -n "$CONDA_HOME" ] && [ -f ~/.conda.on ]; then

    #LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$CONDA_HOME/lib
    #LD_LIBRARY_PATH=$CONDA_HOME/lib:$LD_LIBRARY_PATH
    #export LD_LIBRARY_PATH
 
    #PATH=$PATH:$CONDA_HOME/bin
    PATH=$CONDA_HOME/bin:$PATH
    [ -z "$JAVA_HOME" ] || PATH=$JAVA_HOME/bin:$JAVA_HOME/jre/bin:$PATH
    export PATH

fi
