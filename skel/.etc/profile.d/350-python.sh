# -*- mode: shell-script;-*-

##
#  virtualenv (wrapper) python environment
#


export SYS_PYTHON=`which python`
export VIRTUALENVWRAPPER_PYTHON="$SYS_PYTHON"
export VIRTUALENVWRAPPER_SCRIPT=`which virtualenvwrapper.sh`

export VIRTUALENV_CONFIG=~/.venv.conf
export WORKON_HOME=~/.virtualenvs

set -a
[ -r $VIRTUALENV_CONFIG ] && source $VIRTUALENV_CONFIG || true
set +a
    
[ -x $VIRTUALENVWRAPPER_SCRIPT ] && source $VIRTUALENVWRAPPER_SCRIPT || true

if [ -n "$VE_NAME" ] ; then #@todo test workon defined (type workon function)
    workon "$VE_NAME"
fi
