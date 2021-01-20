# -*- mode: shell-script;-*-

##
#  virtualenv (wrapper) python environment
#

venv_wrap_bash() {

export SYS_PYTHON=`which python`
export VIRTUALENVWRAPPER_PYTHON="$SYS_PYTHON"
export VIRTUALENVWRAPPER_SCRIPT=`which virtualenvwrapper.sh`

export VENV_CONFIG=/etc/venv.conf
export WORKON_HOME=/usr/local/lib/pythonenvs

set -a
[ -r "$VENV_CONFIG" ] && source $VENV_CONFIG || true
set +a

[ -n "$VIRTUALENVWRAPPER_SCRIPT" ] && \
[ -x "$VIRTUALENVWRAPPER_SCRIPT" ] && source $VIRTUALENVWRAPPER_SCRIPT || true

if [ -n "$VENV_NAME" ] ; then 
    workon "$VENV_NAME"
fi

}
