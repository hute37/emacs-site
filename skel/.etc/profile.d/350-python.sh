#!/bin/sh
# -*- mode: shell-script;-*-
export PY_RC_PROFILE=1


##
#  pyenv environment
#
if [ ! -f ~/.py-env.off ]; then
if [ -d $HOME/.pyenv ]; then    

    export PY_RC_ENV=1

py_rc_env_sh() {

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init --path)"


}

py_rc_env_sh

fi
fi


##
#  poetry environment
#
if [ ! -f ~/.py-poetry.off ]; then
if [ -d $HOME/.poetry ]; then    

    export PY_RC_POETRY=1

py_rc_poetry_sh() {

export PATH="$HOME/.poetry/bin:$PATH"

}

py_rc_poetry_sh

fi




##
#  virtualenv (wrapper) python environment
#
if [ -f ~/.py-venv.on ]; then
    export PY_RC_VENV=1
    

py_rc_venv_wrap_sh() {

SYS_PYTHON=`which python`
VIRTUALENVWRAPPER_PYTHON="$SYS_PYTHON"
VIRTUALENVWRAPPER_SCRIPT=`which virtualenvwrapper.sh`

VENV_CONFIG=~/.etc/venv.conf
WORKON_HOME=~/.virtualenvs

export SYS_PYTHON
export VIRTUALENVWRAPPER_PYTHON
export VIRTUALENVWRAPPER_SCRIPT

export VENV_CONFIG
export WORKON_HOME

set -a
[ -r $VENV_CONFIG ] && . $VENV_CONFIG || true
set +a

}

py_rc_venv_wrap_sh

fi


