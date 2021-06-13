#!/bin/sh
# -*- mode: shell-script;-*-

##
#  virtualenv (wrapper) python environment
#

venv_wrap_sh() {

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
