# -*- mode: shell-script;-*-

##
#  pyenv environment
#
if [ "$PY_RC_ENV" ]; then

py_rc_env() {

eval "$(pyenv init -)"

}

# in ~/.zshrc
# py_rc_env


fi

