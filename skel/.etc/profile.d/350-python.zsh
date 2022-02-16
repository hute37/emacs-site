# -*- mode: shell-script;-*-

##
#  pyenv environment
#
if [ "$PY_RC_ENV" = "1" ]; then

py_rc_env() {

eval "$(pyenv init -)"

}

# in ~/.zshrc
# py_rc_env


fi

