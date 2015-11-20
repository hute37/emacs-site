#!/bin/bash

# ------------------------------------------------
export _DOT_BASH_PROFILE_0=`date  --rfc-3339=ns`
# ------------------------------------------------

if [ -f ~/.profile ] ; then
    . ~/.profile
fi


case "$-" in *i*) if [ -r ~/.bashrc ]; then . ~/.bashrc; fi;; esac


# ------------------------------------------------
export _DOT_BASH_PROFILE_1=`date  --rfc-3339=ns`
# ------------------------------------------------
