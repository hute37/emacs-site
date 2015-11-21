#!/bin/bash

##
#  ~/.bash_profile
#


# ------------------------------------------------
export _DOT_BASH_PROFILE_0=`date  --rfc-3339=ns`
# ------------------------------------------------

##
# log
#

function log_bash_profile {
   echo "`date  --rfc-3339=ns` [profile.d/bash] $*" >> ~/.bash_profile.log
}

: > ~/.bash_profile.log
log_bash_profile "## >> ~/.bash_profile (PID: $$) --ARGS $0 $*  --OPTS: $- ##"


##
# bash login mode
#

export BASH_LOGIN=1


##
# POSIX profile
#

if [ -f ~/.profile ] ; then
    log_bash_profile "++ >> .profile"
    . ~/.profile
    log_bash_profile "++ << .profile"
fi


##
# include
#

set -a
[ -r ~/.etc/bash_profile.conf ] && source ~/.etc/bash_profile.conf || true
set +a


# Load profiles from ~/.etc/profile.d
if test -d ~/.etc/profile.d/; then
    for profile in ~/.etc/profile.d/*.bash; do
	if [ -x "$profile" ]; then
            log_bash_profile "++ >> $profile"
	    . "$profile" || true
            log_bash_profile "++ << $profile ($?)"
        fi
    done
    unset profile
fi


##
# RC
#

case "$-" in
    *i*)
        if [ -f ~/.bashrc ] ; then
            log_bash_profile "++ >> .bashrc"
            . ~/.bashrc
            log_bash_profile "++ << .bashrc"
        fi
        ;;
esac


log_bash_profile "## << ~/.bash_profile ##"
# ------------------------------------------------
export _DOT_BASH_PROFILE_1=`date  --rfc-3339=ns`
# ------------------------------------------------
