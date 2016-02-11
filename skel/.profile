#!/bin/sh
# -*- mode: shell-script;-*-

##
#  ~/.profile
#

# WARNING: check it with checkbashism (dash != bash)

# ------------------------------------------------
_DOT_PROFILE_0="$(date  --rfc-3339=ns)"; export _DOT_PROFILE_0
# ------------------------------------------------

##
# log
#

log_profile () {
   echo "$(date  --rfc-3339=ns) [profile.d] $*" >> ~/.profile.log
}

: > ~/.profile.log
log_profile "## >> ~/.profile (PID: $$) --ARGS $0 $* ##"


# the default umask is set in /etc/profile
#umask 022


##
# include
#

set -a
[ -r ~/.etc/profile.conf ] && . ~/.etc/profile.conf || true
set +a


# Load profiles from ~/.etc/profile.d
if [ -d ~/.etc/profile.d ]; then

    case "$0" in
        *csh) ;;
        *)

	    for profile in ~/.etc/profile.d/*.sh; do
	        if [ -x "$profile" ]; then
            	    log_profile "++ >> $profile"
		    . "$profile" || true
            	    log_profile "++ << $profile ($?)"
                fi
	    done
	    unset profile

            ;;
        *none)
            ;;
    esac
fi

env | sort >> ~/.profile.log

log_profile "## << ~/.profile ##"
# ------------------------------------------------
_DOT_PROFILE_1="$(date  --rfc-3339=ns)"; export _DOT_PROFILE_1
# ------------------------------------------------
