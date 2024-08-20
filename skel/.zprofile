#!/bin/zsh

##
#  ~/.zprofile
#


# ------------------------------------------------
export _DOT_ZSH_PROFILE_0="$(date  --rfc-3339=ns)"
# ------------------------------------------------

#% echo "% > ~/.zprofile"

##
# log
#

function log_zsh_profile {
   echo "$(date  --rfc-3339=ns) [profile.d/zsh] $*" >> ~/.zsh_profile.log
}

: > ~/.zsh_profile.log
log_zsh_profile "## >> ~/.zsh_profile (PID: $$) --ARGS: $0 $* --OPTS: $- ##"


##
# zsh login mode
#

export ZSH_LOGIN=1


##
# POSIX profile
#

if [ -f ~/.profile ] ; then
    log_zsh_profile "++ >> .profile"
    [[ -e ~/.profile ]] && emulate sh -c 'source ~/.profile'
    log_zsh_profile "++ << .profile"
fi


##
# include
#
set -a
[ -r ~/.etc/zsh_profile.conf ] && source ~/.etc/zsh_profile.conf || true
set +a


# Load profiles from ~/.etc/profile.d
if test -d ~/.etc/profile.d/; then
    for profile in ~/.etc/profile.d/*.zsh; do
	if [ -x "$profile" ]; then
            log_zsh_profile "++ >> $profile"
	    . "$profile" || true
            log_zsh_profile "++ << $profile ($?)"
        fi
    done
    unset profile
fi


log_zsh_profile "## << ~/.zsh_profile ##"
#% echo "% < ~/.zprofile"
# ------------------------------------------------
export _DOT_ZSH_PROFILE_1="$(date  --rfc-3339=ns)"
# ------------------------------------------------
