# -*- mode: shell-script;-*-

##
#  ~/.profile
#



# ------------------------------------------------
export _DOT_PROFILE_0=`date  --rfc-3339=ns`
# ------------------------------------------------

##
# log
#

function log_profile {
   echo "`date  --rfc-3339=ns` [profile.d] $*" >> ~/.profile.log
}

: > ~/.profile.log
log_profile "## >> ~/.profile (PID: $$) --ARGS $0 $* ##"


# the default umask is set in /etc/profile
#umask 022

##
# shell
#

if [ -n "$BASH_VERSION" ]; then
    export _DOT_SHELL='bash'
fi

if [ -n "$ZSH_VERSION" ]; then
    export _DOT_SHELL='zsh'
fi


##
# include
#

set -a
[ -r ~/.etc/profile.conf ] && source ~/.etc/profile.conf || true
set +a


# Load profiles from ~/.etc/profile.d
if test -d ~/.etc/profile.d/; then
    case "$_DOT_SHELL" in
        bash|zsh)

	    for profile in ~/.etc/profile.d/*.sh; do
	        if [ -x "$profile" ]; then
            	    log_profile "++ >> $profile"
		    . "$profile" || true
            	    log_profile "++ << $profile ($?)"
                fi
	    done
	    unset profile

            ;;
        *)
            ;;
    esac
fi



log_profile "## << ~/.profile ##"
# ------------------------------------------------
export _DOT_PROFILE_1=`date  --rfc-3339=ns`
# ------------------------------------------------
