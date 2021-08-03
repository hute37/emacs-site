#!/bin/sh

[ -f ~/.default-session ] && export U_SESSION_DEFAULT="$(cat ~/.default-session)"

if [ -n "$1" ]; then
   export U_SESSION="$1"
   shift
fi

: ${U_SESSION:=$U_SESSION_DEFAULT}
: ${U_INIT:="$HOME/.X11/xinit.d"}


sx_xfce() {

	/etc/X11/Xsession xfce4-session
    
}

sx_i3() {

    set -x

    [ -e ~/.Xmodmap ]    	&& xmodmap ~/.Xmodmap
    [ -e ~/.Xresources ] 	&& xrdb -merge ~/.Xresources

    [ -x $U_INIT/urvxtd.sh ]    && . $U_INIT/urvxtd.sh

    [ -z "SSH_AUTH_SOCK" ] 	&& eval $(ssh-agent)

    set +x

    dbus-launch i3

}

sx_xfailsafe() {

	exec /usr/bin/xterm -n Login -T Login

}

echo "#session: i\"$U_SESSION\""

case "$U_SESSION" in

	xfce)   sx_xfce $@ ;;
	i3)     sx_i3 $@ ;;
	*)      sx_xfailsafe $@ ;;

esac

