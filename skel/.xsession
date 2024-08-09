[ -f ~/.default-session ] && export U_SESSION="$(cat ~/.default-session)"


case "$U_SESSION" in

	xfce) 

	/etc/X11/Xsession xfce4-session
	;;

	*)

	exec /usr/bin/xterm -n Login -T Login
	;;	
esac

