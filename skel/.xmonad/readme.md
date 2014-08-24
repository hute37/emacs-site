file:///usr/share/doc/libghc-xmonad-contrib-doc/html/index.html

http://www.haskell.org/haskellwiki/Xmonad/Using_xmonad_in_Gnome

http://web.mit.edu/nelhage/Public/xmonad.hs


http://kalgan.cc/blog/posts/Gnome_3_and_XMonad/

https://www.google.it/search?q=xmonad+gnome&client=ubuntu&hs=BE0&channel=fs&source=lnms&tbm=isch&sa=X&ei=Bgv5U7f4HIHVPPj-gLgE&ved=0CAgQ_AUoAQ


http://www.crsr.net/Notes/XMonad.html

http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Actions-CycleWS.html


http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Util-EZConfig.html

http://www.haskell.org/haskellwiki/Xmonad/Config_archive

file:///usr/share/doc/libghc-xmonad-contrib-doc/html/XMonad-Doc.html

apt-get install \
	xmonad \
	libghc-xmonad-doc libghc-xmonad-dev \
	libghc-xmonad-contrib-doc libghc-xmonad-contrib-dev \
	xmobar \
	dzen2 

apt-get install conky-all
apt-get install trayer

sudo apt-get install suckless-tools



conky | dzen2 -x '500' -e '' -fg '#dcdcdc' -bg '#3f3f3f' \
-w '650' -ta r -fn '-*-terminus-*-r-normal-*-*-120-*-*-*-*-iso8859-*' -p



while true; do
    /usr/bin/xmonad | dzen2 -ta l
done


trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 6 --transparent true --alpha 0
 --tint 0x000000 --height 16 &

trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 15 --height 12 --transparent true --tint 0x000000 &



http://arjuna.deltoso.net/index.html%3Fp=242.html


#!/bin/bash

xrdb -merge .Xresources

trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 15 --height 12 --transparent true --tint 0x000000 &

gnome-screensaver

gnome-settings-daemon

if [ -x /usr/bin/gnome-power-manager ] ; then
   sleep 1
   gnome-power-manager
fi

if [ -x /usr/bin/nm-applet ] ; then
   nm-applet --sm-disable &
fi

kmix --keepvisibility

#feh --bg-scale /mnt/archivio/foto/2008-2009-dublino/2009-04-10-stefano/hapenny-desktop.jpg &

exec xmonad




