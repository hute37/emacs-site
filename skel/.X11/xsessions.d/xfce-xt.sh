#!/bin/sh

firefox &
#LG=2=; xfce4-terminal  --geometry 224x32+32-72 --hide-borders -e "less +F $HOME/.xsession-errors" &
LG=2=; xfce4-terminal  --geometry 195x32+34-72 --hide-borders -e "tmux new-session 'less +F $HOME/.xsession-errors' ; split-window 'sudo journalctl -f'" &

es=140; emacs --eval="(set-face-attribute 'mode-line nil  :height $es)" --eval="(set-face-attribute 'default (selected-frame) :height $es)" &

termite &

TILIX_RUN=~/.config/tilix/tilix-run.sh

[   -x $TILIX_RUN ] && $TILIX_RUN --sx 6 &
[ ! -x $TILIX_RUN ] && tilix &



#urxvt &
urxvt -fn 'xft:Droid Sans Mono:pixelsize=13' \
    -e zsh -c "ee=0; ssh-add ~/.ssh/id_rsa; ssh-add ~/.ssh/uh; cat  ~/.rup/* ; ssh-add ~/.ssh/un; ssh-add ~/.ssh/id_e35; exec zsh --login" &
#urxvt -fn 'xft:Droid Sans Mono:pixelsize=13' \
#    -e zsh -c "ee=4; ssh-add; ssh-add -l; cat  ~/.rup/* ; exec zsh --login" &

sleep 12

wmctrl -x -r "Termite" -t 1
wmctrl -x -r "Emacs" -t 2
wmctrl -x -r "Tilix" -t 3
wmctrl -x -r "Firefox" -t 4

MAX='add,maximized_vert,maximized_horz'

wmctrl -x -r "Emacs"    -b $MAX
wmctrl -x -r "Tilix"    -b $MAX
wmctrl -x -r "Firefox"  -b $MAX

wmctrl -x -r "Termite"  -m '1,181,124,1359,844'
wmctrl -x -r "urxvt.URxvt"  -m '0,795,158,794,670'

wmctrl -x -a "urxvt.URxvt"


