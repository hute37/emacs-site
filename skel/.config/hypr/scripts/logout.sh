#!/bin/bash

# pub const LOCK_SCREEN: char = '\u{f033e}'; // 󰌾
# pub const LOGOUT: char = '\u{f0343}'; // 󰍃
# pub const SUSPEND: char = '\u{f04b2}'; // 󰒲
# pub const HIBERNATE: char = '\u{f02ca}'; // 󰋊
# pub const REBOOT: char = '\u{f0709}'; // 󰜉
# pub const SHUTDOWN: char = '\u{f0425}'; // 󰐥
# pub const CANCEL: char = '\u{f0156}'; // 󰅖

TITLE="$(printf "Exit")"
MENU="wofi --dmenu -l 3 -y 10 -W 100 -H 200 -p $TITLE"
OPTIONS="$(printf "󰌾  Lock\n󰒲  Suspend\n󰍃  Logout\n────────────\n󰜉  Reboot\n  Shutdown")"
SELECTION="$($MENU <<< $OPTIONS)"


case $SELECTION in
    *"Lock")
        hyprlock ;;
        # loginctl lock-session;;
    *"Suspend")
        sudo systemctl suspend ;;
    *"Logout")
        hyprctl dispatch exit ;;
        # sudo loginctl terminate-user $USER;;
    *"Reboot")
        sudo systemctl reboot ;;
    *"Shutdown")
        sudo systemctl poweroff ;;
esac
