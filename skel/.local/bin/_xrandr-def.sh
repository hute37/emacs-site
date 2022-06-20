#!/bin/bash

case "$(hostname)" in
    heka)
        xrandr --output VGA-1-1 --primary --output LVDS-1-1 --right-of VGA-1-1
        ;;
    eos)
        xrandr --output Virtual-1 --mode 1360x768
        ;;
    *)
        xrandr --output Virtual-1 --mode 1360x768
        ;;
esac
