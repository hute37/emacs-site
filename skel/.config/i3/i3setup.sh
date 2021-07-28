#!/bin/sh


: ${I3_CONFIG_DIR:="$HOME/.config/i3"}

export I3_CONFIG_DIR

echo "I3_CONFIG_DIR=$I3_CONFIG_DIR"

[ -d "$I3_CONFIG_DIR" ] || { exit 1; }



install_addons() {

    mkdir -p $I3_CONFIG_DIR/lib
    cd       $I3_CONFIG_DIR/lib


    [ -d i3blocks-contrib ] || git clone "https://github.com/vivien/i3blocks-contrib.git"
    ( cd i3blocks-contrib; git pull )
    ( cd i3blocks-contrib; ./configure && make && make install )

}

main() {

    install_addons $@

}

main $@
