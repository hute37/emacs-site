#!/bin/sh
# -*- mode: shell-script;-*-

##
#  Haskell cabal environment
#

if [ -d ~/.cabal/bin ]; then
    PATH=$PATH:~/.cabal/bin
    export PATH
fi    
