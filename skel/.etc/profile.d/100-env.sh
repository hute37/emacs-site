#!/bin/sh
# -*- mode: shell-script;-*-

##
#  environment
#

# set PATH so it includes user's private bin if it exists
if [ -d ~/bin ] ; then
    PATH=~/bin:"${PATH}"
fi


# Preferred editor for local and remote sessions
 if [ -n $SSH_CONNECTION ]; then
   EDITOR='vim'
 else
   # export EDITOR='mvim'
   EDITOR='vim'
 fi

LESS="-i -j.49 -M -R -S -z-2"
PAGER=less

XZ_OPT=-9 

export PATH
export EDITOR
export LESS
export PAGER
export XZ_OPT





