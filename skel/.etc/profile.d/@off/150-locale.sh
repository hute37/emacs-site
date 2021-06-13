#!/bin/sh
# -*- mode: shell-script;-*-

##
#  locale definition
#

env_locale() {

LC_COLLATE=C
LANG=en_GB.UTF-8
LC_ALL=en_GB.UTF-8

export LC_COLLATE
export LANG
export LC_ALL
    
}

#env_locale
