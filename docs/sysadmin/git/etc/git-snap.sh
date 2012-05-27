#!/bin/bash
if [ ! "$(id -u)" = "0" ] ; then
 echo "$0 must be invoked by root"
 exit 1 
fi

cd /etc
git add .
git commit --allow-empty -a -m"manual snapshot: $*"
#echo "git commit (rc:$?)"
#exit 0

