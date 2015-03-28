#!/bin/bash

ssh -XC -c blowfish-cbc,arcfour 192.168.100.101

echo $DISPLAY

startx $(which xterm) -- /usr/bin/Xdmx :5 +xinerama -display :0.0 -display localhost:10.0 -norender -noglxproxy 

/usr/bin/Xdmx :5 +xinerama -display :0.0 -display localhost:10.0 -norender -noglxproxy -ignorebadfontpaths -ac &

