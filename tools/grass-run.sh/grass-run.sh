#!/bin/sh

#script to launch GRASS commands (workaround for systems
#with xterm is setuid/setgid

#http://grass.itc.it/pipermail/grass5/2004-September/015409.html

LD_LIBRARY_PATH="$GISBASE/lib"
export LD_LIBRARY_PATH
exec "$@"

