#!/bin/sh
#% Module
#%  description: Display manager for GRASS
#% End

if [ $# -eq 0 ]
then
 sh $GISBASE/etc/dm/d.m.tcl
 exit
fi

if [ "$1" != "@ARGS_PARSED@" ] ; then
  exec $GISBASE/etc/bin/cmd/g.parser "$0" "$@"
fi
