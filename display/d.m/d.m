#!/bin/sh
#% Module
#%  description: Display manager for GRASS
#% End

if [ $# -eq 0 ]
then
 sh $GISBASE/etc/dm/d.m.tcl
 exit
fi

if [ $# -eq 1 ]
then
 flags=`echo $1 | awk '/^-/ { print $1 }'`
 if [ "$flags" != "" ] ; then
   exec $GISBASE/etc/bin/cmd/g.parser "$0" "$1"
 fi
 if [ -f `pwd`/$1 ] || [ -f $1 ]; then 
   sh $GISBASE/etc/dm/d.m.tcl $1; else
   echo 'File "'$1'" not found - starting blank'
   sh $GISBASE/etc/dm/d.m.tcl
 fi
fi
