#!/bin/sh
#% Module
#%  description: Display manager for GRASS
#% End
#%option
#% key: dmrc
#% type: string
#% description: Name of .dmrc settings file
#% required : no
#%End

if [ $# -eq 0 ] ; then
   exec "$GRASS_WISH" $GISBASE/etc/dm/d.m.tcl
   exit 0
fi

if [ "$1" != "@ARGS_PARSED@" ] ; then
  exec $GISBASE/etc/bin/cmd/g.parser "$0" "$@"
fi

exec "$GRASS_WISH" "$GISBASE/etc/dm/d.m.tcl" "$GIS_OPT_dmrc"
