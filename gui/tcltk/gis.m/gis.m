#!/bin/sh
#% Module
#%  description: Display manager for GRASS
#% End
#%option
#% key: dmrc
#% type: string
#% description: Name of GRASS settings file (.grc)
#% required : no
#%End

if [ $# -eq 0 ] ; then
   exec "$GRASS_WISH" $GISBASE/etc/gm/gm.tcl -name gm_tcl
   exit 0
fi

if [ "$1" != "@ARGS_PARSED@" ] ; then
  exec g.parser "$0" "$@"
fi

exec "$GRASS_WISH" "$GISBASE/etc/gm/gm.tcl" -name gm_tcl "$GIS_OPT_grc" sh &
