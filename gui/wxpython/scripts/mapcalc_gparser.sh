#!/bin/bash
#%Module
#%  description: r.mapcalc - Raster map layer data calculator
#%End
#%option
#% key: expression
#% type: string
#% description: mapcalc expression
#% required : yes
#%end
if   [ "$1" != "@ARGS_PARSED@" ]
then
	exec g.parser "$0" "$@"
fi
exec r.mapcalc "$GIS_OPT_EXPRESSION"

