#!/bin/sh

#g.parser demo script

#%Module
#%  description: g.parser test script   
#%End
#%flag
#%  key: f
#%  description: a flag
#%END
#%option
#% key: raster
#% type: string
#% gisprompt: old,cell,raster
#% description: raster input map
#% required : yes
#%end
#%option
#% key: vector
#% type: string
#% gisprompt: old,dig,vector
#% description: vector input map
#% required : yes
#%end
#%option
#% key: option1
#% type: string
#% description: an option
#% required : no
#%end

if [ "$1" != "@ARGS_PARSED@" ] ; then
  exec $GISBASE/etc/bin/cmd/g.parser "$0" "$@"
fi

#add your code here
echo ""
if [ $GIS_FLAG_f -eq 1 ] ; then
  echo "Flag -f set"
else
  echo "Flag -f not set"
fi

#test if parameter present:
if [ "$GIS_OPT_option1" != "(null)" ] ; then
   echo "Value of GIS_OPT_option1: '$GIS_OPT_option1'"
fi

echo "Value of GIS_OPT_raster: '$GIS_OPT_raster'"
echo "Value of GIS_OPT_vect: '$GIS_OPT_vector'"

#add more code here
