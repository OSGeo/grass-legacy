#!/bin/sh

# g.parser demo script

#%Module
#%  description: g.parser test script
#%  keywords: keyword1, keyword2
#%End
#%flag
#%  key: f
#%  description: A flag
#%end
#%option
#% key: raster
#% type: string
#% gisprompt: old,cell,raster
#% description: Raster input map
#% required : yes
#%end
#%option
#% key: vector
#% type: string
#% gisprompt: old,vector,vector
#% description: Vector input map
#% required : yes
#%end
#%option
#% key: option1
#% type: string
#% description: An option
#% required : yes
#%end

if [ -z "$GISBASE" ] ; then
    echo "You must be in GRASS GIS to run this program." 1>&2
    exit 1
fi

if [ "$1" != "@ARGS_PARSED@" ] ; then
  exec "$GISBASE/bin/g.parser" "$0" "$@"
fi

#add your code here
echo ""
if [ "$GIS_FLAG_F" -eq 1 ] ; then
  echo "Flag -f set"
else
  echo "Flag -f not set"
fi

echo "Value of option1=: '$GIS_OPT_OPTION1'"
echo "Value of raster=: '$GIS_OPT_RASTER'"
echo "Value of vect=: '$GIS_OPT_VECTOR'"

#add your code here
