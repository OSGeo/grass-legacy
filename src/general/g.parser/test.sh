#!/bin/sh
#% Module
#%  description   : g.parser test script   
#% End
#% flag
#%  key: f
#%  description :a flag
#% END
#%option
#% key:option
#% type:string
#% description: an option
#%required : yes
#%end

if [ "$1" != "@ARGS_PARSED@" ] ; then
  exec $GISBASE/etc/bin/cmd/g.parser "$0" "$@"
fi

env

#add code here
echo ""
if [ $GIS_FLAG_f -eq 1 ] ; then
  echo "Flag -f set"
else
  echo "Flag -f not set"
fi

echo "Value of GIS_OPT_option: '$GIS_OPT_option:'"
