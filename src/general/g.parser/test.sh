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
#% key: option1
#% type: string
#% description: an option
#% required : yes
#%end
#%option
#% key: optionB
#% type: string
#% description: another option
#% required : yes
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

echo "Value of GIS_OPT_option1: '$GIS_OPT_option1'"
echo "Value of GIS_OPT_optionB: '$GIS_OPT_optionB'"

#add your code here
