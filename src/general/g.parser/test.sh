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
