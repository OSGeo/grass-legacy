:

eval `g.gisenv`

WEIGHT=${GISDBASE?}/${LOCATION_NAME?}/${MAPSET?}/WEIGHT
if test -d $WEIGHT
then
        echo ""
else
        mkdir $WEIGHT
fi
cd $WEIGHT

exec $GISBASE/etc/r.weight
