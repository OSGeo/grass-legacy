:
eval `g.gisenv`

COMBINE=${GISDBASE?}/${LOCATION_NAME?}/${MAPSET?}/COMBINE

if test -d ${COMBINE}
then
        echo ""
else
        mkdir $COMBINE
fi
cd $COMBINE

exec $GISBASE/etc/r.combine $*
