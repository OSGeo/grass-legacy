#!/bin/sh

eval `gisenv`

if test -d ${GISDBASE?}/${LOCATION_NAME?}/${MAPSET?}/COMBINE
then
        echo ""
else
        mkdir $GISDBASE/$LOCATION_NAME/$MAPSET/COMBINE
fi
cd $GISDBASE/$LOCATION_NAME/$MAPSET/COMBINE

exec $GISBASE/etc/combinate
