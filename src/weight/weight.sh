#!/bin/sh

eval `gisenv`

if test -d ${GISDBASE?}/${LOCATION_NAME?}/${MAPSET?}/WEIGHT
then
        echo ""
else
        mkdir $GISDBASE/$LOCATION_NAME/$MAPSET/WEIGHT
fi
cd $GISDBASE/$LOCATION_NAME/$MAPSET/WEIGHT

exec $GISBASE/etc/weighted
