#!/bin/sh

if test "$GISBASE" = ""; then
 echo "You must be in GRASS GIS to run this program." >&2
 exit 1
fi   
     
eval `g.gisenv`
: ${GISBASE?} ${GISDBASE?} ${LOCATION_NAME?} ${MAPSET?}
LOCATION=$GISDBASE/$LOCATION_NAME/$MAPSET

vector=wdbtemp4
awk -F: ' {if (NF==2){print $2;}}' $LOCATION/dig_cats/$vector | sort -u
