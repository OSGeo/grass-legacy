#!/bin/sh

#@(#)vect_t_cel.sh	2.1   6/26/87
#
# Converts (binary) dig file to grid cell data layer
#
# 1 - Prompts user for dig file for conversion
#
# 2 - Runs entire process in background
#
# 3 - Sends user mail upon completion

#eval `sed '1,$s/^\([A-Za-z_]*\): *\(.*\)/\1="\2"; /' $HOME/.grassrc`
eval `gisenv`

BIN=$GISBASE/bin
ETC=$GISBASE/etc
MAPDIR=$GISDBASE/$LOCATION_NAME/$MAPSET
CELLDIR=$MAPDIR/cell



# Check for existence of directory which holds cell files
if [ ! -d $LOCATION/cell ]
then
	mkdir $LOCATION/cell
fi



name=
if [ $# = 1 ]
then
	MAP=$1
else
	clear
	echo "VECTOR to GRID-CELL conversion routine"
	echo ""

	Gask mapset "Enter name of file to be converted:" \
	    dig Digit /tmp/vtoc.$$

	# source the name file to get name, mapset into variables
	. /tmp/vtoc.$$
	rm -f /tmp/vtoc.$$

	if [ "$mapset" = "" ]
	then
		exit
	fi

	MAP=$name
fi



echo ""
echo "Making cell  file $MAP "

$ETC/vect.to.cell $MAP 

#  setting random colors (creates color file)

 echo ""
 echo "Building color table"
 Gcell.colors  $MAP random
 echo "Done."

