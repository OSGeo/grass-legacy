#!/bin/sh
CHECK.MAP
if test $? = 1
	then
	exit
fi

Gask old "" dig "" tmp
. tmp
if test "$name" = ""; then
	exit
fi
rm tmp
if test $# -eq 1; then
	out=$1;
else
	out=$name
fi

if test ! -f $LOCATION/WIND; then
	echo Error no current window file
	exit
fi
proj=`grep proj: $LOCATION/WIND | awk '/proj:/ {print $2}'`
zone=`grep zone: $LOCATION/WIND | awk '/zone:/ {print $2}'`
PARMS=`cat .PARMS`
if test $proj = 1 -o $proj = 4
	then
	dig_to_geo $mapset $name |\
	proj $PARMS +inv -  > ../dig_geo/$out
else 
	echo "Current window not UTM or AEA so assuming LAT/LONG"
	dig_to_geo $mapset $name > ../dig_geo/$out
fi
tput clear
