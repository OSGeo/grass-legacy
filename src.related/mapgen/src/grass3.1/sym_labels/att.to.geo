#!/bin/sh
CHECK.MAP
if test $? = 1
	then
	exit
fi
proj=`grep proj: $LOCATION/WIND | awk '/proj:/ {print $2}'`
zone=`grep zone: $LOCATION/WIND | awk '/zone:/ {print $2}'`
PARMS=`cat .PARMS`
Gask old "" dig_att "" tmp
. tmp
if test "$file" != ""
then
if test $# -eq 1; then
	out=$1
else
	out=`basename $file`
fi
if test $proj = 1 -o $proj = 4
	then
	grep A $file | awk  '{print $2"	" $3"	" $4}' |proj $PARMS +inv - > tmp
else
	grep A $file | awk  '{print $2"	" $3"	" $4}'  > tmp
fi

grep A $file | awk '{print $4}' | paste tmp - > ../sites_geo/$out
fi
rm tmp
tput clear
