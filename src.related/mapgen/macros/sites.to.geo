#!/bin/sh
CHECK.MAP
if test $? = 1
	then
	exit
fi
proj=`grep proj: $LOCATION/WIND | awk '/proj:/ {print $2}'`
zone=`grep zone: $LOCATION/WIND | awk '/zone:/ {print $2}'`
PARMS=`cat .PARMS`
Gask old "" site_lists "" tmp
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
	cat $file | awk -F'|' '$1 != "name" && $1 != "desc" {print $1"	" $2"	" $3}' |proj $PARMS +inv - > tmp
else
	cat $file | awk -F'|' '$1 != "name" && $1 != "desc" {print $1"	" $2"	" $3}'  > tmp
fi

cat $file | awk -F'|' '$1 != "name" && $1 != "desc" {print $3}' | paste tmp - > ../sites_geo/$out
fi
rm tmp
tput clear
