#!/bin/sh
CHECK.MAP
if test $? = 1
        then
        exit
fi
tmpnm=`date '+%H%M%S'`
Gask old "" dig_cats "" tmp
. tmp
catfile=$file
attfile=`echo $file | sed -e s/dig_cats/dig_att/`
proj=`grep proj: $LOCATION/WIND | awk '/proj:/ {print $2}'`
zone=`grep zone: $LOCATION/WIND | awk '/zone:/ {print $2}'`
PARMS=`cat .PARMS`
if test "$attfile" != "" -a $catfile != ""
then
if test $# -eq 1; then
	out=$1
else
	out=`basename $catfile`
fi
n=`tail -1 $catfile | awk -F: '{print $1}'`
att_cat $n $catfile $attfile > /usr/tmp/$tmpnm
if test $proj = 1 -o $proj = 4
        then
        awk  '{print $2" " $3"   " $4}' /usr/tmp/$tmpnm | proj $PARMS +inv - > tmp
else
        awk  '{print $2" " $3"   " $4}'  /usr/tmp/$tmpnm > tmp
fi

awk '{print $4}' /usr/tmp/$tmpnm | paste tmp - > ../sites_geo/`basename $out`
fi
rm tmp /usr/tmp/$tmpnm
tput clear
