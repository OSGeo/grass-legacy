#!/bin/sh

: ${GISBASE?}

if [ $# != 1 ]
then
    echo Usage: `basename $0` mapname
    exit
fi

# get gis variables 
eval `gisenv`

# find the cell file

eval `Gfindfile cell "$1"`

if [ "$mapset" = "" ]
then
	echo cell file "[$1]" not found
	exit 1
fi

Dclear.screen
if [ $? != 0 ]
then
	exit 1
fi
Dcolormode fixed

# provide a label

Dnew loc 90 100 0 60
Dchoose loc
(echo .S 80; cat $GISDBASE/$LOCATION_NAME/PERMANENT/MYNAME) | Dtext

Dnew name 0 10 0 60
Dchoose name
(echo .S 80; echo $1) | Dtext

# draw the cellfile

Dnew pic 10 90 0 60
Dchoose pic
Dcell $1
Dscale

eval `Gfindfile cats "$1"`
if [ "$file" != "" ]
then
	len=`wc -l $file | sed 's/ *//' | sed 's/ .*//'`
	bottom=`expr 2 \* $len`
	bottom=`expr 100 - $bottom`
	test_for_neg=`expr $bottom \< 0`
	if [ $test_for_neg = 1 ]
	then
		bottom=1;
	fi
	Dnew leg $bottom 100 60 100
	Dchoose leg
	Dlegend $1
fi
