#!/bin/sh

if test "$GISBASE" = ""; then
 echo "You must be in GRASS GIS to run this program." >&2
 exit 1
fi   
     
eval `g.gisenv`
: ${GISBASE?} ${GISDBASE?} ${LOCATION_NAME?} ${MAPSET?}
LOCATION=$GISDBASE/$LOCATION_NAME/$MAPSET

if [ $# -lt 1 ]
then
    echo " "
    echo "USAGE: $0 Tiger.type1"
    echo "  Where Tiger.type1 is path name to Tiger Type 1 Data File"
    exit 2
fi

input1=$1

if [ -f $input1 ]
then
    continue
else
    echo " "
    echo "Cannot locate input file $input1"
    exit 3
fi

ETC=$GISBASE/etc/tiger_support

if [ -f $ETC/CFCC.Master ]
then
    continue
else
    echo " "
    echo "ERROR: Classification Code support file not found"
    exit 4
fi

TMP1=`g.tempfile pid=$$`
TMP2=`g.tempfile pid=$$`

$GISBASE/etc/tig.linefeed < $input1 > $TMP1

case $? in
    0) name1=$TMP1;;
    1) name1=$input1;;
    *)  echo "Error in checking for linefeeds in input data file"
        rm -f $TMP1
        rm -f $TMP2
        exit 2
esac

echo "Identified the following tract numbers:"
echo " "

cat $name1 | colrm 1 170 | colrm 5 > $TMP2
cat $name1 | colrm 1 176 | colrm 5 >> $TMP2
cat $TMP2 | sort -u

echo " "
echo "Found the following Classification Feature Codes:"
echo " "

cat $name1 | colrm 1 55 | colrm 4 | sort -u > $TMP2

for i in `cat $TMP2`
do
	grep "^    $i" $ETC/CFCC.Master
done

rm $TMP1 $TMP2
