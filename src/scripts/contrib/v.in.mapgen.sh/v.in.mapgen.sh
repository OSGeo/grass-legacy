#!/bin/sh

# mapgen2grass.sh -- convert mapgen/MATLAB files 
#                    to grass ascii/binary vector files
# c) Andreas Lange, andreas.lange@rhein-main.de
# $Id$
# 

if test "$GISBASE" = ""; then
 echo "You must be in GRASS GIS to run this program." >&2
 exit 1
fi   
     
eval `g.gisenv`
: ${GISBASE?} ${GISDBASE?} ${LOCATION_NAME?} ${MAPSET?}
LOCATION=$GISDBASE/$LOCATION_NAME/$MAPSET


PROG=`basename $0`
VERSION="$PROG c) 2000 Andreas Lange, andreas.lange@rhein-main.de"
OPTS=""


#### check if we have awk
AWK=`which awk`

if [ "$AWK" = "" ] ; then
  echo "$PROG: awk required, please install awk/gawk first" 1>&2
  exit 1
fi


#### set temporary file
TMP=`g.tempfile pid=$$`


### check how we are called (matlab or mapgen) and shift format
FMT=0
TEST=`basename $0`
if [ `echo $TEST | grep "matlab"` ] ; then
    FMT=1
fi

#### process command line arguments 
OK=0 ; SUP=0 ; ASC=0
while [ $# -ge 1 ] ; do

    case "$1" in
    -h) OK=0
       ;;    
    h*) OK=0
       ;;
    n*) NAME=`echo "$1" | sed 's/.*=//g'` 1>&2 > /dev/null
	echo "vect=$NAME" 1>&2
       ;;
    i*) FILE=`echo "$1" | sed 's/.*=//g'` 1>&2 > /dev/null
	echo "input=$FILE" 1>&2
        OK=1
       ;;
    -v) echo $VERSION 1>&2
	echo " " 1>&2
       ;;
    -s) SUP=1
       ;;
    -f) FMT=1
       ;;
    -a) ASC=1
       ;;
    *)  OK=0
       ;;
    esac
    shift
done


#### print usage and exit
if [ $OK -ne 1 ] ; then
    echo "Description: " 1>&2
    echo " Import MATLAB/mapgen vector files into GRASS binary vector files." 1>&2
    echo " " 1>&2
    echo "Usage: " 1>&2
    echo " $PROG name=vect input=file [-v] [-s] [-f] [-a] [-h]" 1>&2
    echo " " 1>&2
    echo "Flags: " 1>&2
    echo "     -v       verbose output" 1>&2
    echo "     -s       run v.support on new binary vector file" 1>&2
    echo "     -f       use MATLAB format for input" 1>&2
    echo "     -a       leave ascii vector file in $LOCATION/dig_ascii" 1>&2
    echo "     -h       print this message" 1>&2
    echo " " 1>&2
    echo "Parameters: " 1>&2 
    echo " name=vect    name for new binary vector file" 1>&2
    echo " input=file   name for input file in mapgen/MATLAB format" 1>&2
    echo " " 1>&2
    echo $VERSION 1>&2
    rm -f $TMP
    exit 1
fi


#### create dig_ascii if required
if [ ! -d "$LOCATION/dig_ascii" ] ; then
  mkdir -p $LOCATION/dig_ascii
fi
DIG="$LOCATION/dig_ascii/DIG_$$"


#### trap ctrl-c so that we can clean up tmp
trap 'rm -f $TMP' 2 3 9


#### create ascii vector file
if [ $FMT -eq 1 ] ; then
  tac $FILE | $AWK 'BEGIN { FS="," ; R=0 } 
    $1~/\d*/   { printf("L %d\n", R) } 
    $1~/   .*/ { printf(" %lf %lf\n", $2, $1) ; ++R }
    $1~/END/   { }' | tac > $TMP
else
  tac $FILE | $AWK 'BEGIN { FS="\t" ; R=0 } 
    $1~/#.*/ { printf("L %d\n", R) } 
    $1~/\d*\.\d*/ { printf(" %lf %lf\n", $2, $1) ; ++R }' | tac > $TMP
fi


#### find boundaries for digit header
WEST=`cat $TMP  | grep -v "^L" | cut -d' ' -f3 | sort -b -n    | head -1`
EAST=`cat $TMP  | grep -v "^L" | cut -d' ' -f3 | sort -b -n -r | head -1`
NORTH=`cat $TMP | grep -v "^L" | cut -d' ' -f2 | sort -b -n -r | head -1`
SOUTH=`cat $TMP | grep -v "^L" | cut -d' ' -f2 | sort -b -n    | head -1`


#### create digit header
cat << EOF >> $DIG
ORGANIZATION: GRASSroots organization
DIGIT DATE:   `date +%D`
DIGIT NAME:   $PROG
MAP NAME:     $NAME
MAP DATE:     `date +%Y`
MAP SCALE:    1:100000
ZONE:         
WEST EDGE:    $WEST
EAST EDGE:    $EAST
SOUTH EDGE:   $SOUTH
NORTH EDGE:   $NORTH
MAP THRESH:   0
VERTI:
EOF


#### process points list to ascii vector file
cat $TMP >> $DIG


#### if no name for vector file given, cat to stdout
if [ "$NAME" = "" ] ; then
    echo "output to stdout" 1>&2
    cat $DIG 2>/dev/null
else 


    #### import to binary vector file 
    echo "importing with v.in.ascii" 1>&2
    v.in.ascii output=$NAME input=DIG_$$ $OPTS 1>&2 >/dev/null


    #### check success/failure 
    if [ $? -eq 0 ] ; then
	echo "$PROG: binary vector file \"$NAME@$MAPSET\" successfully created" 1>&2


	#### run v.support on file
	if [ $SUP -eq 1 ] ; then
	    echo "$PROG: running v.support on \"$NAME@$MAPSET\"" 1>&2
	    v.support map=$NAME option=build threshold=10
	    if [ $? -eq 0 ] ; then
		echo "$PROG: v.support successfully applied" 1>&2
	    else
		echo "$PROG: error with v.support, check manually" 1>&2
	    fi
	fi
    else
	echo "$PROG: An error occured on creating \"$NAME\" in mapset \"$MAPSET\"," 1>&2
	echo "please check!" 1>&2
    fi
fi


#### clean up the mess
rm -f $TMP 1>&2 > /dev/null
if [ $ASC -eq 1 ] ; then
  true
else
  rm -f $DIG 1>&2 > /dev/null
fi


#### end
exit 0 
