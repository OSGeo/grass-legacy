#!/bin/bash

# v.in.garmin.sh -- import gps data from garmin receiver 
#                   into GRASS binary vector file
# 
# c) Andreas Lange, andreas.lange@rhein-main.de
# $Id$
# 
# requirements: GRASS 4.x or GRASS 5.0 with v.in.ascii
#               gpstrans from Carsten Tschach et al. 
#                 get gpstrans from: ftp://www.mayko.com/pub/gpstrans or from
#                 http://www.metalab.unc.edu/pub/Linux/science/cartography/
#               GNU bourne again shell
#   unix tools: grep, cat, tac, cut, paste, awk/nawk/gawk, sed
#

PROG=`basename $0`
VERSION="$PROG c) 2000 Andreas Lange, andreas.lange@rhein-main.de"
GRASS4="#"
GRASS5="@"
DELIM=$GRASS5
OPTS=""


#### test if GRASS is running
if [ "$GISRC" = "" ] ; then
    echo "$PROG: You must be running GRASS to execute $PROG" 1>&2
    exit 1
fi


#### check for gpstrans 
GPSTRANS=`which gpstrans`

if [ "$GPSTRANS" = "" ] ; then
    echo "$PROG: gpstrans program not found, install it first" 1>&2
    exit 1
fi


#### check if we have awk
AWK=`which awk`

if [ "$AWK" = "" ] ; then
    echo "$PROG: awk required, please install awk/gawk first" 1>&2
    exit 1
fi


#### check that GRASS variables are set
eval `g.gisenv`

: $GISDBASE $LOCATION $MAPSET


#### set temporary files
TMP=/tmp/tmp__$$
if [ ! -d "$LOCATION/dig_ascii" ] ; then 
    mkdir -p $LOCATION/dig_ascii
fi
DIG="$LOCATION/dig_ascii/DIG_$$"


#### trap ctrl-c so that we can clean up tmp
trap 'rm -f ${TMP}*' 2 3 9


#### process command line arguments 
WPT=0 ; RTE=0 ; TRK=0 ; OK=0 ; SWP=0 ; SUP=0
while [ $# -ge 1 ] ; do

    case "$1" in
    -h) OK=0
       ;; 
    h*) OK=0
       ;;
    n*) NAME=`echo "$1" | sed 's/.*=//g'` 1>&2 > /dev/null
	echo "vect=$NAME" 1>&2
       ;;
    p*) GPSPORT=`echo "$1" | sed 's/.*=//g'` 1>&2 > /dev/null
	echo "port=$GPSPORT" 1>&2
	GPSPORT="-p$GPSPORT"
       ;;
    -v) echo $VERSION 1>&2
	echo " " 1>&2
       ;;
    -w) WPT=1 ; OK=1
       ;;
    -r) RTE=1 ; OK=1
       ;;
    -t) TRK=1 ; OK=1
       ;;
    -s) SWP=1
       ;;
    -u) SUP=1
	;;
    *)  OK=0
       ;;
    esac
    shift
done


#### print usage and exit
if [ $OK -ne 1 ] ; then
    echo "Description: " 1>&2
    echo " Upload Waypoints, Routes and Tracks from a garmin gps reciever into" 1>&2
    echo " GRASS binary vector file." 1>&2
    echo " " 1>&2
    echo "Usage: " 1>&2
    echo " $PROG name=vect port=/dev/gps [-v] [-w] [-r] [-t] [-u] [-h]" 1>&2
    echo " " 1>&2
    echo "Flags: " 1>&2
    echo "     -v        verbose output" 1>&2
    echo "     -w        upload Waypoints" 1>&2
    echo "     -r        upload Routes" 1>&2
    echo "     -t        upload Track" 1>&2
#   echo "     -s        swap easting/northing (for tmerc projection)" 1>&2
    echo "     -u        run v.support on new binary vector file" 1>&2
    echo "     -h        print this message" 1>&2
    echo " " 1>&2
    echo "Parameters: " 1>&2
    echo " name=vect     name for new binary vector file" 1>&2
    echo " port=/dev/gps port garmin receiver is connected to" 1>&2
    echo " " 1>&2
    echo $VERSION 1>&2
    rm -f ${TMP}*
    exit 1
fi


#### check that receiver is responding on $GPSPORT
$GPSTRANS "$GPSPORT" -i 1> /dev/null
if [ $? -ne 0 ] ; then
    echo "$PROG: Receiver on $GPSPORT not responding, exiting" 1>&2
    rm -f ${TMP}*
    exit 1
fi


#### receive and pre-process data
if [ $WPT -eq 1 ] ; then
    echo "Uploading Waypoints" 1>&2
    $GPSTRANS "$GPSPORT" -dw > $TMP 2>/dev/null
    if [ $? -ne 0 ] ; then
	echo "$PROG: Error uploading Waypoints" 1>&2
	rm -f ${TMP}*
	exit 1
    fi


    #### check which projection we are working with
    PROJ="`head -1 $TMP | sed -e 's/Format: //' | sed -e 's/  UTC.*//'`"
    # echo ${PROJ}_

    case "$PROJ" in
	UTM)
	    tac $TMP | $AWK 'BEGIN { FS="\t" ; R=0 } $1=="W" { printf(" %lf %lf\n", $8, $7) ; ++R } END { printf("L %d\n", R) }' | tac >> ${TMP}_P
	    ;;
	DDD)
	    tac $TMP | $AWK 'BEGIN { FS="\t" ; R=0 } $1=="W" { printf(" %lf %lf\n", $5, $6) ; ++R } END { printf("L %d\n", R) }' | tac >> ${TMP}_P
	    ;;
	GKK)
	    tac $TMP | $AWK 'BEGIN { FS="\t" ; R=0 } $1=="W" { printf(" %lf %lf\n", $6, $5) ; ++R } END { printf("L %d\n", R) }' | tac >> ${TMP}_P
	    ;;
	*)
	    echo "Unsupported format" 1>&2
	    exit 1
	    ;;
    esac
fi


if [ $RTE -eq 1 ] ; then
    echo "Uploading Routes" 1>&2
    $GPSTRANS "$GPSPORT" -dr > $TMP 2>/dev/null
    if [ $? -ne 0 ] ; then
	echo "$PROG: Error uploading Routes" 1>&2
	rm -f ${TMP}*
	exit 1
    fi


    #### check which projection we are working with
    PROJ="`head -1 $TMP | sed -e 's/Format: //' | sed -e 's/  UTC.*//'`"
    # echo ${PROJ}_

    case "$PROJ" in
	UTM)
	    tac $TMP | $AWK 'BEGIN { FS="\t" ; R=0 } $1=="W" { printf(" %lf %lf\n", $8, $7) ; R=R+1 } $1=="R" { printf("L %u\n", R) ; R=0 }' | tac >> ${TMP}_P
	    ;;
	DDD)
	    tac $TMP | $AWK 'BEGIN { FS="\t" ; R=0 } $1=="W" { printf(" %lf %lf\n", $5, $6) ; R=R+1 } $1=="R" { printf("L %d\n", R) ; R=0 }' | tac >> ${TMP}_P
	    ;;
	GKK)
	    tac $TMP | $AWK 'BEGIN { FS="\t" ; R=0 } $1=="W" { printf(" %lf %lf\n", $6, $5) ; R=R+1 } $1=="R" { printf("L %u\n", R) ; R=0 }' | tac >> ${TMP}_P
	    ;;
	*)
	    echo "Unsupported format" 1>&2
	    exit 1
	    ;;
    esac
fi


if [ $TRK -eq 1 ] ; then
    echo "Uploading Tracks" 1>&2
    $GPSTRANS "$GPSPORT" -dt > $TMP 2>/dev/null 
    if [ $? -ne 0 ] ; then
	echo "$PROG: Error uploading Tracks" 1>&2
	rm -f ${TMP}*
	exit 1
    fi


    #### check which projection we are working with
    PROJ="`head -1 $TMP | sed -e 's/Format: //' | sed -e 's/  UTC.*//'`"
    # echo ${PROJ}_

    case "$PROJ" in
	UTM)
	    tac $TMP | $AWK 'BEGIN { FS="\t" ; R=0 } $1=="T" { printf(" %lf %lf\n", $5, $6) ; ++R } END { printf("L %d\n", R) }' | tac >> ${TMP}_P
	    ;;
	DDD)
	    tac $TMP | $AWK 'BEGIN { FS="\t" ; R=0 } $1=="T" { printf(" %lf %lf\n", $3, $4) ; ++R } END { printf("L %d\n", R) }' | tac >> ${TMP}_P
	    ;;
	GKK)
	    tac $TMP | $AWK 'BEGIN { FS="\t" ; R=0 } $1=="T" { printf(" %lf %lf\n", $4, $3) ; ++R } END { printf("L %d\n", R) }' | tac >> ${TMP}_P
	    ;;
	*)
	    echo "Unsupported format" 1>&2
	    exit 1
	    ;;
    esac
fi


#### find boundaries for digit header
WEST=`cat ${TMP}_P  | grep -v "^L" | cut -d' ' -f3 | sort -b -n    | head -1`
EAST=`cat ${TMP}_P  | grep -v "^L" | cut -d' ' -f3 | sort -b -n -r | head -1`
NORTH=`cat ${TMP}_P | grep -v "^L" | cut -d' ' -f2 | sort -b -n -r | head -1`
SOUTH=`cat ${TMP}_P | grep -v "^L" | cut -d' ' -f2 | sort -b -n    | head -1`


#### create digit header
cat << EOF >> $DIG
ORGANIZATION: GRASSroots organization
DIGIT DATE:   `date +%D`
DIGIT NAME:   $PROG
MAP NAME:     $NAME
MAP DATE:     `date +%Y`
MAP SCALE:    1:1000
ZONE:         
WEST EDGE:    $WEST
EAST EDGE:    $EAST
SOUTH EDGE:   $SOUTH
NORTH EDGE:   $NORTH
MAP THRESH:   0
VERTI:
EOF


#### process points list to ascii vector file
cat ${TMP}_P  >> $DIG
 

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
rm -f ${TMP}* 1>&2 > /dev/null
rm -f $DIG    1>&2 > /dev/null


#### end
exit 0