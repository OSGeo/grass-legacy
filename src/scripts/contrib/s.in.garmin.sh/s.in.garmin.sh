#!/bin/bash

# s.in.garmin.sh -- import gps data from garmin receiver 
#                   into GRASS sites file
# 
# c) Andreas Lange, andreas.lange@rhein-main.de
# $Id$
# 
# requirements: GRASS 4.x or GRASS 5.0 with s.in.ascii
#               gpstrans from Carsten Tschach et al. 
#                 get gpstrans from: ftp://www.mayko.com/pub/gpstrans or from
#                 http://www.metalab.unc.edu/pub/Linux/science/cartography/
#               GNU bourne again shell
#   unix tools: grep, cat, cut, paste, sed
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


#### check that GRASS variables are set
eval `g.gisenv`

: $GISDBASE $LOCATION $MAPSET


#### set temporary files
TMP=/tmp/tmp__$$
# better:
# TMP1=`g.tempfile pid=$$`


#### trap ctrl-c so that we can clean up tmp
trap 'rm -f ${TMP}*' 2 3 9


#### process command line arguments 
WPT=0 ; RTE=0 ; TRK=0 ; OK=0 ; SWP=0
while [ $# -ge 1 ] ; do

    case "$1" in
    -h) OK=0
       ;;    
    h*) OK=0
       ;;
    n*) NAME=`echo "$1" | sed 's/.*=//g'` 1>&2 > /dev/null
	echo "sites=$NAME" 1>&2
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
    *)  OK=0
       ;;
    esac
    shift
done


#### print usage and exit
if [ $OK -ne 1 ] ; then
    echo "Description: " 1>&2
    echo " Upload Waypoints, Routes and Tracks from garmin gps reciever into" 1>&2
    echo " GRASS sites file." 1>&2
    echo " " 1>&2
    echo "Usage: " 1>&2
    echo " $PROG name=sites port=/dev/gps [-v] [-w] [-r] [-t] [-h]" 1>&2
    echo " " 1>&2
    echo "Flags: " 1>&2
    echo "   -v      verbose output" 1>&2
    echo "   -w      upload Waypoints" 1>&2
    echo "   -r      upload Routes" 1>&2
    echo "   -t      upload Track" 1>&2
#   echo "   -s      swap easting/northing (for tmerc projection)" 1>&2
    echo "   -h      print this message" 1>&2
    echo " " 1>&2
    echo "Parameters: " 1>&2
    echo "   name=sites     name for new sites file" 1>&2
    echo "   port=/dev/gps  port garmin receiver is connected to" 1>&2
    echo " " 1>&2
    echo $VERSION 1>&2
    rm -f ${TMP}*
    exit 0
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
    $GPSTRANS "$GPSPORT" -dw >> $TMP 2>/dev/null
    if [ $? -ne 0 ] ; then
	echo "$PROG: Error uploading Waypoints" 1>&2
	rm -f ${TMP}*
	exit 1
    fi
fi

if [ $RTE -eq 1 ] ; then
    echo "Uploading Routes" 1>&2
    $GPSTRANS "$GPSPORT" -dr >> $TMP 2>/dev/null
    if [ $? -ne 0 ] ; then
	echo "$PROG: Error uploading Routes" 1>&2
	rm -f ${TMP}*
	exit 1
    fi
fi

if [ $TRK -eq 1 ] ; then
    echo "Uploading Tracks" 1>&2
    $GPSTRANS "$GPSPORT" -dt >> $TMP 2>/dev/null 
    if [ $? -ne 0 ] ; then
	echo "$PROG: Error uploading Tracks" 1>&2
	rm -f ${TMP}*
	exit 1
    fi
fi


#### swap easting and northing if necessary
if [ $SWP -eq 1 ] ; then
  WPTS="${TMP}_WPN ${TMP}_WPE"
else
  WPTS="${TMP}_WPE ${TMP}_WPN"
fi


#### check which projection we are working with
PROJ="`head -1 $TMP | sed -e 's/Format: //' | sed -e 's/  UTC.*//'`"
# echo ${PROJ}_


#### process waypoint format
case "$PROJ" in 
    UTM)
	cat $TMP | grep "^W" | tr -s '[:space:]' | cut -f8     >> ${TMP}_WPN
	cat $TMP | grep "^W" | tr -s '[:space:]' | cut -f7     >> ${TMP}_WPE
	cat $TMP | grep "^W" | tr -s '[:space:]' | cut -f2,3,4 >> ${TMP}_WN
	;;
    DDD)
        cat $TMP | grep "^W" | tr -s '[:space:]' | cut -f5     >> ${TMP}_WPN
	cat $TMP | grep "^W" | tr -s '[:space:]' | cut -f6     >> ${TMP}_WPE
	cat $TMP | grep "^W" | tr -s '[:space:]' | cut -f2,3,4 >> ${TMP}_WN
	;;
    GKK)
        cat $TMP | grep "^W" | tr -s '[:space:]' | cut -f6     >> ${TMP}_WPN
	cat $TMP | grep "^W" | tr -s '[:space:]' | cut -f5     >> ${TMP}_WPE
	cat $TMP | grep "^W" | tr -s '[:space:]' | cut -f2,3,4 >> ${TMP}_WN
	;;
    *)
	echo "Unsupported format" 1>&2
	exit 1
	;;
esac


#### process track format 
case "$PROJ" in
    UTM)
	cat $TMP | grep "^T" | tr -s '[:space:]' | cut -f6 >> ${TMP}_WPN
	cat $TMP | grep "^T" | tr -s '[:space:]' | cut -f5 >> ${TMP}_WPE
	cat $TMP | grep "^T" | tr -s '[:space:]' | cut -f2 >> ${TMP}_WN
	;;
    DDD)
	cat $TMP | grep "^T" | tr -s '[:space:]' | cut -f3 >> ${TMP}_WPN
	cat $TMP | grep "^T" | tr -s '[:space:]' | cut -f4 >> ${TMP}_WPE
	cat $TMP | grep "^T" | tr -s '[:space:]' | cut -f2 >> ${TMP}_WN
	;;
    GKK)
        cat $TMP | grep "^T" | tr -s '[:space:]' | cut -f4 >> ${TMP}_WPN
	cat $TMP | grep "^T" | tr -s '[:space:]' | cut -f3 >> ${TMP}_WPE
	cat $TMP | grep "^T" | tr -s '[:space:]' | cut -f2 >> ${TMP}_WN
	;;
    *)
	echo "Unsupported format" 1>&2
	exit 1
	;;
esac


#### paste together import file
paste -d"\t$DELIM" $WPTS ${TMP}_WN | sed "s/$DELIM/ $DELIM/g" >> ${TMP}_P


#### if no name for sites file given, cat to stdout
if [ "$NAME" = "" ] ; then
    echo "output to stdout" 1>&2
    cat ${TMP}_P 2>/dev/null
else 


    #### import to sites file 
    echo "importing with s.in.ascii" 1>&2
    s.in.ascii sites=$NAME input=${TMP}_P $OPTS 1>&2 >/dev/null
    # fs=tab


    #### check success/failure 
    if [ $? -eq 0 ] ; then
	echo "$PROG: sites file \"$NAME@$MAPSET\" successfully created" 1>&2
    else
	echo "$PROG: An error occured on creating \"$NAME\" in mapset \"$MAPSET\"," 1>&2
	echo "please check!" 1>&2
    fi
fi


#### clean up the mess
rm -f ${TMP}* 1>&2 > /dev/null


#### end
exit 0