#!/bin/sh

# v.in.garmin.sh -- import gps data from garmin receiver 
#                   into GRASS binary vector file
#
# $Id$
# 
# c) Andreas Lange, andreas.lange@rhein-main.de
#    Updates 2003-2004  Hamish Bowman
# 
# requirements: GRASS 5.3 with v.in.ascii
#      -  gpstrans from Carsten Tschach et al. 
#           get gpstrans from one of:
#             http://gpstrans.sourceforge.net
#             http://www.metalab.unc.edu/pub/Linux/science/cartography/
#             ftp://www.mayko.com/pub/gpstrans
#      -  bourne shell
#      -  unix tools: grep, cat, tac, cut, paste, awk/nawk/gawk, sed
#

#%Module
#%  description: Upload Waypoints, Routes and Tracks from a Garmin GPS reciever into a binary vector.
#%End
#%flag
#%  key: v
#%  description: verbose mode (version info)
#%end
#%flag
#%  key: w
#%  description: upload Waypoints
#%end
#%flag
#%  key: r
#%  description: upload Routes
#%end
#%flag
#%  key: t
#%  description: upload Track
#%end
##%flag
##%  key: s
##%  description: swap easting/northing (for tmerc projection)
##%end
#%flag
#%  key: u
#%  description: run v.support on new binary vector file
#%end
#%flag
#%  key: k
#%  description: do not attempt projection transform from WGS84
#%end
#%option
#% key: vect
#% type: string
#% gisprompt: new,dig,vector
#% description: name for new binary vector file (omit for display to stdout)
#% required : no
#%end
#%option
#% key: port
#% type: string
#% description: port Garmin receiver is connected to
#% answer: /dev/gps
#% required : no
#%end

if [ "$1" != "@ARGS_PARSED@" ] ; then
  exec $GISBASE/etc/bin/cmd/g.parser "$0" "$@"
fi


if test "$GISBASE" = ""; then
 echo "You must be in GRASS GIS to run this program." >&2
 exit 1
fi

eval `g.gisenv`
: ${GISBASE?} ${GISDBASE?} ${LOCATION_NAME?} ${MAPSET?}
LOCATION="$GISDBASE"/"$LOCATION_NAME"/"$MAPSET"

PROG=`basename $0`
VERSION="$PROG c) 2000 Andreas Lange, andreas.lange@rhein-main.de"
GRASS4="#"
GRASS5="@"
DELIM=$GRASS5
OPTS=""


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


#### set temporary files
TMP=/tmp/tmp__$$
if [ ! -d "$LOCATION/dig_ascii" ] ; then 
    mkdir -p $LOCATION/dig_ascii
fi
DIG="$LOCATION/dig_ascii/DIG_$$"


#### trap ctrl-c so that we can clean up tmp
trap 'rm -f ${TMP}*' 2 3 15


#### process command line arguments 
WPT=0 ; RTE=0 ; TRK=0 ; SWP=0 ; SUP=0 ; KEEP_WGS84=0

if [ "$GIS_OPT_vect" != "(null)" ] ; then
    NAME="$GIS_OPT_vect"
    echo "vect=$NAME" 1>&2
fi
if [ "$GIS_OPT_port" != "(null)" ] ; then
    GPSPORT="-p$GIS_OPT_port"
    echo "port=$GIS_OPT_port" 1>&2
fi
if [ $GIS_FLAG_v -eq 1 ] ; then
    echo $VERSION 1>&2
    echo " " 1>&2
fi
if [ $GIS_FLAG_w -eq 1 ] ; then
    WPT=1
fi
if [ $GIS_FLAG_r -eq 1 ] ; then
    RTE=1
fi
if [ $GIS_FLAG_t -eq 1 ] ; then
    TRK=1
fi
#if [ $GIS_FLAG_s -eq 1 ] ; then
#    SWP=1
#fi
if [ $GIS_FLAG_u -eq 1 ] ; then
    SUP=1
fi
if [ $GIS_FLAG_k -eq 1 ] ; then
    KEEP_WGS84=1
fi



#### check that receiver is responding on $GPSPORT
# sadly gpstrans 0.39 returns 0 after timeout.. hopefully fixed someday.
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
    IS_WGS84="`head -1 $TMP | grep 'WGS 84'`"

    case "$PROJ" in
	UTM)
	    tac $TMP | $AWK 'BEGIN { FS="\t" ; R=0 } $1=="W" { printf(" %lf %lf\n", $8, $7) ; ++R } END { printf("L %d\n", R) }' | tac >> ${TMP}_P
	    ;;
	DDD)
	    tac $TMP | $AWK 'BEGIN { FS="\t" ; R=0 } $1=="W" { printf(" %lf %lf\n", $5, $6) ; ++R } END { printf("L %d\n", R) }' | tac >> ${TMP}_P_raw
	    ;;
	GKK)
	    tac $TMP | $AWK 'BEGIN { FS="\t" ; R=0 } $1=="W" { printf(" %lf %lf\n", $6, $5) ; ++R } END { printf("L %d\n", R) }' | tac >> ${TMP}_P
	    ;;
	*)
	    echo "Unsupported format [$PROJ]" 1>&2
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
    IS_WGS84="`head -1 $TMP | grep 'WGS 84'`"

    case "$PROJ" in
	UTM)
	    tac $TMP | $AWK 'BEGIN { FS="\t" ; R=0 } $1=="W" { printf(" %lf %lf\n", $8, $7) ; R=R+1 } $1=="R" { printf("L %u\n", R) ; R=0 }' | tac >> ${TMP}_P
	    ;;
	DDD)
	    tac $TMP | $AWK 'BEGIN { FS="\t" ; R=0 } $1=="W" { printf(" %lf %lf\n", $5, $6) ; R=R+1 } $1=="R" { printf("L %d\n", R) ; R=0 }' | tac >> ${TMP}_P_raw
	    ;;
	GKK)
	    tac $TMP | $AWK 'BEGIN { FS="\t" ; R=0 } $1=="W" { printf(" %lf %lf\n", $6, $5) ; R=R+1 } $1=="R" { printf("L %u\n", R) ; R=0 }' | tac >> ${TMP}_P
	    ;;
	*)
	    echo "Unsupported format [$PROJ]" 1>&2
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
    IS_WGS84="`head -1 $TMP | grep 'WGS 84'`"

    case "$PROJ" in
	UTM)
	    tac $TMP | $AWK 'BEGIN { FS="\t" ; R=0 } $1=="T" { printf(" %lf %lf\n", $5, $6) ; ++R } END { printf("L %d\n", R) }' | tac >> ${TMP}_P
	    ;;
	DDD)
	    tac $TMP | $AWK 'BEGIN { FS="\t" ; R=0 } $1=="T" { printf(" %lf %lf\n", $3, $4) ; ++R } END { printf("L %d\n", R) }' | tac >> ${TMP}_P_raw
	    ;;
	GKK)
	    tac $TMP | $AWK 'BEGIN { FS="\t" ; R=0 } $1=="T" { printf(" %lf %lf\n", $4, $3) ; ++R } END { printf("L %d\n", R) }' | tac >> ${TMP}_P
	    ;;
	*)
	    echo "Unsupported format [$PROJ]" 1>&2
	    exit 1
	    ;;
    esac
fi



#### convert from WGS84 to current projection
if [ -z "$IS_WGS84" ] || [ $KEEP_WGS84 -eq 1 ] ; then
    echo "No projection transformation performed" 1>&2
    if [ -e "${TMP}_P_raw" ] ; then
	mv -f ${TMP}_P_raw ${TMP}_P
    fi
else
    echo "Attempting projection transform with m.proj2" 1>&2
    # invert columns, crop
    cat ${TMP}_P_raw | grep -v "^L" | awk '{print $2 " " $1}' > ${TMP}_P_raw2
    # perform the projection
    m.proj2 -i input=${TMP}_P_raw2 output=${TMP}_P_raw3
    if [ $? -ne 0 ] ; then
        echo "Projection transform failed, retaining WGS84" 1>&2
        mv -f ${TMP}_P_raw ${TMP}_P
    else
	# put projected results back together as grass-vector line
	LINE_NUM="`head -1 ${TMP}_P_raw`"
	echo "$LINE_NUM" > ${TMP}_P
	cat ${TMP}_P_raw3 | awk '{print " " $2 " " $1}' >> ${TMP}_P
    fi
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
OTHER INFO:   Imported by `echo $USER@$HOSTNAME`
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
    echo "Output to stdout" 1>&2
    cat $DIG 2>/dev/null
else 


    #### import to binary vector file 
    echo "Importing with v.in.ascii" 1>&2
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
