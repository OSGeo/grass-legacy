#!/bin/sh

# v.region.sh -- create a binary vector file from the 
#                current region boundaries.
#                To be used in conjunction with v.cutter
#                to cut a vector file to the actual region 
# c) Andreas Lange, andreas.lange@rhein-main.de
# $Id$
# 


PROG=`basename $0`
VERSION="$PROG c) 2001 Andreas Lange, andreas.lange@rhein-main.de"
OPTS=""


#### test if GRASS is running
if [ "$GISRC" = "" ] ; then
  echo "$PROG: You must be running GRASS to execute $PROG" 2>&1
  exit 1
fi


#### check that GRASS variables are set
eval `g.gisenv`

: $GISDBASE $LOCATION $MAPSET


#### process command line arguments 
OK=0 ; SUP=0 ; AUTOLABEL=0 ; VERB=0
while [ $# -ge 1 ] ; do

    case "$1" in
    -h) OK=0
       ;;    
    h*) OK=0
       ;;
    m*) MAP=`echo "$1" | sed 's/.*=//g'` 2>&1 > /dev/null
	echo "vector map = $MAP" 2>&1
	OK=1
       ;;
    -v) echo $VERSION 2>&1
	echo " " 2>&1
	VERB=1
       ;;
    -s) SUP=1
       ;;
    -l) AUTOLABEL=1
       ;;
    *)  OK=0
       ;;
    esac
    shift
done


#### check that SUP is set when we AUTOLABEL
if [ $AUTOLABEL -eq 1 ] ; then
   SUP=1
fi


#### print usage and exit
if [ $OK -ne 1 ] ; then
    echo "Description: " 2>&1
    echo " Create a binary vector file from the current region boundaries." 2>&1
    echo " " 2>&1
    echo "Usage: " 2>&1
    echo " $PROG map=vector [-v] [-l] [-s]" 2>&1
    echo " " 2>&1
    echo "Flags: " 2>&1
    echo "     -v       verbose output" 2>&1
    echo "     -l       automatically label vector file" 2>&1
    echo "     -s       run v.support on new binary vector file" 2>&1
    echo "     -h       print this message" 2>&1
    echo " " 2>&1
    echo "Parameters: " 2>&1 
    echo " map=vector   name for new binary vector file" 2>&1
    echo " " 2>&1
    echo $VERSION 2>&1
    exit 1
fi


#### create dig_ascii if required
if [ ! -d "$LOCATION/dig_ascii" ] ; then
  mkdir -p $LOCATION/dig_ascii
fi
DIG="$LOCATION/dig_ascii/DIG_$$"


#### trap ctrl-c so that we can clean up tmp
trap 'rm -f $DIG' 2 3 9


#### read current region 
CUR_REG=`g.region -g` 2>&1 > /dev/null


#### create boundary values
EAST=`echo  $CUR_REG | cut -d' ' -f4 | sed 's/.*=//g'`
WEST=`echo  $CUR_REG | cut -d' ' -f3 | sed 's/.*=//g'`
NORTH=`echo $CUR_REG | cut -d' ' -f1 | sed 's/.*=//g'`
SOUTH=`echo $CUR_REG | cut -d' ' -f2 | sed 's/.*=//g'`
if [ "$EAST" == "" ] ; then
    echo "Error with east boundary" 2>&1
    exit -1
fi
if [ "$WEST" == "" ] ; then
    echo "Error with west boundary" 2>&1
    exit -1
fi
if [ "$NORTH" == "" ] ; then
    echo "Error with north boundary" 2>&1
    exit -1
fi
if [ "$SOUTH" == "" ] ; then
    echo "Error with south boundary" 2>&1
    exit -1
fi
if [ $VERB -eq 1 ] ; then
    echo "Region boundaries are: " 2>&1
    # g.region -p
    echo " " 2>&1
    echo "                    north=$NORTH        " 2>&1
    echo "                +---------------------+ " 2>&1
    echo "                |                     | " 2>&1
    echo "                |                     | " 2>&1
    echo " west=$WEST                             " 2>&1
    echo "                                        east=$EAST " 2>&1
    echo "                |                     | " 2>&1
    echo "                |                     | " 2>&1
    echo "                +---------------------+ " 2>&1
    echo "                    south=$SOUTH        " 2>&1
    echo " " 2>&1
    echo " " 2>&1
fi


#### create digit header and vertices
cat << EOF >> $DIG
ORGANIZATION: GRASSroots organization
DIGIT DATE:   `date +%D`
DIGIT NAME:   $PROG
MAP NAME:     $MAP
MAP DATE:     `date +%Y`
MAP SCALE:    1:10000
ZONE:         0
WEST EDGE:    $WEST
EAST EDGE:    $EAST
SOUTH EDGE:   $SOUTH
NORTH EDGE:   $NORTH
MAP THRESH:   0
VERTI:
A 5
 $NORTH $WEST
 $NORTH $EAST
 $SOUTH $EAST
 $SOUTH $WEST
 $NORTH $WEST
EOF


#### if no name for vector file given, cat to stdout
if [ "$MAP" = "" ] ; then
    echo "output to stdout" 2>&1
    cat $DIG 2>/dev/null
else 


    #### import to binary vector file 
    echo "importing with v.in.ascii" 2>&1
    v.in.ascii output=$MAP input=DIG_$$ $OPTS 2>&1 >/dev/null


    #### check success/failure 
    if [ $? -eq 0 ] ; then
	echo "$PROG: binary vector file \"$MAP@$MAPSET\" successfully created" 2>&1


	#### run v.support on file
	if [ $SUP -eq 1 ] ; then
	    echo "$PROG: running v.support on \"$MAP@$MAPSET\"" 2>&1
	    if [ $VERB -eq 1 ] ; then
		v.support map=$MAP option=build threshold=10 2>&1
	    else 
		v.support map=$MAP option=build threshold=10  2>&1 > /dev/null
	    fi
	    if [ $? -eq 0 ] ; then
		echo "$PROG: v.support successfully applied" 2>&1
	    else
		echo "$PROG: error with v.support, check manually" 2>&1
		rm -f $DIG 2>&1 > /dev/null
		exit 3
	    fi
	fi


	#### now label file
	if [ $AUTOLABEL -eq 1 ] ; then
	    echo "$PROG: labeling \"$MAP@$MAPSET\"" 2>&1
	    if [ $VERB -eq 1 ] ; then
		v.alabel -i map=$MAP value=1 2>&1
	    else 
		v.alabel -i map=$MAP value=1 2>&1 > /dev/null
	    fi
	    if [ $? -eq 0 ] ; then
		echo "$PROG: v.alabel successfully applied" 2>&1
	    else
		echo "$PROG: error with v.alabel, check manually" 2>&1
		rm -f $DIG 2>&1 > /dev/null
		exit 4
	    fi
	fi
    else
	echo "$PROG: An error occured on creating \"$MAP\" in mapset \"$MAPSET\"," 2>&1
	echo "please check!" 2>&1
	rm -f $DIG 2>&1 > /dev/null
	exit 2
    fi
fi


#### clean up the mess
rm -f $DIG 2>&1 > /dev/null


#### end
exit 0 
