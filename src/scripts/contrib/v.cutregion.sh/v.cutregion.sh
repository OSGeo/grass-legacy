#!/bin/sh

# v.cutregion.sh -- cut a binary vector file to the  
#                   current region boundaries.
# 
# This is just an intermediate solution until a new vector library
# provides better support for this!
# Use at your own risk!                 
#                
# c) Andreas Lange, andreas.lange@rhein-main.de
# $Id$
# 

# parameters:
# input  = map to cut out
# output = map to create
# -s = run v.support on new map
# -r = leave behind region boundary vector file (region)

if test "$GISBASE" = ""; then
 echo "You must be in GRASS GIS to run this program." >&2
 exit 1
fi   
     
eval `g.gisenv`
: ${GISBASE?} ${GISDBASE?} ${LOCATION_NAME?} ${MAPSET?}
LOCATION=$GISDBASE/$LOCATION_NAME/$MAPSET


PROG=`basename $0`
VERSION="$PROG (c) 2001 Andreas Lange, andreas.lange@rhein-main.de"
OPTS=""

#### process command line arguments 
OK=0 ; SUP=0 ; VERB=0; RLEAVE=0
while [ $# -ge 1 ] ; do

    case "$1" in
    -h) OK=0
       ;;    
    h*) OK=0
       ;;
    in*) INPUT=`echo "$1" | sed 's/.*=//g'` 2>&1 > /dev/null
	echo "input vector map = $INPUT" 2>&1
	OK=1
       ;;
    out*) OUTPUT=`echo "$1" | sed 's/.*=//g'` 2>&1 > /dev/null
    	echo "output vector map = $OUTPUT" 2>&1
    	OK=1
       ;;   
    -v) echo $VERSION 2>&1
	echo " " 2>&1
	VERB=1
       ;;
    -s) SUP=1
       ;;
    -r) RLEAVE=1
       ;;
    *)  OK=0
       ;;
    esac
    shift
done


#### check that input and output are correct
if [ "$INPUT" = "" ] ; then
   # echo "no input file given, aborting" 2>&1
   OK=0
fi
if [ "$OUTPUT" = "" ] ; then
   # echo "no output file given, aborting" 2>&1
   OK=0
fi


#### print usage and exit
if [ $OK -ne 1 ] ; then
    echo "Description: " 2>&1
    echo " Cut a binary vector file to the current region boundaries." 2>&1
    echo " " 2>&1
    echo "Usage: " 2>&1
    echo " $PROG input=vector output=vector [-v] [-l] [-s] [-r]" 2>&1
    echo " " 2>&1
    echo "Flags: " 2>&1
    echo "     -v       verbose output" 2>&1
    echo "     -s       run v.support on new binary vector file" 2>&1
    echo "     -r       leave behind region boundary vector file in region$$" 2>&1
    echo "     -h       print this message" 2>&1
    echo " " 2>&1
    echo "Parameters: " 2>&1 
    echo " input=vector    name of existing binary vector file to cut out" 2>&1
    echo " output=vector   name for new binary vector file" 2>&1
    echo " " 2>&1
    echo $VERSION 2>&1
    exit 1
fi


#### create dig_ascii if required
if [ ! -d "$LOCATION/dig_ascii" ] ; then
  mkdir -p $LOCATION/dig_ascii 2>&1 > /dev/null
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
MAP NAME:     region$$
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


#### import ascii vector into binary region vector file
echo "importing region with v.in.ascii" 2>&1
v.in.ascii output=region$$ input=DIG_$$ $OPTS 2>&1 >/dev/null
if [ $? -eq 0 ] ; then
   echo "$PROG: binary region file \"region$$\" sucessfully created"
else 
   echo "$PROG: could not create binary region file, aborting"
   exit -1
fi


#### run v.support on region vector file
echo "running v.support on region vector file"
v.support map=region$$ option=build 2>&1 > /dev/null
# threshold=10
if [ $? -eq 0 ] ; then
   echo "$PROG: v.support ok"
else
   echo "$PROG: v.support failed, exiting"
   exit -1
fi


#### auto-label region vector file
echo "auto-labeling region vector file"
v.alabel -i map=region$$ value=$$ 2>&1 > /dev/null
if [ $? -eq 0 ] ; then
   echo "$PROG: auto-labeling ok"
else
   echo "$PROG: auto-labeling failed, exiting"
   exit -2
fi


#### cut input map to region to output vector file
echo "cutting \"$INPUT\" map with region file to \"$OUTPUT\" map"
v.cutter cutter=region$$ input=$INPUT out=$OUTPUT 2>&1 > /dev/null
if [ $? -eq 0 ] ; then
   echo "$PROG: cutting ok" 2>&1
else
   echo "$PROG: problem with v.cutter, check manually!" 2>&1
   exit -3
fi


#### run v.support on output map if requested
if [ $SUP -eq 1 ] ; then
   echo "running v.support on $OUTPUT" 2>&1
   v.support map=$OUTPUT option=build threshold=10 2>&1 > /dev/null
   if [ $? -eq 0 ] ; then
      echo "$PROG: v.support on $OUTPUT ok" 2>&1
   else
      echo "$PROG: error running v.support on $OUTPUT, check manually" 2>&1
   fi
   echo "now running v.spag -i to remove duplicate lines"
   v.spag -i map=$OUTPUT 2>&1 > /dev/null
   if [ $? -eq 0 ] ; then
      echo "$PROG: v.spag -i ok" 2>&1
   else
      echo "$PROG: v.spag -i failed, check manually!" 2>&1
   fi
else
   echo "Don't forget to run v.support and v.spag -i on " 2>&1
   echo "the new binary vector file \"$OUTPUT@$MAPSET\"!" 2>&1
   echo " " 2>&1
fi


#### remove region vector file if not leave behind
if [ $RLEAVE -eq 1 ] ; then
   echo "binary region bounding vector file is left in \"region$$@$MAPSET\"!"
else 
   echo "removing binary region vector file \"region$$@$MAPSET\""
   g.remove vect=region$$ 2>&1 > /dev/null
fi


#### clean up the mess
rm -f $DIG 2>&1 > /dev/null


#### end
exit 0 
