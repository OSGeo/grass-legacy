#!/bin/sh
#
# Script to create a new GRASS LOCATION from a EPSG code
#
# (c) Markus Neteler 2004, ITC-irst, Trento
#     $Date$
#
# This program is Free Software under the GNU GPL (>=v2).
# create a new LOCATION from a raster data set
# 
# Derived from:
# Reference:
#   Markus Neteler and Helena Mitasova:
#   Open Source GIS: A GRASS GIS Approach. 2nd edition 2004.
#   Kluwer Academic Publishers, Boston, Dordrecht, 464 pp,
#   ISBN: 1-4020-7088-8, http://mpa.itc.it/grasstutor/
#

GRASSVERSION=57
GRASSSTARTSCRIPT=grass$GRASSVERSION
########## nothing to change below ##############################
EPSG=$1
LOCATION=$2
MYGISDBASE=$3
GRASSRC=grassrc6

if [ $# -lt 2 ] ; then
 echo "Script to create a new LOCATION from EPSG code"
 echo "Usage:"
 echo "   make_location_epsg_g$GRASSVERSION.sh epsg newlocation_name [GISDBASE]"
 echo ""
 echo "       epsg: EPSG code number of projection (see /usr/local/share/proj/epsg)"
 echo "       newlocation_name: new location to be created"
 echo "       GISDBASE: full path to GRASS database directory (optional)"
 echo "                 e.g. $HOME/grassdata"
 echo ""
 exit 1
fi

if test -f $HOME/.gislock$GRASSVERSION ; then
 echo "ERROR. GRASS $GRASSVERSION is already running"
 echo "Please close other session first."
 rm -f $HOME/.gislock$GRASSVERSION
 #exit 1
fi

#get/set GISBASE
GRASSSTARTSCRIPTPATH=$GISBASE

#get GISDBASE from previous session:
if [ "$MYGISDBASE" = "" ] ; then
  GISDBASE=`grep GISDBASE $HOME/.$GRASSRC | cut -d' ' -f2`
  if [ "$GISDBASE" = "" ] ; then
   echo "ERROR. Cannot get GISDBASE from $HOME/.$GRASSRC"
   echo "Please specify the GISDBASE parameter"
   exit 1
  fi
else
 GISDBASE=$MYGISDBASE
fi

GISBASE=`echo $GISBASE | sed "s+'++g"`

if test -d $GISDBASE/$LOCATION ; then
 echo "ERROR. Location $LOCATION already exists in $GISDBASE"
 exit 1
fi

# Set LD_LIBRARY_PATH to find GRASS shared libraries
if [ ! "$LD_LIBRARY_PATH" ] ; then
  LD_LIBRARY_PATH=$GISBASE/lib
else
  LD_LIBRARY_PATH=$GISBASE/lib:$LD_LIBRARY_PATH
fi
export LD_LIBRARY_PATH

#generate temporal LOCATION:
TEMPDIR=$$.tmp
mkdir -p $GISDBASE/$TEMPDIR/PERMANENT

#save existing .$GRASSRC
if test -e $HOME/.$GRASSRC ; then
   mv $HOME/.$GRASSRC /tmp/$TEMPDIR.$GRASSRC
fi
echo "LOCATION_NAME: $TEMPDIR" >  $HOME/.$GRASSRC
echo "MAPSET: PERMANENT"       >> $HOME/.$GRASSRC
echo "DIGITIZER: none"         >> $HOME/.$GRASSRC
echo "GISDBASE: $GISDBASE"     >> $HOME/.$GRASSRC

export GISBASE=$GISBASE
export GISRC=$HOME/.$GRASSRC
export PATH=$PATH:$GISBASE/bin:$GISBASE/scripts

#populate temp location with minimal set of files:
echo "proj:       3
zone:       0
north:      72N
south:      27N
east:       42E
west:       11W
cols:       6360
rows:       5400
e-w resol:  0:00:30
n-s resol:  0:00:30" > $GISDBASE/$TEMPDIR/PERMANENT/DEFAULT_WIND

echo "name: Lat/Lon
datum: wgs84
towgs84: 0.000,0.000,0.000
proj: ll
ellps: wgs84" > $GISDBASE/$TEMPDIR/PERMANENT/PROJ_INFO

echo "unit: degree
units: degrees
meters: 1.0" > $GISDBASE/$TEMPDIR/PERMANENT/PROJ_UNITS

# create new location:
g.proj -c proj4='+init=epsg:'$EPSG location=$LOCATION

if [ $? -eq 1 ] ; then
  echo "An error occured. Stop."
  exit 1
fi

#restore previous .$GRASSRC
if test -f /tmp/$TEMPDIR.$GRASSRC ; then
   mv /tmp/$TEMPDIR.$GRASSRC $HOME/.$GRASSRC
fi

#cleanup:
rm -rf $GISDBASE/$TEMPDIR

echo "Now you can launch GRASS with:"
echo "   grass$GRASSVERSION $GISDBASE/$LOCATION/PERMANENT"
echo "and start to import data sets (r.in.gdal -e ... ; v.in.ogr -e ... )."
echo ""
echo "Note: Depending on the EPSG definition some datums are missing."
echo "   check with g.proj -w"
echo "To fix a missing datum definition, either edit the new PROJ_INFO file"
echo "or carefully run g.proj again inside the new location."
