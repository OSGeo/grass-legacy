#!/bin/sh

# GRASS 5 test suite
# $Id$
#
# The script creates itself a GRASS database/location/mapset 
# with random name and UTM projection and runs tests in this
# environment.

# improvement wanted! Please update directly in CVS.
# Please add further tests.
# Markus Neteler <neteler@geog.uni-hannover.de>
#
# based on "batch-grass.sh" by
#        Andrew E Long 
#        aelon@sph.umich.edu
#        ael, Mon Nov 16 08:54:15 EST 1998
# this update by A. Prasad 20. Jan. 2000
#           aprasad/ne_de@fs.fed.us

#--------------------------------------------------------------------------
#
if test ! "$GISBASE" = ""; then
echo "You must exit GRASS to run this GRASS test suite."
exit
fi

export GISBASE=/usr/local/grass5

if test "$1" = "-h"
then
 echo ""
 echo "GRASS test suite"
 echo " The script creates itself a GRASS database/location/mapset"
 echo " with random name and UTM projection and runs tests in this"
 echo " environment."

 echo ""
 echo "Usage:"
 echo "testgrass.sh  [path_to_grassbinaries]"
 echo ""
 echo "Default path: /usr/local/grass5"
else
 if test ! "$1" = ""
 then
   GISBASE=$1
 fi
fi

#--------------------------------------------------------------------------
#generate random numbers to use for location and mapset name and database
# name for session:
RANDOMNAME=`echo gt_$RANDOM$RANDOM`

if test ! -e $GISBASE
then
 echo "GRASS 5 not found at $GISBASE"
 echo "Please specify correct directory name"
 exit
fi

if test ! -f $GISBASE/bin/r.mapcalc
then
 echo "GRASS 5 not properly installed at $GISBASE"
 echo "Please check installation!"
 exit
fi

#-------------------------------------------------------
#database name:
export GISDBASE=./$RANDOMNAME
export LOCATION_NAME=$RANDOMNAME
export MAPSET=PERMANENT

mkdir -p $GISDBASE/$LOCATION_NAME/$MAPSET
PERM="$GISDBASE/$LOCATION_NAME/$MAPSET"

# set the environment:
GISRC=$GISDBASE/.grassrc5.$$
ETC=$GISBASE/etc;
PATH=$GISBASE/bin:$GISBASE/scripts:$PATH:/usr/bin:$GISBASE/etc/bin/main/cmd:$PATH/etc/bin/main/inter
cat << EOF > $GISRC
GISDBASE: $GISDBASE
LOCATION_NAME: $RANDOMNAME
MAPSET: PERMANENT
PAINTER: ppm
MAPLP: stuff.ppm
EOF

export GISBASE GISDBASE ETC PATH GISRC
export LOCATION=${GISDBASE?}/${LOCATION_NAME?}/${MAPSET?}

cat << EOF > $PERM/DEFAULT_WIND
proj:       1
zone:       13
north:      4928000.00
south:      4914000.00
east:       609000.00
west:       590000.00
e-w res:    50.00
n-s res:    50.00
EOF

cp $PERM/DEFAULT_WIND $PERM/WIND
#------------------------------------------------------------
#some functions:

printerror()
{
 echo "Failed! Please report to GRASS 5 developers <neteler@geog.uni-hannover.de>"
 echo "Your platform:"
 echo "   `uname -a`"
 true
}

#------------------------------------------------------------
# do the tests:
#------------------------------------------------------------
echo "testing in UTM projection environment:"
g.region -p
echo ""

# test 1:
echo "Test the -129 r.mapcalc bug:"
r.mapcalc testmap=-129
r.stats -q testmap | grep -v 129
if [ $? -ne 1 ]; then
 printerror
else
 echo "r.mapcalc bug test o.k."
fi

#test 2:

#help me :-)


#------------------------------------------------------------
#cleanup:

#remove the test database:
rm -rf $GISDBASE

#
echo "Tests finished."
