:
#############################################################################
#
# $Id$
#
# MODULE:   	Grass Compilation
# AUTHOR(S):	Original author unknown - probably CERL
#   	    	Justin Hickey - Thailand - jhickey@hpcc.nectec.or.th
# PURPOSE:  	This file creates the gmake and gmakelinks programs storing
#   	    	them in the GRASS_BIN directory.
# COPYRIGHT:    (C) 2000 by the GRASS Development Team
#
#               This program is free software under the GNU General Public
#   	    	License (>=v2). Read the file COPYING that comes with GRASS
#   	    	for details.
#
#############################################################################

ARCH=$1
HEAD_FILE=`dirname $0`"/../head/head.$ARCH"

GRASS_BIN=$2
if [ ! -d $GRASS_BIN ]; then mkdir -p $GRASS_BIN ; fi
if [ $? != 0 ]; then
echo "An error occurred. Stop."
exit 1
fi

# create gmake5 script to be used for local compiling
echo ":"                                   > $GRASS_BIN/gmake$NAME_VER
echo "SRC=$SRC/src"                       >> $GRASS_BIN/gmake$NAME_VER
echo "CMD=$SRC/src/CMD"                   >> $GRASS_BIN/gmake$NAME_VER
echo "HEADER=head.$ARCH"                  >> $GRASS_BIN/gmake$NAME_VER
echo "HASX=yes"                           >> $GRASS_BIN/gmake$NAME_VER
echo "HASMotif=no"                        >> $GRASS_BIN/gmake$NAME_VER
echo "MAKE=$MAKE"                         >> $GRASS_BIN/gmake$NAME_VER
echo ". $SRC/src/CMD/generic/gmake.sh"    >> $GRASS_BIN/gmake$NAME_VER

if [ $? != 0 ]; then
 echo "An error occured. Stop."
 exit 1
fi

chmod ugo+x $GRASS_BIN/gmake$NAME_VER

# create gmakelinks script to be used for linking after
# local compiling
echo ":"                                      > $GRASS_BIN/gmakelinks$NAME_VER
echo "GMAKE=$GRASS_BIN/gmake$NAME_VER"        >> $GRASS_BIN/gmakelinks$NAME_VER
echo ". $SRC/src/CMD/generic/MAKELINKS.sh"   >> $GRASS_BIN/gmakelinks$NAME_VER
chmod ugo+x $GRASS_BIN/gmakelinks$NAME_VER

exit 0
