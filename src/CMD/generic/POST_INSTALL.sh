#############################################################################
#
# $Id$
#
# MODULE:   	Grass Compilation
# AUTHOR(S):	Original author unknown - probably CERL
#   	    	Justin Hickey - Thailand - jhickey@hpcc.nectec.or.th
#		Markus Neteler - Germany - neteler@geog.uni-hannover.de
# PURPOSE:  	This script will perform the last few steps of the GRASS
#   	    	compilation and print a message for users.
# COPYRIGHT:    (C) 2000 by the GRASS Development Team
#
#               This program is free software under the GNU General Public
#   	    	License (>=v2). Read the file COPYING that comes with GRASS
#   	    	for details.
#
#############################################################################

:
ARCH=$1
CURR_DIR=`pwd`

# copy the standard digcap file
cp $GISBASE/etc/digcap.sample $GISBASE/etc/digcap

#copy the fifo creating script (in case they are broken in tarball)
cp $SRCDIR/src/scripts/shells/create_fifos.sh $GISBASE/dev
chmod ugo+x $GISBASE/dev/create_fifos.sh

# create locks directory for GRASS MONITORS and set permissions
SERVERNAME=`uname -n`
if [ ! -d $GISBASE/locks ]; then mkdir $GISBASE/locks ; fi
if [ ! -d $GISBASE/locks/$SERVERNAME ]; then mkdir $GISBASE/locks/$SERVERNAME ; fi
chmod -R 1777 $GISBASE/locks

echo ""
echo "GRASS GIS source code compiled."
echo ""
if grep 'Compilation error' error.log; then
  echo ""
  echo "* In case of errors please check following web page for hints:"
  echo "    http://grass.itc.it/grass54/source/compilation_hints.html"
  echo ""
fi
echo "* Install GRASS with (possibly as root)"
echo "    make install"
