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
cp src/scripts/shells/create_fifos.sh $GISBASE/dev
chmod ugo+x $GISBASE/dev/create_fifos.sh

# create locks directory for GRASS MONITORS and set permissions
SERVERNAME=`uname -n`
if [ ! -d $GISBASE/locks ]; then mkdir $GISBASE/locks ; fi
if [ ! -d $GISBASE/locks/$SERVERNAME ]; then mkdir $GISBASE/locks/$SERVERNAME ; fi
chmod -R 1777 $GISBASE/locks

echo ""
echo "GRASS GIS source code compiled successfully."

# talk about stuff not yet compiled:
echo "* PostgreSQL support please compile as:"
echo "    $CURR_DIR/bin.$ARCH/gmake5 src.garden/grass.postgresql"
echo "    $CURR_DIR/bin.$ARCH/gmakelinks5"
echo ""
echo "* ODBC driver please compile as:"
echo "    $CURR_DIR/bin.$ARCH/gmake5 src/libes/dbmi/drivers/odbc"
echo "    $CURR_DIR/bin.$ARCH/gmakelinks5"
echo ""
echo "* PNGDriver please compile as:"
echo "    $CURR_DIR/bin.$ARCH/gmake5 src/display/devices/PNGdriver"
echo ""
echo "* Note that the absolute paths listed above are necessary to run"
echo "  gmake5 and gmakelinks5 until you run 'make install'"
echo ""
echo "* Check file error.log for modules not been compiled due to error."
echo ""
echo "* Install GRASS with (possibly as root)"
echo "    make install"
