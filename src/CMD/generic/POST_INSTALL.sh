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

# copy the standard digcap file
cp $GISBASE/etc/digcap.sample $GISBASE/etc/digcap

#copy the fifo creating script (in case they are broken in tarball)
cp src/scripts/shells/create_fifos.sh $GISBASE/dev
chmod ugo+x $GISBASE/dev/create_fifos.sh

cp src/scripts/shells/create_ipcs.sh $GISBASE/dev
chmod ugo+x $GISBASE/dev/create_ipcs.sh

# create locks directory for GRASS MONITORS and set permissions
SERVERNAME=`uname -n`
if [ ! -d $GISBASE/locks ]; then mkdir $GISBASE/locks ; fi
if [ ! -d $GISBASE/locks/$SERVERNAME ]; then mkdir $GISBASE/locks/$SERVERNAME ; fi
chmod -R 1777 $GISBASE/locks

echo ""
echo "GRASS GIS source code compiled successfully."

# talk about stuff not yet compiled:
echo "* GRID3D raster volume support please compile as:"
echo "       gmake5 src.contrib/GMSL/g3d"
echo "       gmakelinks5"
echo ""
echo "* PostgreSQL support please compile as:"
echo "       gmake5 src.garden/grass.postgresql"
echo "       gmakelinks5"
echo ""
echo "* ODBC driver please compile as:"
echo "       gmake5 rc/libes/dbmi/drivers/odbc"
echo "       gmakelinks5"
echo ""
echo "* Note that if you try to compile these modules before you run"
echo "  make install then you will need to specify the absolute pathname"
echo "  to gmake5 and gmakelinks5. For example, for GRID3D the commands"
echo "  may be as follows:"
echo "	     /home/grass/bin.i686-pc-linux-gnu/gmake5 src.contrib/GMSL/g3d"
echo "	     /home/grass/bin.i686-pc-linux-gnu/gmakelinks5"
echo ""
echo "* Check file error.log for modules not been compiled due to error."
echo "Install GRASS with (possibly as root)"
echo "       make install"
