:
ARCH=$1

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
echo "* GRID3D raster volume support please compile as:"
echo "       gmake5 src.contrib/GMSL/g3d"
echo "       gmakelinks5"
echo ""
echo "* PostgreSQL support please compile as:"
echo "       gmake5 src.garden/grass.postgresql"
echo "       gmakelinks5"
echo ""
echo "* Check file error.log for modules not been compiled due to error."
echo "Install GRASS with (as root)"
echo "       make install"
