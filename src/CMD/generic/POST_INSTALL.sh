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

# do warning about ps.map compilation:
echo "HINT for ps.map on Linux: "
echo "  You have to recompile this module with additional compile flag:"
echo "  COMPILE_FLAGS = -fwritable-strings"
echo "Add the flag in src/CMD/head/head.$ARCH and clean ps.map OBJ-files before"
echo "recompiling."
echo ""

# talk about NVIZ and other stuff:
echo "* NVIZ Visualization tool:"
echo "   Please compile it separately in"
echo "          src.contrib/GMSL/NVIZ2.2/"
echo "   To compile NVIZ, change to this directory."
echo "   Use 'gmake5'"
echo ""
echo "* GRID3D raster volume support please compile here:"
echo "          src.contrib/GMSL/g3d/"
echo "   To compile GRID3D, change to this directory and run 'gmake5'"
echo ""
echo "* PostgreSQL support please compile here:"
echo "          src.garden/grass.postgresql/"
echo "   To compile SQL-support, change to this directory and run 'gmake5'"
echo ""
echo "* Finally run 'gmakelinks5' to finish the installation process."
echo "* Check file error.log for modules not been compiled due to error."
