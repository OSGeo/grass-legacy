:
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
echo "Add the flag in src/CMD/head/head and clean ps.map OBJ-files before"
echo "recompiling."
echo ""

# talk about NVIZ
echo "NVIZ Visualization tool:"
echo "   Please compile it separately in"
echo "          src.contrib/GMSL/NVIZ2.2/"
echo "   Change to this directory. Use ./configure, gmake5 and gmakelinks5 "
echo "   to compile the NVIZ."
echo ""
