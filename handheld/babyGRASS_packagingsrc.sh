#!/bin/sh

# Markus Neteler 4/2002
# neteler@itc.it
# (c) GPL >= 2
#
#extract babyGRASS from standard source code
#

TARBALL=babyGRASS_src.tar.gz

#are we in the main GRASS directory?
if ! test -f ./configure
then
  echo "Be sure to run this script inside the GRASS source code main directory"
  exit
fi

#list of directories to be included:
LIST='mk
src/CMD
src/include
src/libes/tools
src/libes/datetime
src/libes/gis
src/libes/raster
src/libes/dlg
src/libes/proj
src/libes/display
src/libes/lock
src/libes/D
src/libes/edit
src/libes/vask
src/libes/linkm
src/libes/imagery
src/libes/dig_atts
src/libes/vect32/libes
src/libes/vect32/diglib
src/libes/vect32/Vlib
src/libes/vect32/georef
src/libes/dbmi
src/general/init
src/display/devices/lib
src/display/devices/monitorcap
src/display/devices/XDRIVER
src/fonts
src/front.end
src/general/g.gisenv
src/general/g.mapsets
src/display/d.mon
src/display/d.rast
src/display/d.vect
src/display/d.sites
src/display/d.erase
src/display/d.what.rast
src/display/d.what.vect
src/display/d.what.sites
src/display/d.measure
src/display/d.pan
src/display/d.zoom
src/general/g.region/cmd
src/general/manage
src/raster/r.in.ascii
src/sites/s.in.ascii'

#generate list string:
FullList=""
for i in $LIST
do
 FullList="$FullList $i"
done

#mkdir babysrc dir
mkdir babyGRASS_src
echo "Copying related files to tmp dir..."
tar cf - $FullList | (cd ./babyGRASS_src; tar xf -)

#copy further files:
cp AUTHORS BUGS COPYING NEWS.html README REQUIREMENTS.html TODO.txt Makefile.in config* install-sh INSTALL babyGRASS_compile.sh babyGRASS_src/

#keep rubbish outside:
EXCLUDE="--exclude=CVS* --exclude=*.o --exclude=makefile --exclude=OBJ.* --exclude=LIB.* --exclude=lib*.a"

#packaging:
echo Packaging...
tar cvfz $TARBALL $EXCLUDE babyGRASS_src/*

rm -rf babyGRASS_src

echo "The source code for babyGRASS is stored into: $TARBALL"
