#!/bin/sh

# Markus Neteler 4/2002
# neteler@itc.it
# (c) GPL >= 2
#
#build babyGRASS with shared libraries
#
#TODO: - make compilation a loop to write to ERROR_LOG
#      - add prefix, bindir parameter

### configure:
PREFIX=.  # where to put GRASS (/usr/local/grass5/)
BINDIR=.  # where to put the start script "grass5" (/usr/local/bin/)

##################### nothing to change below #####################
ERROR_LOG=error.log

trap "exit" 2 3 9 15

#are we in the main GRASS directory?
if ! test -f ./configure
then
  echo "Be sure to run this script inside the GRASS source code main directory"
  exit
fi


echo "building new makefiles. Takes a while..."
mk/mkmakefiles   

echo "configuring"

CFLAGS="-Os" LDFLAGS="-s"  ./configure --prefix=$PREFIX --bindir=$BINDIR --without-motif --without-lapack --without-blas --without-fftw --without-freetype --without-gdal --without-dbm --without-tcltk --without-postgres --without-obdc --without-tiff --without-png --without-gd --without-opengl --without-dbm --without-odbc
if [ $? -eq 1 ]
	then
	 echo "an error occured"
	 exit
fi

ARCH=`grep ARCH config.status | cut -d'%' -f3`

cp -f ./mk/Makefile Makefile
cp  ./mk/mid.mk  ./mk/mid.mk.org
cp ./mk/mid.mk.shlib ./mk/mid.mk

##################### prepare local target directories:
mkdir -p $BINDIR/bin.$ARCH 
GISBASE=$PREFIX/dist.$ARCH
# Make sure GISBASE sub-directories exist
for i in \
    bin \
    man man/help man/man1 man/man2 man/man3 man/man4 man/man5 \
    etc etc/paint etc/paint/driver etc/paint/driver.sh etc/paint/driver.uninst \
    etc/sites etc/dig_drivers etc/imagery \
    etc/bin etc/bin/inter etc/bin/cmd \
    txt txt/COMBINE txt/DIGIT txt/DIGIT2 txt/WEIGHT txt/MONITOR \
    lib include \
    driver locks dev
do
    if test ! -d $GISBASE/$i
    then
	mkdir -p $GISBASE/$i || exit 1
	echo $GISBASE/$i created
    fi
done


##################### ready to build now:

echo "Start of compilation: "`date` > ${ERROR_LOG}

#generate ./gmake5 script:
make gmake || echo "Compilation error: " >> ${ERROR_LOG} 

#build the minimum libs:
./gmake5 src/libes/tools || echo "Compilation error: lib" >> ${ERROR_LOG}
./gmake5 src/libes/datetime || echo "Compilation error: " >> ${ERROR_LOG}
./gmake5 src/libes/gis || echo "Compilation error: " >> ${ERROR_LOG}
./gmake5 src/libes/raster || echo "Compilation error: " >> ${ERROR_LOG}
./gmake5 src/libes/dlg || echo "Compilation error: " >> ${ERROR_LOG}
./gmake5 src/libes/proj || echo "Compilation error: " >> ${ERROR_LOG}
./gmake5 src/libes/display || echo "Compilation error: " >> ${ERROR_LOG}
./gmake5 src/libes/lock || echo "Compilation error: " >> ${ERROR_LOG}
./gmake5 src/libes/D || echo "Compilation error: " >> ${ERROR_LOG}
./gmake5 src/libes/edit
./gmake5 src/libes/vask
./gmake5 src/libes/linkm
./gmake5 src/libes/imagery
./gmake5 src/libes/dig_atts
./gmake5 src/libes/vect32/libes || echo "Compilation error: " >> ${ERROR_LOG}
./gmake5 src/libes/vect32/diglib || echo "Compilation error: " >> ${ERROR_LOG}
./gmake5 src/libes/vect32/Vlib || echo "Compilation error: " >> ${ERROR_LOG}
./gmake5 src/libes/vect32/georef || echo "Compilation error: " >> ${ERROR_LOG}
#next also contains the db.* modules:
./gmake5 src/libes/dbmi

#build the internal modules and drivers:
./gmake5 src/general/init || echo "Compilation error: driver" >> ${ERROR_LOG}
./gmake5 src/display/devices/lib || echo "Compilation error: " >> ${ERROR_LOG}
./gmake5 src/display/devices/monitorcap || echo "Compilation error: " >> ${ERROR_LOG}
./gmake5 src/display/devices/XDRIVER || echo "Compilation error: " >> ${ERROR_LOG}
./gmake5 src/fonts/for_grass || echo "Compilation error: " >> ${ERROR_LOG}
./gmake5 src/front.end || echo "Compilation error: " >> ${ERROR_LOG}
./gmake5 src/general/g.gisenv

#build the user modules:
./gmake5 src/display/d.mon || echo "Compilation error: d.mon" >> ${ERROR_LOG}
./gmake5 src/display/d.rast || echo "Compilation error: d.rast " >> ${ERROR_LOG}
./gmake5 src/display/d.vect
./gmake5 src/display/d.sites
./gmake5 src/display/d.erase
./gmake5 src/display/d.what.rast
./gmake5 src/display/d.what.vect
./gmake5 src/display/d.what.sites
./gmake5 src/display/d.measure
./gmake5 src/display/d.pan
./gmake5 src/display/d.zoom
./gmake5 src/general/g.region/cmd
./gmake5 src/general/manage
./gmake5 src/raster/r.in.ascii
./gmake5 src/sites/s.in.ascii

echo "End of compilation: "`date` > ${ERROR_LOG}

#needed to link against front.end:
CURRWD=`pwd`
cp Makefile $PREFIX/dist.$ARCH
cd $PREFIX/dist.$ARCH
make links
cd $CURRWD

########### locally install libs
mkdir -p $PREFIX/dist.$ARCH/lib
cp -af ./src/libes/*.so*  $PREFIX/dist.$ARCH/lib

########### clean up:
echo "Cleaning up stuff not needed now..."
rm -rf $PREFIX/dist.$ARCH/etc/nad   # only NAD27/NAD83
rm -rf $PREFIX/dist.$ARCH/etc/paint
rm -rf $PREFIX/dist.$ARCH/txt
rm -rf $PREFIX/dist.$ARCH/etc/bin/inter/

########### install to destination:

echo "babyGRASS start script is in: $BINDIR/bin.$ARCH"
echo "babyGRASS binaries are in:    $PREFIX/dist.$ARCH"

#or use (after changing PREFIX):
#make install
