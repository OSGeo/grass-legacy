#!/bin/sh
#
# r.da2ll.sh - USGS DEM (DMA formatted) to raster map in a lat-lon region
#    
#   simplifies import of USGS DEM 1:250,000 data obtained from:
#
#	http://edcwww.cr.usgs.gov/doc/edchome/ndcdb/ndcdb.html
#	http://edcftp.cr.usgs.gov/pub/data/DEM/250/
#	ftp://edcftp.cr.usgs.gov/pub/data/DEM/250/
#
# usage:  r.dma2ll.sh <dem_filename> <raster_name>
#
#  Tom Poindexter <tpoindex@nyx.net>,   March 1999
#
#  Notes:  uses m.dmaUSGSread, m.rot90, m.in.bin,  dd, sed, awk
#	   creates and deletes temp files in $TMPDIR, or /tmp if
#          TMPDIR is not set.


#### Check that correct number of arguments have been given.

if [ "$1" = "" -o "$2" = "" -o "$1" = "-help" -o "$1" = "help" ]
then
        echo "Usage: $0 <dem_file_name> <raster_name>" >&2
        exit 1
fi

#### Check that user is running GRASS

if test "$GISRC" = ""
then
        echo "You must be running GRASS to execute $0" >&2
        exit 1
fi

#### Check GRASS variables are set

eval `g.gisenv`
: ${GISBASE?} ${GISDBASE?} ${LOCATION_NAME?} ${MAPSET?}


# check dma dem file

if test ! -f $1 ; then
	echo "$0: dem file $1 does not exist"
	exit 1
fi

# save dem and raster file names
dem=$1
rast=$2

# get north/south/east/west from dem file

deminfo=`dd bs=1 skip=548 count=320 if=$dem  2>/dev/null `

set -- $deminfo

sw_lon=$1
sw_lat=$2

nw_lon=$3
nw_lat=$4

ne_lon=$5
ne_lat=$6

se_lon=$7
se_lat=$8

awkpgm='{printf "%f", ($1+0.0)/3600;}'
north=`echo $nw_lat | sed -e 's/D/E/' | awk "$awkpgm" `
south=`echo $sw_lat | sed -e 's/D/E/' | awk "$awkpgm" `
east=` echo $se_lon | sed -e 's/D/E/' | awk "$awkpgm" `
west=` echo $nw_lon | sed -e 's/D/E/' | awk "$awkpgm" `

echo "dem file: $dem:"
desc=`dd bs=1 count=65 if=$dem 2>/dev/null`
echo "description: $desc"

echo "dem boundaries:"
echo "            north=$north"
echo
echo "west=$west          east=$east "
echo
echo "            south=$south"

# make a temp file name; if TMPDIR is defined, use that dir, /tmp otherwise
tmpdir=${TMPDIR-/tmp}
tmpf=$tmpdir/dma.$$

echo "reading dem file, creating binary data"
m.dmaUSGSread top=1 bottom=1201 left=1 right=1201 output=$tmpf.dma \
              logfile=/dev/null  <$dem

echo "rotating binary data"
m.rot90 input=$tmpf.dma output=$tmpf.rot rows=1201 cols=1201 bpc=2 >/dev/null

/bin/rm -f $tmpf.dma

echo "creating raster $rast from binary data"
r.in.bin input=$tmpf.rot output=$rast title=$rast bytes=2 \
	north=$north south=$south east=$east west=$west r=1201 c=1201

/bin/rm -f $tmpf.rot
echo "done"

exit 0

