#! /bin/sh
#
# r.proj 
#    copies a cell-map from one GRASS location to another
#    using the PROJ_INFO parameters to re-project the map
#    and the current region settings to determine the location, size and resolution
#
#	The user must have write permissions in a couple of places
#
#	r.proj is a script:  it uses 
#	Gerry Evenden's PROJ program to convert the target cell-locations to 
#		the source projection
#	Darrell McCauley's s.sample program to resample the source map at the 
#		target cell-locations; 
#		defaults to nearest-neighbour resampling 
#			(appropriate for "category" maps)
#		but can also use bilinear or cubic interpolation 
#			(appropriate for "continuous" maps)
#
#	Only tested for a limited range of projections.  
#	The intermediate file is ascii so can be very large.
#	The s.sample stage takes a while (!) if the map is large.
#
#	Version 1.0   Simon Cox	9 Jan 1996
#
#	simon@ned.dem.csiro.au
#	CSIRO Exploration & Mining
#	PO Box 437,  Nedlands,  WA  6009  Australia
#
#	(c) Simon Cox and CSIRO Exploration & Mining
#

: ${GISBASE?}

usage()
{
echo "Usage: r.proj [-B|C] -m source_map -l source_location" >&2; 
echo "	[-o target_map] [-s source_mapset] [-d source_gisdbase]" >&2;
echo >&2;
echo "	[-B|C] sets interpolation method used" >&2;
echo "		B=bilinear, C=Cubic, default=nearest-neighbour" >&2;
}

# set up local environment name so that it doesn't interfere 
# with interactive use while r.proj is running 

cp $GISRC rproj.grassrc
GISRC=rproj.grassrc
export GISRC

# in target location
####################

# read environment parameters

eval `g.gisenv`
OUTDBASE=$GISDBASE
OUTLOC=$LOCATION_NAME
OUTSET=$MAPSET
OUTLOCATION=$OUTDBASE/$OUTLOC/$OUTSET
cp $GISRC out.grassrc

# read source map parameters & set up source environment file
# Use command getopt to parse the command line arguments

set -- `getopt HhBCd:l:s:m:o: $*`
while [ $1 != -- ]
do
  case $1 in
    -H|-h) usage 
         exit 0;;
    -B|-b) method="-B";;
    -C|-c) method="-C";;
    -d) INDBASE=$2
       shift;;
    -l) INLOC=$2
       shift;;
    -s) INSET=$2
       shift;;
    -m) MAP=$2
       shift;;
    -o) OMAP=$2
       shift;;
    *) usage
       exit 2;;
  esac
  shift	# next flag
done
shift	# skip double dash

if [ ${MAP:=X} = "X" ]
then
usage
exit 2
fi
: ${OMAP:=$MAP}

if [ ${INLOC:=X} = "X" ]
then
usage
exit 2
fi

: ${method=""}
: ${INDBASE:=$OUTDBASE}
: ${INSET:=$OUTSET}
echo "GISDBASE: "$INDBASE > in.grassrc
echo "LOCATION_NAME: "$INLOC >> in.grassrc
echo "MAPSET: "$INSET >> in.grassrc
INLOCATION=$INDBASE/$INLOC/$INSET

if [ ! -f $INLOCATION/cell/$MAP ]
then
echo "Source map $INLOCATION/cell/$MAP does not exist"
exit 4
fi

# in target location
####################

# write target map header

mkdir $OUTLOCATION/cell 2> /dev/null
g.region -p |grep north > $OUTLOCATION/cell/$OMAP.asc
g.region -p |grep south >> $OUTLOCATION/cell/$OMAP.asc
g.region -p |grep east >> $OUTLOCATION/cell/$OMAP.asc
g.region -p |grep west >> $OUTLOCATION/cell/$OMAP.asc
g.region -p |grep rows >> $OUTLOCATION/cell/$OMAP.asc
g.region -p |grep cols >> $OUTLOCATION/cell/$OMAP.asc

# find a raster map in the target location
# write list of target grid-cell locations 

mkdir $OUTLOCATION/site_lists 2> /dev/null
cellname=`ls -1 $OUTLOCATION/cell |head -1`
if [ $cellname ]
then
r.stats -1g input=$cellname > $OUTLOCATION/site_lists/cell_locations
else
r.mapcalc rproj.tmp='1' 
r.stats -1g input=rproj.tmp > $OUTLOCATION/site_lists/cell_locations
g.remove rast=rproj.tmp
fi

#echo "done r.stats"

# discover target projection info

eval `set | awk -F'=' '/(^lat|^lon)/{print "unset",$1}'`
eval `awk -F': ' '{print $1"="$2}' $GISDBASE/$LOCATION_NAME/PERMANENT/PROJ_INFO`
eval `awk -F': ' '{print $1"="$2}' $GISDBASE/$LOCATION_NAME/PERMANENT/PROJ_UNITS`
if [ "$proj" = "merc" ]
then
	lat_0=$lat_ts
	unset lat_ts
fi
if [ "$proj" = "utm" ]
then
	if [ "$south" = "defined" ]
	then
		params="+south"
		unset south
	fi
fi
params2=`set | awk '/(^lat|^lon|^zone)/{printf ("+%s ",$0)}'`
params="$params $params2"

# convert target grid-cell locations to lat-long

if [ "$proj" = "ll" ]
then
cp $OUTLOCATION/site_lists/cell_locations $OUTLOCATION/site_lists/cell_locations_ll
else
proj -I -f '%.5f' +proj=$proj +a=$a +es=$es $params $OUTLOCATION/site_lists/cell_locations > $OUTLOCATION/site_lists/cell_locations_ll
fi

#echo "done first of two proj"

# move to source location 
######################### 

# set up environment

cp in.grassrc $GISRC
eval `g.gisenv`

# discover source projection info

eval `set | awk -F'=' '/(^lat|^lon)/{print "unset",$1}'`
eval `awk -F': ' '{print $1"="$2}' $GISDBASE/$LOCATION_NAME/PERMANENT/PROJ_INFO`
eval `awk -F': ' '{print $1"="$2}' $GISDBASE/$LOCATION_NAME/PERMANENT/PROJ_UNITS`
if [ "$proj" = "merc" ]
then
	lat_0=$lat_ts
	unset lat_ts
fi
if [ "$proj" = "utm" ]
then
	if [ "$south" = "defined" ]
	then
		params="+south"
		unset south
	fi
fi
params2=`set | awk '/(^lat|^lon|^zone)/{printf ("+%s ",$0)}'`
params="$params $params2"

# convert target grid-cell locations to site-list in source projection

mkdir $INLOCATION/site_lists 2> /dev/null
if [ "$proj" = "ll" ]
then
awk '{print $1"|"$2"|"$3}' $OUTLOCATION/site_lists/cell_locations_ll > $INLOCATION/site_lists/cell_locations
else
proj +proj=$proj +a=$a +es=$es $params $OUTLOCATION/site_lists/cell_locations_ll | awk '{print $1"|"$2"|"$3}' > $INLOCATION/site_lists/cell_locations
fi

#echo "done second of two proj"

# sample source map at target grid-cell locations
##### need option to set sampling method

g.region rast=$MAP
s.sample $method input=cell_locations rast=$MAP | awk -F'|' '{print int($3)}' >> $OUTLOCATION/cell/$OMAP.asc

#echo "done s.sample"

# move to target location
#########################

cp out.grassrc $GISRC
eval `g.gisenv`

# load new cell map

r.in.ascii input=$OUTLOCATION/cell/$OMAP.asc output=$OMAP
cp $INLOCATION/colr/$MAP $OUTLOCATION/colr/$OMAP
cp $INLOCATION/cats/$MAP $OUTLOCATION/cats/$OMAP

# clean up

rm $OUTLOCATION/cell/$OMAP.asc
rm $OUTLOCATION/site_lists/cell_locations
rm $OUTLOCATION/site_lists/cell_locations_ll
rm $INLOCATION/site_lists/cell_locations
rm in.grassrc
rm out.grassrc
rm rproj.grassrc
