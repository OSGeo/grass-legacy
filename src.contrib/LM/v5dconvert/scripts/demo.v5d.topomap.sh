#!/bin/sh

# Demonstrate GRASS v5dconvert programs
# Written by Bev Wallace, beverly.t.wallace@lmco.com, January 2003

if test "$GISBASE" = ""; then
 echo "You must be in GRASS GIS to run this program." >&2
 exit 1
fi   

# The user must supply a vis5d data file (.v5d) as the argument 
if [ $# != 3 ]
then
	echo Please supply a Vis5d data file and GRASS raster and vector files.
	echo Usage: 
	echo `basename $0` vis5d_data_file.v5d grass_raster_file grass_vector_file
	exit
fi

if [ ! -f $1 ]
then
	echo The input file does not exist
	echo $1
	exit
fi

# Set the GRASS region from the $2 data.
echo
echo g.region rast=$2
g.region rast=$2

# Create a Vis5d topo file named $2.topo from the GRASS $2 data.
echo
echo r.out.v5d.topo input=$2 output=$2.topo
r.out.v5d.topo input=$2 output=$2.topo
ls -l $2.topo

# Create a Vis5d map file named $3.map from the GRASS $3 data.
echo
echo v.out.v5d.map input=$3 output=$3.map
v.out.v5d.map input=$3 output=$3.map
ls -l $3.map

# Display with Vis5d (Vis5d must be in the users path).  
# Vis5d requires full paths to the topo and map data.
echo
echo vis5d -topo $PWD/$2.topo -map $PWD/$3.map $1
echo
echo To see the topo and map data, press the TOPO and MAP buttons.
echo
vis5d -topo $PWD/$2.topo -map $PWD/$3.map $1

# Cleanup
rm $2.topo $3.map
