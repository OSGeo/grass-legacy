#!/bin/sh

# Demonstrate GRASS v5dconvert programs
# Written by Bev Wallace, beverly.t.wallace@lmco.com, January 2003

if test "$GISBASE" = ""; then
 echo "You must be in GRASS GIS to run this program." >&2
 exit 1
fi   

# The user must supply a vis5d data file (.v5d) as the argument 
if [ $# != 2 ]
then
	echo Please supply a Vis5d data file and a GRASS raster file
	echo Usage: 
	echo `basename $0` vis5d_data_file.v5d grass_raster_file
	exit
fi

if [ ! -f $1 ]
then
	echo The input file does not exist
	echo $1
	exit
fi

# Set the GRASS region resolution from the nations data.
echo
echo g.region rast=$2
g.region rast=$2

# Set the GRASS region extent (NSEW) from the Vis5d data.
echo
echo g.region.v5d -pr $1
g.region.v5d -pr $1

# Create a Vis5d texture file named $2.sgi from the GRASS $2 data.
echo
echo r.out.v5d.texture $2
r.out.v5d.texture $2
ls -l $2.sgi

# Display with Vis5d (Vis5d must be in the users path).  
# Vis5d requires full paths to the topo and map data.
echo
echo vis5d -texture $PWD/$2.sgi $1
echo
echo To see the texture data, press the TEXTURE button.
echo
vis5d -texture $PWD/$2.sgi $1

# Cleanup
rm $2.sgi
