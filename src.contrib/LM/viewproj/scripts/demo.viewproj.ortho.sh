#!/bin/sh

# Demonstrate viewproj
# Written by Bev Wallace, beverly.t.wallace@lmco.com, January 2003

if test "$GISBASE" = ""; then
 echo "You must be in GRASS GIS to run this program." >&2
 exit 1
fi   

# Set the region to one hemisphere
g.region n=90 s=-90 e=90 w=-90
d.erase

# Use the orthographic projection
# GRASS 5.0.3 needs ellps=wgs84
#    d.set.viewproj proj=ortho ellps=wgs84
# GRASS 5.1 needs ellps=WGS84
#    d.set.viewproj proj=ortho ellps=WGS84
# So use the d.auto.viewproj instead of d.set.viewproj
d.auto.viewproj projname=Orthographic

# Update the monitor
d.mon.viewproj

# Draw a raster map in the projection
d.rast.viewproj map=nations

# Draw a vector map in the projection
d.vect.viewproj map=coastlines color=brown

