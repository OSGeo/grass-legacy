#! /bin/sh
#
# Demonstrate d.auto.viewproj
#
# Draw a raster map, vector map, and grid in a view projection.
# The user will be prompted for the projection.
#
# Written by Bev Wallace, beverly.t.wallace@lmco.com, February 2003
#

if test "$GISBASE" = ""; then
 echo "You must be in GRASS GIS to run this program." >&2
 exit 1
fi   

# The world
g.region n=90 s=-90 w=90 e=450
d.erase

# Adjust for the projection entered by the user
d.auto.viewproj
d.mon.viewproj

# Draw the grid for something to look at while waiting for raster
d.grid.viewproj size=10

# Draw the raster map (may be slow)
d.rast.viewproj map=nations

# Draw the vector map
d.vect.viewproj map=coastlines color=brown

# Draw the grid on top
d.grid.viewproj size=10
