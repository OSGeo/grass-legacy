#! /bin/sh
#
# Test d.auto.viewproj
#
# Loops through the view projections.
# Draws a vector map and a grid.
# Also draws a raster map if there is an argument.
#
# Written by Bev Wallace, beverly.t.wallace@lmco.com, February 2003
#

if test "$GISBASE" = ""; then
 echo "You must be in GRASS GIS to run this program." >&2
 exit 1
fi   

# plist is from the proj parameter options list (change commas to spaces)
plist='Mercator TransverseMercator UniversalTransverseMercator CentralCylindrical Miller LambertCylindricalEqualArea Behrmann GallsOrthographic GallStereographic TransverseCylindricalEqualArea EquidistantCylindrical PlateCaree Cassini Sinusoidal Mollweide Robinson EckertI EckertII EckertIII EckertIV EckertV EckertVI GoodeHomolosine HatanoAsymmetricalEqualArea Loximuthal McBrydeThomasFlatPolarParabolic McBrydeThomasFlatPolarQuartic McBrydeThomasFlatPolarSinusoidal PutninsP2 PutninsP5 WinkelI Collingnon CrasterParabolic LambertConformalConic EquidistantConic AlbersEqualArea LambertEqualArea LambertEqualAreaSouth PolyconicAmerican Gnomonic Orthographic NearSidedPerspective LambertAzimuthalEqualArea VanDerGrintenI'

echo
echo The world centered at lon=-90 lat=0
echo
for i in $plist
do
	echo $i
	g.region n=90 s=-90 w=90 e=450
	d.erase
	d.auto.viewproj projname=$i
	d.mon.viewproj
	if [ $# -gt 0 ]
	then
		d.grid.viewproj size=10 color=gray
		d.rast.viewproj map=nations
	fi
	d.vect.viewproj map=coastlines color=green
	d.grid.viewproj size=10 color=gray
	echo $i > text.tmp
	d.text color=yellow < text.tmp
	echo
	sleep 4
done

echo
echo The northern hemisphere centered at lon=0 lat=45 or lat=90 
echo
for i in $plist
do
	echo $i
	g.region n=90 s=0 w=-180 e=180
	d.erase
	d.auto.viewproj projname=$i
	d.mon.viewproj
	if [ $# -gt 0 ]
	then
		d.grid.viewproj size=10 color=gray
		d.rast.viewproj map=nations
	fi
	d.vect.viewproj map=coastlines color=green
	d.grid.viewproj size=10 color=gray
	echo $i > text.tmp
	d.text color=yellow < text.tmp
	echo
	sleep 4
done

echo
echo The north western quadrant centered at lon=-90 lat=45
echo
for i in $plist
do
	echo $i
	g.region n=90 s=0 w=180 e=360
	d.erase
	d.auto.viewproj projname=$i
	d.mon.viewproj
	if [ $# -gt 0 ]
	then
		d.grid.viewproj size=10 color=gray
		d.rast.viewproj map=nations
	fi
	d.vect.viewproj map=coastlines color=green
	d.grid.viewproj size=10 color=gray
	echo $i > text.tmp
	d.text color=yellow < text.tmp
	echo
	sleep 4
done

rm text.tmp
echo
echo Done
