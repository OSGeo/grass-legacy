#!/bin/sh

# 6/2003 fixes for Lat/Long Gordon Keith <gordon.keith@csiro.au>
#   If n is a number then the ewres and nsres are mulitplied by that scale
#    to calculate the shading.
#   If n is the letter M (either case) the number of metres is degree of
#    latitude is used as the scale.
#   If n is the letter f then the number of feet in a degree is used.
#   It scales latitude and longitude equally, so it's only approximately
#   right, but for shading its close enough. It makes the difference
#   between an unusable and usable shade.
#
# 10/2001 fix for testing for dashes in raster file name
#        by Andreas Lange <andreas.lange@rhein-main.de>
# 10/2001 added parser support - Markus Neteler
# 9/2001 fix to keep NULLs as is (was value 22 before) - Markus Neteler
# 1/2001 fix for NULL by David Finlayson <david_finlayson@yahoo.com>
# 11/99 updated $ewres to ewres() and $nsres to nsres()
#       updated number to FP in r.mapcalc statement Markus Neteler

if test "$GISBASE" = ""; then
 echo "You must be in GRASS GIS to run this program." >&2
 exit 1
fi   

# set nsres and ewres
#eval `g.region -g`
PROG=`basename $0`

if [ "$1" = "-help" -o "$1" = "help" -o "$1" = "-h" -o "$1" = "--help" ]
then
        echo
        echo Please provide the altitude of the sun in degrees above the
        echo horizon and the azimuth of the sun in degrees to the east of
        echo 'north (N:0 E:90 S:180 W:270)'
        echo
        echo Usage:
        echo "     shade.rel.sh [altitude=value] [azimuth=value] [elevation=name] [scale=value|m|f]"
	echo
	exit 1
fi

gotitALT=0
gotitAZ=0
gotitELEV=0
scale=1

for i
do
	case $i in
		altitude=*)
			alt=`echo $i | awk -F '=' '{print $2}'` ; gotitALT=1;;
		azimuth=*)
			az=`echo $i | awk -F '=' '{print $2}'` ; gotitAZ=1;;
		elevation=*)
			elev=`echo $i | awk -F '=' '{print $2}'` ;
                                    gotitELEV=1;
                                    eval `g.findfile element=cell file=$elev` ;
                                    elev="${fullname}" ;
                                    ELEV="${name}" ;
                                    if [ "$elev" = "" ] ; then
                                       echo "ERROR: raster map [`echo $i | awk -F '=' '{print $2}'`] does not exist."
                                       exit 1
                                    fi ;;
		scale=*)
			scale=`echo $i | awk -F '=' '{print $2}'` ;
			case $scale in
			        "m"|"M")
			                scale=111120;;
        			"f"|"F")
                			scale=370400;;
			esac;;
	esac
done



echo ""
echo Please provide the altitude of the sun in degrees above the
echo horizon and the azimuth of the sun in degrees to the east of
echo 'north (N:0 E:90 S:180 W:270)'
echo ""

while test $gotitALT -eq 0
do
	echo -n "altitude: "
	read alt
	if test $alt -gt 0 -a $alt -lt 90
	then
		gotitALT=1
	else
		echo Sorry, altitude must be greater than 0 and less than 90
	fi
done

if test $alt -gt 0 -a $alt -lt 90
then
	gotitALT=1
else
	echo Sorry, altitude must be greater than 0 and less than 90
        exit 1
fi

while test $gotitAZ -eq 0
do
	echo -n "azimuth: "
	read az
	if test $az -ge 0 -a $az -lt 360
	then
		gotitAZ=1
	else
		echo Sorry, azimuth must be greater than -1 and less than 360
	fi
done

if test $az -ge 0 -a $az -lt 360
then
	gotitAZ=1
else
	echo Sorry, azimuth must be greater than -1 and less than 360
	exit 1
fi

while test $gotitELEV -eq 0 
do
echo ""
g.ask type=old element=cell desc=raster prompt="Enter elevation file" unixfile=/tmp/$$
eval `cat /tmp/$$`
rm -f /tmp/$$
if [ ! "$file" ]
then
    exit 0
fi
elev="${fullname}"
ELEV="${name}"
echo "$elev"
gotitELEV=1
done

elev2=`echo $elev | sed -e "s/-//g"`
if [ "$elev" != "$elev2" ] ; then
    echo "Name of raster map ($elev) contains one or more \"-\" (dash(es)), "
    echo "which is not allowed! Please rename raster map before using $PROG."
    echo "Exiting."
    exit 1
fi

echo Using altitude:$alt  azimuth:$az   elevation:$elev

#correct azimuth to East (GRASS convention):
az=`expr $az - 90`


echo ""
echo Running r.mapcalc, please stand by.
echo Your new map will be named $ELEV.relshade.  Please consider renaming.
echo ""

# Note: no space allowed after \\:
r.mapcalc << EOF
$ELEV.relshade = eval( \\
 x=($elev[-1,-1] + 2*$elev[0,-1] + $elev[1,-1] \\
   -$elev[-1,1] - 2*$elev[0,1] - $elev[1,1])/(8.*ewres()*$scale) , \\
 y=($elev[-1,-1] + 2*$elev[-1,0] + $elev[-1,1] \\
   -$elev[1,-1] - 2*$elev[1,0] - $elev[1,1])/(8.*nsres()*$scale) , \\
 slope=90.-atan(sqrt(x*x + y*y)), \\
 a=round(atan(x,y)), \\
 a=if(isnull(a),1,a), \\
 aspect=if(x!=0||y!=0,if(a,a,360.)), \\
 cang = sin($alt)*sin(slope) + cos($alt)*cos(slope) * cos($az-aspect), \\
 if(cang < 0.,0.,100.*cang), \\
 if(isnull(cang), null(), 100.*cang))
EOF

r.colors $ELEV.relshade color=grey

echo ""
echo "Shaded relief map created and named $ELEV.relshade. Consider renaming."
