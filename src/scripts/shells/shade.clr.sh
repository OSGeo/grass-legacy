#!/bin/sh
# 10/2001 fix for testing for dashes in raster file name
#        by Andreas Lange <andreas.lange@rhein-main.de>
# 10/2001 added parser support - Markus Neteler
# updated $ewres to ewres() and $nsres to nsres() 11/99
# updated number to FP in r.mapcalc statement

# set nsres and ewres
# eval `g.region -g`
PROG=`basename $0`

if [ ! "$GISBASE" ]
then
	echo
	echo "ERROR: must be in GRASS to use $PROG"
	echo
	exit 1
fi

if [ "$1" = "-help" -o "$1" = "help" -o "$1" = "-h" -o "$1" = "--help" ]
then
        echo
        echo Please provide the altitude of the sun in degrees above the
        echo horizon and the azimuth of the red, green, and blue lights
        echo in degrees to the east of 'north (N:0 E:90 S:180 W:270)'
        echo Might we suggest 60 for red, 180 for green and 300 for blue.
        echo
        echo Usage:
        echo      shade.clr.sh [altitude=value] [r_azimuth=value] [g_azimuth=value] [b_azimuth=value] [elevation=name]
	echo
	exit 1
fi

gotitALT=0
gotitRAZ=0
gotitGAZ=0
gotitBAZ=0
gotitELEV=0

for i
do
	case $i in
		altitude=*)
			alt=`echo $i | awk -F '=' '{print $2}'` ; gotitALT=1;;
		r_azimuth=*)
			raz=`echo $i | awk -F '=' '{print $2}'` ; gotitRAZ=1;;
		g_azimuth=*)
			gaz=`echo $i | awk -F '=' '{print $2}'` ; gotitGAZ=1;;
		b_azimuth=*)
			baz=`echo $i | awk -F '=' '{print $2}'` ; gotitBAZ=1;;
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

	esac
done


echo ""
echo Please provide the altitude of the sun in degrees above the
echo horizon and the azimuth of the red, green, and blue lights
echo in degrees to the east of 'north (N:0 E:90 S:180 W:270)'
echo Might we suggest 60 for red, 180 for green and 300 for blue.
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


while test $gotitRAZ -eq 0
do
	echo -n "red light azimuth: "
	read raz
	if test $raz -ge 0 -a $raz -lt 360
	then
		gotitRAZ=1
	else
		echo Sorry, azimuth must be greater than -1 and less than 360
	fi
done

if test $raz -ge 0 -a $raz -lt 360
then
	gotitRAZ=1
else
	echo Sorry, azimuth must be greater than -1 and less than 360
	exit 1
fi

while test $gotitGAZ -eq 0
do
	echo -n "green light azimuth: "
	read gaz
	if test $gaz -ge 0 -a $gaz -lt 360
	then
		gotitGAZ=1
	else
		echo Sorry, azimuth must be greater than -1 and less than 360
	fi
done

if test $gaz -ge 0 -a $gaz -lt 360
then
	gotitGAZ=1
else
	echo Sorry, azimuth must be greater than -1 and less than 360
	exit 1
fi

while test $gotitBAZ -eq 0
do
	echo -n "blue light azimuth: "
	read baz
	if test $baz -ge 0 -a $baz -lt 360
	then
		gotitBAZ=1
	else
		echo Sorry, azimuth must be greater than -1 and less than 360
	fi
done

if test $baz -ge 0 -a $baz -lt 360
then
	gotitBAZ=1
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

echo Using altitude:$alt  r_azimuth:$raz g_azimuth:$gaz b_azimuth:$baz  elevation:$elev

#correct azimuths to East (GRASS convention):
raz=`expr $raz - 90`
gaz=`expr $gaz - 90`
baz=`expr $baz - 90`

echo ""
echo Running r.mapcalc, please stand by.
echo Your new map will be named $ELEV.shade. Please consider renaming.
echo ""

r.mapcalc << EOF
$ELEV.shade = eval( \\
 x=($elev[-1,-1] + 2.*$elev[0,-1] + $elev[1,-1] \\
   -$elev[-1,1] - 2.*$elev[0,1] - $elev[1,1])/(8.* ewres()) , \\
 y=($elev[-1,-1] + 2.*$elev[-1,0] + $elev[-1,1] \\
   -$elev[1,-1] - 2.*$elev[1,0] - $elev[1,1])/(8.* nsres()) , \\
 slope=90.-atan(sqrt(x*x + y*y)), \\
 a=round(atan(x,y)), \\
 aspect=if(x||y,if(a,a,360.)), \\
 rang = sin($alt)*sin(slope) + cos($alt)*cos(slope) * cos($raz-aspect), \\
 red = int(if(rang < 0.,0.,4.9*rang)), \\
 gang = sin($alt)*sin(slope) + cos($alt)*cos(slope) * cos($gaz-aspect), \\
 green = int(if(gang < 0.,0.,4.9*gang)), \\
 bang = sin($alt)*sin(slope) + cos($alt)*cos(slope) * cos($baz-aspect), \\
 blue = int(if(bang < 0.,0.,4.9*bang)), \\
 1. + red + 5. * green + 25. * blue )
EOF

# needed?
# r.colors $ELEV.shade color=grey
r.colors $ELEV.shade color=rules 2>&1 > /dev/null << EOF
1     0   0   0
5   255   0   0
6     0  64   0
10  255  64   0
11    0 128   0
15  255 128   0
16    0 192   0
20  255 192   0
21    0 255   0
25  255 255   0
26    0   0  64
30  255   0  64
31    0  64  64
35  255  64  64
36    0 128  64
40  255 128  64
41    0 192  64
45  255 192  64
46    0 255  64
50  255 255  64
51    0   0 128
55  255   0 128
56    0  64 128
60  255  64 128
61    0 128 128
65  255 128 128
66    0 192 128
70  255 192 128
71    0 255 128
75  255 255 128
76    0   0 192
80  255   0 192
81    0  64 192
85  255  64 192
86    0 128 192
90  255 128 192
91    0 192 192
95  255 192 192
96    0 255 192
100 255 255 192
101   0   0 255
105 255   0 255
106   0  64 255
110 255  64 255
111   0 128 255
115 255 128 255
116   0 192 255
120 255 192 255
121   0 255 255
125 255 255 255
EOF

echo ""
echo "Shaded relief map created and named $ELEV.shade. Consider renaming.

