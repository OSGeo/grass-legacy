:

# set nsres and ewres
eval `g.region -g`

echo ""
echo Please provide the altitude of the sun in degrees above the
echo horizon and the azimuth of the red, green, and blue lights
echo in degrees to the east of 'north (N:0 E:90 S:180 W:270)'
echo Might we suggest 60 for red, 180 for green and 300 for blue.
echo ""

gotit=0
while test $gotit -eq 0
do
	echo -n "altitude: "
	read alt
	if test $alt -gt 0 -a $alt -lt 90
	then
		gotit=1
	else
		echo Sorry, altitude must be greater than 0 and less than 90
	fi
done

gotit=0
while test $gotit -eq 0
do
	echo -n "red light azimuth: "
	read raz
	if test $raz -ge 0 -a $raz -lt 360
	then
		gotit=1
	else
		echo Sorry, azimuth must be greater than -1  and less than 360
	fi
done

gotit=0
while test $gotit -eq 0
do
	echo -n "green light azimuth: "
	read gaz
	if test $gaz -ge 0 -a $gaz -lt 360
	then
		gotit=1
	else
		echo Sorry, azimuth must be greater than -1  and less than 360
	fi
done

gotit=0
while test $gotit -eq 0
do
	echo -n "blue light azimuth: "
	read baz
	if test $baz -ge 0 -a $baz -lt 360
	then
		gotit=1
	else
		echo Sorry, azimuth must be greater than -1  and less than 360
	fi
done

echo ""
g.ask type=old element=cell desc=raster prompt="Enter elevation file" unixfile=/tmp/$$
eval `cat /tmp/$$`
rm -f /tmp/$$
if [ ! "$file" ]
then
    exit 0
fi
elev="${fullname}"

echo "$elev"

raz=`expr $raz - 90`
gaz=`expr $gaz - 90`
baz=`expr $baz - 90`

echo ""
echo Running r.mapcalc, please stand by.
echo Your new map will be named shade.  Please consider renaming.
echo ""

r.mapcalc << EOF
shade = eval( \\
 x=($elev[-1,-1] + 2*$elev[0,-1] + $elev[1,-1] \\
   -$elev[-1,1] - 2*$elev[0,1] - $elev[1,1])/(8.*$ewres) , \\
 y=($elev[-1,-1] + 2*$elev[-1,0] + $elev[-1,1] \\
   -$elev[1,-1] - 2*$elev[1,0] - $elev[1,1])/(8.*$nsres) , \\
 slope=90.-atan(sqrt(x*x + y*y)), \\
 a=round(atan(x,y)), \\
 aspect=if(x||y,if(a,a,360)), \\
 rang = sin($alt)*sin(slope) + cos($alt)*cos(slope) * cos($raz-aspect), \\
 red = int(if(rang < 0,0,4.9*rang)), \\
 gang = sin($alt)*sin(slope) + cos($alt)*cos(slope) * cos($gaz-aspect), \\
 green = int(if(gang < 0,0,4.9*gang)), \\
 bang = sin($alt)*sin(slope) + cos($alt)*cos(slope) * cos($baz-aspect), \\
 blue = int(if(bang < 0,0,4.9*bang)), \\
 1 + red + 5 * green + 25 * blue )
EOF

r.colors shade color=grey

echo ""
echo shade.relief map created and named shade.  Consider renaming
r.colors shade color=rules 2>&1 > /dev/null << EOF
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
