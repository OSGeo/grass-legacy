:
# 1/2001 fix for NULL by David Finlayson <david_finlayson@yahoo.com>
# 11/99 updated $ewres to ewres() and $nsres to nsres()
#       updated number to FP in r.mapcalc statement Markus Neteler

# set nsres and ewres
#eval `g.region -g`

echo ""
echo Please provide the altitude of the sun in degrees above the
echo horizon and the azimuth of the sun in degrees to the east of
echo 'north (N:0 E:90 S:180 W:270)'
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
	echo -n "azimuth: "
	read az
	if test $az -ge 0 -a $az -lt 360
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

az=`expr $az - 90`


echo ""
echo Running r.mapcalc, please stand by.
echo Your new map will be named shade.  Please consider renaming.
echo ""

# Note: no space allowed after \\:
r.mapcalc << EOF
shade = eval( \\
 x=($elev[-1,-1] + 2*$elev[0,-1] + $elev[1,-1] \\
   -$elev[-1,1] - 2*$elev[0,1] - $elev[1,1])/(8.*ewres()) , \\
 y=($elev[-1,-1] + 2*$elev[-1,0] + $elev[-1,1] \\
   -$elev[1,-1] - 2*$elev[1,0] - $elev[1,1])/(8.*nsres()) , \\
 slope=90.-atan(sqrt(x*x + y*y)), \\
 a=round(atan(x,y)), \\
 a=if(isnull(a),1,a), \\
 aspect=if(x||y,if(a,a,360.)), \\
 cang = sin($alt)*sin(slope) + cos($alt)*cos(slope) * cos($az-aspect), \\
 if(cang < 0.,0.,100.*cang), \\
 if(isnull(cang), 22, 100.*cang))
EOF

r.colors shade color=grey

echo ""
echo shade.relief map created and named shade.  Consider renaming
