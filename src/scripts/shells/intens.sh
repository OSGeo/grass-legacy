:
#  written by Dave Gerdes  Nov 1991

if [ $# != 4 ] ; then
    echo 'Usage: intent.sh file1 file2 perc outbase'
    echo ''
    echo 'This program is a derivative from blend.sh.  It is designed'
    echo 'for the special case when file2 is an aspect or other gray'
    echo 'scale map.  It avoids the washed out affect of combining'
    echo 'a gray scale map with another map, by using ONLY the colors'
    echo 'from the color map, and using the aspect file to vary only the'
    echo 'intensity creating 3 new files representing the R,G, and B'
    echo 'components of the resultant image.'
    echo ''
    echo 'The files will be named  outbase.r outbase.g outbase.b'
    echo 'and have data values in the range 0-255'
    exit 1
fi
    

first=$1
second=$2
perc=$3
out=$4

if [ $perc -eq 100 ]
then
  perc=1.0
else
  perc=.$perc
fi

r.mapcalc << EOF
$out.r = eval ( \
first = r#$first, \
second = r#$second, \
range = first * $perc, \
base = first *  (1. - $perc), \
gray = second / 255., \
res = range * gray + base, \
if (res > 255, 255, if (res < 0, 0, res)) )
EOF

r.mapcalc << EOF
$out.g = eval ( \
first = g#$first, \
second = g#$second, \
range = first * $perc, \
base = first *  (1. - $perc), \
gray = second / 255., \
res = range * gray + base, \
if (res > 255, 255, if (res < 0, 0, res)) )
EOF

r.mapcalc << EOF
$out.b = eval ( \
first = b#$first, \
second = b#$second, \
range = first * $perc, \
base = first *  (1. - $perc), \
gray = second / 255., \
res = range * gray + base, \
if (res > 255, 255, if (res < 0, 0, res)) )
EOF

exit 0
