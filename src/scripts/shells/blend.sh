:

if [ $# != 4 ] ; then
    echo 'Usage: blend.sh file1 file2 perc outbase'
    echo ''
    echo 'This program will combine the color components of'
    echo 'two raster files by a given percentage of file1'
    echo 'creating 3 new files representing the R,G, and B'
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

r.mapcalc "$out.r = r#$first * .$perc + (1.0 - .$perc) * r#$second"
r.mapcalc "$out.g = g#$first * .$perc + (1.0 - .$perc) * g#$second"
r.mapcalc "$out.b = b#$first * .$perc + (1.0 - .$perc) * b#$second"

exit 0
