#!/bin/sh
# r.to.gnuplot
# Author: James Darrell McCauley, Purdue University
# GRASS-GRID> r.to.gnuplot raster_file > data
# GRASS-GRID> d.mon sta=x0
# GRASS-GRID> g.gnuplot
# gnuplot> set parametric
# gnuplot> set contour base
# gnuplot> set nosurface
# gnuplot> set view 180,0
# gnuplot> splot 'data' notitle with lines

cmd=`echo $0 | sed -e 's:.*/::'`
 
if [ $# -lt 1 ]
then
        echo Usage: $cmd raster_map
        echo
        echo " or   $cmd help"
        exit 1
fi
 
if [ $1 = "help" ]
then
        echo Usage: $cmd raster_map
        echo
        echo $cmd is use to convert a GRASS map into a format suitable for
  	echo plotting with g.gnuplot. For example:
	echo " GRASS-GRID> r.to.gnuplot raster_map > data"
	echo " GRASS-GRID> d.mon sta=x0"
	echo " GRASS-GRID> g.gnuplot"
	echo " gnuplot> set parametric"
	echo " gnuplot> set contour base"
	echo " gnuplot> set nosurface"
	echo " gnuplot> set view 180,0"
	echo " gnuplot> splot 'data' notitle with lines"
        exit 1
fi

r.stats -1x $1 | AWK '$2 != row{print "";row=$2}{print}'
