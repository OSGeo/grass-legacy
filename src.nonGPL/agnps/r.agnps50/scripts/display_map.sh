#!/bin/csh
#
# The way to run this script is to type
#
# display_map.sh rast=mapname res=cellsize (a number)
#
# for example
#
# display_map.sh rast=kiser.asp.111 res=111
#
 
d.erase
g.region rast=$1 res=$2
d.rast $1
