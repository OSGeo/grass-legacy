#!/bin/sh
#  This shell is used to get the cell ID for
#  the grass/agnps interface 
#
#                Zhian Li, July, 1995 
#

# run d.what.rast once
d.what.rast -1t map=temp_cell_num,ansi.wshd > cell_id.out
awk 'BEGIN {FS=":"} NR==3 {print $2}' cell_id.out >cell.id

# draw a black x on the clicked field
awk 'BEGIN {FS=":"} NR==2 {print $1, $2}' cell_id.out | d.points type=x color=black

