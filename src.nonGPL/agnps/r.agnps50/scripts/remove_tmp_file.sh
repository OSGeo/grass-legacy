#!/bin/sh
#
#
#   This Bourne shell will remove the temporary
#   files created by GRASS/AGNPS interface.
#
#                        Zhian Li, July, 1995
#

   rm -f fdlot.dat
   rm -f nfdlot.dat
#  CHANGE BY MIKE FOSTER 10-9-95
#   rm cell_id.out
#
   rm -f cell.out   
#
# ADDITIONAL CHANGES BY MIKE FOSTER 10-17-95
#
# MIKE FOSTER 10-18-95
#
# Remove the remaining temp maps
#
#
# First save temp_cell_num for use later in the AGNPS 
# output viewer.

#  Retrieve the current mapset resolution from temp_file for use in the
#  cell_num.map.* name
# 
   $GISBASE/etc/agnps50/copy_cell_num.sh
#
#   g.copy rast=temp_cell_num,cell_num.map >>tmpoutfile
   g.remove temp_cell_num >>tmpoutfile
#
   g.remove temp_grid_map >>tmpoutfile
