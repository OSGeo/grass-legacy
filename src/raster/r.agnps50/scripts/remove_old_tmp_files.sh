#!/bin/sh
#
#
#   This Bourne shell will remove the temporary
#   files created by GRASS/AGNPS interface.
#   if any were not removed yet for some reason
#
#         Mike Foster, PSU, October 1995
#
   rm -f fdlot.dat
   rm -f nfdlot.dat
   rm -f cell.out 
   rm -f temp_reclass_rule  
   g.remove temp_cell_num
   g.remove temp_grid_map
   g.remove temp_elv.map
   g.remove work_elev.map
   g.remove intermittent.map
#
