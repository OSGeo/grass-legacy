#!/bin/csh
#
#  This C shell calls GRASS function r.reclass
#  to generate maps using the rules generated by 
#  critical area analysis.
#
#  This shell requires 3 input parameters.  The first
#  is the path name for the selected simulation.  The
#  second is the map groups the user wants to display.
#  $2=2  shows the field averaged contamination 
#  severeness and an highlighted cell
#  map overlaying over the field averaged contamination
#  severeness map.  The third is the name of the 
#  pesticide.   
#
####################################################
# Parameters: $1 is mapflag (map type)
#             $2 is the cell size desired for
#                purposes of naming the cell_num.map
#                file, e.g. cell_num.map.100
#             $3 is the name of the pesticide
#####################################################

#  Set correct GIS environment 

   setenv GISRC $HOME/.grassrc 

# 
# 
#  Generate maps 
#
#  FIRST, SET THE RESOLUTION IN THE GRASS MONITOR ITSELF
#
   g.region res=$2
   d.frame -e
#
   
   r.reclass input=cell_num.map.$2 output=pestrun.map <pestrun.rules
   
        d.frame -e
        d.frame -c frame=f1 at=15,100,0,100
        d.frame -c frame=f2 at=0,15,50,100
        d.frame -c frame=f3 at=0,15,0,50

#  Make a temporary directory if it does not exist

        if ( ! -d $HOME/tmp ) mkdir $HOME/tmp

 
        d.frame -s frame=f3
        echo "$3 runoff" |d.text size=20 color=red line=2
        echo "in lbs/acre " |d.text size=20 color=yellow line=4
        psu_agnps_max_cat.sh pestrun.map
        d.frame -s frame=f1
        d.rast map=pestrun.map.reclass
        d.frame -s frame=f2
        d.legend map=pestrun.map.reclass lines=4

 
if ( -f $HOME/tmp/tmp.rule ) rm $HOME/tmp/tmp.rule
#Show the numbers of the cell map.  ADDED BY MIKE FOSTER
# 3-31-96.
 
     d.frame -s frame=f1 
     d.rast.num map=cell_num.map.$2 grid=black
