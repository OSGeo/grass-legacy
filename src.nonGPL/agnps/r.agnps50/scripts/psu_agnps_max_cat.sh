#!/bin/sh
#
#   This script is used to get the value of
#   the maximum category of the contaminant
#   load map generated based on the cell
#   contaminant load in percentages of the
#   threshold value of the critical area
#
#               Zhian Li, June, 1995 
#
############################################
# CHANGED BY MIKE FOSTER FOR PSU_AGNPS MARCH 11, 1996
############################################  

############################################
# Eliminated space-character in awk-command
# by Markus Neteler 19.12.96 
###########################################


#  Save the current directory in which you are executing
   
   EXECDIR=$PWD
   export EXECDIR
#  Switch to the correct map set
        cd $LOCATION/cats
        awk 'BEGIN {FS=" "} NR==1 {print "maxcat="$2}' $1 >$HOME/tmp/max_cat
        . $HOME/tmp/max_cat
        echo "the value for maxcat is $maxcat"

#  Return to the current directory
   
   cd $EXECDIR

#  Make the reclass rules for the load map
#        
   $GISBASE/etc/agnps50/make_display_rules.pl $maxcat
#
#  Reclass the map using the displayrules file
#
   r.reclass input=$1 output=$1.reclass <$EXECDIR/displayrules
#
