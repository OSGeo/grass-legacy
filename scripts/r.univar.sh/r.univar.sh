#!/bin/sh

############################################################################
#
# MODULE:	r.univar
# AUTHOR(S):	Markus Neteler. neteler@itc.it
# PURPOSE:	Calculates univariate statistics from a GRASS raster map
# COPYRIGHT:	(C) 1998,2002,2003 by the GRASS Development Team
#
#		This program is free software under the GNU General Public
#		License (>=v2). Read the file COPYING that comes with GRASS
#		for details.
#
#############################################################################

#%Module
#% description: calculates univariate statistics from a GRASS raster map
#%End
#%flag
#%  key: e
#%  description: extended statistics (quartiles and 90th percentile)
#%END
#%option
#% key: map
#% type: string
#% gisprompt: old,cell,raster
#% description: Name of raster map
#% required : yes
#%End

if [ "$1" != "@ARGS_PARSED@" ] ; then
  exec g.parser "$0" "$@"
fi

COVER="$GIS_OPT_map"

TMP="`g.tempfile pid=$$`"
if [ $? -ne 0 ] || [ -z "$TMP" ] ; then
    echo "ERROR: unable to create temporary files" 1>&2
    exit 1
fi

cleanup()
{
 \rm -f $TMP $TMP.sort
}

# what to do in case of user break:
exitprocedure()
{
 echo "User break!"
 cleanup
 exit 1
}
# shell check for user break (signal list: trap -l)
trap "exitprocedure" 2 3 15

echo "Calculation for map $COVER (ignoring NULL cells)..."
echo "Reading raster map..."
r.stats -1n input=$COVER > "$TMP"

#check if map contains only NULL's in current region
LINES=`wc -l "$TMP" | awk '{print $1}'`
if [ "$LINES" -eq 0 ] ; then
 echo ""
 echo "ERROR: Map $COVER contains only NULL data in current region."
 cleanup
 exit 1
fi

# calculate statistics
echo "Calculating statistics..."
cat $TMP | awk 'BEGIN {sum = 0.0 ; sum2 = 0.0} 
NR == 1{min = $1 ; max = $1}
       {sum += $1 ; sum2 += $1 * $1 ; N++}
       {
        if ($1 > max) {max = $1}
        if ($1 < min) {min = $1}
       }
END{
print ""
print "Number of cells (excluding NULL cells):",N
print "Minimum:",min
print "Maximum:",max
print "Range:",max-min
print "Arithmetic mean:",sum/N
print "Variance:",(sum2 - sum*sum/N)/N
print "Standard deviation:",sqrt((sum2 - sum*sum/N)/N)
print "Variation coefficient:",100*(sqrt((sum2 - sum*sum/N)/N))/(sqrt(sum*sum)/N),"%"
}'

if [ $GIS_FLAG_e -eq 1 ] ; then
  #preparations:
  cat $TMP | sort -n > $TMP.sort
  NUMBER=`cat $TMP.sort | wc -l | awk '{print $1}'`
  ODDEVEN=`echo $NUMBER | awk '{print $1%2}'`

  # 0.25 quartile
  QUARTILE=0.25
  QPOS=`echo $NUMBER $QUARTILE | awk '{printf "%d", $1 * $2 + 0.5}'`
  QELEMENT=`head -$QPOS $TMP.sort | tail -1`
  echo "1st Quartile: $QELEMENT"

  #Calculate median
  if [ $ODDEVEN -eq 0 ]
  then
   # even
   EVENMEDIANNUMBER=`expr $NUMBER / 2`
   EVENMEDIANNUMBERPLUSONE=`expr $EVENMEDIANNUMBER + 1`
   # select two numbers
   SELECTEDNUMBERS=`cat $TMP.sort | head -$EVENMEDIANNUMBERPLUSONE | tail -2`
   RESULTEVENMEDIAN=`echo $SELECTEDNUMBERS | awk '{printf "%f", ($1 + $2)/2.0}'`
   echo "Median (even N): $RESULTEVENMEDIAN"
  else
   # odd
   ODDMEDIANNUMBER=`echo $NUMBER | awk '{printf "%d", int($1/2+.5)}'`
   RESULTODDMEDIAN=`cat $TMP.sort | head -$ODDMEDIANNUMBER | tail -1 | awk '{printf "%f", $1}'`
   echo "Median (odd N): $RESULTODDMEDIAN"
  fi


  # 0.75 quartile
  QUARTILE=0.75
  QPOS=`echo $NUMBER $QUARTILE | awk '{printf "%d", $1 * $2 + 0.5}'`
  QELEMENT=`head -$QPOS $TMP.sort | tail -1`
  echo "3rd Quartile: $QELEMENT"

  # 0.90 percentile
  QUARTILE=0.9
  QPOS=`echo $NUMBER $QUARTILE | awk '{printf "%d", $1 * $2 + 0.5}'`
  QELEMENT=`head -$QPOS $TMP.sort | tail -1`
  echo "90th Percentile: $QELEMENT"

fi

cleanup
