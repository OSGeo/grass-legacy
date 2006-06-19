#!/bin/sh

############################################################################
#
# MODULE:       d.polar
# AUTHOR(S):    Markus Neteler. neteler itc.it
#               algorithm by Bruno Caprile
# PURPOSE:      Draws polar diagram of angle map. The outer circle considers
#               all cells in the map. If one or many of them are NULL (no data),
#               the figure will not reach the outer circle. The vector inside
#               indicates the prevalent direction.
# COPYRIGHT:    (C) 2006 by the GRASS Development Team
#
#               This program is free software under the GNU General Public
#               License (>=v2). Read the file COPYING that comes with GRASS
#               for details.
#
#############################################################################

#%Module
#% description: Draws polar diagram of angle map such as aspect or flow directions
#%End
#%option
#% key: map
#% type: string
#% key_desc: name
#% gisprompt: old,cell,raster
#% description: Name of raster angle map
#% required : yes
#%End
#%option
#% key: undef
#% type: double
#% description: Pixel value to be interpreted as undefined (different from NULL)
#% required : no
#%End

if  [ -z "$GISBASE" ] ; then
    echo "You must be in GRASS GIS to run this program."
 exit 1
fi

if [ "$1" != "@ARGS_PARSED@" ] ; then
  exec g.parser "$0" "$@"
fi

PROG=`basename $0`

#### check if we have awk
if [ ! -x "`which awk`" ] ; then
    echo "$PROG: awk required, please install awk or gawk first" 2>&1
    exit 1
fi

#### check if we have xgraph
if [ ! -x "`which xgraph`" ] ; then
    echo "$PROG: xgraph required, please install first (www.xgraph.org)" 2>&1
    exit 1
fi

# setting environment, so that awk works properly in all languages
unset LC_ALL
export LC_NUMERIC=C

TMP="`g.tempfile pid=$$`"
if [ $? -ne 0 ] || [ -z "${TMP}" ] ; then
    echo "ERROR: unable to create temporary files" 1>&2
    exit 1
fi
#TMP=`dirname $TMP`

wordcount()
{
     awk '
     # Print list of word frequencies
     {
         for (i = 1; i <= NF; i++)
             freq[$i]++
	     total++
     }
 
     END {
         for (word in freq)
             printf "%s %d\n", word, freq[word]
     }' $1
}

cleanup()
{
rm -f ${TMP}_binned ${TMP}_binned_radians \
 ${TMP}_newline \
 ${TMP}_occurencies ${TMP}_outercircle ${TMP}_raw \
 ${TMP}_sine_cosine ${TMP}_sine_cosine_replic \
 ${TMP}_vector ${TMP}_x_unit_vector \
 ${TMP}_y_unit_vector ${TMP}_cos_sums ${TMP}_sine_sums
}

#################################
# this file contains everthing:
r.stats -1 "$GIS_OPT_MAP" > ${TMP}_raw
TOTALNUMBER=`wc -l ${TMP}_raw | awk '{print $1}'`

echo "Calculating statistics for polar diagram... (be patient)"

#wipe out NULL data and undef data if defined by user
# - generate degree binned to integer, eliminate NO DATA (NULL):
# change 360 to 0 to close polar diagram:
cat ${TMP}_raw | grep -v '^*$' | grep -v "^${GIS_OPT_UNDEF}$" | \
    awk '{printf "%d\n", int($1 + .5)}' | sed 's+^360+0+g'  > ${TMP}_binned

# make radians
cat ${TMP}_binned | awk '{printf "%f\n", (3.14159265 * $1 ) / 180.}'  > ${TMP}_binned_radians

#################################
# generate numbers for max circle
TOTALVALIDNUMBER=`wc -l ${TMP}_binned_radians | awk '{print $1}'`

if [ $TOTALVALIDNUMBER == 0 ] ; then
   echo "No data pixel found"
   cleanup
   exit 1
fi

#################################
# unit vector on raw data converted to radians without no data:
cat ${TMP}_raw | grep -v '^*' | grep -v "^${GIS_OPT_UNDEF}$" | awk 'BEGIN {sum = 0.0}
NR == 1{}
       {sum += cos(3.14159265 * $1 / 180.)}
END{print sum}' > ${TMP}_cos_sums

cat ${TMP}_raw | grep -v '^*' | grep -v "^${GIS_OPT_UNDEF}$" | awk 'BEGIN {sum = 0.0}
NR == 1{}
       {sum += sin(3.14159265 * $1 / 180.)}
END{print sum}' > ${TMP}_sin_sums

# cos -> x, sin -> y
echo "`cat ${TMP}_cos_sums` $TOTALVALIDNUMBER" | awk '{printf "%.8f\n", $1/$2}' > ${TMP}_x_unit_vector
echo "`cat ${TMP}_sin_sums` $TOTALVALIDNUMBER" | awk '{printf "%.8f\n", $1/$2}' > ${TMP}_y_unit_vector
UNITVECTOR="`paste -d' ' ${TMP}_x_unit_vector ${TMP}_y_unit_vector`"

#################################
# how many are there?:
wordcount ${TMP}_binned_radians | sort -n -t ' ' -k 1 > ${TMP}_occurencies

# find the maximum value
MAXRADIUS="`cat ${TMP}_occurencies | sort -n -t ' ' -k 2 | tail -n 1 | cut -d' ' -f2`"

# now do cos() sin()
cat ${TMP}_occurencies | awk '{printf "%f %f\n", cos($1) * $2 , sin($1) *$2}' > ${TMP}_sine_cosine

# to close the curve, we replicate the first value
REPLICATE=`head -n 1 ${TMP}_sine_cosine`
echo "\"Real data angles"           >  ${TMP}_sine_cosine_replic
cat ${TMP}_sine_cosine >> ${TMP}_sine_cosine_replic
echo $REPLICATE >> ${TMP}_sine_cosine_replic

rm -f ${TMP}_outercircle
echo "\"All Data incl. NULL"           > ${TMP}_outercircle
for i in `seq 0 360` ; do
 echo "$i $TOTALNUMBER $TOTALVALIDNUMBER $MAXRADIUS" | \
   awk '{printf "%.8f %.8f\n", cos($1 * 3.14159265 / 180.)* $2/$3 * $4, sin($1 * 3.14159265 / 180.) * $2/$3 * $4}' >> ${TMP}_outercircle
done

#################################
# fix vector length to become visible (x? of $MAXRADIUS):
AUTOSTRETCH="1"
echo "\"Direction" >  ${TMP}_vector
echo "0 0"         >> ${TMP}_vector
echo "$UNITVECTOR $MAXRADIUS $AUTOSTRETCH" | awk '{printf "%f %f\n", $1 *$3*$4, $2 *$3*$4}' >> ${TMP}_vector

#################################

echo "" > ${TMP}_newline
cat ${TMP}_sine_cosine_replic ${TMP}_newline ${TMP}_vector ${TMP}_newline ${TMP}_outercircle | xgraph

#################################
#cleanup
cleanup
