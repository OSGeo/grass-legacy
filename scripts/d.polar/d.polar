#!/bin/sh

############################################################################
#
# MODULE:       d.polar
# AUTHOR(S):    Markus Neteler. neteler itc.it
#               algorithm + EPS output by Bruno Caprile
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
#%option
#% key: eps
#% type: string
#% gisprompt: new_file,file,output
#% description: Name of optional EPS output file
#% required : no
#%end

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

#### check if we have xgraph (if no EPS output requested)
if [ -z "$GIS_OPT_EPS" ] ; then
  if [ ! -x "`which xgraph`" ] ; then
    echo "$PROG: xgraph required, please install first (www.xgraph.org)" 2>&1
    exit 1
  fi
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



# Now output: if user didn't specify EPS output, we use xgraph.


#################################
# EPS output (by Bruno Caprile, ITC-irst)

PSOUT="`basename $GIS_OPT_EPS .eps`.eps"
if [ ! -z "$GIS_OPT_EPS" ] ; then

echo "Generating $GIS_OPT_EPS..."

OUTERRADIUS=$MAXRADIUS
FRAMEALLOWANCE=1.1
HALFFRAME=3000
CENTER=$HALFFRAME,$HALFFRAME
#SCALE=$HALFFRAME / ($OUTERRADIUS * $FRAMEALLOWANCE)
SCALE=`echo $HALFFRAME $OUTERRADIUS $FRAMEALLOWANCE | awk '{printf "%f", $1/ ($2 * $3)}'`

# DIAGRAMLINEWIDTH=CEILING ($HALFFRAME / 200)
DIAGRAMLINEWIDTH=`echo $HALFFRAME | awk '{printf "%f", $1 * ($2 / 200)}'`  # ceil?
# AXESLINEWIDTH=CEILING ($HALFFRAME / 500)
AXESLINEWIDTH=`echo $HALFFRAME | awk '{printf "%f", $1 * ($2 / 500)}'` # ceil?
# AXESFONTSIZE=CEILING ($HALFFRAME / 15)
AXESFONTSIZE=`echo $HALFFRAME | awk '{printf "%f", $1 * ($2 / 15)}'` # ceil?
# DIAGRAMFONTSIZE=CEILING ($HALFFRAME / 20)
DIAGRAMFONTSIZE=`echo $HALFFRAME | awk '{printf "%f", $1 * ($2 / 20)}'` # ceil?
HALFFRAME_2=`echo $HALFFRAME | awk '{printf "%.1f", $1 * 2}'`

AVERAGEDIRECTIONCOLOR=1 #(blue)
DIAGRAMCOLOR=4 #(red)
CIRCLECOLOR=2 #(green)
AXESCOLOR=0 #(black)
  
NORTHJUSTIFICATION=2
EASTJUSTIFICATION=6
SOUTHJUSTIFICATION=8
WESTJUSTIFICATION=8
  
NORTHXSHIFT=`echo 1.02 $HALFFRAME | awk '{printf "%.1f", $1 * $2}'`
NORTHYSHIFT=`echo 1.98 $HALFFRAME | awk '{printf "%.1f", $1 * $2}'`
EASTXSHIFT=`echo 1.98 $HALFFRAME  | awk '{printf "%.1f", $1 * $2}'`
EASTYSHIFT=`echo 1.02 $HALFFRAME  | awk '{printf "%.1f", $1 * $2}'`
SOUTHXSHIFT=`echo 1.02 $HALFFRAME | awk '{printf "%.1f", $1 * $2}'`
SOUTHYSHIFT=`echo 0.02 $HALFFRAME | awk '{printf "%.1f", $1 * $2}'`
WESTXSHIFT=`echo 0.01 $HALFFRAME  | awk '{printf "%.1f", $1 * $2}'`
WESTYSHIFT=`echo 1.02 $HALFFRAME  | awk '{printf "%.1f", $1 * $2}'`
  
ALLDATASTRING="All Data (NULL included)"
REALDATASTRING="Real Data Angles"
AVERAGEDIRECTIONSTRING="Avg. Direction"
  
LEGENDSX=`echo 1.95 * $HALFFRAME | awk '{printf "%f", $1 * $2}'`
ALLDATALEGENDY=`echo 1.95 * $HALFFRAME | awk '{printf "%f", $1 * $2}'`
REALDATALEGENDY=`echo 1.90 * $HALFFRAME | awk '{printf "%f", $1 * $2}'`
AVERAGEDIRECTIONLEGENDY=`echo 1.85 * $HALFFRAME | awk '{printf "%f", $1 * $2}'`

##########
cat ${GISBASE}/etc/d.polar/ps_defs.eps > $PSOUT

echo "
0.1 0.1 scale                           %% EPS-SCALE EPS-SCALE scale
%%
%% drawing axes
%%

col0                                    %% colAXES-COLOR
1 setlinewidth                          %% AXES-LINEWIDTH setlinewidth
[] 0 setdash
newpath
 $HALFFRAME     0.0 moveto                  %% HALF-FRAME 0.0 moveto
 $HALFFRAME  $HALFFRAME_2 lineto                  %% HALF-FRAME (2 * HALF-FRAME) lineto
    0.0  $HALFFRAME moveto                  %% 0.0 HALF-FRAME moveto
 $HALFFRAME_2  $HALFFRAME lineto                  %% (2 * HALF-FRAME) HALF-FRAME lineto
stroke

200 /Times-Roman choose-font            %% AXES-FONTSIZE /Times-Roman choose-font
(N) $NORTHXSHIFT $NORTHYSHIFT $NORTHJUSTIFICATION just-string         %% NORTH-X-SHIFT NORTH-Y-SHIFT NORTH-JUSTIFICATION just-string
(E) $EASTXSHIFT $EASTYSHIFT $EASTJUSTIFICATION just-string         %% EAST-X-SHIFT EAST-Y-SHIFT EAST-JUSTIFICATION just-string
(S) $SOUTHXSHIFT $SOUTHYSHIFT $SOUTHJUSTIFICATION just-string           %% SOUTH-X-SHIFT SOUTH-Y-SHIFT SOUTH-JUSTIFICATION just-string
(W) $WESTXSHIFT $WESTYSHIFT $WESTJUSTIFICATION just-string           %% WEST-X-SHIFT WEST-Y-SHIFT WEST-JUSTIFICATION just-string

%%
%% drawing outer circle
%%

col2                                    %% colCIRCLE-COLOR
150 /Times-Roman choose-font            %% DIAGRAM-FONTSIZE /Times-Roman choose-font
2 setlinewidth                          %% DIAGRAM-LINEWIDTH setlinewidth
[] 0 setdash
newpath
                                        %% coordinates of rescaled, translated outer circle follow
                                        %% first point moveto, then lineto
" >> $PSOUT

SUBLENGTH=`wc -l ${TMP}_outercircle | awk '{printf "%d", $1 - 2}'`
LINE1=`head -n 2 ${TMP}_outercircle | tail -n 1`
echo $LINE1 $SCALE $HALFFRAME | awk '{printf "%.2f %.2f moveto\n", $1*$3+$4, $2*$3+$4}' >> $PSOUT

tail -n $SUBLENGTH ${TMP}_outercircle | sed "s+\$+ $SCALE $HALFFRAME+g" > ${TMP}_outercircle_lineto

cat ${TMP}_outercircle_lineto | awk '{printf "%.2f %.2f lineto\n",$1*$3+$4, $2*$3+$4 }' >> $PSOUT
rm -f ${TMP}_outercircle_lineto

echo "stroke

%%
%% drawing real data diagram
%%

col4                                    %% colDIAGRAM-COLOR
2 setlinewidth                          %% DIAGRAM-LINEWIDTH setlinewidth
[] 0 setdash
newpath
                                        %% coordinates of rescaled, translated diagram follow
                                        %% first point moveto, then lineto
" >> $PSOUT

SUBLENGTH=`wc -l ${TMP}_sine_cosine_replic | awk '{printf "%d", $1 - 2}'`
LINE1=`head -n 2 ${TMP}_sine_cosine_replic | tail -n 1`
echo $LINE1 $SCALE $HALFFRAME | awk '{printf "%.2f %.2f moveto\n", $1*$3+$4, $2*$3+$4}' >> $PSOUT

tail -n $SUBLENGTH ${TMP}_sine_cosine_replic | sed "s+\$+ $SCALE $HALFFRAME+g" > ${TMP}_sine_cosine_replic_lineto

cat ${TMP}_sine_cosine_replic_lineto | awk '{printf "%.2f %.2f lineto\n",$1*$3+$4, $2*$3+$4 }' >> $PSOUT
rm -f ${TMP}_sine_cosine_replic_lineto

echo "stroke
%%
%% drawing average direction
%%

col1                                    %% colAVERAGE-DIRECTION-COLOR
2 setlinewidth                          %% DIAGRAM-LINEWIDTH setlinewidth
[] 0 setdash
newpath
                                        %% coordinates of rescaled, translated average direction follow
                                        %% first point moveto, second lineto
" >> $PSOUT

SUBLENGTH=`wc -l ${TMP}_vector | awk '{printf "%d", $1 - 2}'`
LINE1=`head -n 2 ${TMP}_vector | tail -n 1`
echo $LINE1 $SCALE $HALFFRAME | awk '{printf "%.2f %.2f moveto\n", $1*$3+$4, $2*$3+$4}' >> $PSOUT

tail -n $SUBLENGTH ${TMP}_vector | sed "s+\$+ $SCALE $HALFFRAME+g" > ${TMP}_vector_lineto

cat ${TMP}_vector_lineto | awk '{printf "%.2f %.2f lineto\n",$1*$3+$4, $2*$3+$4 }' >> $PSOUT
rm -f ${TMP}_vector_lineto

echo "stroke

%%
%% drawing legends
%%

col2                                    %% colCIRCLE-COLOR
%% Line below: (ALL-DATA-STRING) LEGENDS-X ALL-DATA-LEGEND-Y 4 just-string
(All Data (NULL included)) 5850.0 5850.0 4 just-string

col4                                    %% colDIAGRAM-COLOR
%% Line below: (REAL-DATA-STRING) LEGENDS-X REAL-DATA-LEGEND-Y 4 just-string
(Real Data Angles) 5850.0 5700.0 4 just-string

col1                                    %% colAVERAGE-DIRECTION-COLOR
%% Line below: (AVERAGE-DIRECTION-STRING) LEGENDS-X AVERAGE-DIRECTION-LEGEND-Y 4 just-string
(Avg. Direction) 5850.0 5550.0 4 just-string
" >> $PSOUT

else
echo "" > ${TMP}_newline
cat ${TMP}_sine_cosine_replic ${TMP}_newline ${TMP}_outercircle ${TMP}_newline\
	 ${TMP}_vector | xgraph

fi
#################################
#cleanup
cleanup
