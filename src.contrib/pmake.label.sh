#!/bin/sh
#         @(#) pmake.labels:   27 June 1989  wfl/jaf/jjl
#         @(#) Interactively create GRASS labels file
#
#         Pgm works in conjunction with Dwhat to produce
#         a paint labels file.  User can use the default
#         parameters provided by the pgm, or customize
#         label format as needed.  Output is placed in
#         $GISDBASE/$LOCATION/$MAPSET/paint/labels.  At
#         the present time the program will write over
#         an existing label file if the same output file 
#         name is used more than once.
#

TEMPFILE=/tmp/labels.$$

trap "rm -f $TEMPFILE; exit 1" 2 3 15

clear
if [ -z "$GISRC" ]
then
   echo ""
   echo ""
   echo ""
   echo "You must be running GRASS to use this module !!"
   echo "exiting"
   echo ""
   echo ""
   echo ""
   exit 1
fi
#
#
echo ""
echo ""
echo "==================== pmake.labels ============================"
echo ""
echo "This module allows you to interactively create"
echo "a labels file that will contain the category" 
echo "text label associated with the location(s) you select. "
echo "You will be prompted for this filename. "
echo ""
echo "After the labels file is created it can be displayed "
echo "on the screen immediately with the command:  [Dpaint.labels label_file]"
echo "   WHERE:   label_file is the name you gave the label file."
echo ""
echo ""
echo "Before running this module you need to have run"
echo "Dcell MAP_NAME: where MAP_NAME is the name of the cell map you want"
echo "to create the labels for. "
echo "If you haven't run Dcell exit this program at the prompt."
echo ""
echo ""
echo "============================================================="
echo ""
echo ""
$GISBASE/etc/echo -n "Continue [Y/N]: "
read OKTOGO
case $OKTOGO in
     N|n) exit 1;;
       *) ;;
esac
rm -f $TEMPFILE
Gask new "Enter name to be given to label file" paint/labels labels $TEMPFILE
. $TEMPFILE
rm -f $TEMPFILE
if test "$name" = ""
then
    exit 0
fi

clear
#
#  Set paint labels DEFAULT values
       FS="#"
       REF="lower left"
       COLR="black"
       SIZE="500"
       WIDTH="1"
       HCOLR="none"
       HWID="0"
       BGRND="white"
       BORDER="none"
       OPAQ="yes"
#
echo "Default labels values are:"
echo ""
echo "ref=$REF            color=$COLR         size=$SIZE"
echo "width=$WIDTH                   hcolor=$HCOLR         hwidth=$HWID"
echo "background=$BGRND          border=$BORDER         opaque=$OPAQ"
echo ""
$GISBASE/etc/echo -n "Do you want to use the default values [Y/N]: "
read DEFVAL
case $DEFVAL in
     N|n) echo ""
	 $GISBASE/etc/echo -n "Enter ref: "; read TMP;
                if [ -n "$TMP" ];
                   then REF=$TMP;
                fi ;
          $GISBASE/etc/echo -n "Enter color: "; read TMP;
                if [ -n "$TMP" ];
                   then COLR=$TMP;
                fi ;
          $GISBASE/etc/echo -n "Enter size: "; read TMP;
                if [ -n "$TMP" ];
                   then SIZE=$TMP;
                fi ;
          $GISBASE/etc/echo -n "Enter width: "; read TMP;
                if [ -n "$TMP" ];
                   then WIDTH=$TMP;
                fi ;
          $GISBASE/etc/echo -n "Enter hcolor: "; read TMP;
                if [ -n "$TMP" ];
                   then HCOLR=$TMP;
                fi ;
          $GISBASE/etc/echo -n "Enter hwidth: "; read TMP;
                if [ -n "$TMP" ];
                   then HWID=$TMP;
                fi ;
          $GISBASE/etc/echo -n "Enter background: "; read TMP;
                if [ -n "$TMP" ];
                   then BGRND=$TMP;
                fi ;
          $GISBASE/etc/echo -n "Enter border: "; read TMP;
                if [ -n "$TMP" ];
                   then BORDER=$TMP;
                fi ;
          $GISBASE/etc/echo -n "Enter opaque: "; read TMP;
                if [ -n "$TMP" ];
                   then OPAQ=$TMP;
                fi ;;
       *) ;;
esac
clear
#
# The following line creates the parameters for the labels file
#
echo $REF$FS$COLR$FS$SIZE$FS$WIDTH$FS$HCOLR$FS\
$HWID$FS$BGRND$FS$BORDER$FS$OPAQ > $TEMPFILE
#
# The following line redirects output from Dwhat >> $TEMPFILE
#
Dwhat - >> $TEMPFILE
if test $? != 0
then
    rm -f $TEMPFILE
    echo "Sorry, can't proceed. Paint labels file <$name> not created"
    exit 1
fi
echo "Creating GRASS paint labels file <$name>......"
echo ""
echo ""
#
cat $TEMPFILE |  sed -e "s|(|#|g" \
                -e "s|)|#|g"  | awk '
          BEGIN {FS = "#";RS = ""}
                {if  (NR==1) {
                 REF=$1
                 COLR=$2
                 SIZE=$3
                 WIDTH=$4
                 HCOLR=$5
                 HWID=$6
                 BGRND=$7
                 BORDER=$8
                 OPAQ=$9
                }
               }
                {if  (NR>1) {
                 print "east:           ",$1
                 print "north:          ",$3
                 print "xoffset:"
                 print "yoffset:"
                 print "ref:             ",REF
                 print "color:           ",COLR
                 print "size:            ",SIZE
                 print "width:           ",WIDTH
                 print "hcolor:          ",HCOLR
                 print "hwidth:          ",HWID
                 print "background:      ",BGRND
                 print "border:          ",BORDER
                 print "opaque:          ",OPAQ
                 print " "
                 print "text:"$8
                 print " "
             }
      } ' >> $file
echo "The label file $name has been created in <$LOCATION> <$MAPSET>"
rm -f $TEMPFILE
exit 0
