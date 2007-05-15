#!/bin/sh
############################################################################
#
# TOOL:         module_synopsis.sh
# AUTHOR:       M. Hamish Bowman, Dept. Marine Science, Otago Univeristy,
#                 New Zealand
# PURPOSE:      Runs through GRASS modules and creates a synopsis list of
#		  module names and descriptions.
# COPYRIGHT:    (c) 2007 Hamish Bowman, and the GRASS Development Team
#               This program is free software under the GNU General Public
#               License (>=v2). Read the file COPYING that comes with GRASS
#               for details.
#
#############################################################################

if  [ -z "$GISBASE" ] ; then
    echo "You must be in GRASS GIS to run this program." 1>&2
    exit 1
fi

if [ -e "$GISBASE/etc/module_sysnopsis.txt" ] ; then
#  echo "ERROR: module_sysnopsis.txt already exists" 1>&2
#  exit 1
   #blank it
   g.message -w 'Overwriting "$GISBASE/etc/module_sysnopsis.txt"'
   \rm $GISBASE/etc/module_sysnopsis.txt
fi

TMP="`g.tempfile pid=$$`"
if [ $? -ne 0 ] || [ -z "$TMP" ] ; then
    g.message -e "Unable to create temporary files" 
    exit 1
fi


g.message "Generating module synopsis ..."

cd "$GISBASE"

for DIR in bin scripts ; do
  cd $DIR

  for MODULE in ?\.* r3.* ; do
    unset label
    unset desc
#    echo "[$MODULE]"

    case "$MODULE" in
      g.parser | r.mapcalc | r3.mapcalc | mkftcap | p.out.vrml| d.paint.labels)
	continue
	;;
    esac

    eval `$MODULE --tcltk | head -n 3 | tail -n 2 |  tr '"' "'" | \
        sed -e 's/^ //' -e 's/ {/="/' -e 's/}$/"/'`
    if [ -z "$label" ] && [ -z "$desc" ] ; then
	continue
    fi
    if [ -z "$label" ] ; then
	echo "$MODULE: $desc" >> "$TMP"
    else
	echo "$MODULE: $label" >> "$TMP"
    fi
  done

  cd ..
done

echo "g.parser: Full parser support for GRASS scripts." >> "$TMP"
echo "r.mapcalc: Performs arithmetic on raster map layers." >> "$TMP"
echo "r3.mapcalc: Performs arithmetic on 3D grid volume data." >> "$TMP"

sort --dictionary-order "$TMP" > "$GISBASE/etc/module_sysnopsis.txt"
\rm "$TMP"
