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
   echo 'Overwriting "$GISBASE/etc/module_sysnopsis.txt"' 1>&2
   \rm $GISBASE/etc/module_sysnopsis.txt
fi


echo "Generating module synopsis ..."  1>&2
cd "$GISBASE"/bin


for MODULE in ?\.* ; do
  unset label
  unset desc
#  echo "[$MODULE]"

  if [ "$MODULE" = g.parser ] || [ "$MODULE" = r.mapcalc ] ; then
    continue
  fi

  eval `$MODULE --tcltk | head -n 3 | tail -n 2 |  tr '"' "'" | \
        sed -e 's/^ //' -e 's/ {/="/' -e 's/}$/"/'`
  if [ -z "$label" ] && [ -z "$desc" ] ; then
     continue
  fi
  if [ -z "$label" ] ; then
    echo "$MODULE: $desc" >> "$GISBASE/etc/module_sysnopsis.txt"
  else
    echo "$MODULE: $label" >> "$GISBASE/etc/module_sysnopsis.txt"
  fi
done
