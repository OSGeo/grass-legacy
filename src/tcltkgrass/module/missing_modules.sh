#!/bin/sh
bin=$GISBASE/bin
scripts=$GISBASE/scripts

# Markus Neteler, derived from man/utilities/missing

if test "$GISBASE" = ""; then
 echo "Script to find modules not yet implemented in tcltkgrass"
 echo "You must be in GRASS to run this program."
 exit
fi

list=
for f in `cd $bin; ls`
do
    if [ ! -f $GISBASE/tcltkgrass/module/$f ]
    then
	list="$list $f"
    fi
done
if test "$list" != ""
then
    echo The following module commands are NOT implemented in tcltkgrass:
    (cd $bin; ls -dC $list)
    echo ""
fi

list=
for f in `cd $scripts; ls`
do
    if [ ! -f $GISBASE/tcltkgrass/module/$f ]
    then
	list="$list $f"
    fi
done
if test "$list" != ""
then
    echo The following scripts are NOT implemented in tcltkgrass:
    (cd $scripts; ls -dC $list)
    echo ""
fi

echo "NOTE:"
echo "   Interactive modules might be called by \"run\" directly in menu.tcl"
echo "   Therefore this list might be wrong"
