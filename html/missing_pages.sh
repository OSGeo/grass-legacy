#!/bin/sh
bin=$GISBASE/bin
scripts=$GISBASE/scripts

# Markus Neteler, derived from man/utilities/missing

if test "$GISBASE" = ""; then
 echo "Script to find missing HTML pages (documentation)"
 echo "You must be in GRASS to run this program."
 exit
fi

list=
for f in `cd $bin; ls`
do
    if [ ! -f $GISBASE/man/1/$f.1 ]
    then
	list="$list $f"
    fi
done
if test "$list" != ""
then
    echo The following module commands do NOT have MAN/HTML entries:
#    (cd $bin; ls -dC $list) | sed s'/^/  /'
    (cd $bin; ls -dC $list)
    echo ""
fi

list=
for f in `cd $scripts; ls`
do
    if [ ! -f $GISBASE/man/1/$f.1 ]
    then
	list="$list $f"
    fi
done
if test "$list" != ""
then
    echo The following scripts do NOT have MAN/HTML entries:
#    (cd $scripts; ls -dC $list) | sed s'/^/  /'
    (cd $scripts; ls -dC $list)
    echo ""
fi

