#!/bin/sh
# prepare a list of all dynamic library dependencies
# Tested on Linux, may work on other Unix or may not...
# $Id$
# Andreas.Lange@Rhein-Main.de
# $1 => $GISBASE

OPWD=`pwd`

if [ -d "$1" ] ; then
  cd $1
else
  echo "Please give \$GISBASE as first parameter to this script"
  exit 1
fi

for D in driver/ etc/ etc/bin/cmd/ etc/bin/inter/ etc/nviz2.2/ ; do
  cd $D
  echo "Preparing listing for `pwd`"
  for F in `ls *` ; do
    if [ -x "$F" ] ; then
      ldd $F >> /tmp/lddoutput.txt$$
    fi
  done
  cd $1
done

cd $1

echo "Sorting dependency listing"
cat /tmp/lddoutput.txt$$ | cut -d' ' -f3 | sort -u > /tmp/GRASS-dependencies.txt
rm -f /tmp/lddoutput.txt$$

echo "Dependency listing is in /tmp/GRASS-dependencies.txt"
echo ""

cd $OPWD
exit 0
