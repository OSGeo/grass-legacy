#!/bin/sh
# strip off symbolic information
# on irix platform
# $Id$
# Andreas.Lange@Rhein-Main.de
# $1 => $GISBASE

OPWD=`pwd`

if [ `id -g` -ne 0 ] ; then
  echo "This program needs root permissions!"
  echo "Please change to root account"
  exit 1
fi

if [ -d "$1" ] ; then
  cd $1
else
  echo "Please give \$GISBASE as first parameter to this script"
  exit 1
fi

find . -type f -print -exec strip {} \;

echo "done..."
echo ""

cd $OPWD
exit 0
