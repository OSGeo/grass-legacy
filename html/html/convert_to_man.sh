#!/bin/sh
# Converting HTML file to MAN format, move into man1 directory
#
# This file should become a Gmakefile soon
#
# Markus Neteler
# neteler@geog.uni-hannover.de

######### nothing to change below (hope so) ##############
if [ $# -lt 2 ]
then
 echo "Script can be called from ../Gmakefile only within GRASS 5 sources"
 exit 1
fi

SRCDIR=$1
GISBASE=$2
TARGETDIR=$GISBASE/man/1

if ! test -f $SRCDIR/scripts/contrib/g.html2man/g.html2man
then
 echo "g.html2man not found ($SRCDIR/scripts/contrib/g.html2man/)" 
 exit 1
fi

#create target directory:
if [ ! -d $TARGETDIR ]; then mkdir -p $TARGETDIR ; fi

# get list of files:
cd html
LIST_OF_HTMLS=`ls -1 *.html`

#do the conversion
for i in $LIST_OF_HTMLS ; do
  $SRCDIR/scripts/contrib/g.html2man/g.html2man $i
  FILE=`echo $i | sed s/html=// | sed 's/\.html$//'`
  MANFILE=`echo $FILE | sed s/1=// | sed 's/\.1$//'`
  man ./$MANFILE.1 > $TARGETDIR/$MANFILE
  rm -f $MANFILE.1
done

echo MAN files stored in $TARGETDIR/
