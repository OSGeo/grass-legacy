#!/bin/sh
echo Converting HTML file to MAN format, move into man1 directory

../../src/scripts/contrib/g.html2man/g.html2man $1
FILE=`echo $1 | sed s/html=// | sed 's/\.html$//'`
MANFILE=`echo $FILE | sed s/1=// | sed 's/\.1$//'`
mv $MANFILE.1 ../../man/man1/$MANFILE
echo "Now cd ../../man/man1/ to check-in new file"
