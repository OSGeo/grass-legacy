#!/bin/sh

# script to tag GRASS stable releases
# Markus Neteler
# $Id$

TAG=testbranch_5.0.0

#catch all modules from compile list:
FILE_LIST=`cat src/CMD/lists/GRASS | grep -v '#'`

#list of extra files not in compile list:
FURTHER_FILES=config.guess \
	config.sub GPL.TXT testgrass.sh COPYING README SUBMITTING INSTALL \
	WARNING.html install-sh ONGOING REQUIREMENTS README.xdriver \
	binaryInstall.src README.cygwin Makefile.in AUTHORS BUGS TODO.txt \
	configure.in configure NEWS.html

#list of extra directories not in compile list:
FURTHER_DIRECTORIES=documents ALPHA64 testsuite tools unused


#tag it...
cvs tag $TAG  $FURTHER_FILES $FURTHER_DIRECTORIES $FILE_LIST

