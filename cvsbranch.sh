#!/bin/sh

###################################################
#
# DO NOT USE THIS SCRIPT!
#
# (unless you know what you are doing)
###################################################
# it is intented to tag GRASS stable releases
#
# VERY ALPHA QUALITY!!
#
###################################################
# Markus Neteler
# $Id$

#note: no $,. allowed:
TAG="testbranch_5_0_0stable"

#catch all modules from compile list:
MODULE_LIST=`cat src/CMD/lists/GRASS | grep -v '#'`

#list of extra files not in compile list:
FURTHER_FILES="config.guess \
	config.sub GPL.TXT testgrass.sh COPYING README SUBMITTING INSTALL \
	WARNING.html install-sh ONGOING REQUIREMENTS README.xdriver \
	binaryInstall.src README.cygwin Makefile.in AUTHORS BUGS TODO.txt \
	configure.in configure NEWS.html"

#list of extra directories not in compile list:
FURTHER_DIRECTORIES="documents ALPHA64 testsuite tools unused \
	src/include src/CMD src/fonts src/gui \
	src/libes/vect32_64/shapelib-1.2.8 \
	src/libes/dbmi/drivers/odbc src.garden/grass.postgresql"


#tag it...
cvs tag $TAG  $FURTHER_FILES $FURTHER_DIRECTORIES $MODULE_LIST
