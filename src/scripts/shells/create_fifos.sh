#!/bin/sh
#############################################################################
#
# $Id$
#
# MODULE:   	Grass Compilation
# AUTHOR(S):	Original author unknown - probably CERL
# PURPOSE:  	This script will create the fifo files that are required for
#   	    	the XDRIVER in the passed in location.
# COPYRIGHT:    (C) 2000 by the GRASS Development Team
#
#               This program is free software under the GNU General Public
#   	    	License (>=v2). Read the file COPYING that comes with GRASS
#   	    	for details.
#
#############################################################################

# Get the command name
CMD_NAME=`basename $0`

# Check if ther is only one argument
if [ $# -eq 1 ] ; then

    # Check if the argument is a directory
    if [ -d $1 ] ; then
    
    	# Check if the directory is writable
	if [ -w $1 ] ; then
	    DEV=$1/dev
	else
	    echo "ERROR in $CMD_NAME: directory $1 is not writable"
	    exit
	fi
    else
    	echo "ERROR in $CMD_NAME: $1 is not a directory"
	exit
    fi
else
    echo "Usage:"
    echo "  	$CMD_NAME <BASE_DIR>"
    echo
    echo "Parameters:"
    echo "  	BASE_DIR    the destination directory that will contain the"
    echo "  	    	    dev/ directory. This is usually GISBASE"
    exit
fi
    
test -d ${DEV} || mkdir ${DEV}

rm -f ${DEV}/fifo.tmp
mkfifo ${DEV}/fifo.tmp
if [ -p ${DEV}/fifo.tmp ]; then
    MKFIFO=mkfifo
    MKNOD_ARG=
fi
rm -f ${DEV}/fifo.tmp

if [ -z "${MKFIFO}" ]; then
    mknod ${DEV}/fifo.tmp p
    if [ -p ${DEV}/fifo.tmp ]; then
        MKFIFO=mknod
        MKNOD_ARG=p
    fi
    rm -f ${DEV}/fifo.tmp
fi

i=0
while [ $i -le 20 ]
do
	if [ ! -p ${DEV}/fifo.${i}a ]; then
		rm -f ${DEV}/fifo.${i}a
		${MKFIFO} ${DEV}/fifo.${i}a ${MKNOD_ARG}
	fi
	if [ ! -p ${DEV}/fifo.${i}b ]; then
		rm -f ${DEV}/fifo.${i}b
		${MKFIFO} ${DEV}/fifo.${i}b ${MKNOD_ARG}
	fi
	i=`expr $i + 1`
done

chmod 0666 ${DEV}/fifo.*
