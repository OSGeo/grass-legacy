#!/bin/sh
# create_ipcs.sh

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

i=0
while [ $i -le 20 ]
do
	if [ ! -f ${DEV}/fifo.${i}a ]; then
		rm -f ${DEV}/fifo.${i}a
		touch ${DEV}/fifo.${i}a
	fi
	if [ ! -f ${DEV}/fifo.${i}b ]; then
		rm -f ${DEV}/fifo.${i}b
		touch ${DEV}/fifo.${i}b
	fi
	i=`expr $i + 1`
done

chmod 0666 ${DEV}/fifo.*
