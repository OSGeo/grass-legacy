#!/bin/sh
USAGE='r.in.doq.sh -s -o rast_name [-d diskfile] or [-t device 1 2 ...] 
	-t use tape file device files 1 2 .. 
	-d use disk file name diskfile 
	-o output raster rast_name.[tape file number] 
	-s use secondary datum 
 
r.in.doq.sh -o test -t /dev/nsrt1 1 2 3 -s' 
tmpnm=`g.tempfile pid=$$`

if test $# -lt 4 ;then
	echo "$USAGE"
	exit 2
else
 while getopts s:o:d:t: c
do
	case $c in
	d)	FILE=$OPTARG
		DEVICE="file";;
	t)	TAPE=$OPTARG
		DEVICE="tape";;
	s)	PRI=-s;;
	o)	OUT=$OPTARG;;
	\?)	echo "$USAGE"
		exit 2;;
	esac
done
fi
shift `expr $OPTIND - 1`

if test "$DEVICE" = "file" ;then
	r.in.doq $PRI in=$FILE out=$OUT
else
	while test ${1:-END} != END
	do
	mt -f $TAPE rewind
	mt -f $TAPE fsf `expr $1 - 1`
	dd if=$TAPE ibs=53689 of=$tmpnm
	r.in.doq $PRI in=$tmpnm out=$OUT.$1
	rm $tmpnm
	shift 1
	done
fi
