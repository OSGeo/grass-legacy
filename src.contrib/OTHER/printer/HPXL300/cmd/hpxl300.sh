#!/bin/sh

if [ $# -ne 2 ] ; then
	echo "p.hp300xl  [fast|slow] [map]"
	exit 1
fi

oldprinter=`p.select -p`
if [ $1 = 'slow' ] ; then
	p.select hp300xlNS
else
	p.select hp300xlNF
fi

dir=$LOCATION/.tmp/`hostname`
mkdir $dir 2> /dev/null
outfile=$dir/map$$
p.map $2 2> $outfile
if [ $? ]; then
	echo "Sending p.map output to printer"
	lpr -s -Ppj $outfile
fi
echo "DONE"
p.select $oldprinter
exit 0
