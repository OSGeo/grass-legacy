#!/bin/sh
# auto recompilation script
# $Id$

if [ "$1" = "" ] ; then
	SETFILE="./settings.sh"
else
	if [ ! -f $1 ] ; then
		echo "Can not find settings file $1"
		exit 1
	fi
fi

# source in settings from settings.sh
. $SETFILE

# check if settings are ok
for PROG in $WGET $TAR $NCFTPPUT $MAIL ; do
	if [ ! -x $PROG ] ; then
		echo "$PROG not found in this path, please check"
		exit 1
	fi
done

DATE=`date +%c`

OLDPWD=`pwd`

mkdir -p $BUILDDIR || exit 1
cd $BUILDDIR || exit 1

# get tarball with wget
$WGET -q -t 3 --no-parent "${SRCHOST}${SRCDIR}${TARBALL}${TARBALLEXT}"

if [ $? -ne 0 ] ; then
	$MAIL -s "GRASS autorecompile from $DATE" $MAILTO <<END
	Could not get source file ${TARBALL}${TARBALLEXT} 
 	from host ${SRCHOST}${SRCDIR}.
	Exiting.
	No Binary created. 
END
	exit 2
fi

# unpack tarball in tmp dir
$TAR xzf ${TARBALL}${TARBALLEXT} || exit 1

# change to tmp dir
cd $SUBDIR || exit 1

# configure for tmp dir
./configure --prefix=${TARGETDIR} ${CONFOPTS} &> ./configure-output.txt

if [ $? -ne 0 ] ; then
	CONFOUT=`cat ./configure-output.txt`
	$MAIL -s "GRASS autorecompile from $DATE" $MAILTO <<END
	Could not sucessfully ./configure 
 	with options $CONFOPTS,  
	Exiting.
	No Binary created.
 
	output of ./configure $CONFOPTS:
	$CONFOUT
END
	exit 3
fi

# make
make > $MAKEOUTPUT 2>&1 || exit 4

# check for errors in error.log
N=`wc -l < error.log`

if [ $N -ne 6 ] ; then
	ERRORLOG=`cat error.log`
	$MAIL -s "GRASS autorecompile from $DATE" $MAILTO <<END
	There have been errors while makeing the 
 	sources. Please check manually for the errors.
	File error.log:
	$ERRORLOG
	Exiting.
	No Binary created. 
END
	exit 5
fi

# make strip, install and bindist
make install-strip >> $MAKEOUTPUT 2>&1 || exit 1
make bindist >> $MAKEOUTPUT 2>&1 || exit 1

# upload bin tarball to server
$NCFTPPUT -u anonymous -p $MAILTO -P 21 $DSTHOST $DSTDIR grass5* &> /dev/null
if [ $? -ne 0 ] ; then
	$MAIL -s "GRASS autorecompile from $DATE" $MAILTO <<END
		There was an error uploading the binary distribution
		to ${DSTHOST}${DSTDIR}, please check. 
END
	exit 10
fi

cd $OLDPWD

# clean up temporary build dir
rm -fR $BUILDDIR
rm -fR $TARGETDIR

$MAIL -s "GRASS autorecompile from $DATE" $MAILTO <<END
	Sources (${TARBALL}${TARBALLEXT}) from
	${SRCHOST}${SRCDIR} have been successfully build. 
	Binary was uploaded to ${DSTHOST}${DSTDIR}.
END

exit 0
