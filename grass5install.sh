#!/bin/sh

# GRASS 5 binary package installation tool
# platform indendent

# Version 4/Sun Oct 17 21:51:40 MEST 1999
# 1999 by Markus Neteler, neteler@geog.uni-hannover.de

######################################################
#setting: where to put the GRASS start script:
BINDIR=/usr/local/bin

######################################################
# check for first parameter:
if [ ! "$1" ]
then	echo "

GRASS GIS 5 binary package installation tool

Usage:        sh grass5install.sh grass_binpackage.tar.gz dest_dir

      with:
        grass_binpackage : name of GRASS 5 binary package   
        dist_dir - FULL path name to the installation directory

You may need login as root for installation.
"
	exit
fi

# check for second parameter:
if [ ! "$2" ]
then    echo "

GRASS GIS 5 binary package installation tool
ERROR: Please specify destination directory for installation (full path).
 
Usage:        sh grass5install.sh grass_binpackage.tar.gz dest_dir

      with:
        grass_binpackage : name of GRASS 5 binary package
        dist_dir - FULL path name to the installation directory
                   (example: /usr/local/grass5  )
"
        exit
fi

# check for correct package name:
if test ! -f $1; then 
   echo "ERROR: Wrong package name $1. File does not exists."
   echo ""
   exit
fi

# check, if package is first parameter and in tar.gz compression:
PACKAGETYPE=`file $1|cut -f2 -d' '`
if test "gzip" != "$PACKAGETYPE";then
    echo "ERROR: You need the GRASS binary package in .tar.gz compression."
    echo ""
    exit
fi

# Start the installation job...
echo "GRASS GIS 5 binary package installation tool"
echo ""
echo "The package $1 seems to be o.k. Proceeding..."
echo ""
echo "Checking and creating installation directory..."
if [ ! -d "$2" ]
then	mkdir -p $2
else	echo $2
fi

echo "Installing GRASS binaries in $2"
echo ""

echo "Uncompressing the package..."
gunzip $1

# shorten the name for tar command:
NAME=`echo $1 |sed -e '1,$s/.gz//g'`
echo "Extract to target directory..."
tar -C $2 -xvf $NAME

echo "Creating start script: $BINDIR/grass5.0beta"
echo ":"                          >$BINDIR/grass5.0beta
echo "GISBASE=$2"                >>$BINDIR/grass5.0beta
echo "export GISBASE"            >>$BINDIR/grass5.0beta
echo "exec \$GISBASE/etc/GIS.sh" >>$BINDIR/grass5.0beta
chmod ugo+x $BINDIR/grass5.0beta


echo "Creating the locks directory for monitors..."
SERVERNAME=`uname -n`
rm -rf $2/locks/*
mkdir $2/locks/$SERVERNAME
chmod -R 1777 $2/locks


# update paths in etc/monitorcap:
cp $2/etc/monitorcap $2/etc/monitorcap.orig
NEWPATH=$2
sed 's*/usr/local/grass-5.0b*'${NEWPATH}'*g' $2/etc/monitorcap.orig > $2/etc/monitorcap


echo""
echo "Creating the monitor fifos..."
rm -f $2/dev/fifo.*
# create script with paths updated:
sed 's*/usr/local/grass-5.0b*'${NEWPATH}'*g' $2/dev/create_fifos.sh >$2/dev/create_fifos_new.sh
chmod ugo+x $2/dev/create_fifos_new.sh
sh $2/dev/create_fifos_new.sh


echo "Installation finished. Start GRASS 5 with"
echo "    grass5.0beta"
echo ""
echo "The graphical user interface can be started within GRASS GIS."
echo ""
echo "Welcome to GRASS GIS. Enjoy this open source product!"
echo ""
