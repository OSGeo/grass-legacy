#!/bin/sh

# GRASS 5 binary package installation tool
# platform indendent

# $Id$
# Version 9
# 1999-2000 by Markus Neteler, neteler@geog.uni-hannover.de

######################################################
#setting: where to put the GRASS start script:
BINDIR=/usr/local/bin

#setting: standard path of GRASS installation
DESTDIR=/usr/local/grass-5.0b

######################################################
# check for first parameter:
if [ ! "$1" ]
then	echo "

GRASS GIS 5 binary package installation tool

Usage:        sh grass5install.sh grass_binpackage.tar.gz [dest_dir]

      with:
        grass_binpackage : name of GRASS 5 binary package   
        [dest_dir] - optional: FULL path name to the installation directory
                     (default: /usr/local/grass-5.0b/)

You may need login as root for installation.
"
	exit
fi

# check for second parameter:
if [ "$2" ]
then
  DESTDIR=$2
fi

# check for correct package name:
if test ! -f $1; then 
   echo "ERROR: Wrong package name $1. File does not exists."
   echo ""
   exit
fi

# check, if package is first parameter and in tar.gz compression:
PACKAGETYPE=`file $1`
echo $PACKAGETYPE  | grep gzip > /dev/null
if [ $? -eq 1 ] ; then
    echo "ERROR: You need the GRASS binary package in .tar.gz compression."
    echo ""
    exit
fi

# Start the installation job...
echo "GRASS GIS 5 binary package installation tool"
echo ""
echo "The package $1 seems to be o.k."
echo " Proceeding..."
echo ""
echo "Checking and creating installation directory..."
echo "Installing to $DESTDIR"

if [ ! -d "$DESTDIR" ] ;
then
        #check if a word "grass" is in string $DESTDIR
        echo $DESTDIR |grep -w "grass"
        if [ $? -eq 1 ] ; then
            echo "WARNING: Your destination path $DESTDIR does not contain the word 'grass'"
            echo "Continue (y/n)?"
            read ans         
            if [ "$ans" = "n" -o "$ans" = "N" ] ; then
             exit
            fi
        fi

        mkdir -p $DESTDIR
        if [ $? -eq 1 ] ; then
          echo "An error occured! Exiting."
          exit
        fi
else
     if [ -d $DESTDIR/bin ] ; then
          echo ""
          echo "ERROR: Old GRASS distribution existing in $DESTDIR!"
          echo "Remove first!"
          exit
     else
          #check if a word "grass" is in string $DESTDIR
          echo $DESTDIR |grep -w "grass"
echo $? scheise
          if [ $? -eq 1 ] ; then
            echo "WARNING: Your destination path $DESTDIR does not contain the word 'grass'"
            echo "Continue (y/n)?"
            read ans         
            if [ "$ans" = "n" -o "$ans" = "N" ] ; then
             exit
            fi
          fi
     fi        
fi

echo "Installing GRASS binaries into $DESTDIR"
echo ""

echo "Uncompressing the package and extracting to target directory..."
gunzip -c $1 |tar -C $DESTDIR -xvf -
if [ $? -eq 1 ] ; then
     echo "An error occured/user break! Exiting."
     exit
fi

##Creating start script
if [ ! -d "$BINDIR" ] ;
then
 BINDIR=/usr/bin
fi
echo "Creating start script: $BINDIR/grass5.0beta"
echo ":"                          >$BINDIR/grass5.0beta
if [ $? -eq 1 ] ; then
          echo "An error occured! Exiting."
          echo "You must be 'root' to install into $BINDIR"
          exit
fi
echo "GISBASE=$DESTDIR"          >>$BINDIR/grass5.0beta
echo "export GISBASE"            >>$BINDIR/grass5.0beta
echo "exec \$GISBASE/etc/GIS.sh" >>$BINDIR/grass5.0beta
chmod ugo+x $BINDIR/grass5.0beta


echo "Creating the locks directory for monitors..."
SERVERNAME=`uname -n`
rm -rf $DESTDIR/locks/*
mkdir $DESTDIR/locks/$SERVERNAME
chmod -R 1777 $DESTDIR/locks


# update paths in etc/monitorcap:
cp $DESTDIR/etc/monitorcap $DESTDIR/etc/monitorcap.orig
NEWPATH=$DESTDIR
sed 's*/usr/local/grass-5.0b*'${NEWPATH}'*g' $DESTDIR/etc/monitorcap.orig > $DESTDIR/etc/monitorcap


echo""
echo "Creating the monitor fifos..."
rm -f $DESTDIR/dev/fifo.*
# create script with paths updated:
sed 's*/usr/local/grass-5.0b*'${NEWPATH}'*g' $DESTDIR/dev/create_fifos.sh > $DESTDIR/dev/create_fifos_new.sh
chmod ugo+x $DESTDIR/dev/create_fifos_new.sh
sh $DESTDIR/dev/create_fifos_new.sh


echo "Installation finished. Start GRASS 5 with"
echo "    grass5.0beta"
echo ""
echo "The graphical user interface can be started within GRASS GIS."
echo ""
echo "Welcome to GRASS GIS. Enjoy this open source GNU GRASS GIS!"
echo ""
