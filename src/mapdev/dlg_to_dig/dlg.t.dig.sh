#!/bin/sh


# Read ~/.grassrc, setting the variables as csh variables
	eval `sed '1,$s/^\([A-Za-z_]*\): *\(.*\)/\1="\2"; /' $HOME/.grassrc`

ETC=$GISBASE/etc
BIN=$GISBASE/bin
LOCATION=$GISDBASE/$LOCATION_NAME/$MAPSET

# Check for existence of directory which holds dlg files
if [ ! -d $LOCATION/bdlg ]
then
    echo "Sorry, there are no Bdlg files available for conversion."
    exit
fi

# Check for existence of directory which holds binary digit files
if [ ! -d $LOCATION/dig ]
then
    rm -rf $LOCATION/dig
    mkdir $LOCATION/dig
fi

# Check for existence of directory which holds attribute files
if [ ! -d $LOCATION/dig_att ]
then
    rm -rf $LOCATION/dig_att
    mkdir $LOCATION/dig_att
fi

clear
echo ""

while echo "Enter name of BDLG file to be converted to digit format: "
do
    echo "-or- enter 'list' for a listing" ;  echo "> \c"
    read ans

    if ( test ".$ans" = "." )
    then
	echo ""
	echo "No file name given."
	echo ""
	exit
    elif ( test "$ans" = "list" )
    then
	echo ""
	echo "BDLG Files available for conversion:"
	(cd $LOCATION/bdlg; ls) 
	echo ""
    else
	# Check for existence of bdlg file
	if [ -r $LOCATION/bdlg/$ans ]
	then
	    # got valid location
	    break;  
	else
	    echo "Sorry, $ans does not exist"
	fi
    fi
done


echo ""
while echo "Enter name of new DIG file: "
do
    echo "-or- enter the word 'list' for a list of existing files"
    echo "> \c"
    read ans2

    if ( test ".$ans2" = "." )
    then
	echo ""
	echo "No file name given."
	echo ""
	exit
    elif ( test "$ans2" = "list" )
    then
	echo ""
	echo "DIG Files that currently exist:"
	(cd $LOCATION/dig; ls) 
	echo ""
    else
	# Check for existence of dig file
	if [ -r $LOCATION/dig/$ans2 ]
	then
	    # exists
	    echo "$ans2 already exists. Overwrite it? (y/n): \c"
	    read yn
	    if ( test "$yn" = "yes" || test "$yn" = "y" ) then
		break;
	    fi
	else
	    break;  
	fi
    fi
done

yn=""


echo ""
echo " Determine if this map is composed of Area or Line information."
 

while ( test ".$yn" = "." ) do
    echo " Do you want to give precedence to Areas (opposed to Lines) [y] "
    echo "> \c"
    read yn
    echo ""
    if ( test "$yn" = "yes" || test "$yn" = "y" )
    then
	flag=""
    else
	flag="line"
    fi
done

#  dig file and attribute file HAVE to have same name

exec $ETC/dlg.to.vect $LOCATION/bdlg/$ans $LOCATION/dig/$ans2 $LOCATION/dig_att/$ans2 $flag

