#!/bin/sh


# Read ~/.grassrc, setting the variables as csh variables
        eval `sed '1,$s/^\([A-Za-z_]*\): *\(.*\)/\1="\2"; /' $HOME/.grassrc`

ETC=$GISBASE/etc
BIN=$GISBASE/bin
LOCATION=$GISDBASE/$LOCATION_NAME/$MAPSET

# Check for existence of directory which holds digitized files
if [ ! -d $LOCATION/dig ]
then
	echo "Sorry, there are no digitized files available for conversion."
	exit
fi

# Check for existence of directory which holds binary dlg files
if [ ! -d $LOCATION/bdlg ]
then
	rm -rf $LOCATION/bdlg
	mkdir $LOCATION/bdlg
fi

clear
echo ""

while echo "Enter name of digit file to be converted to dlg format: "
do
	echo "-or- enter the word 'list' for a list"
	echo -n "> "
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
		echo "Files available for conversion:"
		(cd $LOCATION/dig; ls) 
		echo ""
	else
		# Check for existence of digit file
		if [ -r $LOCATION/dig/$ans ]
		then
			$BIN/Dclear.screen
			$BIN/Dscreen
			exec $ETC/dig.to.dlg $LOCATION/dig/$ans $LOCATION/bdlg/$ans
		else
			echo "Sorry, $ans does not exist"
		fi
	fi
done
