#!/bin/sh


### Read ~/.grassrc, setting the variables as csh variables
eval `sed '1,$s/^\([A-Za-z_]*\): *\(.*\)/\1="\2"; /' $HOME/.grassrc`
LOCATION=${GISDBASE?}/${LOCATION_NAME?}/${MAPSET?}
ETC=${GISBASE?}/etc
BIN=$GISBASE/bin

### Check for existence of directory which holds bdlg files
if [ ! -d $LOCATION/bdlg ]
then
	echo "No dlg files available for labeling"
	exit
fi
### turn off interrupts
##  trap "" 2 3
trap 'echo ""'  2 3

### Get name of bdlg file to be label
gotit=no
while [ $gotit != "yes" ]
do
	echo "Enter name of dlg file to be labeled: "
	echo "-or- enter the word 'list' for a list"
	$ETC/echo -n "> "
	read ans

	if ( test ".$ans" = "." )
	then
		exit
	elif ( test "$ans" = "list" )
	then
		echo ""
		echo "Files available for labeling:"
		(cd $LOCATION/bdlg; ls) 
		echo ""
	else
		# Check for existence of bdlg file
		if [ -r $LOCATION/bdlg/$ans ]
		then
			gotit=yes
		else
			echo "Sorry, $ans does not exist"
		fi
	fi
done

###  Set up the screen.
$BIN/Dclear.screen
$BIN/Dscreen

### Run dlglabel, using /tmp to hold temp. files
### with interrupts enbaled for dlglabel only
$ETC/dlglabel $LOCATION/bdlg/$ans /tmp/$$.n /tmp/$$.t

### If new labeled file is empty (i.e., user never wrote the edited file out),
### nothing more needs to be done
if ( test ! -s /tmp/$$.n )
then
rm -f /tmp/$$.t /tmp/$$.n
exit
fi

### Get a new name for the file

while (test -f /tmp/$$.n)
do
	echo ""
	echo "What name shall the labeled file be given?"
	echo "-or- enter 'list' for list of used names"
	$ETC/echo -n "> "
	read ans

	if ( test ".$ans" = "." )
	then
		echo ""
		echo "Please enter a name"
	elif ( test "$ans" = "list" )
	then
		echo ""
		echo "Existing bdlg files:"
		echo ""
		ls $LOCATION/bdlg
	else
		file=$LOCATION/bdlg/$ans
		if ( test -f $file )
		then
			echo ""
			echo "Sorry, then name <$ans> is already used"
		else
			mv /tmp/$$.n $file
		fi
	fi
done

### Cleanup
rm -f /tmp/$$.t /tmp/$$.n
