#!/bin/sh

#@(#)dlg_t_cel.sh	2.1   6/26/87
#
# Converts (binary) dlg file to grid cell data layer
#
# 1 - Prompts user for bdlg file for conversion
#
# 2 - Runs entire process in background
#
# 3 - Sends user mail upon completion

eval `sed '1,$s/^\([A-Za-z_]*\): *\(.*\)/\1="\2"; /' $HOME/.grassrc`

BIN=$GISBASE/bin
ETC=$GISBASE/etc
MAPDIR=$GISDBASE/$LOCATION_NAME/$MAPSET
CELLDIR=$MAPDIR/cell



# Check for existence of directory which holds cell files
if [ ! -d $LOCATION/cell ]
then
	mkdir $LOCATION/cell
fi



if [ $# = 1 ]
then
	MAP=$1
else
	clear
	echo "DLG to GRID-CELL conversion routine"
	echo ""

	while  echo  "What file do you want converted - or -"
	do
		echo -n "enter 'list' for a listing of dlg files: "
		read MAP

		if ( test ".$MAP" = "." )
		then
			echo  ""
			echo  "No name given."
			echo  ""
			exit
		
		elif ( test "$MAP" = "list" )
		then
			echo ""
			echo "Available dlg files:"
			echo ""
			ls  $MAPDIR/bdlg
			echo ""
		else
			break
		fi
	done

fi


echo ""
echo "Making cell  file $MAP "

$ETC/dlg_t_bmif $MAP  | sort | $ETC/bmif_to_cell $MAP

