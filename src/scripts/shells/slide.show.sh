:

# $Id$
# Markus Neteler 1/2000: added prefix character option to view only select files
# Angus Carr 1999: bugfix on MAPSET

if [ $? != 0 ]
then
	exit 1
fi

#d.colormode fixed

# set defaults
DOWN=3
ACROSS=4

if [ "$1" = "help" ] 
then
  echo "Slide show of GRASS raster maps."
  echo "Options: [across=#maps_across] [down=#_maps_down] [prefix=character[s]]"
  echo "Defaults:"
  echo "   across =   $ACROSS"
  echo "   down   =   $DOWN"
  echo "   prefix = * (show all maps. Specify character(s) to view selected maps only)"
 exit
fi
# set gis variables
eval `g.gisenv`
LOCATION=$GISDBASE/$LOCATION_NAME/$MAPSET


# evaluate arguments
for i do
	case $i in
		prefix=*) 
			PREFIX=`echo $i | sed s/prefix=//`;;
		down=*)
			DOWN=`echo $i | sed s/down=//` ;;
		height=*)
			DOWN=`echo $i | sed s/height=//` ;;
		across=*)
			ACROSS=`echo $i | sed s/across=//` ;;
		width=*)
			ACROSS=`echo $i | sed s/width=//` ;;
		mapsets=*)
			MAPSETS=`echo $i | sed s/mapsets=//` ;;
		mapset=*)
			MAPSETS=`echo $i | sed s/mapset=//` ;;
		*)
			echo ""
			echo "Unrecognized option: $i"
			echo Options: [across=#maps_across] [down=#_maps_down] [prefix=character[s]] 
			echo Defaults:
			echo "   across =   $ACROSS"
			echo "   down   =   $DOWN"
			echo "   prefix = * (show all maps)"
			exit
	esac
done

d.frame -e
# figure height and widths of the windows
avail_width=`expr 99 - $ACROSS`
avail_height=`expr 99 - $DOWN`
map_width=`expr $avail_width / $ACROSS`
wind_height=`expr $avail_height / $DOWN`
label_height=`expr $wind_height / 10`
map_height=`expr $wind_height - $label_height`

# generate the needed windows
at_horiz=0
left=1
while [ $at_horiz -lt $ACROSS ]
do
	at_vert=0
	top=99
	right=`expr $left + $map_width`
	while [ $at_vert -lt $DOWN ]
	do
		bottom=`expr $top - $map_height`
		d.frame -c map.$at_horiz.$at_vert at=$bottom,$top,$left,$right
		top=$bottom
		bottom=`expr $top - $label_height`
		d.frame -c lab.$at_horiz.$at_vert at=$bottom,$top,$left,$right
		at_vert=`expr $at_vert + 1`
		top=`expr $bottom - 1`
	done
	at_horiz=`expr $at_horiz + 1`
	left=`expr $right + 1`
done

# Get list of current mapsets

if [ x$MAPSETS = x ]
then
	if [ -r $LOCATION/SEARCH_PATH ]
	then
		MAPSETS=`cat $LOCATION/SEARCH_PATH`
	else
#		MAPSETS=PERMANENT # fix by Agus Carr 10/99
		MAPSETS=`echo $MAPSET PERMANENT`
	fi
fi

# Draw the maps

atnum=0
totmaps=`expr $ACROSS \* $DOWN`

for mapset in $MAPSETS
do
	cd $LOCATION/../$mapset
	if [ ! -d cell ]
	then
		continue
	fi
	for i in MASK `ls cell/$PREFIX*`
	do
		i=`echo $i | sed 's/cell\///'`
		if [ ! $i = "MASK" ]
		then
			atnum=`expr $atnum % $totmaps`
			at_vert=`expr $atnum % $DOWN`
			at_hori=`expr $atnum / $DOWN`

			d.frame -s lab.$at_hori.$at_vert
			d.erase
			echo $i in $mapset | d.text size=80

			d.frame -s map.$at_hori.$at_vert
			d.erase
			d.rast $i

			atnum=`expr $atnum + 1`
		fi
	done
done
d.frame -s full_screen
