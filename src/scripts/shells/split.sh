:

#    Written by the GRASS Team, April 89


# set defaults
CMD=d.rast
CMD2=
VIEW=vert
MAP1=
MAP2=
USAGE=no

# evaluate arguments
for i do
	case $i in
		cmd=*)
			CMD=`echo $i | sed s/cmd=//` ;;
		cmd2=*)
			CMD2=`echo $i | sed s/cmd2=//` ;;
		view=*)
			VIEW=`echo $i | sed s/view=//`
			if [ ! "$VIEW" = "horiz" ]
			then
				USAGE=yes
			fi
			;;
		*)
			if [ "$MAP1" = "" ]
			then
				MAP1=$i
			else
				if [ "$MAP2" = "" ]
				then
					MAP2=$i
				else
					USAGE=yes
				fi
			fi
			;;
	esac
done

#  make sure there are maps to display
if [ "$MAP1" = "" ]
then
	USAGE=yes
fi

if [ "$MAP2" = "" ]
then
	USAGE=yes
fi

#  the commands didn't make sense
if [ "$USAGE" = "yes" ]
then
	echo ""
	echo "Usage: $0  mapname  mapname "
	echo "Options:"
	echo "         [cmd=GRASS command]    default is d.rast "
	echo "         [cmd2=GRASS command]   Command for second window "
	echo "         [view=horiz]   "
	echo ""
	exit
fi

if [ "$CMD2" = "" ]
then
	CMD2=$CMD
fi

echo ""

d.frame -e


#  Set up the two windows
if [ "$VIEW" = "vert" ]
then
#  split it: left (win1) and right (win2)
	d.frame -c frame=win1  at=0,100,0,49
	d.frame -c frame=win2  at=0,100,50,100
else
#  split it: top (win1) and bottom (win2)
	d.frame -c frame=win1  at=50,100,0,100
	d.frame -c frame=win2  at=0,49,0,100
fi


echo VIEW DISPLAYED: $VIEW
echo ""

d.frame -s frame=win1
echo WINDOW 1: $CMD $MAP1
$CMD  $MAP1

d.frame -s frame=win2
echo WINDOW 2: $CMD2 $MAP2
$CMD2  $MAP2



