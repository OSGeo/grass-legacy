
------------------------------------------------------------------------------
src/D/prog_cmd/split/split.sh

#!/bin/sh

#    Written by the GRASS Team, April 89


# set defaults
CMD=Dcell
CMD2=Dcell                #--> RLG, SCS added 3/12/90
VIEW=vert
MAP1=
MAP2=
USAGE=no

# evaluate arguments
for i do
	case $i in
		cmd=*)
			CMD=`echo $i | sed s/cmd=//` ;;
		cmd2=*)                #--> RLG, SCS added 3/12/90
			CMD2=`echo $i | sed s/cmd2=//` ;;
		view=*)
			VIEW=`echo $i | sed s/view=//`
			if [ "$VIEW" = "horiz" ]
			then
				echo ""
			else
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
	echo "Usage: $0  mapname  mapname   "
	echo "Options:"
	echo "         {cmd=GRASS command}  default is Dcell   "
	echo "         {view=horiz}   "
	echo ""
	exit
fi

echo ""
Dclear.screen


#  Set up the two windows
if [ "$VIEW" = "vert" ]
then
#  split it: left (win1) and right (win2)
	Dnew  name=win1  top=100 bottom=0 left=0 right=49
	Dnew  name=win2  top=100 bottom=0 left=50 right=100
else
#  split it: top (win1) and bottom (win2)
	Dnew  name=win1  top=100 bottom=50 left=0 right=100
	Dnew  name=win2  top=49 bottom=0 left=0 right=100
fi


echo VIEW DISPLAYED: $VIEW
echo ""

Dchoose win1
echo WINDOW 1: $CMD $MAP1
$CMD  $MAP1

Dchoose win2
#  check for cmd2=                #--> RLG, SCS added 3/12/90
if [ "$CMD2" = "" ]
then
    echo WINDOW 2: $CMD $MAP2
    $CMD  $MAP2
else
    echo WINDOW 2: $CMD2 $MAP2
    $CMD2  $MAP2
fi


