#!/bin/sh
tput clear
CHECK.MAP
if test $? = 1
	then
	exit
fi
if test ! -w $GRAPH/etc/pattern
	then
	echo Error: You can not create patterns see the MAPGEN/GRASS administrator
	exit
fi

echo -n "Enter the name for this pattern: "
read name
if test -f $GRAPH/etc/pattern/$name
	then
	echo ERROR: Pattern $name exists
	exit
fi
echo -n "Enter the line spacing for this fill in centimeters [.1]: "
read space
echo -n "Enter the angle for the fill lines [0]: "
read angle
#echo -n "Enter the line type for this pattern [solid] or (dashed): "
#read line
line=solid

echo space=${space:-.1} > $GRAPH/etc/pattern/$name
if test $? = 0
	then
	echo angle=${angle:-0} >> $GRAPH/etc/pattern/$name
	echo line=${line:-solid} >> $GRAPH/etc/pattern/$name
else
	echo Error: You can not create patterns see the system admin
fi
tput clear
