#!/bin/sh
ETC=${GISBASE?}/etc

# ask the user for site file name
name=
Gask old "Enter name of a site list that you want displayed" \
	site_lists sites /tmp/sites.$$

# source the name file to get name, mapset into variables
. /tmp/sites.$$
rm -f /tmp/sites.$$

if [ "$mapset" = "" ]
then
	exit
fi

# Get type of symbol desired

t=0
while [ $t = 0 ]
do
	echo ""
	echo "What type of symbol would you like displayed at each site?"
	echo "1: box"
	echo "2: +"
	echo "3: diamond"
	echo "4: x"
	$ETC/echo -n "> "

	read request
	case $request in
		1) t=1 ; symb=box ;;
		2) t=1 ; symb=+ ;;
		3) t=1 ; symb=diamond ;;
		4) t=1 ; symb=x ;;
		box) t=1 ; symb=box ;;
		+) t=1 ; symb=+ ;;
		diamond) t=1 ; symb=diamond ;;
		x) t=1 ; symb=x ;;
		*) ;;
	esac
done

# Get color desired

t=0
while [ $t = 0 ]
do
	echo ""
	echo What color is desired for your $symb\'s
	echo " 1:  red        2:  orange       3:  yellow"
	echo " 4:  green      5:  blue         6:  indigo"
	echo " 7:  violet     8:  white        9:  gray"
	echo "10: black"
	$ETC/echo -n "> "

	read request
	case $request in
		1)  t=1 ; color=red ;;
		2)  t=1 ; color=orange ;;
		3)  t=1 ; color=yellow ;;
		4)  t=1 ; color=green ;;
		5)  t=1 ; color=blue ;;
		6)  t=1 ; color=indigo ;;
		7)  t=1 ; color=violet ;;
		8)  t=1 ; color=white ;;
		9)  t=1 ; color=gray ;;
		10) t=1 ; color=black ;;
		red)    t=1 ; color=red ;;
		orange) t=1 ; color=orange ;;
		yellow) t=1 ; color=yellow ;;
		green)  t=1 ; color=green ;;
		blue)   t=1 ; color=blue ;;
		indigo) t=1 ; color=indigo ;;
		violet) t=1 ; color=violet ;;
		white)  t=1 ; color=white ;;
		gray)   t=1 ; color=gray ;;
		black)  t=1 ; color=black ;;
		*) ;;
	esac
done

# Get size desired

t=0
while [ $t = 0 ]
do
	echo ""
	echo What size is desired for the $color $symb\'s
	echo   "(A number between 1 and 9)"
	$ETC/echo -n "> "

	read size
	case $size in
		[1-9])  t=1 ;;
		*)  ;;
	esac
done

echo Gsites -w \"$name in $mapset\"
echo Dpoints color=$color size=$size type=$symb

Gsites -w "$name in $mapset" | Dpoints color=$color size=$size type=$symb
